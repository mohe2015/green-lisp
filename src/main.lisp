;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; green-lisp - an eco-friendly lisp
;;;; Copyright (C) 2019 Moritz Hedtke <Moritz.Hedtke@t-online.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage green-lisp
  (:use :cl :green-lisp.avr.instructions :green-lisp.avr.architecture)
  (:import-from :green-lisp.bits :file->bit-reader :bit-writer :bit-writer->bytes :bit-writer->file)
  (:shadowing-import-from :green-lisp.logger :log))
(in-package :green-lisp)

(defparameter *program*
  `(
    ,@(loop repeat 18 collect
	    `(jmp :entry0))
    
    (jmp :uart_interrupt)
    
    ,@(loop repeat 16 collect
	    `(jmp :entry0))    
    
    (label :uart_interrupt)

    (_push R24)
    (in R24 UDR0) ;; USART Data Register UDR
    (out UDR0 R24)
    (_pop R24)
    (reti)



    (label :entry0)

    (bset 7) ;; TODO FIXME (sei) should also work
    
    (ldi R24 #x19)
    (out UBRR0L R24) ;; USART Baud Rate Register

    (ldi R24 #x98)
    (out UCSR0B R24) ;; USART Control and Status Register B
    
    (label :loop)
    (jmp :loop)
    
    #|      ;; some general outputs
    (ldi 24 #x80)
    (out +DDRB+ 24) ;; TODO FIXME enforce types
    (out +PORTB+ 24)
    
    (label :main)
    ;; red
    (ldi 24 #xF0)
    (out +DDRE+ 24)
    (out +PORTE+ 24)
    
    ;; green
    (ldi 24 #x0F)
    (out +DDRC+ 24)
    (out +PORTC+ 24)
    
    ;; defun delay
    ;; constant numbers
    (ldi 20 #x00)
    (ldi 21 #x01)
    
    (ldi 22 #x00)
    (ldi 23 #x00)
    (ldi 24 #x00)
    
    (label :target1)
    (adc 22 21)
    (adc 23 20)
    (adc 24 20)
    (brcc :target1) ;; :target

    ;; red
    (ldi 24 #x0F)
    (out +DDRE+ 24)
    (out +PORTE+ 24)

    ;; green
    (ldi 24 #xF0)
    (out +DDRC+ 24)
    (out +PORTC+ 24)  


    ;; defun delay
    ;; constant numbers
    (ldi 20 #x00)
    (ldi 21 #x01)
    
    (ldi 22 #x00)
    (ldi 23 #x00)
    (ldi 24 #x00)

    (label :target2)
    (adc 22 21)
    (adc 23 20)
    (adc 24 20)
    (brcc :target2)

    (rjmp :main)))|#

    ))

(defparameter *labels* (make-hash-table))

(let ((offset 0))
  (loop for element in *program* do
    (if (eq 'label (car element))
	(setf (gethash (car (cdr element)) *labels*) offset)
	(setf offset (+ offset (instruction-size (car element)))))))

(defparameter *bit-writer* (make-instance 'bit-writer))

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

(maphash #'print-hash-entry *labels*)

(let ((offset 0))
  (loop for element in *program* do
    (unless (eq 'label (car element))
      (setf offset (+ offset (instruction-size (car element))))
      (cond ((or (eq 'jmp (car element)))
	     (write-instruction (car element) *bit-writer* (make-instance 'address :internal-value (gethash (car (cdr element)) *labels*))))
	    ((or (eq 'rjmp (car element)) (eq 'brcc (car element)))
	     (write-instruction (car element) *bit-writer* (make-instance 'address :internal-value (- (gethash (car (cdr element)) *labels*) offset))))
	    (t
	     (apply #'write-instruction (cons (car element) (cons *bit-writer* (mapcar #'eval (cdr element)))))))
      )))

(bit-writer->file *bit-writer* "test.bin")


;;(let ((i (file->bit-reader #P"/home/moritz/Documents/green-lisp/binary.bin")))
;;  (loop repeat 30 do (read-any-instruction i)))

;;(uiop:run-program "avr-objcopy -I binary -O ihex test.bin test.ihex && avrdude -c stk500v2 -P /dev/ttyACM0 -p atmega128 -B 2 -U flash:w:test.ihex" :output *standard-output* :force-shell t :error-output *standard-output*)
