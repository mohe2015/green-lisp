
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
  (:use :cl21 :green-lisp.avr.instructions :green-lisp.avr.architecture)
  (:import-from :green-lisp.bits :file->bit-reader :bit-writer :bit-writer->bytes :bit-writer->file)
  (:import-from :green-lisp.compiler :compile-asm :label)
  (:import-from :green-lisp.serial-interface :serial-connect :serial-read :serial-write :serial-close)
  (:shadowing-import-from :green-lisp.logger :log)
  (:export :main))
(in-package :green-lisp)

#|
(maybe load random values into register?)
(push all registers and sreg and pc (later add offset))
(run target instruction)
(push sreg, all registers and pc (later substract offset)
(pop stack and send using uart)
;; client can then analyze the data
;; do this multiple times using fuzzing
|#
;; Another idea would be to flash one instruction using the serial cable so you don't need to reflash the whole thing again and again.

;; TODO FIXME
(defparameter UDRE0 5)
(defparameter RXC0 7)

(defmacro pop-and-transmit ()
  (let ((gen-label (intern (symbol-name (gensym)) :keyword)))
    ``((label ,,gen-label)
       (sbis UCSR0A UDRE0)
       (rjmp ,,gen-label)
       (_pop R31)
       (out UDR0 R31))))

(defmacro receive-and-push ()
  (let ((gen-label (intern (symbol-name (gensym)) :keyword)))
    ``((label ,,gen-label)
       (sbis UCSR0A RXC0)
       (rjmp ,,gen-label)
       (in R31 UDR0)
       (_push R31))))

;; AAAH The stack is not initialized
(defparameter *program*
  `(
    ,@(loop repeat 35 collect
	    `(jmp :entry0))
    
    (label :entry0)
    ;; stack pointer init
    (ldi R31 16)
    (out SPH R31)
    (ldi R31 0)
    (out SPL R31)
    ;; leds init
    (ldi R31 #x80)
    (out DDRB R31)
    (out PORTB R31)
    (ldi R31 #xff)
    (out DDRE R31)
    (out DDRC R31)
    ;; usart0 init
    (ldi R31 103) ;; 16000000/16/9600-1
    (out UBRR0L R31)
    (ldi R31 24) ;; (1<<RXEN 3)|(1<<TXEN 4)
    (out UCSR0B R31)
    ;; leds on
    (ldi R31 #xff)
    (out PORTE R31)

    (label :end)
    
    
    

    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)

    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)

    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)

    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)
    ,@(receive-and-push)

    (_pop R31)
    (_pop R30)
    (_pop R29)
    (_pop R28)
    (_pop R27)
    (_pop R26)
    (_pop R25)
    (_pop R24)
    (_pop R23)
    (_pop R22)
    (_pop R21)
    (_pop R20)
    (_pop R19)
    (_pop R18)
    (_pop R17)
    (_pop R16)
    (_pop R15)
    (_pop R14)
    (_pop R13)
    (_pop R12)
    (_pop R11)
    (_pop R10)
    (_pop R9)
    (_pop R8)
    (_pop R7)
    (_pop R6)
    (_pop R5)
    (_pop R4)
    (_pop R3)
    (_pop R2)
    (_pop R1)

    ;;(_pop R0)
    ;;(out SPH R0)

    ;;(_pop R0)
    ;;(out SPL R0)

    (_pop R0)
    (out SREG R0)

    (_pop R0)

    (ldi R0 #x13)

    (_push R0)

    (in R0 SREG)
    (_push R0)

    ;;(in R0 SPL)
    ;;(_push R0)
    
    ;;(in R0 SPH)
    ;;(_push R0)

    (_push R1)
    (_push R2)
    (_push R3)
    (_push R4)
    (_push R5)
    (_push R6)
    (_push R7)
    (_push R8)
    (_push R9)
    (_push R10)
    (_push R11)
    (_push R12)
    (_push R13)
    (_push R14)
    (_push R15)
    (_push R16)
    (_push R17)
    (_push R18)
    (_push R19)
    (_push R20)
    (_push R21)
    (_push R22)
    (_push R23)
    (_push R24)
    (_push R25)
    (_push R26)
    (_push R27)
    (_push R28)
    (_push R29)
    (_push R30)
    (_push R31)

    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)

    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)

    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)

    ,@(pop-and-transmit)
    ,@(pop-and-transmit)
    ,@(pop-and-transmit)

    (label :end)
    
    (rjmp :end)))

(defun main ()
  (let ((file-bin (merge-pathnames "test.bin" (asdf:system-source-directory :green-lisp)))
	(file-ihex (merge-pathnames "test.ihex" (asdf:system-source-directory :green-lisp))))
    (bit-writer->file (compile-asm *program*) file)
    (uiop:run-program (format nil "avr-objcopy -I binary -O ihex ~s ~s && avrdude -c stk500v2 -P /dev/serial/by-id/usb-16c0_092e-if00 -p atmega128 -B 2 -U flash:w:~s" file-bin file-ihex file-ihex) :output *standard-output* :force-shell t :error-output *standard-output*)
    (let ((serial (serial-connect))
	  (old-values (make-hash-table))
	  (new-values (make-hash-table)))
      (sleep 1)
      
      (let ((r0-value (random 256)))
	(setf (getf old-values :r0) r0-value)
	(serial-write serial r0-value)) ;; R0
      (serial-write serial 0)  ;; SREG
      ;;(serial-write serial 0)  ;; SPL
      ;;(serial-write serial 16) ;; SPH
      (doeach (register '(R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17 R18 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 R30 R31))
	(let ((register-value (random 256)))
	  (format t "~a ~x | " register register-value)
	  (serial-write serial register-value))) ;; R1 - R31

      (format t "~%")
      
      (doeach (register '(R31 R30 R29 R28 R27 R26 R25 R24 R23 R22 R21 R20 R19 R18 R17 R16 R15 R14 R13 R12 R11 R10 R9 R8 R7 R6 R5 R4 R3 R2 R1))
	(let ((register-value (serial-read serial)))
	  (format t "~a ~x | " register register-value))) ;; R31 - R1

      ;;(print (serial-read serial)) ;; SPH
      ;;(print (serial-read serial)) ;; SPL
      (let ((sreg-value (serial-read serial)))
	(format t "sreg ~x | " sreg-value)) ;; SREG
      (let ((r0-value (serial-read serial)))
	(format t "r0 ~x~%" r0-value));; R0

      (serial-close serial))))
