
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
    ,@(pop-and-transmit)
    
    (rjmp :end)))
  
(bit-writer->file (compile-asm *program*) "test.bin")

(defun main ()
  (uiop:run-program "avr-objcopy -I binary -O ihex test.bin test.ihex && avrdude -c stk500v2 -P /dev/serial/by-id/usb-16c0_092e-if00 -p atmega128 -B 2 -U flash:w:test.ihex" :output *standard-output* :force-shell t :error-output *standard-output*)
  (let ((serial (serial-connect)))
    (sleep 3)
    (serial-write serial (print (random 256))) ;; R0
    (print (serial-read serial)) ;; R0
    (serial-close serial)))

;; (random 256)
