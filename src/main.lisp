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

;; TODO FIXME
(defparameter UDRE0 5)
(defparameter RXC0 7)

;; AAAH The stack is not initialized
(defparameter *program*
  `(
    ,@(loop repeat 35 collect
	    `(jmp :entry0))

    (label :transmit)
    (sbis UCSR0A UDRE0)
    (rjmp :transmit)
    (out UDR0 R24)
    (ret)

    (label :receive)
    (sbis UCSR0A RXC0)
    (rjmp :receive)
    (in R24 UDR0)
    (ret)
    
    (label :entry0)
    ;; stack pointer init
    (ldi R24 16)
    (out SPH R24)
    (ldi R24 0)
    (out SPL R24)
    ;; leds init
    (ldi R24 #x80)
    (out DDRB R24)
    (out PORTB R24)
    (ldi R24 #xff)
    (out DDRE R24)
    (out DDRC R24)
    ;; usart0 init
    (ldi R24 25) ;; 16000000/16/38400-1
    (out UBRR0L R24)
    (ldi R24 24) ;; (1<<RXEN 3)|(1<<TXEN 4)
    (out UCSR0B R24)
    ;; leds on
    (ldi R24 #xff)
    (out PORTE R24)

    (call :receive) ;; TODO relative call
    (out PORTE R24)
    
    ;; send some test data
    (ldi R24 (char-code #\h))
    (call :transmit)
    (ldi R24 (char-code #\a))
    (call :transmit)
    (ldi R24 (char-code #\l))
    (call :transmit)
    (ldi R24 (char-code #\o))
    (call :transmit)
    
    (label :end)
    
    (call :receive) ;; TODO relative call
    (out PORTE R24)

    (rjmp :end)))
  
(bit-writer->file (compile-asm *program*) "test.bin")

(defun main ()
  (uiop:run-program "avr-objcopy -I binary -O ihex test.bin test.ihex && avrdude -c stk500v2 -P /dev/ttyACM0 -p atmega128 -B 2 -U flash:w:test.ihex" :output *standard-output* :force-shell t :error-output *standard-output*)
  (let ((serial (serial-connect)))
    (sleep 2)
    (print (serial-write serial 0))
    (print (serial-read serial))
    (print (serial-read serial))
    (print (serial-read serial))
    (print (serial-read serial))
    (print (serial-write serial #xf0))
    (serial-close serial)))

;; (random 256)
