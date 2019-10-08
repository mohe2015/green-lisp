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

;; AAAH The stack is not initialized
(defparameter *program*
  `(
    ,@(loop repeat 35 collect
	    `(jmp :entry0))

    (label :pop_and_transmit)
    (sbis UCSR0A UDRE0)
    (rjmp :pop_and_transmit)
    (pop R31)
    (out UDR0 R31)
    (ret)

    (label :receive_and_push)
    (sbis UCSR0A RXC0)
    (rjmp :receive_and_push)
    (in R31 UDR0)
    (push R31)
    (ret)
    
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
    (ldi R31 25) ;; 16000000/16/38400-1
    (out UBRR0L R31)
    (ldi R31 24) ;; (1<<RXEN 3)|(1<<TXEN 4)
    (out UCSR0B R31)
    ;; leds on
    (ldi R31 #xff)
    (out PORTE R31)

    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)

    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)

    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)

    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)
    (call :receive_and_push)

    (pop R31)
    (pop R30)
    (pop R29)
    (pop R28)
    (pop R27)
    (pop R26)
    (pop R25)
    (pop R24)
    (pop R23)
    (pop R22)
    (pop R21)
    (pop R20)
    (pop R19)
    (pop R18)
    (pop R17)
    (pop R16)
    (pop R15)
    (pop R14)
    (pop R13)
    (pop R12)
    (pop R11)
    (pop R10)
    (pop R9)
    (pop R8)
    (pop R7)
    (pop R6)
    (pop R5)
    (pop R4)
    (pop R3)
    (pop R2)
    (pop R1)
    (pop R0)
    (pop SPH)
    (pop SPL)
    (pop SREG)

    (ldi R13 17)

    (push SREG)
    (push SPL)
    (push SPH)
    (push R0)
    (push R1)
    (push R2)
    (push R3)
    (push R4)
    (push R5)
    (push R6)
    (push R7)
    (push R8)
    (push R9)
    (push R10)
    (push R11)
    (push R12)
    (push R13)
    (push R14)
    (push R15)
    (push R16)
    (push R17)
    (push R18)
    (push R19)
    (push R20)
    (push R21)
    (push R22)
    (push R23)
    (push R24)
    (push R25)
    (push R26)
    (push R27)
    (push R28)
    (push R29)
    (push R30)
    (push R31)

    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)

    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)

    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)

    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    (call :pop_and_transmit)
    
    
    (label :end)
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
