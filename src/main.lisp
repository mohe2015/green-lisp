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

    (label :transmit)
    (sbis UCSR0A UDRE0)
    (rjmp :transmit)
    (out UDR0 R31)
    (ret)

    (label :receive)
    (sbis UCSR0A RXC0)
    (rjmp :receive)
    (in R31 UDR0)
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

    #|before_R0_receive|#(call :receive) (mov R0 R31)
    #|before_R1_receive|#(call :receive) (mov R1 R31)
    #|before_R2_receive|#(call :receive) (mov R2 R31)
    #|before_R3_receive|#(call :receive) (mov R3 R31)
    #|before_R4_receive|#(call :receive) (mov R4 R31)
    #|before_R5_receive|#(call :receive) (mov R5 R31)
    #|before_R6_receive|#(call :receive) (mov R6 R31)
    #|before_R7_receive|#(call :receive) (mov R7 R31)
    #|before_R8_receive|#(call :receive) (mov R8 R31)
    #|before_R9_receive|#(call :receive) (mov R9 R31)
    #|before_R10_receive|#(call :receive) (mov R10 R31)
    #|before_R11_receive|#(call :receive) (mov R11 R31)
    #|before_R12_receive|#(call :receive) (mov R12 R31)
    #|before_R13_receive|#(call :receive) (mov R13 R31)
    #|before_R14_receive|#(call :receive) (mov R14 R31)
    #|before_R15_receive|#(call :receive) (mov R15 R31)
    #|before_R16_receive|#(call :receive) (mov R16 R31)
    #|before_R17_receive|#(call :receive) (mov R17 R31)
    #|before_R18_receive|#(call :receive) (mov R18 R31)
    #|before_R19_receive|#(call :receive) (mov R19 R31)
    #|before_R20_receive|#(call :receive) (mov R20 R31)
    #|before_R21_receive|#(call :receive) (mov R21 R31)
    #|before_R22_receive|#(call :receive) (mov R22 R31)
    #|before_R23_receive|#(call :receive) (mov R23 R31)
    #|before_R24_receive|#(call :receive) (mov R24 R31)
    #|before_R25_receive|#(call :receive) (mov R25 R31)
    #|before_R26_receive|#(call :receive) (mov R26 R31)
    #|before_R27_receive|#(call :receive) (mov R27 R31)
    #|before_R28_receive|#(call :receive) (mov R28 R31)
    #|before_R29_receive|#(call :receive) (mov R29 R31)
    #|before_R30_receive|#(call :receive) (mov R30 R31)
    #|before_random_stack_value_receive|#(call :receive) (mov R31 R31)
    #|push_SPH|#          (push SPH)
    #|push_SPL|#          (push SPL)
    #|push_SREG|#         (push SREG)
    #|random_stack_value|#(push R31)
    #|before_R31_receive|#(call :receive) (mov R31 R31)
    #|target-instruction|#(ldi R22 #x33)
    #|after_SREG|#        (push SREG)
    #|after_R31_send|#    (mov R31 R31)   (call :transmit)
    #|after_SPL_send|#    (mov R31 SPL)   (call :transmit)
    #|after_SPH_send|#    (mov R31 SPH)   (call :transmit)
    #|after_SREG_send|#   (pop R31)       (call :transmit)
    
    
    (label :end)
    
    (call :receive) ;; TODO relative call
    (out PORTE R31)

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
