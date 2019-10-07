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
    (in R24 UDR0)
    (out UDR0 R24)
    (_pop R24)
    (reti)

    (label :entry0)
    (bset 7) ;; TODO FIXME (sei) should also work
    (ldi R24 #x19)
    (out UBRR0L R24)
    (ldi R24 #x98)
    (out UCSR0B R24)
    (label :loop)
    (jmp :loop)))
  
(bit-writer->file (compile-asm *program*) "test.bin")

;;(uiop:run-program "avr-objcopy -I binary -O ihex test.bin test.ihex && avrdude -c stk500v2 -P /dev/ttyACM0 -p atmega128 -B 2 -U flash:w:test.ihex" :output *standard-output* :force-shell t :error-output *standard-output*)
