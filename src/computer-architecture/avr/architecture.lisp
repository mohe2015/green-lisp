;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; green-lisp - an eco-friendly lisp
;;;; Copyright (C) 2019 Moritz Hedtke <Moritz.Hedtke@t-online.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

;; atmega128 http://ww1.microchip.com/downloads/en/DeviceDoc/doc2467.pdf
;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_instruction_list.html

;;(cserial-port:open-serial "/dev/ttyACM0" :baud-rate 38400 :parity :even)

;;(cserial-port:write-serial-string "Test" #v1:0

;; HACK to prevent "also exports" error
(when (find-package :green-lisp.avr.architecture)
  (do-symbols (symbol :green-lisp.avr.architecture)
    (unexport symbol :green-lisp.avr.architecture)))

(defpackage :green-lisp.avr.architecture
  (:use :common-lisp :cserial-port :green-lisp.bits :green-lisp.logger)
  (:import-from :green-lisp.bits :bit-reader :bit-reader-bits :read-bit :file->bit-reader :bit-writer :write-bit :bit-writer->bytes)
  (:import-from :green-lisp.logger :+trace+ :+debug+ :+info+ :+warning+ :+error+)
  (:shadowing-import-from :green-lisp.logger :log))
(in-package :green-lisp.avr.architecture)

(setf *on-package-variance* '(:warn () :error ()))

(defclass io-register ()
  ((name :initarg :name
	 :reader register-name)
   (data-memory-address :initarg :data-memory-address ;; this is the big address
			:reader register-data-memory-address)))
(export 'io-register)

(defclass register (io-register)
  ())
(export 'register)

(defclass avr-architecture ()
  ())
(export 'avr-architecture)

(defclass simulated-avr-architecture (avr-architecture)
  ((registers :initarg :registers
	      :reader registers)))
(export 'simulated-avr-architecture)

(defclass register-pair ()
  ())
(export 'register-pair)

(defclass address ()
  ((value :type integer
	  :initarg :value
	  :reader value)))
(export 'address)
(export 'value)

(defclass constant ()
  ())
(export 'constant)

(defmacro define-register (name address type)
  `(let ((register (make-instance ',type :name ',name :data-memory-address ,address)))
     (export (defparameter ,name register))
     register))

(defmacro define-registers (&rest registers)
  `(make-instance 'simulated-avr-architecture :registers (list
		  ,@(loop for register in registers collect
			  `(define-register ,(nth 0 register) ,(nth 1 register) ,(nth 2 register))))))

(define-registers
  (R0     #x00 register)
  (R1     #x01 register)
  (R2     #x02 register)
  (R3     #x03 register)
  (R4     #x04 register)
  (R5     #x05 register)
  (R6     #x06 register)
  (R7     #x07 register)
  (R8     #x08 register)
  (R9     #x09 register)
  (R10    #x0a register)
  (R11    #x0b register)
  (R12    #x0c register)
  (R13    #x0d register)
  (R14    #x0e register)
  (R15    #x0f register)
  (R16    #x10 register)
  (R17    #x11 register)
  (R18    #x12 register)
  (R19    #x13 register)
  (R20    #x14 register)
  (R21    #x15 register)
  (R22    #x16 register)
  (R23    #x17 register)
  (R24    #x18 register)
  (R25    #x19 register)
  (R26    #x1a register)
  (R27    #x1b register)
  (R28    #x1c register)
  (R29    #x1d register)
  (R30    #x1e register)
  (R31    #x1f register)
  (PINF   #x20 io-register)
  (PINE   #x21 io-register)
  (DDRE   #x22 io-register)
  (PORTE  #x23 io-register)
  (ADCL   #x24 io-register)
  (ADCH   #x25 io-register)
  (ADCSRA #x26 io-register)
  (ADMUX  #x27 io-register)
  (ACSR   #x28 io-register)
  (UBRR0L #x29 io-register)
  (UCSR0B #x2a io-register)
  (UCSR0A #x2b io-register)
  (UDR0   #x2c io-register)
  (SPCR   #x2d io-register)
  (SPSR   #x2e io-register)
  (SPDR   #x2f io-register)
  (PIND   #x30 io-register)
  (DDRD   #x31 io-register)
  (PORTD  #x32 io-register)
  (PINC   #x33 io-register)
  (DDRC   #x34 io-register)
  (PORTC  #x35 io-register)
  (PINB   #x36 io-register)
  (DDRB   #x37 io-register)
  (PORTB  #x38 io-register)
  (PINA   #x39 io-register)
  (DDRA   #x3a io-register)
  (PORTA  #x3b io-register)
  (EECR   #x3c io-register)
  (EEDR   #x3d io-register)
  (EEARL  #x3e io-register)
  (EEARH  #x3f io-register)
  (SFIOR  #x40 io-register)
  (WDTCR  #x41 io-register)
  (OCDR   #x42 io-register)
  (OCR2   #x43 io-register)
  (TCNT2  #x44 io-register)
  (TCCR2  #x45 io-register)
  (ICR1L  #x46 io-register)
  (ICR1H  #x47 io-register)
  (OCR1BL #x48 io-register)
  (OCR1BH #x49 io-register)
  (OCR1AL #x4a io-register)
  (ORC1AH #x4b io-register)
  (TCNT1L #x4c io-register)
  (TCNT1H #x4d io-register)
  (TCCR1B #x4e io-register)
  (TCCR1A #x4f io-register)
  (ASSR   #x50 io-register)
  (OCR0   #x51 io-register)
  (TCNT0  #x52 io-register)
  (TCCR0  #x53 io-register)
  (MCUCSR #x54 io-register)
  (MCUCR  #x55 io-register)
  (TIFR   #x56 io-register)
  (TIMSK  #x57 io-register)
  (EIFR   #x58 io-register)
  (EIMSK  #x59 io-register)
  (EICRB  #x5a io-register)
  (RAMPZ  #x5b io-register)
  (XDIV   #x5c io-register)
  (SPL    #x5d io-register)
  (SPH    #x5e io-register)
  (SREG   #x5f io-register))
;; TODO missing things




#|

(define-pin 21 vcc)
(define-pin 22 ground)
(define-pin 47 (port a 4))
(define-pin 48 (port a 3))
(define-pin 49 (port a 2))
(define-pin 50 (port a 1))
(define-pin 51 (port a 0))
; ... page 5				; ;

; 32 registers				; ;
(define-register (status-register 7) global-interrupt-enable)
(define-register (status-register 6) bit-copy-storage)
(define-register (status-register 5) half-carry-flag)
(define-register (status-register 4) sign-bit)
(define-register (status-register 3) twos-complement-overflow-flag)
(define-register (status-register 2) negative-flag)
(define-register (status-register 1) zero-flag)
(define-register (status-register 0) carry-flag)
(define-register (register #x00 r0))
(define-register (register #x01 r1))
(define-register (register #x02 r2))
; ... X Y Z register			; ;
(define-register (stack-pointer sph))
(define-register (stack-pointer spl))

;; RAMPZ

(define-flash 128000)

(define-eeprom 4096)
;; EEARH EEARL EEDR EECR

(define-sram 4096)

(define-external-sram)

(define-memory-locations (registers 32) (io-memory 64) (extended-io-memory 160) (internal-data-sram 4096))

(define-real-time-counter ) ;

(define-timers ) ; TODO page 35

(define-usarts ); 

(define-two-wire-serial-interface ) ;

(define-analog-digital-converter ) ;

(define-watchdog-timer ) ;

(define-spi-serial-port ) ;

(define-jtag-interface ) ; page 48

(define-sleep-modes (idle power-down power-save adc-noise-reduction standby extended-standby)) ;; TODO page 44

(define-reset) ; page 49

(define-qtouch-library ) ;;

;; page 59
(define-interrupt-vector #x0000 reset)
(define-interrupt-vector #x0002 int0)

(define-interrupt-vectors
(reset
external-interrupt-0
external-interrupt-1
external-interrupt-2
external-interrupt-3
external-interrupt-4
external-interrupt-5
external-interrupt-6
external-interrupt-7
timer-2-compare-match
timer-2-overflow
timer-1-capture-event
timer-1-compare-match-a
timer-1-compare-match-b
timer-1-overflow
timer-0-compare-match
timer-0-overflow
serial-transfer-complete
usart-0-receive-complete
usart-0-data-register-empty
usart-0-transfer-complete
adc-conversion-complete
eeprom-ready
analog-comparator
timer-1-compare-match-c
;; ...


;; external interrupts page 89

;; a-bit timer with pwm and asynchronous operation page 92

;; output compare modulator page 160

;; serial peripheral interface p 162

;; usart page 170

;; two-wire serial interface page 197

;; analog comparator page 227

;; analog to digital converter page 230

;; jtag interface and on-chip debug system page 246

;; boot loader support - read-while-write self-programming page 273

;; parallel programming page 290

|#
