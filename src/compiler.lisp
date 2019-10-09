;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; green-lisp - an eco-friendly lisp
;;;; Copyright (C) 2019 Moritz Hedtke <Moritz.Hedtke@t-online.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
(defpackage green-lisp.compiler
  (:use :cl21 :green-lisp.avr.instructions :green-lisp.avr.architecture)
  (:import-from :green-lisp.bits :file->bit-reader :bit-writer :bit-writer->bytes :bit-writer->file)
  (:shadowing-import-from :green-lisp.logger :log)
  (:export :compile-asm :label))
(in-package :green-lisp.compiler)

(export 'label)

(defun compile-asm (code)
  (let ((labels (make-hash-table))
	(bit-writer (make-instance 'bit-writer)))
	
    (let ((offset 0))
      (loop for element in code do
	(if (eq 'label (car element))
	    (setf (gethash (car (cdr element)) labels) offset)
	    (setf offset (+ offset (instruction-size (car element)))))))
  
    (let ((offset 0))
      (loop for element in code do
	(unless (eq 'label (car element))
	  (setf offset (+ offset (instruction-size (car element))))
	  (cond ((or (eq 'jmp (car element)) (eq 'call (car element)))
		 (write-instruction (car element) bit-writer (make-instance 'address :internal-value (gethash (car (cdr element)) labels))))
		((or (eq 'rjmp (car element)) (eq 'brcc (car element)))
		 (write-instruction (car element) bit-writer (make-instance 'address :internal-value (- (gethash (car (cdr element)) labels) offset))))
		(t
		 (apply #'write-instruction (cons (car element) (cons bit-writer (map #'eval (cdr element))))))))))
    bit-writer))
