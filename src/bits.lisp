;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; green-lisp - an eco-friendly lisp
;;;; Copyright (C) 2019 Moritz Hedtke <Moritz.Hedtke@t-online.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
;; http://www.gigamonkeys.com/book/files-and-file-io.html chapter 24

(defpackage :green-lisp.bits
  (:use :cl21)
  (:export :bit-reader :bit-reader-bits :read-bit :file->bit-reader :file->byte-pairs :file->bytes :byte->bits :bytes->bits :file->bits
	   :bit-writer :write-bit :bit-writer->bytes :bit-writer->file))
(in-package :green-lisp.bits)

(defun file->byte-pairs (filename)
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (loop for byte = (list (read-byte in nil) (read-byte in nil)) while (and (car byte) (cdr byte)) collect byte)))

(defun file->bytes (filename)
  (apply
   #'concatenate
   (cons
    'list
    (map
     (lambda (i)
       (list (cadr i) (car i)))
     (file->byte-pairs filename)))))

(defun byte->bits (byte)
  (declare ((unsigned-byte 8) byte))
  (let ((bits '()))
    (dotimes (index 8 bits)
      (push (if (logbitp index byte) 1 0) bits))))

(defun bytes->bits (bytes)
  (apply #'concatenate (cons 'list (map #'byte->bits bytes))))

(defun file->bits (filename)
  (bytes->bits (file->bytes filename)))

(defclass bit-reader ()
  ((bits
    :initarg :bits
    :accessor bit-reader-bits
    :initform '())))

(defun file->bit-reader (filename)
  (make-instance 'bit-reader :bits (file->bits filename)))

(defmethod read-bit ((bit-reader bit-reader))
  (pop (bit-reader-bits bit-reader)))


(defclass bit-writer ()
  ((bits
    :initarg :bits
    :accessor bit-writer-bits
    :initform '())))

(defmethod write-bit ((bit-writer bit-writer) bit)
  (push bit (bit-writer-bits bit-writer)))

(defun bits->bits-list (bits)
  (loop for x from 0 to (- (length bits) 1) by 16 append
       (list (loop for y from 8 to 15 collect (nth bits (+ x y)))
	     (loop for y from 0 to 7 collect (nth bits (+ x y))))))

(defun bits->byte (bits)
  (reduce #'(lambda (a b) (+ (ash a 1) b)) bits))

(defun bits-list->bytes (bits-list)
  (map #'bits->byte bits-list))

(defun bit-writer->bytes (bit-writer)
  (bits-list->bytes (bits->bits-list (reverse (bit-writer-bits bit-writer)))))

(defun bytes->file (bytes filename)
  (with-open-file (out filename :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
    (loop for byte in bytes do
	 (write-byte byte out))))

(defun bit-writer->file (bit-writer filename)
  (bytes->file (bit-writer->bytes bit-writer) filename))
