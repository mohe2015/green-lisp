;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; green-lisp - an eco-friendly lisp
;;;; Copyright (C) 2019 Moritz Hedtke <Moritz.Hedtke@t-online.de>
;;;;
;;;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

(defpackage :green-lisp.logger
  (:use :common-lisp)
  (:shadow :log)
  (:export :log :+trace+ :+debug+ :+info+ :+warning+ :+error+))
(in-package :green-lisp.logger)

(defconstant +trace+ 0)
(defconstant +debug+ 1)
(defconstant +info+ 2)
(defconstant +warning+ 3)
(defconstant +error+ 4)

(defparameter *LOG-LEVEL* +debug+)

(defun log (level message)
  (if (>= level *LOG-LEVEL*)
      (princ message)))
