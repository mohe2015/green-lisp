(when (find-package :green-lisp.avr.instructions)
  (do-symbols (symbol :green-lisp.avr.instructions)
    (unexport symbol :green-lisp.avr.instructions)))

(defpackage :green-lisp.avr.instructions
  (:use :common-lisp :cserial-port :green-lisp.bits :green-lisp.logger :green-lisp.avr.architecture)
  (:import-from :green-lisp.bits :bit-reader :bit-reader-bits :read-bit :file->bit-reader :bit-writer :write-bit :bit-writer->bytes)
  (:import-from :green-lisp.logger :+trace+ :+debug+ :+info+ :+warning+ :+error+)
  (:shadowing-import-from :green-lisp.logger :log))
(in-package :green-lisp.avr.instructions)

(setf *on-package-variance* '(:warn () :error ()))

(export 'read-instruction)
(defmacro read-format (instruction format &body body)
  (let ((name (car instruction))
	(args (mapcar #'car (cdr instruction))))
    `(progn
       (defmethod read-instruction ((name (eql ',name)) (bit-reader bit-reader)) 
	 (let ,(mapcar (lambda (s) `(,s 0)) args)
	   ,@(loop for item in format collect
				      (cond ((or (eql item 0) (eql item 1))
					     `(let ((bit (read-bit bit-reader)))
						(log +trace+ (format nil "readi ~a" bit))
						(unless (= ,item bit)
						  (log +trace+ (format nil "fail-not-a-~a" ,item))
						  (return-from read-instruction nil))))
					    (t
					     `(let ((bit (read-bit bit-reader)))
						(log +trace+ (format nil "read ~a" bit))
						(setf ,item (logior (ash ,item 1) bit))))))
	   ,@(mapcar
	      (lambda (item)
		`(setf ,(nth 0 item) ,(nth 2 item)))
	      (cdr instruction))
	   ,@body
	   t)))))

(export 'instruction-size)
(export 'write-instruction)
(defmacro write-format (instruction format)
  (let ((name (car instruction))
	(args (mapcar (lambda (e) (subseq e 0 2)) (cdr instruction))))
    `(progn
       ;; TODO hash-map?
       (defmethod instruction-size ((name (eql ',name)))
	 ,(/ (length format) 8))

       ;; TODO check preconditions of parameters
       (export ',name)
       (defmethod ,name ((bit-writer bit-writer) ,@args)
	 ,@(mapcar
	    (lambda (item)
	      `(setf ,(nth 0 item) (value ,(nth 0 item) ',(nth 1 item))))
	    (cdr instruction))
	 ,@(mapcar
	    (lambda (item)
	      `(setf ,(nth 0 item) ,(nth 3 item)))
	    (cdr instruction))
	 ,@(maplist
	    (lambda (item-list)
	      (let ((item (car item-list)))
		(cond ((or (eql item 0) (eql item 1))
		       `(progn
			  (write-bit bit-writer ,item)
			  (log +trace+ (format nil "writei ~a" ,item))))
		      (t
		       `(let* ((index ,(- (count-if (lambda (i) (eql i item)) item-list) 1))
			       (bit (if (logbitp index ,item) 1 0)))
			  (log +trace+ (format nil "write ~a" bit))
			  (write-bit bit-writer bit))))))
	    format)
	 (bit-writer->bytes bit-writer))
       
       
       (defmethod write-instruction ((name (eql ',name)) &rest parameters)
	 (apply ',name parameters)))))

(defmacro define-assembly-instruction (instruction instruction-format cycles &body body)
  (declare (ignore cycles))
  `(progn
     (write-format ,instruction ,instruction-format)
     (read-format ,instruction ,instruction-format ,@body)
     (lambda (bit-reader)
       (read-instruction ',(car instruction) bit-reader))))

(defgeneric read-instruction (name binary-reader)
  (:documentation "Reads the instruction of the specified type from the binary reader."))

;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_JMP.html

(defparameter +all-instructions+
  (list

   (define-assembly-instruction (adc (d register d d (and (<= 0 d) (<= d 31))) (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 1 1 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "adc r~d, r~d~%" d r)))

   (define-assembly-instruction (add (d register d d (and (<= 0 d) (<= d 31))) (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 0 1 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "add r~d, r~d~%" d r)))

   (define-assembly-instruction (adiw (d register-pair d d (or (= d 24) (= d 26) (= d 28) (= d 30))) (k constant k k (and (<= 0 k) (<= k 63))))
       (1 0 0 1 0 1 1 0  k k d d k k k k)
       2
     (log +debug+ (format nil "adiw r~d, 0x~x~%" d k)))

   (define-assembly-instruction (_and (d register d d (<= 0 d) (<= d 31)) (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 1 0 0 0 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "and r~d, r~d~%" d r)))

   (define-assembly-instruction (andi (d register d d (and (<= 0 d) (<= d 31))) (k constant k k (and (<= 0 k) (<= k 255))))
       (0 1 1 1 k k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "andi r~d, 0x~x~%" d k)))

   (define-assembly-instruction (asr (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 1 0 1)
       1
     (log +debug+ (format nil "asr r~d~%" d)))

   (define-assembly-instruction (bclr (s integer s s (and (<= 0 s) (<= s 7))))
       (1 0 0 1 0 1 0 0  1 s s s 1 0 0 0)
       1
     (log +debug+ (format nil "bclr ~d~%" s)))

   (define-assembly-instruction (bld (d register d d (and (<= 0 d) (<= d 31))) (b integer b b (and (<= 0 b) (<= b 7))))
       (1 1 1 1 1 0 0 d  d d d d 0 b b b b)
       1
     (log +debug+ (format nil "bld r~d, ~d~%" d b)))
   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRBC.html
   (define-assembly-instruction (brbc (s integer s s (and (<= 0 s) (<= s 7)))
				      (k address (ash k 1) (ash k -1) (and (<= -64 k) (<= k 63))))
       (1 1 1 1 0 1 k k  k k k k k s s s)
       (if (= 0 (getf (sreg s))) 2 1)
     (log +debug+ (format nil "brbc ~d, 0x~x~%" s k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRBS.html
   (define-assembly-instruction (brbs (s integer s s (and (<= 0 s) (<= s 7)))
				      (k address (ash k 1) (ash k -1) (and (<= -64 k) (<= k 63))))
       (1 1 1 1 0 0 k k  k k k k k s s s)
       (if (= 1 (getf (sreg s))) 2 1)
     (log +debug+ (format nil "brbs ~d, 0x~x~%" s k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRCC.html
   (defmacro brcc (k)
     `(brbc 0 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRCS.html
   (defmacro brcs (k)
     `(brbs 0 ,k))

   (define-assembly-instruction (_break)
       (1 0 0 1 0 1 0 1  1 0 0 1 1 0 0 0)
       1
     (log +debug+ (format nil "break~%")))

   (defmacro breq (k)
     `(brbs 1 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRGE.html
   (defmacro brge (k)
     `(brbc 4 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRHC.html
   (defmacro brhc (k)
     `(brbc 5 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRHS.html
   (defmacro brhs (k)
     `(brbs 5 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRID.html
   (defmacro brid (k)
     `(brbc 7 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRIE.html
   (defmacro brie (k)
     `(brbs 7 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRLO.html
   (defmacro brlo (k)
     `(brbs 0 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRLT.html
   (defmacro brlt (k)
     `(brbs 4 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRMI.html
   (defmacro brmi (k)
     `(brbs 2 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRNE.html
   (defmacro brne (k)
     `(brbc 1 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRPL.html
   (defmacro brpl (k)
     `(brbc 2 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRSH.html
   (defmacro brsh (k)
     `(brbc 0 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRTC.html
   (defmacro brtc (k)
     `(brbc 6 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRTS.html
   (defmacro brts (k)
     `(brbs 6 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRVC.html
   (defmacro brvc (k)
     `(brbc 3 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BRVS.html
   (defmacro brvs (k)
     `(brbs 3 ,k))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BSET.html
   (define-assembly-instruction (bset (s integer s s (and (<= 0 s) (<= s 7))))
       (1 0 0 1 0 1 0 0  0 s s s 1 0 0 0)
       1
     (log +debug+ (format nil "bset ~d~%" s)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_BST.html
   (define-assembly-instruction (bst (d register d d (and (<= 0 d) (<= d 31)))
				     (b integer b b (and (<= 0 b) (<= b 7))))
       (1 1 1 1 1 0 1 d  d d d d 0 b b b)
       1
     (log +debug+ (format nil "bst r~d, ~d~%" d b)))

   (define-assembly-instruction (call (k address (ash k 1) (ash k -1) (and (<= 0 k) (<= k 64000))))
       (1 0 0 1 0 1 0 k  k k k k 1 1 1 k  k k k k k k k k  k k k k k k k k)
       4 ;; TODO FIXME THIS IS ONLY CORRECT FOR ATMEGA128
     (log +debug+ (format nil "call 0x~x~%" k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CBI.html
   (define-assembly-instruction (cbi (a io-register a a (and (<= 0 a) (<= a 31)))
				     (b integer b b (and (<= 0 b) (<= b 7))))
       (1 0 0 1 1 0 0 0  a a a a a b b b)
       2 ;; FIXME this can be different
     (log +debug+ (format nil "cbi ~d, ~d~%" a b)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CBR.html
   ;; TODO ANDI

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLC.html
   (defmacro clc ()
     `(bclr 0))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLH.html
   (defmacro clh ()
     `(bclr 5))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLI.html
   (defmacro cli ()
     `(bclr 7))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLN.html
   (defmacro cln ()
     `(bclr 2))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLR.html
   ;; TODO EOR Rd, Rd

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLS.html
   (defmacro cls ()
     `(bclr 4))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLT.html
   (defmacro clt ()
     `(bclr 6))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLV.html
   (defmacro clv ()
     `(bclr 3))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CLZ.html
   (defmacro clz ()
     `(bclr 1))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_COM.html
   (define-assembly-instruction (com (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 0 0 0)
       1
     (log +debug+ (format nil "com r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CP.html
   (define-assembly-instruction (cp (d register d d (and (<= 0 d) (<= d 31)))
				    (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 1 0 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "cp r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CPC.html
   (define-assembly-instruction (cpc (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 0 0 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "cpc r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CPI.html
   (define-assembly-instruction (cpi (d register d d (and (<= 16 #| TODO FIXME |# d) (<= d 31)))
				     (k constant k k (and (<= 0 k) (<= k 255))))
       (0 0 1 1 k k k k d d d d k k k k)
       1
     (log +debug+ (format nil "cpi r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_CPSE.html
   (define-assembly-instruction (cpse (d register d d (and (<= 0 d) (<= d 31)))
				      (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 1 0 0 r d  d d d d r r r r)
       bruh ;; TODO
     (log +debug+ (format nil "cpse r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_DEC.html
   (define-assembly-instruction (dec (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 1 0 1 0)
       1
     (log +debug+ (format nil "dec r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_EICALL.html
   (define-assembly-instruction (eicall)
       (1 0 0 1 0 1 0 1  0 0 0 1 1 0 0 1)
       4
     (log +debug+ (format nil "eicall~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_EIJMP.html
   (define-assembly-instruction (eijmp)
       (1 0 0 1 0 1 0 0  0 0 0 1 1 0 0 1)
       2
     (log +debug+ (format nil "eijmp~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ELPM.html
   ;; FIXME The result of these combinations is undefined:
   (define-assembly-instruction (elpm0)
       (1 0 0 1 0 1 0 1  1 1 0 1 1 0 0 0)
       3
     (log +debug+ (format nil "elpm~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ELPM.html
   (define-assembly-instruction (elpm (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 0 1 1 0)
       3
     (log +debug+ (format nil "elpm r~d~%" d)))
   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ELPM.html
   (define-assembly-instruction (elpm+ (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 0 1 1 1)
       3
     (log +debug+ (format nil "elpm+ r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_EOR.html
   (define-assembly-instruction (eor (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 1 0 0 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "eor r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_FMUL.html
   (define-assembly-instruction (fmul (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 23)))
				      (r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 23))))
       (0 0 0 0 0 0 1 1  0 d d d 1 r r r)
       2
     (log +debug+ (format nil "fmul r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_FMULS.html
   (define-assembly-instruction (fmuls (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 23)))
				       (r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 23))))
       (0 0 0 0 0 0 1 1  1 d d d 0 r r r)
       2
     (log +debug+ (format nil "fmuls r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_FMULSU.html
   (define-assembly-instruction (fmulsu (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 23)))
					(r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 23))))
       (0 0 0 0 0 0 1 1  1 d d d 1 r r r)
       2
     (log +debug+ (format nil "fmulsu r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ICALL.html
   (define-assembly-instruction (icall)
       (1 0 0 1 0 1 0 1 0 0 0 0 1 0 0 1)
       3 ;; TODO FIXME
     (log +debug+ (format nil "icall~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_IJMP.html
   (define-assembly-instruction (ijmp)
       (1 0 0 1 0 1 0 0  0 0 0 0 1 0 0 1)
       2
     (log +debug+ (format nil "ijmp~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_IN.html
   (define-assembly-instruction (in (d register d d (and (<= 0 d) (<= d 31)))
				    (a io-register a a (and (<= 0 a) (<= a 64))))
       (1 0 1 1 0 a a d  d d d d a a a a)
       1
     (log +debug+ (format nil "in r~d, 0x~x~%" d a)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_INC.html
   (define-assembly-instruction (inc (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 0 1 1)
       1
     (log +debug+ (format nil "inc r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_JMP.html
   (define-assembly-instruction (jmp (k address (ash k 1) (ash k -1) (and (<= 0 k) (< k 4000000))))
       (1 0 0 1 0 1 0 k  k k k k 1 1 0 k  k k k k k k k k  k k k k k k k k)
       3 ;; cycles
     ;;(setf program-counter k))
     (log +debug+ (format nil "jmp 0x~x~%" k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ld (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 1 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, X" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ld+ (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 1 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, X+" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ld- (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 1 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, -X" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LAT.html
   (define-assembly-instruction (lat (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 0 r  r r r r 0 1 1 1)
       2
     (log +debug+ (format nil "lat r~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LAS.html
   (define-assembly-instruction (las (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 0 r  r r r r 0 1 0 1)
       2
     (log +debug+ (format nil "las r~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LAC.html
   (define-assembly-instruction (lac (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 0 r  r r r r 0 1 1 0)
       2
     (log +debug+ (format nil "lac r~d~%" r)))
   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ldy (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 0 0 0 0 d  d d d d 1 0 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, Y" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ldy+ (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 0 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, Y+" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ld-y (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 0 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, -Y" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (lddy (d register d d (and (<= 0 d) (<= d 31)))
				      (q constant q q (and (<= 0 q) (<= q 63))))
       (1 0 q 0 q q 0 d  d d d d 1 q q q)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ldd r~d, Y+~d" d q)))

   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ldz (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 0 0 0 0 d  d d d d 1 0 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, Z" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ldz+ (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 0 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, Z+" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (ld-z (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 0 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ld r~d, -Z" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (lddz (d register d d (and (<= 0 d) (<= d 31)))
				      (q constant q q (and (<= 0 q) (<= q 63))))
       (1 0 q 0 q q 0 d  d d d d 1 q q q)
       2 ;; TODO FIXME
     (log +debug+ (format nil "ldd r~d, Z+~d" d q)))
   
   (define-assembly-instruction (ldi (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31))) (k io-register k k (and (<= 0 k) (<= k 255))))
       (1 1 1 0 k k k k  d d d d k k k k)
       1
     ;;(setf (register d) k)
     ;;(setf program-counter (+ program-counter 1)) ;; the program counter addresses words
     (log +debug+ (format nil "ldi r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDS.html
   (define-assembly-instruction (lds (d register d d (and (<= 0 d) (<= d 31)))
				     (k address k k (and (<= 0 k) (<= k 65535))))
       (1 0 0 1 0 0 0 d  d d d d 0 0 0 0  k k k k k k k k  k k k k k k k k)
       2 ;; TODO FIXME
     (log +debug+ (format nil "lds r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LDS_-_Load_direct_from_SRAM.html
   (define-assembly-instruction (lds-sram (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
					  (k address k k (and (<= #x40 k) (<= k #xBF))))
       (1 0 1 0 0 k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "lds-sram r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LPM.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (lpm0)
       (1 0 0 1 0 1 0 1  1 1 0 0 1 0 0 0)
       3
     (log +debug+ (format nil "lpm~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LPM.html
   ;; The result of these combinations is undefined: 
   (define-assembly-instruction (lpm (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 0 1 0 0)
       3
     (log +debug+ (format nil "lpm r~d, Z~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LPM.html
   ;; The result of these combinations is undefined: 
   (define-assembly-instruction (lpm (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 0 1 0 1)
       3
     (log +debug+ (format nil "lpm r~d, Z+~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LSL.html
   ;; TODO Add Rd, Rd

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_LSR.html
   (define-assembly-instruction (lsr (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 1 1 0)
       1
     (log +debug+ (format nil "lsr r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_MOV.html
   (define-assembly-instruction (mov (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 1 0 1 1 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "mov r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_MOVW.html
   (define-assembly-instruction (movw (d register (ash d 1) (ash d -1) (and (<= 0 d) (<= d 30) (evenp d)))
				      (r register (ash r 1) (ash r -1) (and (<= 0 r) (<= r 30) (evenp r))))
       (0 0 0 0 0 0 0 1  d d d d r r r r)
       1
     (log +debug+ (format nil "movw r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_MUL.html
   (define-assembly-instruction (mul (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 1 1 r d  d d d d r r r r)
       2
     (log +debug+ (format nil "mul r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_MULS.html
   (define-assembly-instruction (muls (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
				      (r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 31))))
       (0 0 0 0 0 0 1 0  d d d d r r r r)
       2
     (log +debug+ (format nil "muls r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_MULSU.html
   (define-assembly-instruction (mulsu (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 23)))
				       (r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 23))))
       (0 0 0 0 0 0 1 1  0 d d d 0 r r r)
       2
     (log +debug+ (format nil "mulsu r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_NEG.html
   (define-assembly-instruction (neg (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 0 0 1)
       1
     (log +debug+ (format nil "neg r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_NOP.html
   (define-assembly-instruction (nop)
       (0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0)
       1
     (log +debug+ (format nil "nop~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_OR.html
   (define-assembly-instruction (_or (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 1 0 1 0 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "or r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ORI.html
   (define-assembly-instruction (ori (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
				     (k constant k k (and (<= 0 k) (<= k 255))))
       (0 1 1 0 k k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "ori r~d, 0x~x~%" d k)))
   
   ;; out
   (define-assembly-instruction (out (a io-register a a (and (<= 0 a) (<= a 64))) (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 1 1 1 a a r  r r r r a a a a)
       1
     ;; (out a r)
     ;; (setf program-counter (+ program-counter 2))
     (log +debug+ (format nil "out 0x~x, r~d~%" a r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_POP.html
   (define-assembly-instruction (_pop (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 0 d  d d d d 1 1 1 1)
       2
     (log +debug+ (format nil "pop r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_PUSH.html
   (define-assembly-instruction (_push (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 0 1 d  d d d d 1 1 1 1)
       2
     (log +debug+ (format nil "push r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_RCALL.html
   (define-assembly-instruction (rcall (k address (ash k 1) (ash k -1) (and (<= -2048 k) (<= k 2048)))) ;; TODO FIXME 4096?
       (1 1 0 1 k k k k  k k k k k k k k)
       3 ;; TODO FIXME
     (log +debug+ (format nil "rcall 0x~x~%" k)))
   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_RET.html
   (define-assembly-instruction (ret)
       (1 0 0 1 0 1 0 1  0 0 0 0 1 0 0 0)
       4 ;; TODO FIXME
     (log +debug+ (format nil "ret~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_RETI.html
   (define-assembly-instruction (reti)
       (1 0 0 1 0 1 0 1  0 0 0 1 1 0 0 0)
       4 ;; TODO FIXME
     (log +debug+ (format nil "reti~%")))
   
   ;; rjmp
   (define-assembly-instruction (rjmp (k address (ash k 1) (ash k -1) (and (<= -2048 k) (<= k 2048))))
       (1 1 0 0 k k k k  k k k k k k k k)
       2
     (log +debug+ (format nil "rjmp 0x~x~%" k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ROL.html
   ;; SEE ADC Rd, Rd

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ROR.html
   (define-assembly-instruction (ror (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 1 1 1)
       1
     (log +debug+ (format nil "ror r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBC.html
   (define-assembly-instruction (sbc (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 0 1 0 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "sbc r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBCI.html
   (define-assembly-instruction (sbci (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
				      (k constant k k (and (<= 0 k) (<= k 255))))
       (0 1 0 0 k k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "sbci r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBI.html
   (define-assembly-instruction (sbi (a io-register a a (and (<= 0 a) (<= a 31)))
				     (b integer b b (and (<= 0 b) (<= b 7))))
       (1 0 0 1 1 0 1 0  a a a a a b b b)
       2 ;; TODO FIXME
     (log +debug+ (format nil "sbi 0x~x, ~d~%" a b)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBIC.html
   (define-assembly-instruction (sbic (a io-register a a (and (<= 0 a) (<= a 31)))
				      (b integer b b (and (<= 0 b) (<= b 7))))
       (1 0 0 1 1 0 0 1  a a a a a b b b)
       1 ;; TODO FIXME I HATE SKIP
     (log +debug+ (format nil "sbic 0x~x, ~d~%" a b)))


   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBIS.html
   (define-assembly-instruction (sbis (a io-register a a (and (<= 0 a) (<= a 31)))
				      (b integer b b (and (<= 0 b) (<= b 7))))
       (1 0 0 1 1 0 1 1  a a a a a b b b)
       1 ;; TODO FIXME I HATE SKIP
     (log +debug+ (format nil "sbis 0x~x, ~d~%" a b)))   

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBIW.html
   (define-assembly-instruction (sbiw (d register (+ (ash d 1) 24) (ash (- d 24) -1) (or (= d 24) (= d 26) (= d 28) (= d 30)))
				      (k constant k k (and (<= 0 k) (<= k 63))))
       (1 0 0 1 0 1 1 1  k k d d k k k k)
       2
     (log +debug+ (format nil "sbiw p~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBR.html
   (define-assembly-instruction (sbr (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
				     (k constant k k (and (<= 0 k) (<= k 255))))
       (0 1 1 0 k k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "sbr r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBRC.html
   (define-assembly-instruction (sbrc (r register r r (and (<= 0 r) (<= r 31)))
				      (b integer b b (and (<= 0 b) (<= b 7))))
       (1 1 1 1 1 1 0 r  r r r r 0 b b b)
       1 ;; TODO FIXME
     (log +debug+ (format nil "sbrc r~d, ~d~%" r b)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SBRS.html
   (define-assembly-instruction (sbrs (r register r r (and (<= 0 r) (<= r 31)))
				      (b integer b b (and (<= 0 b) (<= b 7))))
       (1 1 1 1 1 1 1 r  r r r r 0 b b b)
       1 ;; TODO FIXME
     (log +debug+ (format nil "sbrs r~d, ~d~%" r b)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEC.html
   (defmacro sec ()
     `(bset 0)) ;; TODO REPLACE 0

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEH.html
   (defmacro seh ()
     `(bset 5))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEI.html
   (defmacro sei ()
     `(bset 7))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEN.html
   (defmacro sen ()
     `(bset 2))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SES.html
   (defmacro ses ()
     `(bset 4))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SET.html
   (defmacro _set ()
     `(bset 6))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEV.html
   (defmacro sev ()
     `(bset 3))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SEZ.html
   (defmacro sez ()
     `(bset 1))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SER.html
   (define-assembly-instruction (ser (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31))))
       (1 1 1 0 1 1 1 1  d d d d 1 1 1 1)
       1
     (log +debug+ (format nil "ser r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SLEEP.html
   (define-assembly-instruction (_sleep)
       (1 0 0 1 0 1 0 1  1 0 0 0 1 0 0 0)
       1
     (log +debug+ (format nil "sleep~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SPM.html
   (define-assembly-instruction (spm)
       (1 0 0 1 0 1 0 1  1 1 1 0 1 0 0 0)
       1337 ;; TODO FIXME
     (log +debug+ (format nil "spm~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ST.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (st (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 1 1 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st X, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ST.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (st+ (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 1 1 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st X+, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_ST.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (st- (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 1 1 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st -X, R~d~%" r)))






   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (sty (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 0 0 0 1 r  r r r r 1 0 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st Y, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (sty+ (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 1 0 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st Y+, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (st-y (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 1 0 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st -Y, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (stdy (r register r r (and (<= 0 r) (<= r 31)))
				      (q constant q q (and (<= 0 q) (<= q 63))))
       (1 0 q 0 q q 1 r  r r r r 1 q q q)
       2 ;; TODO FIXME
     (log +debug+ (format nil "std Y+~d, R~d~%" q r)))





   
   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (stz (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 0 0 0 1 r  r r r r 0 0 0 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st Z, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (stz+ (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 0 0 0 1)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st Z+, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (st-z (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 1 r  r r r r 0 0 1 0)
       2 ;; TODO FIXME
     (log +debug+ (format nil "st -Z, R~d~%" r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STD_Z.html
   ;; The result of these combinations is undefined:
   (define-assembly-instruction (stdz (r register r r (and (<= 0 r) (<= r 31)))
				      (q constant q q (and (<= 0 q) (<= q 63))))
       (1 0 q 0 q q 1 r  r r r r 0 q q q)
       2 ;; TODO FIXME
     (log +debug+ (format nil "std Z+~d, R~d~%" q r)))


   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STS.html
   (define-assembly-instruction (sts (d register d d (and (<= 0 r) (<= d 31)))
				     (k constant k k (and (<= 0 k) (<= k 65535))))
       (1 0 0 1 0 0 1 d  d d d d 0 0 0 0  k k k k k k k k  k k k k k k k k)
       2
     (log +debug+ (format nil "sts 0x~x, r~d~%" k d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_STS_-_Store_Direct_to_SRAM.html
   (define-assembly-instruction (sts2 (r register (+ r 16) (- r 16) (and (<= 16 r) (<= r 31)))
				      (k constant (+ k #x40) (- k #x40) (and (<= #x40 k) (<= k #xbf))))
       (1 0 1 0 1 k k k  r r r r k k k k)
       1
     (log +debug+ (format nil "sts 0x~x, r~d~%" k r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SUB.html
   (define-assembly-instruction (sub (d register d d (and (<= 0 d) (<= d 31)))
				     (r register r r (and (<= 0 r) (<= r 31))))
       (0 0 0 1 1 0 r d  d d d d r r r r)
       1
     (log +debug+ (format nil "sub r~d, r~d~%" d r)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SUBI.html
   (define-assembly-instruction (subi (d register (+ d 16) (- d 16) (and (<= 16 d) (<= d 31)))
				      (k constant k k (and (<= 0 k) (<= k 255))))
       (0 1 0 1 k k k k  d d d d k k k k)
       1
     (log +debug+ (format nil "subi r~d, 0x~x~%" d k)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_SWAP.html
   (define-assembly-instruction (swap (d register d d (and (<= 0 d) (<= d 31))))
       (1 0 0 1 0 1 0 d  d d d d 0 0 1 0)
       1
     (log +debug+ (format nil "swap r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_TST.html
   (define-assembly-instruction (tst (d register d d (and (<= 0 d) (<= d 31))))
       (0 0 1 0 0 0 d d   d d d d d d d d)
       1
     (log +debug+ (format nil "tst r~d~%" d)))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_WDR.html
   (define-assembly-instruction (wdr)
       (1 0 0 1 0 1 0 1  1 0 1 0 1 0 0 0)
       1
     (log +debug+ (format nil "wdr~%")))

   ;; https://www.microchip.com/webdoc/avrassembler/avrassembler.wb_XCH.html
   (define-assembly-instruction (xch (r register r r (and (<= 0 r) (<= r 31))))
       (1 0 0 1 0 0 0 r  r r r r 0 1 0 0)
       2
     (log +debug+ (format nil "xch r~d~%" r)))
   
   ))

(defun read-any-instruction (bit-reader)
  (loop for method in +all-instructions+ do
    (let* ((new-bit-reader (make-instance 'bit-reader :bits (bit-reader-bits bit-reader)))
	   (result (funcall method new-bit-reader)))
      (if result
	  (return-from read-any-instruction new-bit-reader)))))
