(mapcar (lambda (g)
	  (do-external-symbols (symbol g)
	    (SB-PROFILE::MAPC-ON-NAMED-FUNS #'SB-PROFILE::PROFILE-1-FUN (list symbol))))
	(remove-if-not
	 (lambda (p)
	   (equal "GREEN-LISP" (ignore-errors (subseq (package-name p) 0 10))))
	 (list-all-packages)))

(reset)
(green-lisp:main)
(report)
