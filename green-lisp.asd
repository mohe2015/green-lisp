(defsystem "green-lisp"
  :version "0.1.0"
  :author "Moritz Hedtke"
  :license "AGPL-3.0"
  :depends-on ("cserial-port" "cl21")
  :components ((:module "src"
                :components
                ((:file "logger")
		 (:file "bits")
		 (:file "computer-architecture/avr/architecture")
		 (:file "computer-architecture/avr/instructions")
		 (:file "compiler")
		 (:file "serial-interface")
		 (:file "main")
		 )))
  :description "An eco-friendly lisp"
  :in-order-to ((test-op (test-op "green-lisp/tests"))))

(defsystem "green-lisp/tests"
  :author "Moritz Hedtke"
  :license "AGPL-3.0"
  :depends-on ("green-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "bits")
		 (:file "main"))))
  :description "Test system for green-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
