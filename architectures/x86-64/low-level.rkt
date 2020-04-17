(module low-level racket/base
  (require green-lisp/architectures/x86-64/bit-port)
  
  ;(define-instruction x86-64-call
  ;  (_                (constant-bits '(1 1 1 0 1 0 0 0)))
  ;  (relative-address (signed-integer 4)))

  ;;https://docs.racket-lang.org/binary-class/index.html
  
  (struct binary (read write))
  
  (struct constant-bits-data (bits))
  
  (define constant-bits
    (binary
     (lambda (in)
       null)
     (lambda (out)
       null)))
     
  
  )