(module low-level racket/base

  ;(define-instruction x86-64-call
  ;  (_                (constant-bits '(1 1 1 0 1 0 0 0)))
  ;  (relative-address (signed-integer 4)))

  (struct x86-64-call (relative-address))
    
  
  
  )