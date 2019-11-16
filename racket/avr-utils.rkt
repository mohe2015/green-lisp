(module avr-utils typed/racket

  (require (for-syntax syntax/parse))

  (define-syntax (define-assembly-instruction stx)
    (syntax-parse stx
      [(_ (instruction-name:id (instruction-argument:id instruction-argument-type:id) ...)
          (format-atom:expr ...)
          instruction-cycles:expr
          body ...+)
       #'(print 'instruction-name)]))
  
  
  (define-assembly-instruction (ldi (d register) (k integer))
    (1 1 1 0 k k k k  d d d d k k k k)
    1
    (set! (register m d) k)
    (increment! (program-counter m))) ;; the program counter addresses words
  
  )