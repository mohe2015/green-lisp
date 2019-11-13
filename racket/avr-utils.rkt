(module avr-utils typed/racket

  
  
  (define-assembly-instruction (ldi (d register) (k integer))
    (1 1 1 0 k k k k  d d d d k k k k)
    1
    (set! (register m d) k)
    (increment! (program-counter m))) ;; the program counter addresses words
  
  )