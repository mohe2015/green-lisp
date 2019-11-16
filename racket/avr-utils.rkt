(module avr-utils typed/racket

  (require (for-syntax syntax/parse))

  (begin-for-syntax
    (define-syntax-class instruction-argument
      #:description "instruction argument"
      (pattern (instruction-argument-var:id instruction-argument-type:id)))

    (define-syntax-class distinct-instruction-arguments
      #:description "sequence of distinct instruction arguments"
      (pattern (instruction-argument:instruction-argument ...)
               #:fail-when (check-duplicate-identifier
                            (syntax->list #'(instruction-argument.instruction-argument-var ...)))
               "duplicate variable name"
               #:with (instruction-argument-var ...) #'(instruction-argument.instruction-argument-var ...)
               #:with (instruction-argument-type ...) #'(instruction-argument.instruction-argument-type ...))))
  
  (define-syntax (define-assembly-instruction stx)
    (syntax-parse stx
      [(_ instruction-name:id instruction-arguments:distinct-instruction-arguments
          (format-atom:expr ...)
          instruction-cycles:expr
          body ...+)
       "duplicate variable name"
       #'(define (instruction-name [byte2 : Byte] [byte1 : Byte])

           (print #'instruction-arguments)
           null
           
           
           )]))

  
  (define (parse-ldi [byte2 : Byte] [byte1 : Byte])
    (if (= (bitwise-and (- (arithmetic-shift 1 8) (arithmetic-shift 1 4)) byte1)
           #b11100000)
        (let* ([d : Byte (arithmetic-shift byte2 -4)]
               [k : Integer (bitwise-ior
                             (bitwise-and (- (arithmetic-shift 1 8) (arithmetic-shift 1 4))
                                          (arithmetic-shift byte1 4))
                             (bitwise-and (sub1 (arithmetic-shift 1 4))
                                          byte2))])
          (list d k))
        null))
  
  (define-assembly-instruction ldi ((d register) (k integer))
    (1 1 1 0 k k k k  d d d d k k k k)
    1
    (set! (register m d) k)
    (increment! (program-counter m))) ;; the program counter addresses words

  (ldi 1 1)
  
  )