(module avr-utils typed/racket

  (require typed/racket/stream)
  (require (for-syntax syntax/parse))

  ;; TODO merge with the shit from binary-format.rkt
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
               #:with (instruction-argument-type ...) #'(instruction-argument.instruction-argument-type ...)))

    (define-syntax-class bits
      #:description "bits"
      (pattern (bit:expr ...)))
    )
  
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

  '(define-assembly-instruction-argument register
     (arithmetic-shift register 3) ;; parsing
     (arithmetic-shift register -3) ;; building
     (and (<= 0 register) (<= register 31))) ;; constraints

  (define-syntax (get-bits-from-binary stx)
    (syntax-parse stx
      [(_ bits:bits) ;; (1 0 0 0 1 0 0 0) means extracting these bit and building a new integer with 2 bits
       #'(lambda (binary)
            (for ([i (in-range (- (length 'bits) 1) -1 -1)])
              (let ((value (list-ref 'bits i))) 
                (print `(arithmetic-shift 1 ,i))))
           null)]))
  
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
  ((get-bits-from-binary (1 0 0 0 1)) 17)
  )
