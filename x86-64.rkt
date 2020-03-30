(module x86-64 racket
  (provide label%)

  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (/ bits 8) #f)))

  (define instruction-interface
    (interface () length get-bytes))

  (define syscall%
    (class* object% (instruction-interface)
      (super-new)

      (define/public (get-bytes label-addresses)
        (bytes #x0f #x05))
    
      (define/public (length)
        2)))

  (define (syscall)
    (new syscall%))

  (define mov-imm8%
    (class* object% (instruction-interface)
      (init register value)
      (define the-register register)
      (define the-value value)
      (super-new)
    
      (define/public (get-bytes label-addresses)
        (bytes-append
         (if (= the-register 7)
             (unsigned 8 #x40)
             (bytes)) ; REX prefix to access dil instead of bh
         (bytes (bitwise-ior #xb0 the-register) the-value)))

      (define/public (length)
        (if (= the-register 7) 3 2))))

  (define (mov-imm8 register value)
    (new mov-imm8% [register register] [value value]))

  (define mov-imm64%
    (class* object% (instruction-interface)
      (init register value)
      (define the-register register)
      (define the-value value)
      (super-new)
    
      (define/public (get-bytes label-addresses)
        (bytes-append
         (unsigned 8 #b01001000) ;; REX.W
         (unsigned 8 (+ #xb8 the-register)) ;; opcode with register
         (unsigned 64 (second (assoc the-value label-addresses))))) ;; value
    
      (define/public (length)
        10)))

  (define (mov-imm64 register value)
    (new mov-imm64% [register register] [value value]))

  (define jmp%
    (class* object% (instruction-interface)
      (init displacement)
      (define the-displacement displacement)
      (super-new)
    
      (define/public (get-bytes label-addresses)
        (bytes-append (bytes #xeb) (integer->integer-bytes the-displacement 1 #t)))
    
      (define/public (length)
        2)))

  (define (jmp displacement)
    (new jmp% [displacement displacement]))

  (define data-unsigned%
    (class* object% (instruction-interface)
      (init bits value)
      (define the-bits bits)
      (define the-value value)
      (super-new)

      (define/public (get-bytes label-addresses)
        (integer->integer-bytes the-value (/ the-bits 8) #f))

      (define/public (length)
        (/ the-bits 8))))

  (define (data-unsigned bits value)
    (new data-unsigned% [bits bits] [value value]))

  (define label%
    (class* object% (instruction-interface)
      (init label)
      (define the-label label)
      (super-new)

      (define/public (get-label)
        the-label)

      (define/public (get-bytes label-addresses)
        (bytes))
    
      (define/public (length)
        0)))

  (define (label label)
    (new label% [label label])))