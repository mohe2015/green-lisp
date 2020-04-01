(module x86-64 racket
  (require green-lisp/label-interface)
  (provide syscall% syscall mov-imm8% mov-imm8 mov-imm64% mov-imm64 jmp% jmp)

  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (/ bits 8) #f)))

  (define syscall%
    (class* object% (data-interface)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))
      
      (define/public (get-bytes current-address label-addresses)
        (bytes #x0f #x05))
    
      (define/public (length offset)
        2)))

  (define (syscall)
    (new syscall%))

  (define mov-imm8%
    (class* object% (data-interface)
      (init register value)
      (define the-register register)
      (define the-value value)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))
      
      (define/public (get-bytes current-address label-addresses)
        (bytes-append
         (if (= the-register 7)
             (unsigned 8 #x40)
             (bytes)) ; REX prefix to access dil instead of bh
         (bytes (bitwise-ior #xb0 the-register) (dynamic the-value label-addresses))))

      (define/public (length offset)
        (if (= the-register 7) 3 2))))

  (define (mov-imm8 register value)
    (new mov-imm8% [register register] [value value]))

  (define mov-imm64%
    (class* object% (data-interface)
      (init register value)
      (define the-register register)
      (define the-value value)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))
    
      (define/public (get-bytes current-address label-addresses)
        (bytes-append
         (unsigned 8 #b01001000) ;; REX.W
         (unsigned 8 (+ #xb8 the-register)) ;; opcode with register
         (unsigned 64 (dynamic the-value label-addresses)))) ;; value
    
      (define/public (length offset)
        10)))

  (define (mov-imm64 register value)
    (new mov-imm64% [register register] [value value]))

  (define jmp%
    (class* object% (data-interface)
      (init displacement)
      (define the-displacement displacement)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))
    
      (define/public (get-bytes current-address label-addresses)
        (bytes-append (bytes #xeb) (integer->integer-bytes (dynamic the-displacement label-addresses) 1 #t)))
    
      (define/public (length offset)
        2)))

  (define (jmp displacement)
    (new jmp% [displacement displacement])))