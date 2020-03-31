(module label-interface racket
  (provide data-interface label% label data-unsigned% data-unsigned data-list% data-list)
  
  (define data-interface
    (interface () length get-bytes))

  (define label%
    (class* object% (data-interface)
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
    (new label% [label label]))

  (define data-unsigned%
    (class* object% (data-interface)
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

  (define data-list%
    (class* object% (data-interface)
      (init list)
      (define the-list list)
      (super-new)

      (define/public (get-bytes label-addresses)
        (bytes-append* (bytes) (map (λ (v) (send v get-bytes label-addresses)) the-list)))

      (define/public (length)
        (foldr + 0 (map (λ (v) (send v length)) the-list)))))

  (define (data-list . list)
    (new data-list% [list list])))