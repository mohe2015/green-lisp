(module label-interface racket
  (provide data-interface label% label)
  
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
    (new label% [label label])))