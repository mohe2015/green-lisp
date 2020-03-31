(module label-interface racket
  (provide data-interface label% label data-unsigned% data-unsigned data-list% data-list data-string% data-string)
  
  (define data-interface
    (interface () length get-bytes get-label-addresses))

  (define label%
    (class* object% (data-interface)
      (init label)
      (define the-label label)
      (super-new)

      (define/public (get-label)
        the-label)

      (define/public (get-label-addresses offset)
        (list (list the-label offset)))
      
      (define/public (get-bytes current-address label-addresses)
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
      
      (define/public (get-label-addresses offset)
        (list))

      (define/public (get-bytes current-address label-addresses)
        (integer->integer-bytes (eval
                                 `(let ,label-addresses
                                    ,the-value)
                                 (make-base-namespace))
                                (/ the-bits 8) #f))

      (define/public (length)
        (/ the-bits 8))))

  (define (data-unsigned bits value)
    (new data-unsigned% [bits bits] [value value]))

  (define data-string%
    (class* object% (data-interface)
      (init string)
      (define the-string string)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))

      (define/public (get-bytes current-address label-addresses)
        the-string)

      (define/public (length)
        (bytes-length the-string))))

  (define (data-string string)
    (new data-string% [string string]))

  (define data-list%
    (class* object% (data-interface)
      (init list)
      (define the-list list)
      (super-new)

      (define (list->label-addresses a-list offset)
        (cond [(null? a-list) '()]
              [(pair? a-list)
               (append
                (list->label-addresses (first a-list) offset)
                (list->label-addresses (rest a-list) (+ offset (send (first a-list) length))))]
              [else
               (send a-list get-label-addresses offset)]))
      
      (define/public (get-label-addresses offset)
        (list->label-addresses the-list offset))

      (define (list->bytes a-list current-address label-addresses)
        (cond [(null? a-list) (bytes)]
              [(pair? a-list)
               (bytes-append
                (list->bytes (first a-list) current-address label-addresses)
                (list->bytes (rest a-list) (+ current-address (send (first a-list) length)) label-addresses))]
              [else
               (send a-list get-bytes current-address label-addresses)]))

      (define/public (get-bytes current-address label-addresses)
        (list->bytes the-list current-address label-addresses))

      (define/public (length)
        (foldr + 0 (map (λ (v) (send v length)) the-list)))))

  (define (data-list . list)
    (new data-list% [list list])))