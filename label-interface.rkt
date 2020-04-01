(module label-interface racket
  (provide dynamic data-interface label% label data-unsigned% data-unsigned data-list% data-list data-string% data-string align% align)

  (define (dynamic value label-addresses)
    (eval
     `(let ,label-addresses
        ,value)
     (make-base-namespace)))
  
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
    
      (define/public (length offset)
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
        (integer->integer-bytes (dynamic the-value label-addresses) (/ the-bits 8) #f))

      (define/public (length offset)
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

      (define/public (length offset)
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
                (list->label-addresses (rest a-list) (+ offset (send (first a-list) length offset))))]
              [else
               (send a-list get-label-addresses offset)]))
      
      (define/public (get-label-addresses offset)
        (list->label-addresses the-list offset))

      (define (list->bytes a-list current-address label-addresses)
        (cond [(null? a-list) (bytes)]
              [(pair? a-list)
               (bytes-append
                (list->bytes (first a-list) current-address label-addresses)
                (list->bytes (rest a-list) (+ current-address (send (first a-list) length current-address)) label-addresses))]
              [else
               (send a-list get-bytes current-address label-addresses)]))

      (define/public (get-bytes current-address label-addresses)
        (list->bytes the-list current-address label-addresses))

      (define (sum-length a-list offset)
        (cond [(null? a-list) 0]
              [(pair? a-list)
               (+
                (sum-length (first a-list) offset)
                (sum-length (rest a-list) (+ offset (sum-length (first a-list) offset))))]
              [else
               (send a-list length offset)])) 
      
      (define/public (length offset)
        (sum-length the-list offset))))

  (define (data-list . list)
    (new data-list% [list list]))

  (define align%
    (class* object% (data-interface)
      (init alignment-bits)
      (define the-alignment-bits alignment-bits)
      (super-new)

      (define/public (get-label-addresses offset)
        (list))

      (define/public (get-bytes current-address label-addresses)
        (make-bytes (get-byte-count-to-align current-address) 0))

      (define (get-byte-count-to-align offset)
        (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 the-alignment-bits) -1) (- the-alignment-bits)) the-alignment-bits) offset))

      (define/public (length offset)
        (get-byte-count-to-align offset))))

  (define (align alignment-bits)
    (new align% [alignment-bits alignment-bits])))