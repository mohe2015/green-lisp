(module label-interface typed/racket
  (provide
   dynamic data-interface%
   label% label
   data-unsigned% data-unsigned
   data-list% data-list
   data-string% data-string
   align% align
   data-array% data-array
   )

  (: dynamic (-> Any (Listof (List Symbol Integer)) Integer))
  (define (dynamic value label-addresses)
    (eval
     `(let ,label-addresses
        ,value)
     (make-base-namespace)))

  (define-type data-interface-type
     (Class
      (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
      (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
      (length (-> Integer Integer))))
  (define data-interface%
    (class object%
      (super-new)

      (: length (-> Integer Integer))
      (define/public (length offset)
        0)

      (: get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
      (define/public (get-bytes current-address label-addresses)
        (bytes))
      
      (: get-label-addresses (-> Integer (Listof (List Symbol Integer))))
      (define/public (get-label-addresses offset)
        '())))

  (define-type label-type
     (Class
      (init (label Symbol))
      (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
      (get-label (-> Symbol))
      (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
      (length (-> Integer Integer))))
  (define label%
    (class data-interface%
      (init [label : Symbol])
      (: the-label Symbol)
      (define the-label label)
      (super-new)

      (: get-label (-> Symbol))
      (define/public (get-label)
        the-label)

      (define/override (get-label-addresses offset)
        (list (list the-label offset)))
      
      (define/override (get-bytes current-address label-addresses)
        (bytes))
    
      (define/override (length offset)
        0)))

  (: label (-> Symbol (Instance label-type)))
  (define (label label)
    (new label% [label label]))

  (define-type data-unsigned-type
    (Class
     (init (bits Integer) (value Integer))
     (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
     (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
     (length (-> Integer Integer))))
  (define data-unsigned%
    (class data-interface%
      (init [bits : Integer] [value : Integer])
      (: the-bits Integer) ;; TODO multiple of eight
      (define the-bits bits)
      (: the-value Integer)
      (define the-value value)
      (super-new)
      
      (define/override (get-label-addresses offset)
        (list))

      (define/override (get-bytes current-address label-addresses)
        (integer->integer-bytes (dynamic the-value label-addresses) (arithmetic-shift the-bits -3) #f))

      (define/override (length offset)
        (arithmetic-shift the-bits -3))))

  (: data-unsigned (-> Integer Integer (Instance data-unsigned-type)))
  (define (data-unsigned bits value)
    (new data-unsigned% [bits bits] [value value]))

  (define-type data-string-type
    (Class
     (init (string Bytes))
     (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
     (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
     (length (-> Integer Integer))))
  (define data-string%
    (class data-interface%
      (init [string : Bytes])
      (: the-string Bytes)
      (define the-string string)
      (super-new)

      (define/override (get-label-addresses offset)
        (list))

      (define/override (get-bytes current-address label-addresses)
        the-string)

      (define/override (length offset)
        (bytes-length the-string))))

  (: data-string (-> Bytes (Instance data-string-type)))
  (define (data-string string)
    (new data-string% [string string]))

  (define-type data-array-type
    (Class
     (init (size Integer))
     (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
     (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
     (length (-> Integer Integer))))
  (define data-array%
    (class data-interface%
      (init [size : Integer])
      (: the-size Integer)
      (define the-size size)
      (super-new)

      (define/override (get-label-addresses offset)
        (list))

      (define/override (get-bytes current-address label-addresses)
        (make-bytes the-size 0))

      (define/override (length offset)
        the-size)))

  (: data-array (-> Integer (Instance data-array-type)))
  (define (data-array size)
    (new data-array% [size size]))

  (define data-list%
    (class data-interface%
      (init [list : (Listof (Instance data-interface-type))])
      (: the-list (Listof (Instance data-interface-type)))
      (define the-list list)
      (super-new)

      (: list->label-addresses
         (-> (Listof (Instance data-interface-type))
             Integer
             (Listof (List Symbol Integer))))
      (define (list->label-addresses a-list offset)
        (cond [(null? a-list) '()]
              [(pair? a-list)
               (append
                (list->label-addresses (first a-list) offset)
                (list->label-addresses (rest a-list) (+ offset (send (first a-list) length offset))))]
              [else
               (send a-list get-label-addresses offset)]))
      
      (define/override (get-label-addresses offset)
        (list->label-addresses the-list offset))

      (: list->bytes (-> (Listof (Instance data-interface-type))
                         Integer
                         (Listof (List Symbol Integer))
                         Bytes))
      (define (list->bytes a-list current-address label-addresses)
        (cond [(null? a-list) (bytes)]
              [(pair? a-list)
               (bytes-append
                (list->bytes (first a-list) current-address label-addresses)
                (list->bytes (rest a-list) (+ current-address (send (first a-list) length current-address)) label-addresses))]
              [else
               (send a-list get-bytes current-address label-addresses)]))

      (define/override (get-bytes current-address label-addresses)
        (list->bytes the-list current-address label-addresses))

      (: sum-length (-> (Listof (Instance data-interface-type))
                        Integer
                        Integer))
      (define (sum-length a-list offset)
        (cond [(null? a-list) 0]
              [(pair? a-list)
               (+
                (sum-length (first a-list) offset)
                (sum-length (rest a-list) (+ offset (sum-length (first a-list) offset))))]
              [else
               (send a-list length offset)])) 
      
      (define/override (length offset)
        (sum-length the-list offset))))

  (define (data-list . list)
    (new data-list% [list list]))

  (define align%
    (class data-interface%
      (init alignment-bits)
      (define the-alignment-bits alignment-bits)
      (super-new)

      (define/override (get-label-addresses offset)
        (list))

      (define/override (get-bytes current-address label-addresses)
        (make-bytes (get-byte-count-to-align current-address) 0))

      (define (get-byte-count-to-align offset)
        (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 the-alignment-bits) -1) (- the-alignment-bits)) the-alignment-bits) offset))

      (define/override (length offset)
        (get-byte-count-to-align offset))))

  (define (align alignment-bits)
    (new align% [alignment-bits alignment-bits])))