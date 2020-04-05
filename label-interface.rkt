#lang typed/racket
(require typed/racket/unsafe)
(unsafe-require/typed green-lisp/untyped-utils [dynamic (-> Any Integer)])
(provide
 data-interface% data-interface-type
 label% label
 data-unsigned% data-unsigned
 data-list% data-list data-list-type
 data-string% data-string
 align% align
 data-array% data-array
 )

(define-type data-unsigned-type
  (Class
   (init (bits Integer) (value Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define data-unsigned%
  (class data-interface%
    (init [bits : Integer] [value : Any])
    (: the-bits Integer) ;; TODO multiple of eight
    (define the-bits bits)
    (: the-value Any)
    (define the-value value)
    (super-new)
      
    (define/override (get-label-addresses offset)
      (list))

    (define/override (get-bytes current-address label-addresses)
      (integer->integer-bytes (dynamic the-value) (arithmetic-shift the-bits -3) #f))

    (define/override (length offset)
      (arithmetic-shift the-bits -3))))

(: data-unsigned (-> Integer Any (Instance data-unsigned-type)))
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

(define-type align-type
  (Class
   (init (alignment-bits Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define align%
  (class data-interface%
    (init [alignment-bits : Integer])
    (: the-alignment-bits Integer)
    (define the-alignment-bits alignment-bits)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))
    
    (: get-byte-count-to-align (-> Integer Integer))
    (define/private (get-byte-count-to-align offset)
      (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 the-alignment-bits) -1) (- the-alignment-bits)) the-alignment-bits) offset))

    (define/override (get-bytes current-address label-addresses)
      (make-bytes (get-byte-count-to-align current-address) 0))

    (define/override (length offset)
      (get-byte-count-to-align offset))))

(: align (-> Integer (Instance align-type)))
(define (align alignment-bits)
  (new align% [alignment-bits alignment-bits]))