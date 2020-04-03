(module untyped-utils racket/base
  (provide dynamic)
  
   (define (dynamic value label-addresses)
    (eval
     `(let ,label-addresses
        ,value)
     (make-base-namespace))))