#lang typed/racket
(provide list->label-addresses)

(: list->label-addresses (-> (Listof Symbol) (Listof Integer) (Listof Any) Integer (Values (Listof Any) (Listof Any))))
(define (list->label-addresses symbols sizes codes offset)
  (cond [(or (null? symbols) (null? sizes) (null? codes)) (values (list) (list))]
        [else
         (let* ([symbol (car symbols)]
                [size (car sizes)]
                [code (car codes)])
           (let-values ([(cara carb) (if (eq? symbol 'null)
                                         (values (list)                      (list code))
                                         (values (list (list symbol offset)) (list code)))]
                        [(cdra cdrb) (list->label-addresses (cdr symbols) (cdr sizes) (cdr codes) (+ offset size))])
             (values
              (append cara cdra)
              (append carb cdrb))))]))