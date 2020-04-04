#lang racket
(provide list->label-addresses)

(define (list->label-addresses symbols sizes codes offset)
  (cond [(null? symbols) (values (list) (list))]
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