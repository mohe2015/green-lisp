#lang racket
(provide list->label-addresses)

(define (list->label-addresses symbols sizes offset)
  (cond [(null? symbols) '()]
        [else
         (let ([symbol (car symbols)]
               [size (car sizes)])
           (append
            (if (eq? symbol 'null) (list) (list (list symbol offset)))
            (list->label-addresses (cdr symbols) (cdr sizes) (+ offset size))))]))