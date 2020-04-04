#lang racket
(provide dynamic define-label-addresses)

(define namespace (make-base-namespace))

(define (define-label-addresses label-addresses)
  (map
   (lambda (v)
     (namespace-set-variable-value! (first v) (second v) #f namespace #f))
   label-addresses))

(define (dynamic value)
  (eval value namespace))