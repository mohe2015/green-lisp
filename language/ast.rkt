#lang racket
(provide function% number%)

(define function%
  (class object%
    (super-new)))

(define number%
  (class object%
    (super-new)
    (init-field value)))