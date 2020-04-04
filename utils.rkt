#lang typed/racket/base

(provide Opt None Some Some-v)
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Opt a) (U None (Some a)))