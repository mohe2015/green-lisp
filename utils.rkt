#lang typed/racket/base

(provide Opt)
(struct None ())
(struct (a) Some ([v : a]))
(define-type (Opt a) (U None (Some a)))