#lang racket
(require green-lisp/stack-machine/index)

(define (simplify instructions)
  (match instructions
    [(list '(push (- rbp 16)) '(set! r1 (pop)) rest ...)
     (cons '(set! r1 (- rbp 16)) (simplify rest))]
    [(list) null]
    [(list first rest ...)
     (cons first (simplify rest))]
    ))

(let ((instructions (compile '((lambda (a) a) 1) (env-initial 0))))
  (simplify instructions))