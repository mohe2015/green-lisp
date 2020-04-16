#lang racket
(require green-lisp/stack-machine/index)

(define (simplify instructions)
  (println instructions)
  (match instructions
    [(list '(push (- rbp 16))
           '(set! r1 (pop))
           rest ...
           ) (simplify rest)]
    [(list) null]
    [(list first rest ...)
     (println first)
     (cons first (simplify rest))]


    ))

(let ((instructions (compile '((lambda (a) a) 1) (env-initial 0))))
  (simplify instructions))