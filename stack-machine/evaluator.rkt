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

'((push 1)
  (push rbp)
  (set! rbp rsp)
  (set! r1 (- rbp 16))
  (pop rbp)
  (pop)
  (push r1)
  (ret))

'((push 1)
  (push rbp)
  (set! rbp rsp)
  (set! r1 1)
  (pop rbp)
  (pop)
  (push r1)
  (ret))

'((push 1)
  (push rbp)
  (set! rbp rsp)
  (set! r1 1)
  (pop rbp)
  (pop)
  (push 1)
  (ret))

'((push rbp)
  (set! rbp rsp)
  (set! r1 1)
  (pop rbp)
  (push 1)
  (ret))

;; assume r1 is local
'((push rbp)
  (set! rbp rsp)
  (pop rbp)
  (push 1)
  (ret))

'((push rbp)
  (pop rbp)
  (push 1)
  (ret))

'((push 1)
  (ret))