#lang racket/base
(require racket/match)
(provide rd64-to-binary)

(define (rd64-to-binary value)
  (match value
    ['(register rax) 0]
    ['(register rcx) 1]
    ['(register rdx) 2]
    ['(register rbx) 3]
    ['(register rsp) 4]
    ['(register rbp) 5]
    ['(register rsi) 6]
    ['(register rdi) 7]))