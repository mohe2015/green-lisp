#lang racket/base
(require racket/match)
(provide rd64-to-binary)

(define (rd64-to-binary value)
  (match value
    ['rax 0]
    ['rcx 1]
    ['rdx 2]
    ['rbx 3]
    ['rsp 4]
    ['rbp 5]
    ['rsi 6]
    ['rdi 7]))