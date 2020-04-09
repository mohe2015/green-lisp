#lang racket/base
(require racket/match)
(provide mod11-to-binary)

;; alternative way
;; (define rax (register 64 0))
;; (define eax (register 32 0))
;; ..
;; so types can be checked by caller
;; e.g. add may have different opcode depending on size
;; or some opcode may not even work with wrong one
  
;; todo different reg size e.g. al and ax for eax
(define (reg-to-binary value)
  (match value
    ['rax 0]
    ['rcx 1]
    ['rdx 2]
    ['rbx 3]
    ['rsp 4]
    ['rbp 5]
    ['rsi 6]
    ['rdi 7]))

(define (mod11-rm-to-binary value)
  (match value
    ['rax 0]
    ['rcx 1]
    ['rdx 2]
    ['rbx 3]
    ['rsp 4]
    ['rbp 5]
    ['rsi 6]
    ['rdi 7]))

(define (mod11-to-binary reg1 reg2)
  (let* ((mod #b11000000)
         (rm (mod11-rm-to-binary reg1))
         (reg (reg-to-binary reg2)))
    (+ mod (arithmetic-shift reg 3) rm)))