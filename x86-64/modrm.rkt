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
    ['(register rax) 0]
    ['(register rcx) 1]
    ['(register rdx) 2]
    ['(register rbx) 3]
    ['(register rsp) 4]
    ['(register rbp) 5]
    ['(register rsi) 6]
    ['(register rdi) 7]))

(define (mod11-rm-to-binary value)
  (match value
    ['(register rax) 0]
    ['(register rcx) 1]
    ['(register rdx) 2]
    ['(register rbx) 3]
    ['(register rsp) 4]
    ['(register rbp) 5]
    ['(register rsi) 6]
    ['(register rdi) 7]))

(define (mod11-to-binary reg1 reg2)
  (let* ((mod #b11000000)
         (rm (mod11-rm-to-binary reg1))
         (reg (reg-to-binary reg2)))
    (+ mod (arithmetic-shift reg 3) rm)))