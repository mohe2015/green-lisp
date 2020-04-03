#lang typed/racket/base
(require green-lisp/utils racket/match)
(provide mod11-to-binary)
;; (register eax)
;; (pointer (register eax))
;; (pointer (+ (register eax) displacement))
;; (register al) vs (register (part 1 eax)) or so
;; or maybe both

;; alternative way
;; (define rax (register 64 0))
;; (define eax (register 32 0))
;; ..
;; so types can be checked by caller
;; e.g. add may have different opcode depending on size
;; or some opcode may not even work with wrong one
  
;; todo different reg size e.g. al and ax for eax
(: reg-to-binary (-> (Listof Symbol) Integer))
(define (reg-to-binary value)
  (match value
    ['(register eax) 0]
    ['(register ecx) 1]
    ['(register edx) 2]
    ['(register ebx) 3]
    ['(register esp) 4]
    ['(register ebp) 5]
    ['(register esi) 6]
    ['(register edi) 7]))

(: mod11-rm-to-binary (-> (Listof Symbol) Integer))
(define (mod11-rm-to-binary value)
  (match value
    ['(register eax) 0]
    ['(register ecx) 1]
    ['(register edx) 2]
    ['(register ebx) 3]
    ['(register esp) 4]
    ['(register ebp) 5]
    ['(register esi) 6]
    ['(register edi) 7]))
  
(: mod11-to-binary (-> (Listof Symbol) (Listof Symbol) Integer))
(define (mod11-to-binary reg1 reg2)
  (let* ((mod #b11000000)
         (rm (mod11-rm-to-binary reg1))
         (reg (reg-to-binary reg2)))
    (+ mod (arithmetic-shift reg 3) rm)))