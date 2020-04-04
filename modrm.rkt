#lang typed/racket
(require green-lisp/utils)
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
(: reg-to-binary (-> (List Symbol) Byte))
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

(: mod11-rm-to-binary (-> (List Symbol) (Opt Byte)))
(define (mod11-rm-to-binary value)
  (if (eq? (first value) 'register)
      (let ((register (second value)))
        (cond
          [(eq? register 'eax) (ann 0 Byte)]
          [(eq? register 'ecx) (ann 0 Byte)]
          [(eq? register 'edx) (ann 0 Byte)]
          [(eq? register 'ebx) (ann 0 Byte)]
          [(eq? register 'esp) (ann 0 Byte)]
          [(eq? register 'ebp) (ann 0 Byte)]
          [(eq? register 'esi) (ann 0 Byte)]
          [(eq? register 'edi) (ann 0 Byte)]
          [else (None)]))
    (None)))
  
(: mod11-to-binary (-> (List Symbol) (List Symbol) Integer))
(define (mod11-to-binary reg1 reg2)
  (let* ((mod #b11000000)
         (rm (mod11-rm-to-binary reg1))
         (reg (reg-to-binary reg2)))
    (+ mod (arithmetic-shift reg 3) (cast rm Byte))))