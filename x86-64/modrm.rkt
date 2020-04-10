#lang racket/base
(require racket/match)
(provide mod11-to-binary mod10-to-binary mod00-to-binary)

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

(define (mod00-rm-to-binary value)
  (match value
    ['(at eax) 0]
    ['(at ecx) 1]
    ['(at edx) 2]
    ['(at ebx) 3]
    ['(--) 4] ;; SIB byte would follow
    ;;https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=46&zoom=100,28,760
    ['(displacement32 rip) 5]
    ['(at esi) 6]
    ['(at edi) 7]))

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

(define (mod10-rm-to-binary value)
  (match value
    ['(displacement32 eax) 0]
    ['(displacement32 ecx) 1]
    ['(displacement32 edx) 2]
    ['(displacement32 ebx) 3]
    ['(displacement32 --) 4] ;; SIB byte would follow 
    ['(displacement32 ebp) 5]
    ['(displacement32 esi) 6]
    ['(displacement32 edi) 7]))

(define (mod00-to-binary reg1 reg2)
  (let* ((mod #b00000000)
         (rm (mod00-rm-to-binary reg2))
         (reg (reg-to-binary reg1)))
    (+ mod (arithmetic-shift reg 3) rm)))

(define (mod10-to-binary reg1 reg2)
  (let* ((mod #b10000000)
         (rm (mod10-rm-to-binary reg2))
         (reg (reg-to-binary reg1)))
    (+ mod (arithmetic-shift reg 3) rm)))

(define (mod11-to-binary reg1 reg2)
  (let* ((mod #b11000000)
         (rm (mod11-rm-to-binary reg1))
         (reg (reg-to-binary reg2)))
    (+ mod (arithmetic-shift reg 3) rm)))