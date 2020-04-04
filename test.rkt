#lang typed/racket
(define-code
  (label 'code-start)
  (call 'code-start))

;; define-code is a macro
;; mov-imm64 is a macro that returns a size, labels and an object build function
;; label is also a macro that returns size, labels and object build functin
;; define-code iterates over children and collects labels
;; then it returns code with let binding for all labels using the size to calculate offset
;; the object build functions as body
;; this can then be compiled:

(let ((code-start 0))
  (bytes-append*
   (nop)
   (generate-call code-start)))

;; generate-call and nop are functions that return the bytes (functional coding)

;; ---------------------------------------------------------------------------------------------

(define-file
  (elf
   (section text
           ... code)
   (section rodata
            ...)))

;; elf, section etc. are just macros that expand to the above things
;; think about whether this is the best way to implement it