#lang racket
(require green-lisp/label-interface)
(require (for-syntax racket/list) (for-syntax green-lisp/test-utils))

'(define-code
  (label 'code-start)
  (call 'code-start))

;; define-code is a macro
;; mov-imm64 is a macro that returns a size, labels and an object build function
;; label is also a macro that returns size, labels and object build functin
;; define-code iterates over children and collects labels
;; then it returns code with let binding for all labels using the size to calculate offset
;; the object build functions as body
;; this can then be compiled:

'(let ((code-start 0))
  (bytes-append*
   (nop)
   (generate-call code-start)))

;; generate-call and nop are functions that return the bytes (functional coding)

;; ---------------------------------------------------------------------------------------------

'(define-file
  (elf
   (section text
           ... code)
   (section rodata
            ...)))

;; elf, section etc. are just macros that expand to the above things
;; think about whether this is the best way to implement it
(define-syntax-rule (label2 symbol)
  (list 0 symbol (begin)))

(define-syntax-rule (call2 target)
  (list 5 null (bytes-append (bytes #xe8) (integer->integer-bytes (- (dynamic the-address) current-address (length current-address)) 4 #t))))


(define-syntax (list2 stx)
  (syntax-case stx ()
    [(_ x ...)
     (let* ((children (syntax-e #'(x ...)))
            (expanded (map (lambda (c)
                             (syntax->datum (local-expand c 'expression #f)))
                           children))
            (sizes (map (lambda (c)
                         (second c))
                       expanded))
            (symbols (map (lambda (c)
                         (third c))
                       expanded))
            (code (map (lambda (c)
                         (fourth c))
                       expanded))
            (labels (list->label-addresses symbols sizes 0))
            )

           
       (datum->syntax stx (format "~s" (syntax->datum (datum->syntax #f labels)))))
     ]))

(list2
 (label2 code-start)
 (call2 code-start))