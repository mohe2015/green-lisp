#lang racket
(require green-lisp/label-interface)
(require (for-syntax racket/list) (for-syntax green-lisp/test-utils))

'(define-file
  (elf
   (section text
           ... code)
   (section rodata
            ...)))

(define-syntax-rule (label2 symbol)
  (list 0 symbol (bytes)))

(define-syntax-rule (call2 target)
  (list 5 null (bytes-append (bytes #xe8) (integer->integer-bytes (- target 1337 5) 4 #t)))) ;; TODO 1337 current-address

(define-syntax (list2 stx)
  (syntax-case stx ()
    [(_ x ...)
     (let* ((children (syntax-e #'(x ...)))
            (expanded (map (lambda (c) (syntax->datum (local-expand c 'expression #f))) children))
            (sizes (map (lambda (c) (second c)) expanded)) ;; TODO size may depend on offset
            (symbols (map (lambda (c) (third c)) expanded))
            (code (map (lambda (c) (fourth c)) expanded))
            (labels (list->label-addresses symbols sizes 0)))
       (datum->syntax stx #`(let #,labels (bytes-append #,@code))))]))

(list2
 (label2 code-start)
 (call2 code-start))