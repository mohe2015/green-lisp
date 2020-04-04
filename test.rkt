#lang racket
(require green-lisp/test-utils)
(require (for-syntax racket/list) (for-syntax green-lisp/test-utils))

'(define-file
  (elf
   (section text
           ... code)
   (section rodata
            ...)))

(define-syntax-rule (label2 symbol)
  (list 0 symbol (lambda (current-address) (bytes))))

(define-syntax-rule (call2 target)
  (list 5 null (lambda (current-address) (bytes-append (bytes #xe8) (integer->integer-bytes (- target current-address 5) 4 #t))))) ;; TODO 1337 current-address

(define-syntax (list2 stx)
  (syntax-case stx ()
    [(_ x ...)
     (let* ((children (syntax-e #'(x ...)))
            (expanded (map (lambda (c) (syntax->datum (local-expand c 'expression #f))) children))
            (sizes (map (lambda (c) (second c)) expanded)) ;; TODO size may depend on offset
            (symbols (map (lambda (c) (third c)) expanded))
            (codes (map (lambda (c) (fourth c)) expanded)))
       (let-values ([(labels code) (list->label-addresses symbols sizes codes 0)])
         (datum->syntax stx (format "~s" `,code))))]))
       ;;(datum->syntax stx #`(let #,labels (bytes-append #,@code))))]))

(list2
 (label2 l1)
 (call2 l1)
 (label2 l2)
 (call2 l2))