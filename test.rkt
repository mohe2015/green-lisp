#lang racket
(require green-lisp/test-utils)
(require (for-syntax racket/list) (for-syntax green-lisp/test-utils))

(define REX.W #b01001000)

'(define-file
  (elf
   (section text
           ... code)
   (section rodata
            ...)))

(define unsigned
  (lambda (bits value)
    (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

(define-syntax-rule (data-unsigned bytes value)
  (list bytes null (lambda (current-address) (integer->integer-bytes value bytes #f))))

(define-syntax-rule (data-bytestring bytestring)
  (list (bytes-length bytestring) null (lambda (_) bytestring)))

(define-syntax-rule (data-filled-array size)
  (list size null (lambda (_) (make-bytes size 0))))

(define (get-byte-count-to-align alignment-bits offset)
  (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 alignment-bits) -1) (- alignment-bits)) alignment-bits) offset))

(define-syntax-rule (data-align alignment-bits)
  (list (lambda (current-address) (get-byte-count-to-align alignment-bits current-address))
        null
        (lambda (current-address) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0))))

(define-syntax-rule (label symbol)
  (list 0 symbol (lambda (current-address) (bytes))))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=224&zoom=100,28,726
(define-syntax-rule (call target)
  (list 5 null (lambda (current-address) (bytes-append (bytes #xe8) (integer->integer-bytes (- target current-address 5) 4 #t)))))

(define-syntax-rule (mov-imm64 register value)
  (list 10 null (lambda (current-address)
                  (bytes-append
                   (unsigned 8 REX.W)
                   (unsigned 8 (+ #xb8 register)) ;; opcode with register
                   (unsigned 64 value))))) ;; value

(define-syntax (data-list stx)
  (syntax-case stx ()
    [(_ x ...)
     (let* ((children (syntax-e #'(x ...)))
            (expanded (map (lambda (c) (syntax->datum (local-expand c 'expression #f))) children))
            (sizes (map (lambda (c) (second c)) expanded)) ;; TODO size may depend on offset
            (symbols (map (lambda (c) (third c)) expanded))
            (codes (map (lambda (c) (fourth c)) expanded)))
       (let-values ([(labels code) (list->label-addresses symbols sizes codes 0)])
         (datum->syntax stx #`(let #,labels (bytes-append #,@code)))))]))

(data-list
 (label l1)
 (call l1)
 (mov-imm64 6 l1)
 (label l2)
 (call l2))