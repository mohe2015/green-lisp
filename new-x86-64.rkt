#lang racket
(require (for-syntax racket/list))
(provide get-the-code data-list data-align label mov-imm64 syscall push pop call add)

(define REX.W #b01001000)

(define unsigned
  (lambda (bits value)
    (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

(define-syntax-rule (data-unsigned bytes value)
  (list (lambda (_) bytes)
        null
        (lambda (current-address rodata-addresses) (integer->integer-bytes value bytes #f))
        (list)
        ))

(define-syntax-rule (data-bytestring bytestring)
  (list (lambda (_) (bytes-length bytestring))
        null
        (lambda (_ rodata-addresses) bytestring)
        (list)
        ))

(define-syntax-rule (data-filled-array size)
  (list (lambda (_) size)
        null
        (lambda (_ rodata-addresses) (make-bytes size 0))
        (list)
        ))

(define (get-byte-count-to-align alignment-bits offset)
  (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 alignment-bits) -1) (- alignment-bits)) alignment-bits) offset))

(define-syntax-rule (data-align alignment-bits)
  (list (lambda (current-address) (get-byte-count-to-align alignment-bits current-address))
        null
        (lambda (current-address rodata-addresses) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0))
        (list)
        ))

(define-syntax-rule (label symbol)
  (list (lambda (_) 0)
        symbol
        (lambda (current-address rodata-addresses) (bytes))
        (list)
        ))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=224&zoom=100,28,726
(define-syntax-rule (call target)
  (list (lambda (_) 5)
        null
        (lambda (current-address rodata-addresses) (bytes-append (bytes #xe8) (integer->integer-bytes (- target current-address 5) 4 #t)))
        (list)
        ))

(define-syntax-rule (syscall)
  (list (lambda (_) 2)
        null
        (lambda (_ rodata-addresses) (bytes #x0f #x05))
        (list)))

(define-syntax-rule (mov-imm8 register value)
  (list (lambda (_) (if (= register 7) 3 2))
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (if (= the-register 7)
               (unsigned 8 #x40)
               (bytes)) ; REX prefix to access dil instead of bh
           (bytes (bitwise-ior #xb0 the-register) (dynamic the-value))))
        (list)
        ))

(define-syntax-rule (mov-imm64 register value)
  (list (lambda (_) 10)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 (+ #xb8 register)) ;; opcode with register
           (unsigned 64 value)))
        (list)
        )) ;; value

(define-syntax-rule (mov-string register value)
  (list (lambda (_) 10)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 (+ #xb8 register)) ;; opcode with register
           (unsigned 64 1))) ;; (first rodata-addresses)
        (list value) ;; .rodata
        ))

(define-syntax-rule (jmp target)
  (list (lambda (_) 2)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append (bytes #xeb) (integer->integer-bytes (- target current-address 2) 1 #t)))
        (list)
        ))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1163&zoom=100,-7,754
(define-syntax-rule (push register)
  (list (lambda (_) 1)
        null
        (lambda (_ rodata-addresses)
          (bytes (+ #x50 register)))
        (list)
        ))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1037&zoom=auto,-17,727
(define-syntax-rule (pop register)
  (list (lambda (_) 1)
        null
        (lambda (_ rodata-addresses)
          (bytes (+ #x58 register)))
        (list)
        ))
;; (bytes-append (bytes #x8f) (integer->integer-bytes the-register 1 #f)))
 
;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=133&zoom=100,-7,726
(define-syntax-rule (add destination source)
  (list (lambda (_) 3)
        null
        (lambda (_ rodata-addresses)
          ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=40&zoom=100,28,745
          (bytes REX.W 01 (mod11-to-binary destination source)))
        (list)
        ))

(begin-for-syntax
  (define (list->label-addresses symbols sizes codes rodatas offset rodata-offset)
    (cond [(or (null? symbols) (null? sizes) (null? codes)) (values (list)
                                                                    (list)
                                                                    (list) ;; .rodata
                                                                    )]
          [else
           (let* ([current-element-symbol (car (generate-temporaries '(element)))] ;; syntax element
                  [symbol (car symbols)] ;; syntax element
                  [size #`(#,(car sizes) #,current-element-symbol)] ;; syntax element of lambda call e.g. code that calculates alignment size
                  [code (car codes)]
                  [rodata (car rodatas)]
                  [current-rodata-size-symbol (car (generate-temporaries '(rodata-size)))]
                  [rodata-size #`(foldl + 0 (map (lambda (a) (bytes-length a)) #,rodata))]
                  ) ;; syntax element)
             (let-values ([(cara carb carc) (if (eq? (syntax-e symbol) 'null)
                                           (values
                                            (list (list current-element-symbol offset) (list current-rodata-size-symbol rodata-offset))
                                            `(, #`(#,code #,current-element-symbol #,current-rodata-size-symbol))
                                            `((rodata)) ;; .rodata
                                            )
                                           (values
                                            (list (list current-element-symbol offset) (list current-rodata-size-symbol rodata-offset) (list symbol current-element-symbol))
                                            `(, #`(#,code #,current-element-symbol #,current-rodata-size-symbol))
                                            `((rodata)) ;; .rodata
                                            ))]
                          [(cdra cdrb cdrc) (list->label-addresses
                                             (cdr symbols)
                                             (cdr sizes)
                                             (cdr codes)
                                             (cdr rodatas)
                                             #`(+ #,current-element-symbol #,size)
                                             #`(+ #,current-rodata-size-symbol #,rodata-size) ;; rodata-offset
                                             )])
               (values
                (append cara cdra) ;; labels
                (append carb cdrb) ;; code
                (append carc cdrc) ;; .rodata
                )))])))

;; TODO get offset and then return an object like all the other macros
(define-syntax (data-list stx)
  (syntax-case stx ()
    [(_ x ...)
     (let* ((children (syntax-e #'(x ...))) ;; list of syntax children
            (expanded (map (lambda (c) (syntax-e (syntax-disarm (local-expand c 'expression #f) #f))) children)) ;; expanded list of children (with syntax elements)
            (sizes (map (lambda (c) (second c)) expanded)) ;; syntax list of all sizes
            (symbols (map (lambda (c) (third c)) expanded)) ;; syntax list of all symbols
            (codes (map (lambda (c) (fourth c)) expanded)) ;; syntax list of all codes
            (rodatas (map (lambda (c) (fifth c)) expanded)) ;; syntax list of .rodata
            (tainted (map (lambda (c) (syntax-tainted? c)) sizes)))
       (let-values ([(labels code rodata) (list->label-addresses symbols sizes codes rodatas 0 0)])
         #`(let* #,labels
             ;;(println #,rodata)
             (bytes-append #,@code))))]))

(define (get-the-code)
  (data-list
   (label code-start)

   (mov-imm64 2 18)  ; dl / rdx: length of string
   (mov-string 6 #"test") ;; rsi load string -> should be able to return .data data -> maybe gets passed the address later
   (mov-imm64 0 1)  ; al / rax: set write to command
   (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
   (syscall) ;; write(stdout, "Hello\n")
   ;; TODO check return value?

   (mov-imm64 2 1024) ;; rdx: buffer length?
   (mov-imm64 6 code-start) ;; rsi: buffer?
   (mov-imm64 7 1) ;; rdi: stdin?
   (mov-imm64 0 0) ;; rax: read syscall
   (syscall) ;; read(stdin, buffer, 1024)
   ;; CHECK RETURN VALUE!

   ;; write "Hello "
   (mov-imm64 2 6)  ; dl / rdx: length of string
   (mov-imm64 6 code-start) ;; rsi load string
   (mov-imm64 0 1)  ; al / rax: set write to command
   (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
   (syscall)


   ;; echo
   ;; TODO mov rdx, rax
   (mov-imm64 2 1024)  ; dl / rdx: length of string

   (mov-imm64 6 code-start) ;; rsi load string
   (mov-imm64 0 1)  ; al / rax: set write to command
   (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
   (syscall)

   (mov-imm64 0 60) ;; rax: exit syscall
   (mov-imm64 7 0)  ;; rdi: exit code
   (syscall) ;; exit(0)

   (push 1)
   (pop 1)
   (call code-start)
   ;;(jmp -2) ;; size of jmp instruction

   ;; TODO overflow
   (label +)
   (pop 0)
   (pop 1)
   ; (add (register eax) (register ecx))
   (push 0)

   (label code-end)))




;; alternative proposal
;;'(define-method test (jo)
;;   "hi")
;; returns a function object
;; has fields like symbols (test, jo), code, data (), rodata ("hi"), size, .bss ()
;; compiler gets those and combines them
;; can also optimize by moving around things etc., dead code elimination, etc,
;; then it calls the objects get-bytes with data map as parameter from which get-bytes can get the addresses (what about PIE?)

;; macroexpanding, ... before all that, automated code rewriting, ..., inlining

;; but still source code location needs to be preserved so maybe store it inside of the objects

;; lambda macro should return object that accepts address and returns the code for that lambda
;; locally resolves labels
;; maybe labels not even required in some cases because they can be optimized away before

;; should be PIE code so the offset should be irrelevant?
;; except for strings its currently not absolute

;; TODO conditionals
;; (if (= a 1) 1 0)
;; converted into cmp, ... by the if macro?

;; variable itself are hard to implement...
;; with macros? - check how (maybe stack with currently live variables), but functional...

;; jumps to symbols are hard to implement (global jumps to other function, local jumps for loops etc.) - functional...

;; memory allocation:
;; https://linux.die.net/man/2/mmap2
;; https://linux.die.net/man/2/mremap
;; (new test-object%)
;; -> (call allocate-bytes 10)
;; check for 0
;; (initialize-test-object pointer)
;; done

;; http://matt.might.net/articles/implementing-a-programming-language/

;; but maybe this is the quick proposal? (no, do it right the first time)
