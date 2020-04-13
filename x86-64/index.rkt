#lang racket
(require green-lisp/x86-64/modrm green-lisp/x86-64/rd green-lisp/elf/symbol green-lisp/utils)
(require (for-syntax racket/list))
(provide get-the-code data-list data-align label mov-imm64 syscall push pop call add)

;; important pages:
;; interpreting the instruction reference pages:
;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=103&zoom=auto,-17,575
;; introduction
;; https://software.intel.com/en-us/articles/introduction-to-x64-assembly

(define REX.W #b01001000)

(define-syntax-rule (data-unsigned bytes value)
  (list (lambda (_) bytes)
        null
        (lambda (current-address rodata-addresses) (integer->integer-bytes value bytes #f))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (data-bytestring bytestring)
  (list (lambda (_) (bytes-length bytestring))
        null
        (lambda (_ rodata-addresses) bytestring)
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (data-filled-array size)
  (list (lambda (_) size)
        null
        (lambda (_ rodata-addresses) (make-bytes size 0))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (data-align alignment-bits)
  (list (lambda (current-address) (get-byte-count-to-align alignment-bits current-address))
        null
        (lambda (current-address rodata-addresses) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0))
        (list)
        (lambda (_) (list))
        ))

(define-syntax (label stx)
  (syntax-case stx ()
    [(global-symbol symbol)
     #`(list (lambda (_) 0)
             symbol
             (lambda (current-address rodata-addresses) (bytes))
             (list)
             (lambda (current-address)
               (list (new elf-symbol% [name #,(string->bytes/utf-8 (symbol->string (syntax-e #'symbol)))] [type 'func] [binding 'local] [section #".text"] [value current-address] [size #xc7]))) ;; TODO FIXME
             )]))

(define-syntax (global-symbol stx)
  (syntax-case stx ()
    [(global-symbol symbol)
     #`(list (lambda (_) 0) ;; size
             symbol ;; local symbol(s)
             (lambda (current-address rodata-addresses) (bytes)) ;; code
             (list) ;; rodata-list
             (lambda (current-address)
               (list (new elf-symbol% [name #,(string->bytes/utf-8 (symbol->string (syntax-e #'symbol)))] [type 'func] [binding 'global] [section #".text"] [value current-address] [size #xc7]))) ;; TODO FIXME
             )])) ;; elf-symbols

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=224&zoom=100,28,726
(define-syntax-rule (call target)
  (list (lambda (_) 5)
        null
        (lambda (current-address rodata-addresses) (bytes-append (bytes #xe8) (integer->integer-bytes (- target current-address 5) 4 #t)))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (jmp target)
  (list (lambda (_) 5)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append (bytes #xe9) (integer->integer-bytes (- target current-address 5) 4 #t)))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (syscall)
  (list (lambda (_) 2)
        null
        (lambda (_ rodata-addresses) (bytes #x0f #x05))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (ret)
  (list (lambda (_) 1)
        null
        (lambda (_ rodata-addresses) (bytes #xc3))
        (list)
        (lambda (_) (list))
        ))

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
        (lambda (_) (list))
        ))

(define-syntax-rule (mov-imm64 register value)
  (list (lambda (_) 10)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 (+ #xb8 (rd64-to-binary 'register))) ;; opcode with register
           (unsigned 64 value)))
        (list)
        (lambda (_) (list))
        )) ;; value

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=686&zoom=auto,-16,19
(define-syntax-rule (mov destination source)
  (list (lambda (_) 3)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 #x89)
           (unsigned 8 (mod11-to-binary 'destination 'source))))
        (list)
        (lambda (_) (list))
        ))

(define-syntax-rule (lea-string register value)
  (list (lambda (_) 7)
        null
        (lambda (current-address rodata-addresses)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 #x8d) ;; opcode with register

           (unsigned 8 (mod00-to-binary 'register '(displacement32 rip)))

           ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=37&zoom=100,28,269
           (signed 32 (- rodata-addresses current-address 7)) ;; displacement
          ))
        (list value) ;; .rodata
        (lambda (_) (list))
        ))

(define-syntax (let-string stx)
  (syntax-case stx ()
    [(let-string string-register string-size-register string)
     #`(data-list (lea-string string-register string)
                  (mov-imm64 string-size-register (bytes-length string)))]))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1163&zoom=100,-7,754
(define-syntax-rule (push register)
  (list (lambda (_) 1)
        null
        (lambda (_ rodata-addresses)
          (bytes (+ #x50 (rd64-to-binary 'register))))
        (list)
        (lambda (_) (list))
        ))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1037&zoom=auto,-17,727
(define-syntax-rule (pop register)
  (list (lambda (_) 1)
        null
        (lambda (_ rodata-addresses)
          (bytes (+ #x58 (rd64-to-binary 'register))))
        (list)
        (lambda (_) (list))
        ))
;; (bytes-append (bytes #x8f) (integer->integer-bytes the-register 1 #f)))
 
;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=133&zoom=100,-7,726
(define-syntax-rule (add destination source)
  (list (lambda (_) 3)
        null
        (lambda (_ rodata-addresses)
          ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=40&zoom=100,28,745
          (bytes REX.W 01 (mod11-to-binary 'destination 'source)))
        (list)
        (lambda (_) (list))
        ))

(begin-for-syntax
  (define (list->label-addresses symbols sizes codes rodatas real-symbols offset rodata-offset)
    (cond [(or (null? symbols) (null? sizes) (null? codes)) (values (list)
                                                                    (list)
                                                                    (list)
                                                                    (list) ;; .rodata
                                                                    (list)
                                                                    )]
          [else
           (let* ([current-element-symbol (car (generate-temporaries '(element)))] ;; syntax element
                  [symbol (car symbols)] ;; syntax element
                  [size #`(#,(car sizes) #,current-element-symbol)] ;; syntax element of lambda call e.g. code that calculates alignment size
                  [code (car codes)]
                  [rodata (car rodatas)]
                  [real-symbol (car real-symbols)]
                  [current-rodata-size-symbol (car (generate-temporaries '(rodata-size)))]
                  [rodata-size #`(foldl + 0 (map (lambda (a) (bytes-length a)) #,rodata))]
                  ) ;; syntax element)
             (let-values ([(cara carb carc card care)
                           (values
                            (if (eq? (syntax-e symbol) 'null)
                                (list ;; data labels
                                 (list current-element-symbol offset))
                                (list ;; data labels
                                 (list current-element-symbol offset)
                                 (list symbol current-element-symbol)))
                                             
                            (list (list current-rodata-size-symbol rodata-offset)) ;; rodata labels
                                             
                            `(, #`(#,code #,current-element-symbol #,current-rodata-size-symbol)) ;; code
                            `(, #`(bytes-append* #,rodata)) ;; .rodata
                            `(, #`(#,real-symbol #,current-element-symbol)) ;; real symbols
                            )]
                          [(cdra cdrb cdrc cdrd cdre)
                           (list->label-addresses
                            (cdr symbols)
                            (cdr sizes)
                            (cdr codes)
                            (cdr rodatas)
                            (cdr real-symbols)
                            #`(+ #,current-element-symbol #,size)
                            #`(+ #,current-rodata-size-symbol #,rodata-size) ;; rodata-offset
                            )])
               (values
                (append cara cdra) ;; code labels
                (append carb cdrb) ;; rodata labels
                (append carc cdrc) ;; code
                (append card cdrd) ;; .rodata
                (append care cdre) ;; real symbols
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
            (real-symbols (map (lambda (c) (sixth c)) expanded)) ;; syntax list of elf symbol lambdas
            (.code-base-symbol (car (generate-temporaries '(.code-base))))
            (.rodata-base-symbol (car (generate-temporaries '(.rodata-base))))
            )
       (let-values ([(code-labels rodata-labels code rodata real-symbols) (list->label-addresses symbols sizes codes rodatas real-symbols .code-base-symbol .rodata-base-symbol)]) ;; TODO calculate this shit
         #`(list
            ;; FIRST IS BASICALLY THIRD'S LENGTH (TODO MAYBE OPTIMIZE AWAY)
            (lambda (code-address) (+ #,@(map (lambda (size) #`(#,size 0)) sizes))) ;; TODO FIXME the zero is wrong and doesn't work with aligned things

            ;; SECOND JUST null as no symbols are returned to parent?
            null
            
            (lambda (#,.code-base-symbol #,.rodata-base-symbol)
              (let* (#,@code-labels #,@rodata-labels)
                (bytes-append #,@code))) ;; exactly like the [THIRD]

            (list #,@rodata) ;; returns rodata as bytes instead of list -> could be changed? [FOURTH]
            
            (lambda (#,.code-base-symbol)
              (let* #,code-labels
                (append #,@real-symbols))) ;; elf-symbols [FIFTH]
            
            )))]))

(define get-the-code
  (data-list
   (global-symbol green_lisp_demo)
   
   (let-string rsi rdx #"What is your name?\n\0")
   (call write)

   (let-string rsi rdx #"EEEEEEEEE\0")
   (call read)

   (mov rdx rax)
   (call write)

   (mov-imm64 rdi 0)
   (call exit)
   (ret)

   ;; rsi string, rdx string-length
   (global-symbol write) ;; TODO these need a size
   (mov-imm64 rax 1)  ; al / rax: set write to command
   (mov-imm64 rdi 1)  ; bh / dil / rdi: set destination index to 1 (stdout)
   (syscall)
   ;; TODO check return value
   (ret)

   ;; rsi buffer, rdx buffer-length
   (global-symbol read)
   (mov-imm64 rdi 1) ;; rdi: stdin?
   (mov-imm64 rax 0) ;; rax: read syscall
   (syscall) ;; read(stdin, buffer, 1024)
   (ret)

   ;; rdi exit-code
   (global-symbol exit)
   (mov-imm64 rax 60) ;; rax: exit syscall
   (syscall) ;; exit(0) -> this should quit the process
   ;; check return value anyways?
   (ret)
   
   ))

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
