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
  (list (lambda (_) bytes) null (lambda (current-address) (integer->integer-bytes value bytes #f))))

(define-syntax-rule (data-bytestring bytestring)
  (list (lambda (_) (bytes-length bytestring)) null (lambda (_) bytestring)))

(define-syntax-rule (data-filled-array size)
  (list (lambda (_) size) null (lambda (_) (make-bytes size 0))))

(define (get-byte-count-to-align alignment-bits offset)
  (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 alignment-bits) -1) (- alignment-bits)) alignment-bits) offset))

(define-syntax-rule (data-align alignment-bits)
  (list (lambda (current-address) (get-byte-count-to-align alignment-bits current-address))
        null
        (lambda (current-address) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0))))

(define-syntax-rule (label symbol)
  (list (lambda (_) 0) symbol (lambda (current-address) (bytes))))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=224&zoom=100,28,726
(define-syntax-rule (call target)
  (list (lambda (_) 5) null (lambda (current-address) (bytes-append (bytes #xe8) (integer->integer-bytes (- target current-address 5) 4 #t)))))

(define-syntax-rule (syscall)
  (list (lambda (_) 2) null (bytes #x0f #x05)))

(define-syntax-rule (mov-imm8 register value)
  (list (lambda (_) (if (= register 7) 3 2))
        null
        (lambda (current-address)
          (bytes-append
           (if (= the-register 7)
               (unsigned 8 #x40)
               (bytes)) ; REX prefix to access dil instead of bh
           (bytes (bitwise-ior #xb0 the-register) (dynamic the-value))))))

(define-syntax-rule (mov-imm64 register value)
  (list (lambda (_) 10)
        null
        (lambda (current-address)
          (bytes-append
           (unsigned 8 REX.W)
           (unsigned 8 (+ #xb8 register)) ;; opcode with register
           (unsigned 64 value))))) ;; value

(define-syntax-rule (jmp target)
  (list (lambda (_) 2)
        null
        (lambda (current-address)
          (bytes-append (bytes #xeb) (integer->integer-bytes (- target current-address 2) 1 #t)))))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1163&zoom=100,-7,754
(define-syntax-rule (push register)
  (list (lambda (_) 1)
        null
        (lambda (_)
          (bytes (+ #x50 the-register)))))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1037&zoom=auto,-17,727
(define-syntax-rule (pop register)
  (list (lambda (_) 1)
        null
        (lambda (_)
          (bytes (+ #x58 the-register)))))
;; (bytes-append (bytes #x8f) (integer->integer-bytes the-register 1 #f)))
 
;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=133&zoom=100,-7,726
(define-syntax-rule (add destination source)
  (list (lambda (_) 3)
        null
        (lambda (_)
          ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=40&zoom=100,28,745
          (bytes REX.W 01 (mod11-to-binary destination source)))))

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
 (data-align 12)
 (label code-start)

 (mov-imm64 2 18)  ; dl / rdx: length of string
;; (mov-imm64 6 your-name-question-string) ;; rsi load string
 (mov-imm64 0 1)  ; al / rax: set write to command
 (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
 (syscall) ;; write(stdout, "Hello\n")
 ;; TODO check return value?

 (mov-imm64 2 1024) ;; rdx: buffer length?
 (mov-imm64 6 buffer) ;; rsi: buffer?
 (mov-imm64 7 1) ;; rdi: stdin?
 (mov-imm64 0 0) ;; rax: read syscall
 (syscall) ;; read(stdin, buffer, 1024)
 ;; CHECK RETURN VALUE!

 ;; write "Hello "
 (mov-imm64 2 6)  ; dl / rdx: length of string
 (mov-imm64 6 greet-string) ;; rsi load string
 (mov-imm64 0 1)  ; al / rax: set write to command
 (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
 (syscall)

       
 ;; echo
 ;; TODO mov rdx, rax
 (mov-imm64 2 1024)  ; dl / rdx: length of string
       
 (mov-imm64 6 buffer) ;; rsi load string
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
 (add (register eax) (register ecx))
 (push 0)
       
 ;; TODO .bss section for buffers etc.

 ;; TODO conditionals

 ;; memory allocation:
 ;; https://linux.die.net/man/2/mmap2
 ;; https://linux.die.net/man/2/mremap
       
 (label code-end))