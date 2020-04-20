(module low-level racket
  (require green-lisp/architectures/x86-64/bit-port)
  (require
    (for-syntax racket syntax/parse racket/syntax syntax/id-table))
  ;; important pages:
  ;; interpreting the instruction reference pages:
  ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=103&zoom=auto,-17,575
  ;; introduction
  ;; https://software.intel.com/en-us/articles/introduction-to-x64-assembly
  
  ;; https://docs.racket-lang.org/binary-class/index.html
  
  (struct binary (read write))
  
  (define constant-bits
    (binary
     (letrec ((read-constant-bits
               (lambda (in constant-bits)
                 (cond [(empty? constant-bits) #t]
                       [else
                        (if (= (read-bit in) (car constant-bits))
                            (read-constant-bits in (cdr constant-bits))
                            (error "invalid constant bits"))]))))
       read-constant-bits)
     (letrec ((write-constant-bits
               (lambda (out constant-bits)
                 (cond [(empty? constant-bits) #t]
                       [else (write-bit out (car constant-bits))
                             (write-constant-bits out (cdr constant-bits))]))))
       write-constant-bits)))

  (define constant-bytes
    (binary
     (letrec ((read-constant-bytes
               (lambda (in constant-bytes)
                 (cond [(empty? constant-bytes) #t]
                       [else
                        (if (= (bit-port-read-byte in) (car constant-bytes))
                            (read-constant-bytes in (cdr constant-bytes))
                            (error "invalid constant bytes"))]))))
       read-constant-bytes)
     (letrec ((write-constant-bytes
               (lambda (out constant-bytes)
                 (cond [(empty? constant-bytes) #t]
                       [else
                        (bit-port-write-byte out (car constant-bytes))
                        (write-constant-bytes out (cdr constant-bytes))]))))
       write-constant-bytes)))

  (define signed-integer
    (binary
     (letrec ((read-signed-integer
               (lambda (in bits)
                 (unless (= (remainder bits 8) 0)
                   (error "not multiple of 8 bits integer"))
                 (integer-bytes->integer
                  (bit-port-read-bytes in (/ bits 8))
                  #t))))
       read-signed-integer)
     (letrec ((write-signed-integer
               (lambda (out bits integer)
                 (bit-port-write-bytes out (integer->integer-bytes integer (/ bits 8) #t)))))
       write-signed-integer)))

  ;(define-instruction x86-64-call
  ;  (_                (constant-bytes '(#xe8)))
  ;  (relative-address (signed-integer 32)))


  (begin-for-syntax 
    (define-syntax-class ids
      (pattern (id* ...+) #:attr ids #'(id* ...))
      (pattern id #:attr ids #'(id))))
  
  (define-syntax (define-binary stx)
    (syntax-parse stx
      [(_ NAME:id ((FNAME:ids FTYPE:expr ARG:expr ...) ...)
          BODY ...)
       #'null]))
       
  
  (struct x86-64-call-data (relative-address) #:transparent)
  
  (define x86-64-call
    (binary
     (lambda (in)
       (let* ((_ ((binary-read constant-bytes) in '(#xe8)))
              (relative-address ((binary-read signed-integer) in 32)))
         (x86-64-call-data relative-address)))
     (lambda (out data)
       ((binary-write constant-bytes) out '(#xe8))
       ((binary-write signed-integer) out 32 (x86-64-call-data-relative-address data)))))

  (let* ((original-out (open-output-bytes))
         (out (bit-port original-out '())))
    ((binary-write x86-64-call) out (x86-64-call-data 1337))
    
    (let* ((original-in (open-input-bytes (get-bytes out)))
           (in (bit-port original-in '())))
      ((binary-read x86-64-call) in))))