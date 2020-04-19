(module low-level racket
  (require green-lisp/architectures/x86-64/bit-port)

  ;; important pages:
  ;; interpreting the instruction reference pages:
  ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=103&zoom=auto,-17,575
  ;; introduction
  ;; https://software.intel.com/en-us/articles/introduction-to-x64-assembly
  
  ;(define-instruction x86-64-call
  ;  (_                (constant-bits '(1 1 1 0 1 0 0 0)))
  ;  (relative-address (signed-integer 32)))

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

  (let* ((original-out (open-output-bytes))
         (out (bit-port original-out '())))
    ((binary-write constant-bits) out '(0 1 0 0 0 0 0 1))
    ((binary-write signed-integer) out 32 1337)

    (let* ((original-in (open-input-bytes (get-bytes out)))
           (in (bit-port original-in '())))
      ((binary-read constant-bits) in '(0 1 0 0 0 0 0 1))
      ((binary-read signed-integer) in 32))))