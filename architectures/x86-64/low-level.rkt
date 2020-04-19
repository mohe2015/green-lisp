(module low-level racket
  (require green-lisp/architectures/x86-64/bit-port)
  
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