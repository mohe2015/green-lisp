(module low-level racket
  (require green-lisp/architectures/x86-64/bit-port)
  
  ;(define-instruction x86-64-call
  ;  (_                (constant-bits '(1 1 1 0 1 0 0 0)))
  ;  (relative-address (signed-integer 4)))

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
     
  (let* ((original-in (open-input-bytes #"A"))
         (in (bit-port original-in '())))
    ((binary-read constant-bits) in '(0 1 0 0 0 0 0 1)))

   (let* ((original-out (open-output-bytes))
          (out (bit-port original-out '())))
     ((binary-write constant-bits) out '(0 1 0 0 0 0 0 1))
     (get-bytes out))
  )