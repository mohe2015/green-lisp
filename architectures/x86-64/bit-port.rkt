(module bit-port racket

  (struct bit-port (port pending-bits)
    #:transparent #:mutable)

  (define (read-bit bit-port)
    null

    )

  (define (bits->byte bits)
    (cond [(empty? bits) 0]
          [(empty? (rest bits)) (car bits)]
          [else (+ (arithmetic-shift (car bits) (length (cdr bits))) (bits->byte (cdr bits)))]))
  
  (define (write-bit bit-port bit)
    (set-bit-port-pending-bits! bit-port
                                (cons bit
                                      (bit-port-pending-bits bit-port)))
    (when (= 8 (length (bit-port-pending-bits bit-port)))
      (write-byte (bits->byte (bit-port-pending-bits bit-port)) (bit-port-port bit-port))
      (set-bit-port-pending-bits! bit-port '())))

  (define (get-bytes bit-port)
    (when (not (empty? (bit-port-pending-bits bit-port)))
      (error "pending bits (not full byte)"))
    (get-output-bytes (bit-port-port bit-port))
    )

  (let ((in (open-input-bytes #"Hidffs")))

    null)

  (let* ((original-out (open-output-bytes))
         (out (bit-port original-out '())))
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (write-bit out 1)
    (get-bytes out)
    )
  
  )