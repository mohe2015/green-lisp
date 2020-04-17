(module bit-port racket

  (struct bit-port (port pending-bits)
    #:transparent #:mutable)

  (define (read-bit bit-port)
    (when (empty? (bit-port-pending-bits bit-port))
      (set-bit-port-pending-bits! bit-port (byte->bits (read-byte (bit-port-port bit-port)))))
    (let ((bits (bit-port-pending-bits bit-port)))
      (set-bit-port-pending-bits! bit-port (cdr bits)) 
      (car bits)))

  (define (byte->bits byte)
    (list (if (bitwise-bit-set? byte 7) 1 0)
          (if (bitwise-bit-set? byte 6) 1 0)
          (if (bitwise-bit-set? byte 5) 1 0)
          (if (bitwise-bit-set? byte 4) 1 0)
          (if (bitwise-bit-set? byte 3) 1 0)
          (if (bitwise-bit-set? byte 2) 1 0)
          (if (bitwise-bit-set? byte 1) 1 0)
          (if (bitwise-bit-set? byte 0) 1 0)))

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

  (let* ((original-in (open-input-bytes #"Aidffs"))
         (in (bit-port original-in '())))
    (println (read-bit in))
    (println (read-bit in))
    (println (read-bit in))
    (println (read-bit in))
    (println (read-bit in)))

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