(module bit-port racket/base

  (struct bit-port (port pending-bits)
    #:transparent #:mutable)

  (define (read-bit bit-port)
    null

    )

  (define (write-bit bit-port bit)
    (set-bit-port-pending-bits! bit-port
                                (cons bit
                                      (bit-port-pending-bits bit-port))))

  (let ((in (open-input-bytes #"Hidffs")))

    null)

  (let* ((original-out (open-output-bytes))
         (out (bit-port original-out '())))
    (write-bit out 1)
    (get-output-bytes original-out)
    )
  
  )