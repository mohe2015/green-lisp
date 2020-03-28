#lang racket
(require (only-in racket/base (write-byte racket-write-byte)))

(define bit-writer%
  (class object%
    (define bits '())

    (super-new)

    (define/public (write-bit bit)
      (set! bits (cons bit bits)))

    (define/public (write-byte byte)
      (for ((i (in-range 7 -1 -1)))
        (write-bit (if (bitwise-bit-set? byte i) 1 0))))

    (define/public (write-to-file file)
      (call-with-output-file file
        (lambda (out)
          (let ((bits (reverse bits)))
            (for ((byte-index (in-range 0 (length bits) 8)))
              (let ((byte 0))
                (for ((bit-index (in-range 8)))
                  (set! byte (bitwise-ior byte (arithmetic-shift (car bits) bit-index)))
                  (displayln byte)
                  (set bits (cdr bits)))
                (racket-write-byte byte out)))))
          #:mode 'binary #:exists 'truncate/replace))
    
    (define/public (get-bits)
      (reverse bits))))

(define (jmp writer displacement)
  (send writer write-byte #xeb)
  (send writer write-byte displacement))


(let ((writer (new bit-writer%)))
  (jmp writer #xfe)
  (send writer get-bits)
  (send writer write-to-file "/tmp/a.bin"))