#lang racket

(define bit-writer%
  (class object%
    (define bits '())

    (super-new)

    (define/public (write-bit bit)
      (set! bits (cons bit bits)))

    (define/public (write-byte byte)
      (for ((i (in-range 7 -1 -1)))
        (write-bit (if (bitwise-bit-set? byte i) 1 0))))

    (define/public (get-bits)
      (reverse bits))))

(let ((writer (new bit-writer%)))
  (send writer write-byte 1)
  (send writer write-byte 2)
  (send writer get-bits))