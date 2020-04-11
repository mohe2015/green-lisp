(module utils racket
  (provide unsigned signed BASE get-byte-count-to-align)

  (define BASE 0)
  
  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

  (define signed
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #t)))

  
  (define (get-byte-count-to-align alignment-bits offset)
    (- (arithmetic-shift (arithmetic-shift (+ offset (arithmetic-shift 1 alignment-bits) -1) (- alignment-bits)) alignment-bits) offset))

  )