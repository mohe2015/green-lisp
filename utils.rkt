(module utils racket
  (provide unsigned signed BASE)

  (define BASE 0)
  
  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

  (define signed
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #t)))
  )