(module utils racket
  (provide unsigned)
  
  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #f))))