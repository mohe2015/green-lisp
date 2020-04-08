(module utils racket
  (provide unsigned BASE)

  (define BASE #x400000)
  
  (define unsigned
    (lambda (bits value)
      (integer->integer-bytes value (arithmetic-shift bits -3) #f))))