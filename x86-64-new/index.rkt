(module index racket
  (provide (rename-out (module-begin #%module-begin))) ;; #%module-begin #%app #%datum #%top

  (define-syntax (module-begin stx)
    (raise-syntax-error #f "no" stx)))