(module index racket
  (provide #%datum)
  (provide #%top)
  (provide #%app)
  (provide (rename-out [module-begin #%module-begin]))

  ;; https://docs.racket-lang.org/guide/module-languages.html
  
  (begin-for-syntax
    (define abc
      (lambda (test)
        3)))
  
  (define-syntax (module-begin stx)
    ;(raise-syntax-error #f "no" stx)))
    (abc stx)
    #'(#%module-begin 2)))