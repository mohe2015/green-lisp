(module index racket
  (provide #%datum)
  (provide #%top)
  (provide #%app)
  (provide #%top-interaction)
  (provide (rename-out [module-begin #%module-begin]))
  (require (for-syntax syntax/parse))

  ;; https://docs.racket-lang.org/guide/module-languages.html

  ;; (raise-syntax-error #f "no" test)
  
  (begin-for-syntax
    (define evaluate
      (lambda (expression environment)
        (syntax-parse expression
          [x:id #`1]

          )
        )
      )
    )
  
  (define-syntax (module-begin stx)
    #`(#%module-begin #,(evaluate #`stx `()))))