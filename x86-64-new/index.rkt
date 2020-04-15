(module index racket
  (provide #%datum)
  (provide #%top)
  (provide #%app)
  (provide #%top-interaction)
  (provide (rename-out [module-begin #%module-begin]))
  (require (for-syntax syntax/parse))

  ;; https://docs.racket-lang.org/guide/module-languages.html

  ;; (raise-syntax-error #f "no" test)
  ;; https://docs.racket-lang.org/syntax/stxparse-patterns.html
  ;; https://docs.racket-lang.org/syntax/Parsing_Syntax.html
  ;; https://docs.racket-lang.org/reference/stxops.html
  
  (begin-for-syntax
    (define environment-lookup
      (lambda (environment expression)
        #`1337))
    
    (define evaluate
      (lambda (expression environment)
        (syntax-parse expression
          [x:id (cond
                  [(symbol? (syntax-e #'x))
                   (environment-lookup environment expression)]
                  [(number? (syntax-e #'x))
                   #`2]


                  )
                ]
          )
        )
      )
    )
  
  (define-syntax module-begin
    (lambda (stx)
      (syntax-case stx ()
        [(_ body) #`(#%module-begin (quote #,(evaluate #`body `())))]))))