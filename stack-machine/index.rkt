(module stack-machine/index racket/base
  (require racket/match)

  (define (compile expression environment)
    (match expression
      [(? number?) `(push ,expression)]


      [`(let ,bindings ,body) (compile-let bindings body environment)]


      ))

  (define (compile-with environment) 
    (lambda (expression) (compile expression environment)))

  (define (compile-let bindings body environment)
    (let* ((variables (map car bindings))
           (expressions (map cadr bindings))
           (values (map (compile-with environment) expressions))
           (environment* (env-extend* environment variables values)))
      (compile body environment*)))

  (define (env-empty) (hash))

  (define (env-extend* env vars values)
    (match `(,vars ,values)
      [`((,v . ,vars) (,val . ,values))
       ; =>
       (env-extend* (hash-set env v (make-cell val)) vars values)]
    
      [`(() ())
       ; =>
       env]))

  (define-struct cell ([value #:mutable]))
  
  (compile '(let ((a 1)) 1) (env-empty))

  )