(module stack-machine/index racket
  (require racket/match)

  (define (compile expression environment)
    (match expression
      [(? number?) `((push ,expression))]
      [`(let ,bindings ,body) (compile-let bindings body environment)]
      ))

  (define (compile-with environment) 
    (lambda (expression) (compile expression environment)))

  (define (compile-let bindings body environment)
    (let* ((variables (map car bindings))
           (expressions (map cadr bindings))
           (values (map (compile-with environment) expressions))
           (environment* (env-extend* environment variables values)))
      `(,@(append* values)
        ,@(compile body environment*) ;; should return value at current position in stack
        (set! r1 (pop))
        (popn ,(length values))
        (push r1)
        )))

  (define (env-empty) (hash))

  (define (env-extend* env vars values)
    (match `(,vars ,values)
      [`((,v . ,vars) (,val . ,values))
       ; =>
       (env-extend* (hash-set env v (make-cell val)) vars values)]
    
      [`(() ())
       ; =>
       env]))

  (define-struct cell ([location #:mutable]))
  
  (compile '(let ((a 1)) 2) (env-empty))

  )