(module stack-machine/index racket
  (require racket/match)
  (provide compile env-initial)

  (define (compile expression environment)
    (match expression
      [(? symbol?)  `((push ,(environment-lookup environment expression)))]
      [(? number?)  `((push ,expression))]
      [(? boolean?) `((push ,expression))]
      [`(if ,ec ,et ,ef) `(,@(compile ec environment)
                            (pop r1 r1)
                            (test r1 r1)
                            (jne :false)
                            ,@(compile et environment)
                            (jmp :end)
                            (:false)
                            ,@(compile ef environment)
                            (:end))]
      [`(let ,bindings ,body) (compile-let bindings body environment)]
      [`(lambda ,parameters ,body) (compile-lambda parameters body)] ;; TODO make this an objects as lambdas are first class objects

      [`(,f . ,args) `(
                       ,@(append* (map (compile-with environment) args))
                       ;; TODO call instruction
                       ,@(compile f environment)

                        )]
      
      ))

  (define (compile-with environment) 
    (lambda (expression) (compile expression environment)))

  (define (compile-lambda parameters body)
    (let* ((environment (env-initial 8))
           (locations (map (lambda (offset) (+ (cell-location (hash-ref environment 'base-pointer-offset)) offset 8))
                           (stream->list (in-range 0 (* 8 (length parameters)) 8))))
           (environment* (env-extend* environment parameters locations)))
      `((push rbp)
        (set! rbp rsp)
        ,@(compile body environment*) ;; should return value at current position
        (set! r1 (pop))
        (pop rbp)
        ,@(build-list (length parameters) (const '(pop)))
        (push r1)
        (ret))))
  
  (define (compile-let bindings body environment)
    (let* ((variables (map car bindings))
           (expressions (map cadr bindings))
           (values (map (compile-with environment) expressions))
           (locations (map (lambda (offset) (+ (cell-location (hash-ref environment 'base-pointer-offset)) offset 8)) (stream->list (in-range 0 (* 8 (length values)) 8))))
           (environment* (env-extend* environment variables locations)))
      `(,@(append* values)
        ,@(compile body environment*) ;; should return value at current position in stack
        (set! r1 (pop))
        ,@(build-list (length values) (const '(pop)))
        (push r1)
        )))
  
  (define (env-initial base-offset)
    (env-extend* 
     (hash 'base-pointer-offset (make-cell base-offset))
     '(+  -  /  *  <=  void  display  newline)
     (map (lambda (s) (list 'primitive s))
          `(,+ ,- ,/ ,* ,<= ,void ,display ,newline))))

  (define (env-extend* env vars values)
    (match `(,vars ,values)
      [`((,v . ,vars) (,val . ,values))
       ; =>
       (env-extend* (hash-set env v (make-cell val)) vars values)]
    
      [`(() ())
       ; =>
       env]))

  (define (environment-lookup env var)
    `(- rbp ,(cell-location (hash-ref env var))))

  (define-struct cell ([location #:mutable]))

  (compile '((lambda (a) a) 1) (env-initial 0))
  )

;; push
;; Decrements the stack pointer and then stores the source operand on the top
;; of the stack. Address and operand sizes are determined and used as follows:

;; pop
;; Loads the value from the top of the stack to the location specified with
;; the destination operand (or explicit opcode) and then increments the stack
;; pointer. The destination operand can be a general-purpose register, memory
;; loca-tion, or segment register.