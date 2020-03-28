(module binary-format typed/racket
  
  (require/typed macro-debugger/stepper
                 (expand/step (-> Syntax Any)))
  (require/typed macro-debugger/syntax-browser
                 (browse-syntax (-> Syntax Any)))
  (require/typed macro-debugger/stepper-text
                 (expand/step-text (-> Syntax Any)))
  ;(require macro-debugger/emit)
  ;(require macro-debugger/expand)
  (require/typed racket
                 [index-of (-> (Listof Any) Any Integer)])
  (require (for-syntax syntax/parse))
  
  ;; this should also support bit-elements

  ;; https://refspecs.linuxfoundation.org/elf/ARMELF.pdf

  ;; TODO replace this with the define enum declaration
  (define-type ei_class_type (U 'class_none 'class_32 'class_64))

  (define ei_class_list : (Listof ei_class_type) '(class_none class_32 class_64))

  (define ei_class
    (class object% 
      (super-new)

      (: abc ei_class_type)
      (define abc 'class_none)

      (define/public (read [in : Input-Port])
        (set! abc (list-ref ei_class_list (cast (read-byte in) Byte))))

      (define/public (write [out : Output-Port])
        (write-byte (cast (index-of ei_class_list abc) Byte) out))
    
      ))
  
  '(define-enum ei_class
     ((ELFCLASSNONE 0 "Invalid class")
      (ELFCLASS32 1 "32-bit objects")
      (ELFCLASS64 2 "64-bit objects")))

  (begin-for-syntax
    (define-syntax-class typed-name
      #:description "typed name"
      (pattern (name:id type:id)))

    (define-syntax-class unique-typed-names
      #:description "unique typed names"
      (pattern (typed-name:typed-name ...)
               #:fail-when (check-duplicate-identifier
                            (syntax->list #'(typed-name.name ...)))
               "duplicate variable name")))

  (define-syntax-rule (define-binary-class name unique-typed-names)
    (begin
      (print #'unique-typed-names)
      (define name
        (class object% 
          (super-new)

          ; (: abc ei_class_type)
          ; (define abc 'class_none)

          (define/public (read [in : Input-Port])
            `(print #'(car (syntax-e unique-typed-names)))
            
            null)

          (define/public (write [out : Output-Port])
            null)
    
          ))))

  (define-binary-class elephant ((test ei_class)))

  '(define-binary-class e_ident
     ((EI_MAG0 (constant #x7f)
               EI_MAG1 (constant #\E)
               EI_MAG2 (constant #\L)
               EI_MAG3 (constant #\F)
               ;;        EI_CLASS ei_class
               ;       EI_DATA
               ;       EI_VERSION
               ;       EI_PAD
               ;       EI_NIDENT
               )))

  '(define-binary-class elf-header
     ((e_ident e_ident)
      (e_type Elf32_Half)
      (e_machine Elf32_Half)
      (e_version Elf32_Word)
      (e_entry Elf32_Addr)
      (e_phoff Elf32_Off)
      (e_shoff Elf32_Off)
      (e_flags Elf32_Word)))

  '(define-binary-class elf-file
     ((elf-header elf-header)))

  (call-with-input-file "hello"
    (λ ([in : Input-Port])
      (print (cast (read-byte in) Byte))
      (assert (= (cast (read-byte in) Byte) (char->integer #\E)))


      ))
  )