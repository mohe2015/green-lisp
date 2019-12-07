(module binary-format typed/racket

  (require/typed racket [index-of (-> (Listof Any) Any Integer)])
  ;; this should also support bit-elements

  ;; https://refspecs.linuxfoundation.org/elf/ARMELF.pdf

  ;; TODO replace this with the define enum declaration
  (define-type ei_class_type (U 'class_none 'class_32 'class_64))

  (: ei_class_list (Listof ei_class_type))
  (define ei_class_list '(class_none class_32 class_64))

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