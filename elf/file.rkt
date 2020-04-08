(module file racket
  (require green-lisp/utils green-lisp/elf/section green-lisp/elf/string-table)
  (provide elf-file%)

  (define ELFMAG0 #x7f) ;; /* EI_MAG */
  (define ELFMAG1 (char->integer #\E))
  (define ELFMAG2 (char->integer #\L))
  (define ELFMAG3 (char->integer #\F))

  (define ELFCLASS64 2)

  (define ELFDATA2LSB 1) ;; TODO maybe also support msb???

  (define EV_CURRENT 1)

  (define ELFOSABI_SYSV 0)

  ;; e_type
  (define ET_REL  1)
  (define ET_EXEC 2)
  (define ET_DYN  3)
  (define ET_CORE 4)

  (define EM_X86_64 62)

  ;; immutable elf class with merge function to create clean functional code
  (define elf-file%
    (class object%
      (super-new)
      (init-field
       [sections'()] ;; TODO null section
       [program-headers '()]
       [symbols '()]) ;; formally this is also just a section ;; TODO null symbol

      (define/public (get-symbols)
        symbols)

      (define/public (get-sections)
        sections)

      (define/public (get-program-headers)
        program-headers)
    
      (define/public (merge that)
        (new elf-file%
             [sections (append sections (get-field sections that))]
             [program-headers (append program-headers (get-field program-headers that))]
             [symbols (append symbols (get-field symbols that))]))

      (define (get-sections-bytes section-header-string-table remaining-sections current-offset)
        (cond [(null? remaining-sections) (bytes)]
              [else
               (let* ((current-section (car remaining-sections))
                      (section-string-offset (send section-header-string-table get-string-offset (get-field name current-section)))
                      (section-bytes (send current-section get-bytes current-offset section-string-offset)))
                 (bytes-append section-bytes
                               (get-sections-bytes section-header-string-table (cdr remaining-sections) (+ current-offset (bytes-length (get-field content current-section))))))]))

      ;; not every section has a program header (at least in our simplified implementation)
      (define (get-program-headers-bytes remaining-sections remaining-program-headers current-offset)
        (cond [(null? remaining-program-headers) (bytes)]
              [else
               (let ((current-section (car remaining-sections))
                      (current-program-header (car remaining-program-headers)))
                 (cond [(eq? (get-field section current-program-header) current-section)
                       (let* ((program-header-bytes (send current-program-header get-bytes current-offset (bytes-length (get-field content current-section)))))
                         (bytes-append program-header-bytes
                                       (get-program-headers-bytes (cdr remaining-sections) (cdr remaining-program-headers) (+ current-offset (bytes-length (get-field content current-section))))))]
                       [else
                        (get-program-headers-bytes (cdr remaining-sections) remaining-program-headers (+ current-offset (bytes-length (get-field content current-section))))]
                       ))]))
      
      (define/public (internal-get-bytes section-header-string-table)
        (bytes-append*
         (get-elf-header-bytes) ;; 64
         null-section-header ;; 64
         (get-sections-bytes section-header-string-table sections (+ 128 (* 64 (length sections)) (* 56 (length program-headers))))
         (get-program-headers-bytes sections program-headers (+ 128 (* 64 (length sections)) (* 56 (length program-headers))))
         (map (lambda (s) (get-field content s)) sections)
         ))
    
      (define/public (get-bytes)      
        (let* ((section-header-string-table (new elf-string-table% [strings (cons #".shstrtab" (map (lambda (section) (get-field name section)) sections))]))
               (section-header-string-table-bytes (send section-header-string-table get-bytes))
               (section-header-string-table-section (new elf-section%
                                                         [name #".shstrtab"]
                                                         [type 'strtab]
                                                         [content section-header-string-table-bytes]))
               (new-elf-file (merge (new elf-file% [sections (list section-header-string-table-section)]))))
          (send new-elf-file internal-get-bytes section-header-string-table)))

      (define/public (get-elf-header-bytes) ;; 64 bytes
        (bytes-append
         (unsigned 8 ELFMAG0)
         (unsigned 8 ELFMAG1)
         (unsigned 8 ELFMAG2)
         (unsigned 8 ELFMAG3)
         (unsigned 8 ELFCLASS64)
         (unsigned 8 ELFDATA2LSB)
         (unsigned 8 EV_CURRENT)
         (unsigned 8 ELFOSABI_SYSV)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)
         (unsigned 8 0)

         (unsigned 16 ET_EXEC) ;; e_type
         (unsigned 16 EM_X86_64) ;; e_machine
         (unsigned 32 EV_CURRENT) ;; e_version

         ;; TODO entrypoint needs to be big so the stack can grow below
         ;; currently the first program header needs to be the entrypoint 
         (unsigned 64 (+ BASE (+ 128 (* 64 (length sections)) (* 56 (length program-headers)))))

         (unsigned 64 (+ 128 (* 64 (length sections)))) ;; program headers offset
         (unsigned 64 64) ;; start of section headers
         (unsigned 32 0) ;; e_flags
         (unsigned 16 64) ;; constant headersize
         (unsigned 16 56) ;; constant program header size
         (unsigned 16 (length program-headers)) ;; number of program headers
         (unsigned 16 64) ;; constant size per section header
         (unsigned 16 (+ 1 (length sections)))  ;; number of sections
         (unsigned 16 2)))  ;;  TODO calculate e_shstrndx section header string index
      ))
  )