(module file racket
  (require green-lisp/utils green-lisp/elf/section green-lisp/elf/program-header green-lisp/elf/string-table green-lisp/elf/dynamic)
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
                      (current-aligned-offset (+ current-offset (get-byte-count-to-align (get-field alignment current-section) current-offset)))
                      (section-bytes (send current-section get-bytes current-aligned-offset section-string-offset)))
                 (bytes-append section-bytes
                               (get-sections-bytes section-header-string-table (cdr remaining-sections) (+ current-aligned-offset (bytes-length (get-field content current-section))))))]))

      ;; not every section has a program header (at least in our simplified implementation)
      (define (get-program-headers-bytes remaining-sections remaining-program-headers current-offset)
        (cond [(null? remaining-program-headers) (bytes)]
              [else
               (let* ((current-section (car remaining-sections))
                     (current-program-header (car remaining-program-headers))
                     (current-aligned-offset (+ current-offset (get-byte-count-to-align (get-field alignment current-section) current-offset)))
                     )
                 (cond [(eq? (get-field section current-program-header) current-section)
                        (let* ((program-header-bytes (send current-program-header get-bytes current-aligned-offset (bytes-length (get-field content current-section)))))
                         (bytes-append program-header-bytes
                                       (get-program-headers-bytes (cdr remaining-sections) (cdr remaining-program-headers) (+ current-aligned-offset (bytes-length (get-field content current-section))))))]
                       [else
                        (get-program-headers-bytes (cdr remaining-sections) remaining-program-headers (+ current-aligned-offset (bytes-length (get-field content current-section))))]
                       ))]))
      
      (define/public (internal-get-bytes)
        (let* (

               (section-header-string-table (new elf-string-table% [strings (cons #".dynamic" (cons #".shstrtab" (map (lambda (section) (get-field name section)) sections)))]))
               (section-header-string-table-bytes (send section-header-string-table get-bytes))
               (section-header-string-table-section (new elf-section%
                                                         [name #".shstrtab"]
                                                         [type 'strtab]
                                                         [content section-header-string-table-bytes]))


               (.dynamic
                (list
                 ;; TODO .gnu hash
                 (new elf-dyn% [tag 'strtab] [value (get-section-offset #".dynstr")]) ;; .dynstr offset
                 (new elf-dyn% [tag 'symtab] [value (get-section-offset #".dynsym")]) ;; .dynsym offset
                 (new elf-dyn% [tag 'strsz] [value (bytes-length (get-field content (get-section-by-name #".dynstr")))]) ;; size of .dynstr
                 (new elf-dyn% [tag 'syment] [value 24]) ;; size of symbol
                 (new elf-dyn% [tag 'null] [value 0])))
               (.dynamic-bytes (bytes-append*
                                (map (lambda (dyn) (send dyn get-bytes)) .dynamic)))
               (.dynamic-section (new elf-section%
                                      [name #".dynamic"]
                                      [type 'dynamic]
                                      [flags '(write alloc)]
                                      [link (- (length sections) 1)] ;; TODO FIXME
                                      [entry-size #x10]
                                      [alignment 3]
                                      [content .dynamic-bytes]))


               (.dynamic-program-header (new elf-program-header%
                                          [type 'dynamic]
                                          [flags '(read write)]
                                          [section .dynamic-section]
                                          [alignment 8]
                                          ))
               
               (new-elf-file (merge (new elf-file%
                                         [sections (list .dynamic-section section-header-string-table-section)]
                                         [program-headers (list .dynamic-program-header)]))))
          (send new-elf-file internal-get-bytes2 section-header-string-table)))

      (define (get-sections-content-bytes remaining-sections current-offset)
        (cond [(null? remaining-sections) (bytes)]
              [else
               (let* ((current-section (car remaining-sections))
                      (current-alignment-size (get-byte-count-to-align (get-field alignment current-section) current-offset))
                      (current-aligned-offset (+ current-offset current-alignment-size)))
                 (bytes-append
                  (make-bytes current-alignment-size)
                  (get-field content current-section)
                  (get-sections-content-bytes (cdr remaining-sections) (+ current-aligned-offset (bytes-length (get-field content current-section))))))]))
      
      (define/public (internal-get-bytes2 section-header-string-table)        
        (bytes-append
         (get-elf-header-bytes) ;; 64
         null-section-header ;; 64
         (get-sections-bytes section-header-string-table sections (+ 128 (* 64 (length sections)) (* 56 (length program-headers))))
         (get-program-headers-bytes sections program-headers (+ 128 (* 64 (length sections)) (* 56 (length program-headers))))

         (get-sections-content-bytes sections (+ 128 (* 64 (length sections)) (* 56 (length program-headers))))
         ))
    
      (define/public (get-bytes)      
        (let* ((symbols-string-table (new elf-string-table% [strings (map (lambda (symbol) (get-field name symbol)) symbols)]))
               (symbols-string-table-bytes (send symbols-string-table get-bytes))
               (symbols-string-table-section (new elf-section%
                                          [name #".dynstr"]
                                          [type 'strtab]
                                          [flags '(alloc)]
                                          [content symbols-string-table-bytes]))

               (symbols-table-bytes (bytes-append*
                                     (make-bytes 24) ;; NULL SYMBOL
                                     (map (lambda (symbol)
                                                          (send symbol get-bytes
                                                                (index-where sections (lambda (s) (equal? (get-field name s) (get-field section symbol))))
                                                                (send symbols-string-table get-string-offset (get-field name symbol))
                                                                ))
                                                        symbols)))
               
               (symbols-table-section (new elf-section%
                                           [name #".dynsym"]
                                           [type 'dynsym]
                                           [flags '(alloc)]
                                           [link (+ (length sections) 1)] ;; TODO FIXME
                                           [info 1] ;; TODO index of start of global symbols
                                           [entry-size 24] ;; size of one symbol
                                           [content symbols-table-bytes]))
               (new-elf-file (merge (new elf-file% [sections (list symbols-string-table-section symbols-table-section)]))))
          (send new-elf-file internal-get-bytes)))

      (define/public (get-section-by-name section-name)
        (findf (lambda (s) (not (equal? (get-field name s) section-name))) sections))

      (define (get-section-offset-internal remaining-sections current-offset)
        (cond [(null? remaining-sections) current-offset]
              [else
               (let* ((current-section (car remaining-sections))
                      (current-aligned-offset (+ current-offset (get-byte-count-to-align (get-field alignment current-section) current-offset))))
                 (if (null? (cdr remaining-sections)) ;; last one
                     current-aligned-offset
                     (get-section-offset-internal
                      (cdr remaining-sections)
                      (+ current-aligned-offset (bytes-length (get-field content current-section))))))]))
      
      (define/public (get-section-offset section-name)
        (get-section-offset-internal (take sections (+ 1 (index-where sections (lambda (s) (equal? (get-field name s) section-name)))))
                                     (+ 128
                                        (* 64 (length sections))
                                        (* 56 (length program-headers)))))
        
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

         (unsigned 16 ET_DYN) ;; e_type
         (unsigned 16 EM_X86_64) ;; e_machine
         (unsigned 32 EV_CURRENT) ;; e_version

         ;; TODO entrypoint needs to be big so the stack can grow below
         ;; currently the first program header needs to be the entrypoint 
         (unsigned 64 (+ BASE (get-section-offset #".text"))) ;; TODO calculate

         (unsigned 64 (+ 128 (* 64 (length sections)))) ;; program headers offset
         (unsigned 64 64) ;; start of section headers
         (unsigned 32 0) ;; e_flags
         (unsigned 16 64) ;; constant headersize
         (unsigned 16 56) ;; constant program header size
         (unsigned 16 (length program-headers)) ;; number of program headers
         (unsigned 16 64) ;; constant size per section header
         (unsigned 16 (+ 1 (length sections)))  ;; number of sections
         (unsigned 16 (+ 1 (index-where sections (lambda (s) (equal? (get-field name s) #".shstrtab"))))))) ;; section header string index
      ))
  )