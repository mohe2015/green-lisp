#lang typed/racket
(require green-lisp/x86-64 green-lisp/label-interface)
(provide file)

(define text-shdr
  (lambda ()
    (data-list
     (label 'text-shdr-start)
     (data-unsigned 32 '(- text-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_PROGBITS) ;; sh_type:   type of section
     (data-unsigned 64 (+ SHF_ALLOC SHF_EXECINSTR)) ;; sh_flags:  section attributes
     (data-unsigned 64 'code-start) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- code-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- code-end code-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 1) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'text-shdr-end))))
  
(define shstrtab
  (lambda ()
    (data-list
     (label 'shstrtab-start)
     (data-string #"\0")
     (label 'text-string)
     (data-string #".text\0")
     (label 'shstrtab-string)
     (data-string #".shstrtab\0")
     (label 'strtab-string)
     (data-string #".strtab\0")
     (label 'symtab-string)
     (data-string #".symtab\0")
     (label 'rodata-string)
     (data-string #".rodata\0")
     (label 'data-string)
     (data-string #".data\0")
     (label 'shstrtab-end))))
  
(define shstrtab-shdr
  (lambda ()
    (data-list
     (label 'shstrtab-shdr-start)
     (data-unsigned 32 '(- shstrtab-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_STRTAB) ;; sh_type:   type of section
     (data-unsigned 64 0) ;; sh_flags:  section attributes
     (data-unsigned 64 0) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- shstrtab-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- shstrtab-end shstrtab-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 1) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'shstrtab-shdr-end))))
  
(define strtab
  (lambda ()
    (data-list
     (label 'strtab-start)
     (data-string #"\0")
     (label 'message-string)
     (data-string #"message\0")
     (label 'strtab-end))))
  
(define strtab-shdr
  (lambda ()
    (data-list
     (label 'strtab-shdr-start)
     (data-unsigned 32 '(- strtab-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_STRTAB) ;; sh_type:   type of section
     (data-unsigned 64 0) ;; sh_flags:  section attributes
     (data-unsigned 64 0) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- strtab-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- strtab-end strtab-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 1) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'strtab-shdr-end))))
  
(define symtab-shdr
  (lambda ()
    (data-list
     (label 'symtab-shdr-start)
     (data-unsigned 32 '(- symtab-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_SYMTAB) ;; sh_type:   type of section
     (data-unsigned 64 0) ;; sh_flags:  section attributes
     (data-unsigned 64 0) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- symbols-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- symbols-end symbols-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 3) ;; TODO FIXME calculate this sh_link:   index of another section
     (data-unsigned 32 3) ;; TODO CALCULATE WHAT DOES THIS MEAN sh_info:   additional section information "The global symbols immediately follow the local symbols in the symbol table. The first global symbol is identified by the symbol table sh_info value. Local and global symbols are always kept separate in this manner, and cannot be mixed together."
     (data-unsigned 64 8) ;; sh_addralign: section alignment
     (data-unsigned 64 24) ;; TODO THIS IS THE SIZE OF ONE SYMBOL sh_entsize:   entry size if section holds table
     (label 'symtab-shdr-end))))

(define (shdrs)
  (data-list
   (label 'shdrs-start)
   (null-shdr)
   (text-shdr)
   (shstrtab-shdr)
   (strtab-shdr)
   (symtab-shdr)
   (rodata-shdr)
   (data-shdr)
   (label 'shdrs-end)))

(define (phdrs)
  (data-list
   (label 'phdrs-start)
   (phdr)
   (phdr-rodata)
   (phdr-data)
   (label 'phdrs-end)))

(: phdr (-> (Instance data-list-type)))
(define phdr
  (lambda ()
    (data-list
     (label 'phdr-start)
     (data-unsigned 32 1) ;; p_type
     (data-unsigned 32 5) ;; p_flags ;; TODO read + execute
     (data-unsigned 64 '(- code-start start)) ;; p_offset
     (data-unsigned 64 'code-start) ;; p_vaddr aTODO current addr
     (data-unsigned 64 'code-start) ;; p_paddr aTODO current addr
     (data-unsigned 64 '(- code-end code-start)) ;; p_filesz aTODO filesize
     (data-unsigned 64 '(- code-end code-start)) ;; p_memsz aTODO filesize
     (data-unsigned 64 #x1000) ;; p_align
     (label 'phdr-end))))

(: phdr-rodata (-> (Instance data-list-type)))
(define phdr-rodata
  (lambda ()
    (data-list
     (label 'phdr-rodata-start)
     (data-unsigned 32 1) ;; p_type
     (data-unsigned 32 4) ;; p_flags ; ; TODO read + execute
     (data-unsigned 64 '(- rodata-start start)) ;; p_offset
     (data-unsigned 64 'rodata-start) ;; p_vaddr aTODO current addr
     (data-unsigned 64 'rodata-start) ;; p_paddr aTODO current addr
     (data-unsigned 64 '(- rodata-end rodata-start)) ;; p_filesz aTODO filesize
     (data-unsigned 64 '(- rodata-end rodata-start)) ;; p_memsz aTODO filesize
     (data-unsigned 64 #x1000) ;; p_align
     (label 'phdr-rodata-end))))

  
(: phdr-data (-> (Instance data-list-type)))
(define phdr-data
  (lambda ()
    (data-list
     (label 'phdr-data-start)
     (data-unsigned 32 1) ;; p_type
     (data-unsigned 32 6) ;; p_flags ;; TODO read + write
     (data-unsigned 64 '(- data-start start)) ;; p_offset
     (data-unsigned 64 'data-start) ;; p_vaddr aTODO current addr
     (data-unsigned 64 'data-start) ;; p_paddr aTODO current addr
     (data-unsigned 64 '(- data-end data-start)) ;; p_filesz aTODO filesize
     (data-unsigned 64 '(- data-end data-start)) ;; p_memsz aTODO filesize
     (data-unsigned 64 #x1000) ;; p_align
     (label 'phdr-data-end))))

  
(: file (-> Integer (Instance data-list-type) (Instance data-list-type) (Instance data-list-type) (Instance data-list-type)))
(define file
  (lambda (base code rodata data)
    (data-list
     (label 'start)
     (ehdr)
     (phdrs)
     (shdrs)
     (symbols)
     (shstrtab)
     (w)
     code
     rodata
     data
     (label 'end))))
