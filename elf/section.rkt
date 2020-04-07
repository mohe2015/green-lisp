(module section racket
  (require green-lisp/utils)

  ;/* sh_type */
  (define SHT_NULL	0)
  (define SHT_PROGBITS	1)
  (define SHT_SYMTAB	2)
  (define SHT_STRTAB	3)
  (define SHT_RELA	4)
  (define SHT_HASH	5)
  (define SHT_DYNAMIC	6)
  (define SHT_NOTE	7)
  (define SHT_NOBITS	8)
  (define SHT_REL		9)
  (define SHT_SHLIB	10)
  (define SHT_DYNSYM	11)
  (define SHT_NUM		12)
  (define SHT_LOPROC	#x70000000)
  (define SHT_HIPROC	#x7fffffff)
  (define SHT_LOUSER	#x80000000)
  (define SHT_HIUSER	#xffffffff)
  (define elf-section-type '(null progbits symtab strtab rela
                                  hash dynamic note nobits
                                  rel shlib dynsym num))
  (define (elf-section-type->byte type)
    (match type
      ['null SHT_NULL]
      ['progbits SHT_PROGBITS]
      ['symtab SHT_SYMTAB]
      ['strtab SHT_STRTAB]))

  ;; sh_flags
  (define SHF_WRITE		#x1)
  (define SHF_ALLOC		#x2)
  (define SHF_EXECINSTR		#x4)
  (define SHF_RELA_LIVEPATCH	#x00100000)
  (define SHF_RO_AFTER_INIT	#x00200000)
  (define SHF_MASKPROC		#xf0000000)
  (define elf-section-flag '(write alloc exec))
  (define (elf-section-flags->byte flags)
    (match flags
      ['() 0]
      ['(alloc exec) (+ SHF_ALLOC SHF_EXECINSTR)]
      ['(alloc) (+ SHF_ALLOC)]
      ['(alloc write) (+ SHF_ALLOC SHF_WRITE)]))

  (define elf-section% ;; typedef struct elf64_shdr Elf64_Shdr
    (class object%
      (super-new)
      (init-field
       content
       name ;; Elf64_Word sh_name;		/* Section name, index in string tbl */
       type ;; Elf64_Word sh_type;		/* Type of section */
       (flags '()) ;; Elf64_Xword sh_flags;		/* Miscellaneous section attributes */
       (address 0) ;; Elf64_Addr sh_addr;		/* Section virtual addr at execution */
       ;;offset ;; Elf64_Off sh_offset;		/* Section file offset */
       ;;size ;; Elf64_Xword sh_size;		/* Size of section in bytes */
       (link 0) ;; Elf64_Word sh_link;		/* Index of another section */
       (info 0) ;; Elf64_Word sh_info;		/* Additional section information */
       (alignment 1) ;; Elf64_Xword sh_addralign;	/* Section alignment */
       (entry-size 0))  ;; Elf64_Xword sh_entsize;	/* Entry size if section holds table */

      (define/public (get-bytes offset section-name-string-table-index)
        (bytes-append
         (unsigned 32 section-name-string-table-index) ;; Elf64_Word sh_name;		/* Section name, index in string tbl */
         (unsigned 32 (elf-section-type->byte type)) ;; Elf64_Word sh_type;		/* Type of section */
         (unsigned 64 (elf-section-flags->byte flags)) ;; Elf64_Xword sh_flags;		/* Miscellaneous section attributes */
         (unsigned 64 address) ;; Elf64_Addr sh_addr;		/* Section virtual addr at execution */
         (unsigned 64 offset) ;; Elf64_Off sh_offset;		/* Section file offset */
         (unsigned 64 (bytes-length content)) ;; Elf64_Xword sh_size;		/* Size of section in bytes */
         (unsigned 32 link) ;; Elf64_Word sh_link;		/* Index of another section */
         (unsigned 32 info) ;; Elf64_Word sh_info;		/* Additional section information */
         (unsigned 64 alignment) ;; Elf64_Xword sh_addralign;	/* Section alignment */
         (unsigned 64 entry-size) ;; Elf64_Xword sh_entsize;	/* Entry size if section holds table */

         ))))

  (define null-section-header (make-bytes 64)) ;; 64 bytes

  )