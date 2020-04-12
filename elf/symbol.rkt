(module symbol racket
  (require green-lisp/utils)
  (provide elf-symbol%)
  
  (define elf-symbol-binding '(local global weak))
  (define STB_LOCAL 0)
  (define STB_GLOBAL 1)
  (define STB_WEAK 2)
  (define (elf-symbol-binding->byte binding)
    (match binding
      ['local STB_LOCAL]
      ['global STB_GLOBAL]
      ['weak STB_WEAK]))

  (define elf-symbol-type '(notype object func section file common tls))
  (define STT_NOTYPE 0)
  (define STT_OBJECT 1)
  (define STT_FUNC 2)
  (define STT_SECTION 3)
  (define STT_FILE 4)
  (define STT_COMMON 5)
  (define STT_TLS 6)
  (define (elf-symbol-type->byte type)
    (match type
      ['notype STT_NOTYPE]
      ['object STT_OBJECT]
      ['func STT_FUNC]
      ['section STT_SECTION]
      ['file STT_FILE]
      ['common STT_COMMON]
      ['tls STT_TLS]))

  (define (elf-symbol-binding-and-type->byte binding type)
    (+ (arithmetic-shift (elf-symbol-binding->byte binding) 4) (elf-symbol-type->byte type)))

  ;#define ELF_ST_BIND(x)		((x) >> 4)
  ;#define ELF_ST_TYPE(x)		(((unsigned int) x) & 0xf)
  ;#define ELF64_ST_BIND(x)	ELF_ST_BIND(x)
  ;#define ELF64_ST_TYPE(x)	ELF_ST_TYPE(x)

  (define elf-symbol% ;; typedef struct elf64_sym Elf64_Sym
    (class object%
      (super-new)
      (init-field name    ;; Elf64_Word st_name;		/* Symbol name, index in string tbl */
                  type    ;; see below
                  binding ;; unsigned char	st_info;	/* Type and binding attributes */
                  ;; unsigned char	st_other;	/* No defined meaning, 0 */
                  section ;; Elf64_Half st_shndx;		/* Associated section index */
                  value   ;; Elf64_Addr st_value;		/* Value of the symbol */
                  size)   ;; Elf64_Xword st_size;		/* Associated symbol size */
    
      (define/public (get-bytes section-index name-offset)
        (bytes-append
         (unsigned 32 name-offset) ;; u32 Elf64_Word st_name;		/* Symbol name, index in string tbl */
         (bytes (elf-symbol-binding-and-type->byte binding type)) ;; unsigned char	st_info;	/* Type and binding attributes */
         (bytes 0) ;; unsigned char	st_other;	/* No defined meaning, 0 */
         (unsigned 16 section-index ) ;; u16 Elf64_Half st_shndx;		/* Associated section index */
         (unsigned 64 value) ;; u64 Elf64_Addr st_value;		/* Value of the symbol */
         (unsigned 64 size) ;; u64 Elf64_Xword st_size;		/* Associated symbol size */
         ))
      )))