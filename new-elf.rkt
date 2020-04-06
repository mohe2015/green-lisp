#lang typed/racket
(require typed/racket/class)

;; Derived from: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
;; Licensed under /* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */

(define unsigned
  (lambda ([bits : Integer] [value : Integer])
     (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

(define-type elf-symbol-binding-type (U 'local 'global 'weak))
(define [elf-symbol-binding : elf-symbol-binding-type] '(local global weak))
;#define STB_LOCAL  0
;#define STB_GLOBAL 1
;#define STB_WEAK   2

(define-type elf-symbol-type-type (U 'notype 'object 'func 'section 'file 'common 'tls))
(define [elf-symbol-type : elf-symbol-type-type] '(notype object func section file common tls))
;#define STT_NOTYPE  0
;#define STT_OBJECT  1
;#define STT_FUNC    2
;#define STT_SECTION 3
;#define STT_FILE    4
;#define STT_COMMON  5
;#define STT_TLS     6

;#define ELF_ST_BIND(x)		((x) >> 4)
;#define ELF_ST_TYPE(x)		(((unsigned int) x) & 0xf)
;#define ELF64_ST_BIND(x)	ELF_ST_BIND(x)
;#define ELF64_ST_TYPE(x)	ELF_ST_TYPE(x)

(define elf-symbol% ;; typedef struct elf64_sym Elf64_Sym
  (class object%
    (super-new)
    (init-field name    ;; Elf64_Word st_name;		/* Symbol name, index in string tbl */
                [type : elf-symbol-type-type]    ;; see below
                [binding : elf-symbol-binding-type] ;; unsigned char	st_info;	/* Type and binding attributes */
                ;; unsigned char	st_other;	/* No defined meaning, 0 */
                section ;; Elf64_Half st_shndx;		/* Associated section index */
                value   ;; Elf64_Addr st_value;		/* Value of the symbol */
                size)   ;; Elf64_Xword st_size;		/* Associated symbol size */
    
    ;; (+ (arithmetic-shift binding 4) type)
    ))

(new elf-symbol%
     [name "test"]
     [type 'notype]
     [binding 'local]
     [section ".text"]
     [value 0]
     [size 0])


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
(define-type elf-section-type-type (U 'null 'progbits 'symtab 'strtab 'rela 'hash
                                      'dynamic 'note 'nobits 'rel 'shlib 'dynsym 'num))
(define [elf-section-type : elf-section-type-type] '(null progbits symtab strtab rela
                                                          hash dynamic note nobits
                                                          rel shlib dynsym num))

;; sh_flags
(define SHF_WRITE		#x1)
(define SHF_ALLOC		#x2)
(define SHF_EXECINSTR		#x4)
(define SHF_RELA_LIVEPATCH	#x00100000)
(define SHF_RO_AFTER_INIT	#x00200000)
(define SHF_MASKPROC		#xf0000000)
(define-type elf-section-flag-type (U 'write 'alloc 'exec))
(define [elf-section-flag : elf-section-flag-type] '(write alloc exec))

(define elf-section% ;; typedef struct elf64_shdr Elf64_Shdr
  (class object%
    (super-new)
    (init-field 
     name ;; Elf64_Word sh_name;		/* Section name, index in string tbl */
     [type : elf-section-type-type] ;; Elf64_Word sh_type;		/* Type of section */
     [flags : elf-section-flag-type] ;; Elf64_Xword sh_flags;		/* Miscellaneous section attributes */
     address ;; Elf64_Addr sh_addr;		/* Section virtual addr at execution */
     offset ;; Elf64_Off sh_offset;		/* Section file offset */
     size ;; Elf64_Xword sh_size;		/* Size of section in bytes */
     link ;; Elf64_Word sh_link;		/* Index of another section */
     info ;; Elf64_Word sh_info;		/* Additional section information */
     alignemnt ;; Elf64_Xword sh_addralign;	/* Section alignment */
     entry-size) ;; Elf64_Xword sh_entsize;	/* Entry size if section holds table */
    ))

(define null-section-header (make-bytes 64)) ;; 64 bytes



(define elf-string-table%
  (class object%
    (super-new)
    (init-field strings)))





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
    (init-field ;elf-header
              [sections '()] ;; TODO null section
              [program-headers '()]
              [symbols '()]) ;; formally this is also just a section ;; TODO null symbol

    (define/public (merge that)
      null)
    
    (define/public (get-bytes)
      (bytes-append
       (get-elf-header-bytes)
       null-section-header))

    (: get-elf-header-bytes (-> Bytes))
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
       (unsigned 64 0); 'code-start) ;; aTODO entrypoint) ;; e_entry
       (unsigned 64 0); '(- phdrs-start start)) ;; e_phoff aTODO phdr - $$
       (unsigned 64 64) ;; start of section headers
       (unsigned 32 0) ;; e_flags
       (unsigned 16 64) ;; constant headersize
       (unsigned 16 0);'(- phdr-end phdr-start)) ;; e_phentsize aTODO phdrsize
       (unsigned 16 0)  ;; e_phnum number of program headers
       (unsigned 16 64) ;; constant size per section header
       (unsigned 16 1)  ;; number of sections
       (unsigned 16 0)))  ;; e_shstrndx section header string index TODO calculate
       
    ))

(let ((bytes (send (new elf-file%) get-bytes)))
  (call-with-output-file "out.elf"
    (lambda ([out : Output-Port])
      (write-bytes bytes out))
    #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))