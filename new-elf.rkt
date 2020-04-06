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
    (init-field elf-header
              [sections '()] ;; TODO null section
              [program-headers '()]
              [symbols '()]) ;; formally this is also just a section ;; TODO null symbol

    (define/public (merge that)
      null)
    
    (define/public (get-bytes)
      null)

    (define/public (get-elf-header-bytes)
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
       (unsigned 64 0); '(- phdrs-end start)) ;; e_shoff
       (unsigned 32 0) ;; e_flags
       (unsigned 16 0);'(- ehdr-end ehdr-start)) ;; e_ehsize aTODO headersize
       (unsigned 16 0);'(- phdr-end phdr-start)) ;; e_phentsize aTODO phdrsize
       (unsigned 16 3)  ;; e_phnum p
       (unsigned 16 0);'(- null-shdr-end null-shdr-start)) ;; e_shentsize
       (unsigned 16 0);'(/ (- shdrs-end shdrs-start) (- null-shdr-end null-shdr-start)))  ;; e_shnum p
       (unsigned 16 2)  ;; e_shstrndx section header string index TODO calculate
       ))
    
    ))