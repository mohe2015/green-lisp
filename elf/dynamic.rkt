#lang racket/base
(require racket/match racket/class green-lisp/utils)
(provide elf-dyn%)

(define DT_NULL		0)
(define DT_NEEDED	1)
(define DT_PLTRELSZ	2)
(define DT_PLTGOT	3)
(define DT_HASH		4)
(define DT_STRTAB	5)
(define DT_SYMTAB	6)
(define DT_RELA		7)
(define DT_RELASZ	8)
(define DT_RELAENT	9)
(define DT_STRSZ	10)
(define DT_SYMENT	11)
(define DT_INIT		12)
(define DT_FINI		13)
(define DT_SONAME	14)
(define DT_RPATH 	15)
(define DT_SYMBOLIC	16)
(define DT_REL	        17)
(define DT_RELSZ	18)
(define DT_RELENT	19)
(define DT_PLTREL	20)
(define DT_DEBUG	21)
(define DT_TEXTREL	22)
(define DT_JMPREL	23)
(define DT_ENCODING	32)
(define OLD_DT_LOOS	#x60000000)
(define DT_LOOS		#x6000000d)
(define DT_HIOS		#x6ffff000)
(define DT_VALRNGLO	#x6ffffd00)
(define DT_VALRNGHI	#x6ffffdff)
(define DT_ADDRRNGLO	#x6ffffe00)
(define DT_ADDRRNGHI	#x6ffffeff)
(define DT_VERSYM	#x6ffffff0)
(define DT_RELACOUNT	#x6ffffff9)
(define DT_RELCOUNT	#x6ffffffa)
(define DT_FLAGS_1	#x6ffffffb)
(define DT_VERDEF	#x6ffffffc)
(define	DT_VERDEFNUM	#x6ffffffd)
(define DT_VERNEED	#x6ffffffe)
(define	DT_VERNEEDNUM	#x6fffffff)
(define OLD_DT_HIOS     #x6fffffff)
(define DT_LOPROC	#x70000000)
(define DT_HIPROC	#x7fffffff)
(define DT_GNU_HASH #x6ffffef5)

(define (elf-dynamic-tag->byte tag)
    (match tag
      ['null DT_NULL]
      ['needed DT_NEEDED]
      ['pltrelsz DT_PLTRELSZ]
      ['pltgot DT_PLTGOT]
      ['hash DT_HASH]
      ['strtab DT_STRTAB]
      ['symtab DT_SYMTAB]
      ['strsz DT_STRSZ]
      ['syment DT_SYMENT]
      ['gnu-hash DT_GNU_HASH]
      ))

(define elf-dyn% ;; typedef struct { } Elf64_Dyn;
  (class object%
    (super-new)
    (init-field
     tag ;; Elf64_Sxword d_tag;		/* entry tag value */
     value) ;;  union { Elf64_Xword d_val; Elf64_Addr d_ptr; } d_un;

    (define/public (get-bytes)
      (bytes-append
       (unsigned 64 (elf-dynamic-tag->byte tag))
       (unsigned 64 value)))))