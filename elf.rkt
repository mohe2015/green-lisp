#lang typed/racket
(require green-lisp/x86-64 green-lisp/label-interface)
(provide file)

;; https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
;;typedef struct elf64_hdr {
;;  unsigned char	e_ident[EI_NIDENT];	/* ELF "magic number" */
;;  Elf64_Half e_type;
;;  Elf64_Half e_machine;
;;  Elf64_Word e_version;
;;  Elf64_Addr e_entry;		/* Entry point virtual address */
;;  Elf64_Off e_phoff;		/* Program header table file offset */
;;  Elf64_Off e_shoff;		/* Section header table file offset */
;;  Elf64_Word e_flags;
;;  Elf64_Half e_ehsize;
;;  Elf64_Half e_phentsize;
;;  Elf64_Half e_phnum;
;;  Elf64_Half e_shentsize;
;;  Elf64_Half e_shnum;
;;  Elf64_Half e_shstrndx;
;;} Elf64_Ehdr;

(define EI_MAG0	0) ;; /* e_ident[] indexes */
;;#define	EI_MAG1		1
;;#define	EI_MAG2		2
;;#define	EI_MAG3		3
;;#define	EI_CLASS	4
;;#define	EI_DATA		5
;;#define	EI_VERSION	6
;;#define	EI_OSABI	7
;;#define	EI_PAD		8

(define ELFMAG0 #x7f) ;; /* EI_MAG */
(define ELFMAG1 (char->integer #\E))
(define ELFMAG2 (char->integer #\L))
(define ELFMAG3 (char->integer #\F))

;;#define	ELFCLASSNONE	0		/* EI_CLASS */
;;#define	ELFCLASS32	1
(define ELFCLASS64 2)
;;#define	ELFCLASSNUM	3

;;#define ELFDATANONE	0		/* e_ident[EI_DATA] */
(define ELFDATA2LSB 1)
;;#define ELFDATA2MSB	2

;;#define EV_NONE		0		/* e_version, EI_VERSION */
(define EV_CURRENT 1)
;;#define EV_NUM		2

;;#define ELFOSABI_NONE	0
;;#define ELFOSABI_LINUX	3
(define ELFOSABI_SYSV 0)

;; e_type
;;#define ET_NONE   0
;;#define ET_REL    1
(define ET_EXEC 2)
;;#define ET_DYN    3
;;#define ET_CORE   4
;;#define ET_LOPROC 0xff00
;;#define ET_HIPROC 0xffff

;; e_machine
;; https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.eheader.html
(define EM_X86_64 62)

  
;;/* special section indexes */
(define SHN_UNDEF 0)
;;#define SHN_LORESERVE	0xff00
;;#define SHN_LOPROC	0xff00
;;#define SHN_HIPROC	0xff1f
;;#define SHN_LIVEPATCH	0xff20
;;#define SHN_ABS		0xfff1
;;#define SHN_COMMON	0xfff2
;;#define SHN_HIRESERVE	0xffff

(define ehdr
  (lambda ()
    (data-list
     (label 'ehdr-start)
     ;; e_ident
     (data-unsigned 8 ELFMAG0)
     (data-unsigned 8 ELFMAG1)
     (data-unsigned 8 ELFMAG2)
     (data-unsigned 8 ELFMAG3)
     (data-unsigned 8 ELFCLASS64)
     (data-unsigned 8 ELFDATA2LSB)
     (data-unsigned 8 EV_CURRENT)
     (data-unsigned 8 ELFOSABI_SYSV)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)
     (data-unsigned 8 0)

     (data-unsigned 16 ET_EXEC) ;; e_type
     (data-unsigned 16 EM_X86_64) ;; e_machine
     (data-unsigned 32 EV_CURRENT) ;; e_version
     (data-unsigned 64 'code-start) ;; aTODO entrypoint) ;; e_entry
     (data-unsigned 64 '(- phdrs-start start)) ;; e_phoff aTODO phdr - $$
     (data-unsigned 64 '(- phdrs-end start)) ;; e_shoff
     (data-unsigned 32 0) ;; e_flags
     (data-unsigned 16 '(- ehdr-end ehdr-start)) ;; e_ehsize aTODO headersize
     (data-unsigned 16 '(- phdr-end phdr-start)) ;; e_phentsize aTODO phdrsize
     (data-unsigned 16 3)  ;; e_phnum p
     (data-unsigned 16 '(- null-shdr-end null-shdr-start)) ;; e_shentsize
     (data-unsigned 16 '(/ (- shdrs-end shdrs-start) (- null-shdr-end null-shdr-start)))  ;; e_shnum p
     (data-unsigned 16 2)  ;; e_shstrndx TODO calculate
     (label 'ehdr-end))))

;;typedef struct elf64_shdr {
;;  Elf64_Word sh_name;		/* Section name, index in string tbl */
;;  Elf64_Word sh_type;		/* Type of section */
;;  Elf64_Xword sh_flags;		/* Miscellaneous section attributes */
;;  Elf64_Addr sh_addr;		/* Section virtual addr at execution */
;;  Elf64_Off sh_offset;		/* Section file offset */
;;  Elf64_Xword sh_size;		/* Size of section in bytes */
;;  Elf64_Word sh_link;		/* Index of another section */
;;  Elf64_Word sh_info;		/* Additional section information */
;;  Elf64_Xword sh_addralign;	/* Section alignment */
;;  Elf64_Xword sh_entsize;	/* Entry size if section holds table */
;;} Elf64_Shdr;

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

(define null-shdr
  (lambda ()
    (data-list
     (label 'null-shdr-start)
     (data-unsigned 32 0) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_NULL) ;; sh_type:   type of section
     (data-unsigned 64 0) ;; sh_flags:  section attributes
     (data-unsigned 64 0) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 0) ;; sh_offset: section file offset
     (data-unsigned 64 0) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 0) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'null-shdr-end))))

;; sh_flags
(define SHF_WRITE		#x1)
(define SHF_ALLOC		#x2)
(define SHF_EXECINSTR		#x4)
(define SHF_RELA_LIVEPATCH	#x00100000)
(define SHF_RO_AFTER_INIT	#x00200000)
(define SHF_MASKPROC		#xf0000000)
  
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

(define rodata-shdr
  (lambda ()
    (data-list
     (label 'rodata-shdr-start)
     (data-unsigned 32 '(- rodata-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_PROGBITS) ;; sh_type:   type of section
     (data-unsigned 64 SHF_ALLOC) ;; sh_flags:  section attributes
     (data-unsigned 64 'rodata-start) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- rodata-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- rodata-end rodata-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 1) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'rodata-shdr-end))))

(define data-shdr
  (lambda ()
    (data-list
     (label 'data-shdr-start)
     (data-unsigned 32 '(- data-string shstrtab-start)) ;; sh_name:   section name, index in string table
     (data-unsigned 32 SHT_PROGBITS) ;; sh_type:   type of section
     (data-unsigned 64 (+ SHF_ALLOC SHF_WRITE)) ;; sh_flags:  section attributes
     (data-unsigned 64 'data-start) ;; sh_addr:   section virtual address at execution
     (data-unsigned 64 '(- data-start start)) ;; sh_offset: section file offset
     (data-unsigned 64 '(- data-end data-start)) ;; sh_size:   size of section in bytes
     (data-unsigned 32 0) ;; sh_link:   index of another section
     (data-unsigned 32 0) ;; sh_info:   additional section information
     (data-unsigned 64 1) ;; sh_addralign: section alignment
     (data-unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     (label 'data-shdr-end))))
  
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

;; typedef struct elf64_sym {
;;  Elf64_Word st_name;	         	/* Symbol name, index in string tbl */
;;  unsigned char	st_info;	/* Type and binding attributes */
;;  unsigned char	st_other;	/* No defined meaning, 0 */
;;  Elf64_Half st_shndx;		/* Associated section index */
;;  Elf64_Addr st_value;		/* Value of the symbol */
;;  Elf64_Xword st_size;		/* Associated symbol size */
;;} Elf64_Sym;

(define (symbol0)
  (data-list
   (data-unsigned 32 0)   ;; st_name
   (data-unsigned 8 0)    ;; st_info
   (data-unsigned 8 0)    ;; st_other
   (data-unsigned 16 0)   ;; st_shndx
   (data-unsigned 64 0)   ;; st_value
   (data-unsigned 64 SHN_UNDEF))) ;; st_size

;; st_info binding
(define STB_LOCAL 	0)
(define STB_GLOBAL 	1)
(define STB_WEAK 	2)
(define STB_LOOS 	10)
(define STB_HIOS 	12)
(define STB_LOPROC 	13)
(define STB_HIPROC 	15)

;; st_info type
(define STT_NOTYPE 	0)
(define STT_OBJECT 	1)
(define STT_FUNC 	2)
(define STT_SECTION 	3)
(define STT_FILE 	4)
(define STT_COMMON 	5)
(define STT_TLS 	6)
(define STT_LOOS 	10)
(define STT_HIOS 	12)
(define STT_LOPROC 	13)
(define STT_HIPROC 	15)

;; st_other symbol visibility
(define STV_DEFAULT 	0)
(define STV_INTERNAL 	1)
(define STV_HIDDEN 	2)
(define STV_PROTECTED 3)
  
(define (symbol)
  (data-list
   (data-unsigned 32 0)   ;; st_name
   (data-unsigned 8 (+ (arithmetic-shift STB_LOCAL 4) STT_SECTION))    ;; st_info
   (data-unsigned 8 STV_DEFAULT)    ;; st_other
   (data-unsigned 16 1)   ;; st_shndx associated section index (.text)
   (data-unsigned 64 'code-start) ;; st_value
   (data-unsigned 64 0))) ;; st_size
  
(define (symbol2)
  (data-list
   (data-unsigned 32 '(- message-string strtab-start))   ;; st_name
   (data-unsigned 8 (+ (arithmetic-shift STB_LOCAL 4) STT_NOTYPE))    ;; st_info
   (data-unsigned 8 STV_DEFAULT)    ;; st_other
   (data-unsigned 16 1)   ;; st_shndx
   (data-unsigned 64 'your-name-question-string) ;; st_value
   (data-unsigned 64 0))) ;; st_size

(define (symbols)
  (data-list
   (align 3)
   (label 'symbols-start)
   ;; If the symbol table contains any local symbols, the second entry of the symbol table is an STT_FILE symbol giving the name of the file.
   (symbol0)
   (symbol)
   (symbol2)
   (label 'symbols-end)))
  
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
     (strtab)
     code
     rodata
     data
     (label 'end))))
