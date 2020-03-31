(module elf racket
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

  (define ehdr
    (lambda (base ehdr-size phdr-size shdr-size shstrtab-size)
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
       (data-unsigned 64 (+ base ehdr-size phdr-size shdr-size shstrtab-size)) ;; aTODO entrypoint) ;; e_entry
       (data-unsigned 64 64) ;; e_phoff aTODO phdr - $$
       (data-unsigned 64 (+ ehdr-size phdr-size)) ;; e_shoff
       (data-unsigned 32 0) ;; e_flags
       (data-unsigned 16 '(- ehdr-end ehdr-start)) ;; e_ehsize aTODO headersize
       (data-unsigned 16 phdr-size) ;; e_phentsize aTODO phdrsize
       (data-unsigned 16 1)  ;; e_phnum p
       (data-unsigned 16 shdr-size) ;; e_shentsize
       (data-unsigned 16 1)  ;; e_shnum p
       (data-unsigned 16 0)  ;; e_shstrndx
       (label 'ehdr-end))))

  (define ehdr-size
    (lambda ()
      (send (ehdr 0 0 0 0 0) length)))

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

  (define shstrtab
    (lambda ()
      (data-list
       (data-string #"\0.strtab\0"))))

  (define shstrtab-size
    (lambda ()
      (send (shstrtab) length)))

  (define shdr
    (lambda (offset size)
      (data-list
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
       )))

  (define shdr-size
    (lambda ()
      (send (shdr 0 0) length)))

  (define phdr
    (lambda (base ehdr-size phdr-size shdr-size shstrtab-size code-size)
      (data-list
       (data-unsigned 32 1) ;; p_type
       (data-unsigned 32 5) ;; p_flags ;; read + execute
       (data-unsigned 64 0) ;; p_offset
       (data-unsigned 64 base) ;; p_vaddr aTODO current addr
       (data-unsigned 64 base) ;; p_paddr aTODO current addr
       (data-unsigned 64 (+ ehdr-size phdr-size shdr-size shstrtab-size code-size)) ;; p_filesz aTODO filesize
       (data-unsigned 64 (+ ehdr-size phdr-size shdr-size shstrtab-size code-size)) ;; p_memsz aTODO filesize
       (data-unsigned 64 #x1000)))) ;; p_align

  (define phdr-size
    (lambda ()
      (send (phdr 0 0 0 0 0 0) length)))

  (define code-size
    (lambda (code)
      (second (assoc 'end (send code get-label-addresses 0)))))

  (define file
    (lambda (base code)
      (data-list
       (ehdr base (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size))
       (phdr base (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size) (code-size code))
       (shdr (+ base (ehdr-size) (phdr-size) (shdr-size)) (shstrtab-size))
       (shstrtab)
       code))))

;; (code->bytes code (code->label-addresses code (+ (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size) base)