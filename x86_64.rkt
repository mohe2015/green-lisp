#lang racket
(compile-enforce-module-constants #f)

(define instruction-interface
  (interface () length get-bytes))

(define syscall%
  (class* object% (instruction-interface)
    (super-new)

    (define/public (get-bytes label-addresses)
      (bytes #x0f #x05))
    
    (define/public (length)
      2)))

(define (syscall)
  (new syscall%))

(define mov-imm8%
  (class* object% (instruction-interface)
    (init register value)
    (define the-register register)
    (define the-value value)
    (super-new)
    
    (define/public (get-bytes label-addresses)
      (bytes-append
       (if (= the-register 7)
           (unsigned 8 #x40)
           (bytes)) ; REX prefix to access dil instead of bh
       (bytes (bitwise-ior #xb0 the-register) the-value)))

    (define/public (length)
      (if (= the-register 7) 3 2))))

(define (mov-imm8 register value)
  (new mov-imm8% [register register] [value value]))

(define mov-imm64%
  (class* object% (instruction-interface)
    (init register value)
    (define the-register register)
    (define the-value value)
    (super-new)
    
    (define/public (get-bytes label-addresses)
      (bytes-append
       (unsigned 8 #b01001000) ;; REX.W
       (unsigned 8 (+ #xb8 the-register)) ;; opcode with register
       (unsigned 64 (second (assoc the-value label-addresses))))) ;; value
    
    (define/public (length)
      10)))

(define (mov-imm64 register value)
  (new mov-imm64% [register register] [value value]))

(define jmp%
  (class* object% (instruction-interface)
    (init displacement)
    (define the-displacement displacement)
    (super-new)
    
    (define/public (get-bytes label-addresses)
      (bytes-append (bytes #xeb) (integer->integer-bytes the-displacement 1 #t)))
    
    (define/public (length)
      2)))

(define (jmp displacement)
  (new jmp% [displacement displacement]))

(define data-unsigned%
  (class* object% (instruction-interface)
    (init bits value)
    (define the-bits bits)
    (define the-value value)
    (super-new)

    (define/public (get-bytes label-addresses)
      (integer->integer-bytes the-value (/ the-bits 8) #f))

    (define/public (length)
      (/ the-bits 8))))

(define (data-unsigned bits value)
  (new data-unsigned% [bits bits] [value value]))

(define label%
  (class* object% (instruction-interface)
    (init label)
    (define the-label label)
    (super-new)

    (define/public (get-label)
      the-label)

    (define/public (get-bytes label-addresses)
      (bytes))
    
    (define/public (length)
      0)))

(define (label label)
  (new label% [label label]))


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

(define unsigned
  (lambda (bits value)
    (integer->integer-bytes value (/ bits 8) #f)))

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
    (bytes-append
     (bytes ELFMAG0 ELFMAG1 ELFMAG2 ELFMAG3 ELFCLASS64 ELFDATA2LSB EV_CURRENT ELFOSABI_SYSV 0 0 0 0 0 0 0 0) ;; e_ident
     (unsigned 16 ET_EXEC) ;; e_type
     (unsigned 16 EM_X86_64) ;; e_machine
     (unsigned 32 EV_CURRENT) ;; e_version
     (unsigned 64 (+ base ehdr-size phdr-size shdr-size shstrtab-size)) ;; aTODO entrypoint) ;; e_entry
     (unsigned 64 64) ;; e_phoff aTODO phdr - $$
     (unsigned 64 128) ;; e_shoff
     (unsigned 32 0) ;; e_flags
     (unsigned 16 64) ;; e_ehsize aTODO headersize
     (unsigned 16 56) ;; e_phentsize aTODO phdrsize
     (unsigned 16 1)  ;; e_phnum p
     (unsigned 16 64) ;; e_shentsize
     (unsigned 16 1)  ;; e_shnum p
     (unsigned 16 0)))) ;; e_shstrndx

(define ehdr-size
  (lambda ()
    (bytes-length (ehdr 0 0 0 0 0))))

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
    #"\0.strtab\0"))

(define shstrtab-size
  (lambda ()
    (bytes-length (shstrtab))))

(define shdr
  (lambda (offset size)
    (bytes-append
     (unsigned 32 0) ;; sh_name:   section name, index in string table
     (unsigned 32 SHT_NULL) ;; sh_type:   type of section
     (unsigned 64 0) ;; sh_flags:  section attributes
     (unsigned 64 0) ;; sh_addr:   section virtual address at execution
     (unsigned 64 0) ;; sh_offset: section file offset
     (unsigned 64 0) ;; sh_size:   size of section in bytes
     (unsigned 32 0) ;; sh_link:   index of another section
     (unsigned 32 0) ;; sh_info:   additional section information
     (unsigned 64 0) ;; sh_addralign: section alignment
     (unsigned 64 0) ;; sh_entsize:   entry size if section holds table
     )))

(define shdr-size
  (lambda ()
    (bytes-length (shdr 0 0))))

(define phdr
  (lambda (base ehdr-size phdr-size shdr-size shstrtab-size code-size)
    (bytes-append
     (unsigned 32 1) ;; p_type
     (unsigned 32 5) ;; p_flags ;; read + execute
     (unsigned 64 0) ;; p_offset
     (unsigned 64 base) ;; p_vaddr aTODO current addr
     (unsigned 64 base) ;; p_paddr aTODO current addr
     (unsigned 64 (+ ehdr-size phdr-size shdr-size shstrtab-size code-size)) ;; p_filesz aTODO filesize
     (unsigned 64 (+ ehdr-size phdr-size shdr-size shstrtab-size code-size)) ;; p_memsz aTODO filesize
     (unsigned 64 #x1000)))) ;; p_align

(define phdr-size
  (lambda ()
    (bytes-length (phdr 0 0 0 0 0 0))))

(define code
  (lambda ()
    (list
     (mov-imm8 2 6)  ; dl / rdx: length of string
     (mov-imm64 6 'hello) ;; load string
     (mov-imm8 0 1)  ; al / rax: set write to command
     (mov-imm8 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
     (syscall)
     (mov-imm8 0 60) ;; exit syscall
     (mov-imm8 7 0)  ;; exit code
     (syscall)
     (jmp -2) ;; size of jmp instruction
     (label 'hello)
     (data-unsigned 8 (char->integer #\H))
     (data-unsigned 8 (char->integer #\e))
     (data-unsigned 8 (char->integer #\l))
     (data-unsigned 8 (char->integer #\l))
     (data-unsigned 8 (char->integer #\o))
     (data-unsigned 8 (char->integer #\newline)))))

(define (code->label-addresses code offset)
  (cond [(empty? code) (list (list 'end offset))]
        [(list? code) ;; list of instructions
         (append
          (code->label-addresses (first code) offset)
          (code->label-addresses (rest code) (+ offset (send (first code) length))))]
        [(is-a? code label%) ;; a label instruction
         (list (list (send code get-label) offset))]
        [else (list)])) ;; end

(define (code->bytes code label-addresses)
  (cond [(pair? code)
         (bytes-append
          (code->bytes (first code) label-addresses)
          (code->bytes (rest code) label-addresses))]
        [(is-a? code instruction-interface)
         (send code get-bytes label-addresses)]
        [else (bytes)]))

(define code-size
  (lambda ()
    (second (assoc 'end (code->label-addresses (code) 0)))))

(define file
  (lambda (base)
    (bytes-append
     (ehdr base (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size))
     (phdr base (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size) (code-size))
     (shdr (+ base (ehdr-size) (phdr-size) (shdr-size)) (shstrtab-size))
     (shstrtab)
     (code->bytes (code) (code->label-addresses (code) (+ (ehdr-size) (phdr-size) (shdr-size) (shstrtab-size) base)))
     )))

(call-with-output-file "/tmp/a.bin"
  (lambda (out)
    (write-bytes (file #x401000) out)) #:mode 'binary #:exists 'truncate/replace)
(file-or-directory-permissions "/tmp/a.bin" (bitwise-ior user-read-bit user-execute-bit))