#lang racket
(compile-enforce-module-constants #f)

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

(define unsigned
  (lambda (bits value)
    (integer->integer-bytes value (/ bits 8) #f)))

'(define (jmp displacement)
  (bytes-append (bytes #xeb) (integer->integer-bytes displacement 1 #t)))

'(define (mov-imm8 register value)
  (bytes-append
   (if (= register 7)
       (unsigned 8 #x40)
       (bytes)) ; REX prefix to access dil instead of bh
   (bytes (bitwise-ior #xb0 register) value)))

'(define (mov-imm64 register value)
  (bytes-append
   (unsigned 8 #b01001000) ;; REX.W
   (unsigned 8 (+ #xb8 register)) ;; opcode with register
   (unsigned 64 value))) ;; value? TODO CALCULATE THIS ADDRESS

'(define (syscall)
  (bytes #x0f #x05))

(define instruction-interface
  (interface () length))

(define syscall%
  (class* object% (instruction-interface)
    (super-new)

    (define/public (length)
      2)))

(define (syscall)
  (new syscall%))

(define label%
  (class* object% (instruction-interface)
    (init label)
    (define the-label label)
    (super-new)

    (define/public (get-label)
      the-label)
    
    (define/public (length)
      0)))

(define (label label)
  (new label% [label label]))

(define ehdr
  (lambda (base ehdr-size phdr-size)
    (bytes-append
     (bytes ELFMAG0 ELFMAG1 ELFMAG2 ELFMAG3 ELFCLASS64 ELFDATA2LSB EV_CURRENT ELFOSABI_SYSV 0 0 0 0 0 0 0 0) ;; e_ident
     (unsigned 16 ET_EXEC) ;; e_type
     (unsigned 16 EM_X86_64) ;; e_machine
     (unsigned 32 EV_CURRENT) ;; e_version
     (unsigned 64 (+ base ehdr-size phdr-size)) ;; aTODO entrypoint) ;; e_entry
     (unsigned 64 64) ;; e_phoff aTODO phdr - $$
     (unsigned 64 0) ;; e_shoff
     (unsigned 32 0) ;; e_flags
     (unsigned 16 64) ;; e_ehsize aTODO headersize
     (unsigned 16 56) ;; e_phentsize aTODO phdrsize

     (unsigned 16 1) ;; e_phnum p
     (unsigned 16 0) ;; e_shentsize
     (unsigned 16 0) ;; e_shnum p
     (unsigned 16 0)))) ;; e_shstrndx

(define ehdr-size
  (lambda ()
    (bytes-length (ehdr 0 0 0))))

(define phdr
  (lambda (base ehdr-size phdr-size code-size)
    (bytes-append
     (unsigned 32 1) ;; p_type
     (unsigned 32 5) ;; p_flags ;; read + execute
     (unsigned 64 0) ;; p_offset
     (unsigned 64 base) ;; p_vaddr aTODO current addr
     (unsigned 64 base) ;; p_paddr aTODO current addr
     (unsigned 64 (+ ehdr-size phdr-size code-size)) ;; p_filesz aTODO filesize
     (unsigned 64 (+ ehdr-size phdr-size code-size)) ;; p_memsz aTODO filesize
     (unsigned 64 #x1000)))) ;; p_align

(define phdr-size
  (lambda ()
    (bytes-length (phdr 0 0 0 0))))

(define code
  (lambda ()
    (list
     ;(mov-imm8 2 6)  ; dl / rdx: length of string
     ;(mov-imm64 6 'hello) ;; load string
     ;(mov-imm8 0 1)  ; al / rax: set write to command
     ;(mov-imm8 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
     (syscall)
     ;(mov-imm8 0 60) ;; exit syscall
     ;(mov-imm8 7 0)  ;; exit code
     (syscall)
     ;(jmp -2) ;; size of jmp instruction
     (label 'hello))))
     ;(unsigned 8 (char->integer #\H))
     ;(unsigned 8 (char->integer #\e))
     ;(unsigned 8 (char->integer #\l))
     ;(unsigned 8 (char->integer #\l))
     ;(unsigned 8 (char->integer #\o))
     ;(unsigned 8 (char->integer #\newline)))))

(define (code->label-addresses code offset)
  (cond [(pair? code)
         (append (code->label-addresses (first code) offset) (code->label-addresses (rest code) (+ offset (send (first code) length))))
                 
         ]
        [(is-a? code label%) ;; a label instruction
         (list (cons (send code get-label) offset))
         ]
        [else '()]))

;; first pass: count instruction bytes
;; if label, store current count
;; second pass: add the address to all references

(define code-size
  (lambda ()
    (length (code)))) ;; TODO FIXME

(define file
  (lambda (base)
    (bytes-append
     (ehdr base (ehdr-size) (phdr-size))
     (phdr base (ehdr-size) (phdr-size) (code-size))
     (code->label-addresses (code) 0))))

(with-output-to-file "/tmp/a.bin"
  (lambda ()
    (write-bytes (file #x401000))) #:mode 'binary #:exists 'truncate/replace)
(file-or-directory-permissions "/tmp/a.bin" (bitwise-ior user-read-bit user-execute-bit))