#lang racket
(require (only-in racket/base (write-byte racket-write-byte)))

(define bit-writer%
  (class object%
    (define bits '())

    (super-new)

    (define/public (write-bit bit)
      (set! bits (cons bit bits)))

    (define/public (write-unsigned-16 integer)
      (let ((value (integer->integer-bytes integer 2 #f)))
        (write-byte (bytes-ref value 0)) ;; TODO big / little endian
        (write-byte (bytes-ref value 1))))

    (define/public (write-unsigned-32 integer)
      (let ((value (integer->integer-bytes integer 4 #f)))
        (write-byte (bytes-ref value 0))
        (write-byte (bytes-ref value 1))
        (write-byte (bytes-ref value 2))
        (write-byte (bytes-ref value 3))))

    (define/public (write-unsigned-64 integer)
      (let ((value (integer->integer-bytes integer 8 #f)))
        (write-byte (bytes-ref value 0))
        (write-byte (bytes-ref value 1))
        (write-byte (bytes-ref value 2))
        (write-byte (bytes-ref value 3))
        (write-byte (bytes-ref value 4))
        (write-byte (bytes-ref value 5))
        (write-byte (bytes-ref value 6))
        (write-byte (bytes-ref value 7))))

    (define/public (write-byte byte)
      (for ((i (in-range 7 -1 -1)))
        (write-bit (if (bitwise-bit-set? byte i) 1 0))))

    (define/public (write-to-file file)
      (call-with-output-file file
        (lambda (out)
          (let ((bits (reverse bits)))
            (for ((byte-index (in-range 0 (length bits) 8)))
              (let ((byte 0))
                (for ((bit-index (in-range 7 -1 -1)))
                  (set! byte (bitwise-ior byte (arithmetic-shift (car bits) bit-index)))
                  (set! bits (cdr bits)))
                (racket-write-byte byte out)))))
          #:mode 'binary #:exists 'truncate/replace))
    
    (define/public (get-bits)
      (reverse bits))))

(define (jmp writer displacement)
  (send writer write-byte #xeb)
  (send writer write-byte (bytes-ref (integer->integer-bytes displacement 1 #t) 0)))

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
(define EM_X86_64 64)

(let ((writer (new bit-writer%)))
  ;; header size 56
  ;; e_ident
  (send writer write-byte ELFMAG0)
  (send writer write-byte ELFMAG1)
  (send writer write-byte ELFMAG2)
  (send writer write-byte ELFMAG3)
  (send writer write-byte ELFCLASS64)
  (send writer write-byte ELFDATA2LSB)
  (send writer write-byte EV_CURRENT)
  (send writer write-byte ELFOSABI_SYSV)
  (for ((i (in-range 8)))
    (send writer write-byte 0))
  (send writer write-unsigned-16 ET_EXEC) ;; e_type
  (send writer write-unsigned-16 EM_X86_64) ;; e_machine
  (send writer write-unsigned-32 EV_CURRENT) ;; e_version
  (send writer write-unsigned-64 106) ;; aTODO entrypoint) ;; e_entry
  (send writer write-unsigned-64 32) ;; e_phoff aTODO phdr - $$
  (send writer write-unsigned-64 0) ;; e_shoff
  (send writer write-unsigned-32 0) ;; e_flags
  (send writer write-unsigned-16 56) ;; e_ehsize aTODO headersize
  (send writer write-unsigned-16 50) ;; e_phentsize aTODO phdrsize
  (send writer write-unsigned-16 1) ;; e_phnum
  (send writer write-unsigned-16 0) ;; e_shentsize
  (send writer write-unsigned-16 0) ;; e_shnum
  (send writer write-unsigned-16 0) ;; e_shstrndx

  ;; phdr Elf64_Phdr size 50
  (send writer write-byte 1) ;; p_type
  (send writer write-byte 5) ;; p_flags
  (send writer write-unsigned-64 0) ;; p_offset
  (send writer write-unsigned-64 64) ;; p_vaddr aTODO current addr
  (send writer write-unsigned-64 72) ;; p_paddr aTODO current addr
  (send writer write-unsigned-64 108) ;; p_filesz aTODO filesize
  (send writer write-unsigned-64 108) ;; p_memsz aTODO filesize
  (send writer write-unsigned-64 #x1000) ;; p_align
  
  (jmp writer -2) ;; size of jmp instruction
  (send writer get-bits)
  (send writer write-to-file "/tmp/a.bin"))