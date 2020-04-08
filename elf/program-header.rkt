(module program-header racket
  (require green-lisp/utils)
  (provide elf-program-header%)
  
  ;; /* These constants are for the segment types stored in the image headers */
  (define PT_NULL    0)
  (define PT_LOAD    1)
  (define PT_DYNAMIC 2)
  (define PT_INTERP  3)
  (define PT_NOTE    4)
  (define PT_SHLIB   5)
  (define PT_PHDR    6)
  (define PT_TLS     7)
  (define elf-program-header-type '(null load dynamic interp note shlib phdr tls))
  (define (elf-program-header-type->byte type)
    (match type
      ['null PT_NULL]
      ['load PT_LOAD]
      ['dynamic PT_DYNAMIC]
      ['interp PT_INTERP]))

  ;; /* These constants define the permissions on sections in the program header, p_flags. */
  (define PF_R #x4)
  (define PF_W #x2)
  (define PF_X #x1)
  (define elf-program-header-flags '(read write execute))
  (define (elf-program-header-flags->byte flags)
    (match flags
      ['() 0]
      ['(read execute) (+ PF_R PF_X)]
      ['(read) (+ PF_R)]
      ['(read write) (+ PF_R PF_W)]))

  (define elf-program-header% ;; typedef struct elf64_phdr {} Elf64_Phdr;
    (class object%
      (super-new)
      (init-field
       type ;; Elf64_Word p_type;
       flags ;; Elf64_Word p_flags;
       section
       (alignment 1)) ;; Elf64_Xword p_align;		/* Segment alignment, file & memory */

      (define/public (get-bytes offset size)
        (bytes-append
         (unsigned 32 (elf-program-header-type->byte type)) ;; Elf64_Word p_type;
         (unsigned 32 (elf-program-header-flags->byte flags)) ;; Elf64_Word p_flags;
         (unsigned 64 offset) ;; Elf64_Off p_offset;		/* Segment file offset */
         (unsigned 64 offset) ;; Elf64_Addr p_vaddr;		/* Segment virtual address */
         (unsigned 64 offset) ;; Elf64_Addr p_paddr;		/* Segment physical address */
         (unsigned 64 size) ;; Elf64_Xword p_filesz;		/* Segment size in file */
         (unsigned 64 size) ;; Elf64_Xword p_memsz;		/* Segment size in memory */
         (unsigned 64 alignment) ;; Elf64_Xword p_align;		/* Segment alignment, file & memory */
        )))))