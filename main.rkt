(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  (define code
    (lambda ()
      (data-list
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
       (data-string #"Hello\n")
       (label 'end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (write-bytes (send (file #x401000 (code)) get-bytes #x401000 (send (file #x401000 (code)) get-label-addresses #x401000)) out)) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-execute-bit)))