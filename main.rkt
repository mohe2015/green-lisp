(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  (define code
    (lambda ()
      (data-list
       (label 'code-start)
       (mov-imm8 2 6)  ; dl / rdx: length of string
       (mov-imm64 6 'hello-string) ;; load string
       (mov-imm8 0 1)  ; al / rax: set write to command
       (mov-imm8 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall)
       (mov-imm8 0 60) ;; exit syscall
       (mov-imm8 7 0)  ;; exit code
       (syscall)
       (jmp -2) ;; size of jmp instruction
       (label 'hello-string)
       (data-string #"Hello\n")
       (label 'code-end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (let* ((base #x401000)
             (the-file (file base (code))))
      (write-bytes (send the-file get-bytes base (send the-file get-label-addresses base)) out))) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-execute-bit)))