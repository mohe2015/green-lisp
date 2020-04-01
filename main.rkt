(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  ;; two program headers (probably not the problem)
  ;; Section to Segment mapping: (probably not the problem)
  ;; STRIPPING HELPS!!!
  
  (define code
    (lambda ()
      (data-list
       (align 12)
       (label 'code-start)
       (mov-imm64 2 6)  ; dl / rdx: length of string
       (mov-imm64 6 'hello-string) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall)
       (mov-imm64 0 60) ;; rax: exit syscall
       (mov-imm64 7 0)  ;; rdi: exit code
       (syscall)
       (push 1)
       (pop 1)
       (jmp -2) ;; size of jmp instruction
       (label 'hello-string)
       (data-string #"Hello\n\0")
       (label 'code-end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (let* ((base #x400000)
             (the-file (file base (code))))
      (write-bytes (send the-file get-bytes base (send the-file get-label-addresses base)) out))) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))