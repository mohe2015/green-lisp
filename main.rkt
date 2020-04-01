(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  ;; text section AX flags
  ;; program header align 0x1000
  ;; two program headers
  ;; program headers strange values: load offset
  ;; Section to Segment mapping:
  ;; TODO ALIGN ENTRYPOINT ADDRESS
  
  (define code
    (lambda ()
      (data-list
       (label 'code-start)
       (mov-imm64 2 6)  ; dl / rdx: length of string
       (mov-imm64 6 'hello-string) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall)
       (mov-imm64 0 60) ;; rax: exit syscall
       (mov-imm64 7 0)  ;; rdi: exit code
       (syscall)
       (jmp -2) ;; size of jmp instruction
       (label 'hello-string)
       (data-string #"Hello\n\0")
       (label 'code-end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (let* ((base #x401000)
             (the-file (file base (code))))
      (write-bytes (send the-file get-bytes base (send the-file get-label-addresses base)) out))) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-execute-bit)))