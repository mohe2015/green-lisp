(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  ;; TODO two program headers
  ;; TODO Section to Segment mapping
  
  (define code
    (lambda ()
      (data-list
       (align 12)
       (label 'code-start)
       (mov-imm64 2 6)  ; dl / rdx: length of string
       (mov-imm64 6 'hello-string) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall) ;; write(stdout, "Hello\n")
       (mov-imm64 0 60) ;; rax: exit syscall
       (mov-imm64 7 0)  ;; rdi: exit code
       (syscall) ;; exit(0)
       (push 1)
       (pop 1)
       (call 'code-start)
       ;;(jmp -2) ;; size of jmp instruction
      

       ;; TODO overflow
       (label '+)
       (pop 0)
       (pop 1)
       (add 0 1)
       (push 0)


       ;; TODO put in data section
       (label 'hello-string)
       (data-string #"Hello\n\0")
       
       (label 'code-end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (let* ((base #x400000)
             (the-file (file base (code))))
      (write-bytes (send the-file get-bytes base (send the-file get-label-addresses base)) out))) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))