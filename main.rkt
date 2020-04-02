(module main racket
  (require green-lisp/elf green-lisp/x86-64 green-lisp/label-interface)

  (define (rodata)
    (data-list
     (align 12)
     (label 'rodata-start)
     (label 'your-name-question-string)
     (data-string #"What's your name?\n\0")
     (label 'greet-string)
     (data-string #"Hello \0")
     (label 'rodata-end)))

  (define (data)
    (data-list
     (align 12) ;; needed?
     (label 'data-start)

     (label 'buffer)
     (data-array 1024)

     (label 'data-end)))
  
  (define code
    (lambda ()
      (data-list
       (align 12)
       (label 'code-start)

       (mov-imm64 2 18)  ; dl / rdx: length of string
       (mov-imm64 6 'your-name-question-string) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall) ;; write(stdout, "Hello\n")
       ;; TODO check return value?

       (mov-imm64 2 1024) ;; rdx: buffer length?
       (mov-imm64 6 'buffer) ;; rsi: buffer?
       (mov-imm64 7 1) ;; rdi: stdin?
       (mov-imm64 0 0) ;; rax: read syscall
       (syscall) ;; read(stdin, buffer, 1024)
       ;; CHECK RETURN VALUE!

       ;; write "Hello "
       (mov-imm64 2 6)  ; dl / rdx: length of string
       (mov-imm64 6 'greet-string) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall)

       
       ;; echo
       ;; TODO mov rdx, rax
       (mov-imm64 2 1024)  ; dl / rdx: length of string
       
       (mov-imm64 6 'buffer) ;; rsi load string
       (mov-imm64 0 1)  ; al / rax: set write to command
       (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
       (syscall)
       
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
       
       ;; TODO .bss section for buffers etc.

       ;; TODO conditionals

       ;; memory allocation:
       ;; https://linux.die.net/man/2/mmap2
       ;; https://linux.die.net/man/2/mremap
       
       (label 'code-end))))

  (call-with-output-file "out.elf"
    (lambda (out)
      (let* ((base #x400000)
             (the-file (file base (code) (rodata) (data))))
      (write-bytes (send the-file get-bytes base (send the-file get-label-addresses base)) out))) #:mode 'binary #:exists 'truncate/replace)
  (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))