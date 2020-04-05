#lang racket

(require green-lisp/test)

(time (call-with-output-file "out.bin"
    (lambda (out)
      (write-bytes
       (data-list
        (data-align 12)
        (label code-start)
        (mov-imm64 2 18)  ; dl / rdx: length of string
        (mov-imm64 6 code-start) ;; rsi load string
        (mov-imm64 0 1)  ; al / rax: set write to command
        (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
        (syscall) ;; write(stdout, "Hello\n")
        ;; TODO check return value?
        (mov-imm64 2 1024) ;; rdx: buffer length?
        (mov-imm64 6 code-start) ;; rsi: buffer?
        (mov-imm64 7 1) ;; rdi: stdin?
        (mov-imm64 0 0) ;; rax: read syscall
        (syscall) ;; read(stdin, buffer, 1024)
        ;; CHECK RETURN VALUE!
        ;; write "Hello "
        (mov-imm64 2 6)  ; dl / rdx: length of string
        (mov-imm64 6 code-start) ;; rsi load string
        (mov-imm64 0 1)  ; al / rax: set write to command
        (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
        (syscall)
        ;; echo
        ;; TODO mov rdx, rax
        (mov-imm64 2 1024)  ; dl / rdx: length of string
        (mov-imm64 6 code-start) ;; rsi load string
        (mov-imm64 0 1)  ; al / rax: set write to command
        (mov-imm64 7 1)  ; bh / dil / rdi: set destination index to rax (stdout)
        (syscall)
        (mov-imm64 0 60) ;; rax: exit syscall
        (mov-imm64 7 0)  ;; rdi: exit code
        (syscall) ;; exit(0)
        (push 1)
        (pop 1)
        (call code-start)
        ;;(jmp -2) ;; size of jmp instruction
        ;; TODO overflow
        (label +)
        (pop 0)
        (pop 1)
       ; (add (register eax) (register ecx))
        (push 0)
        ;; TODO .bss section for buffers etc.
        ;; TODO conditionals
        ;; memory allocation:
        ;; https://linux.die.net/man/2/mmap2
        ;; https://linux.die.net/man/2/mremap
        (label code-end))
       out))
    #:mode 'binary #:exists 'truncate/replace))
