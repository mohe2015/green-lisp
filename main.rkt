#lang racket

(define code
  (lambda ()
    (list
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
     (data-unsigned 8 (char->integer #\H))
     (data-unsigned 8 (char->integer #\e))
     (data-unsigned 8 (char->integer #\l))
     (data-unsigned 8 (char->integer #\l))
     (data-unsigned 8 (char->integer #\o))
     (data-unsigned 8 (char->integer #\newline)))))

(call-with-output-file "/tmp/a.bin"
  (lambda (out)
    (write-bytes (file #x401000) out)) #:mode 'binary #:exists 'truncate/replace)
(file-or-directory-permissions "/tmp/a.bin" (bitwise-ior user-read-bit user-execute-bit))