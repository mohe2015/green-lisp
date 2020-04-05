#lang typed/racket
(require typed/racket/unsafe)
(unsafe-require/typed green-lisp/untyped-utils
                      [dynamic (-> Any Integer)]
                      [define-label-addresses (-> (Listof (List Symbol Integer)) Any)])
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
     )))

(call-with-output-file "out.elf"
  (lambda ([out : Output-Port])
   (let* ((base #x400000)
         (the-file (file base (code) (rodata) (data)))
         (label-addresses (send the-file get-label-addresses base)))
     (define-label-addresses label-addresses)
     (write-bytes (send the-file get-bytes base label-addresses) out))) #:mode 'binary #:exists 'truncate/replace)
(file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit))