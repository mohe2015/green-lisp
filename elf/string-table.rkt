(module string-table racket
  (require green-lisp/utils)
  (provide elf-string-table%)
  
  (define elf-string-table%
    (class object%
      (super-new)
      (init-field strings)

      (define/public (get-bytes)
        (bytes-append #"\0" (bytes-join strings #"\0") #"\0"))
    
      (define/public (get-string-offset string)
        (let* ((strings-before (takef strings (lambda (s) (not (equal? s string)))))
               (strings-length-before (map bytes-length strings-before))
               (strings-length-before-plus-null-terminator (map (λ (l) (+ l 1)) strings-length-before))
               (sum-strings-length-before (foldl + 1 strings-length-before-plus-null-terminator)))
          sum-strings-length-before))
      ))
  )