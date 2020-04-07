(module main racket
  (require green-lisp/elf/section)
  (require green-lisp/elf/program-header)
  (require green-lisp/elf/file)
  (require green-lisp/new-x86-64)

  ;; Derived from: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
  ;; Licensed under /* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */

  (let* ((.text (get-the-code))
         (.text-section (new elf-section%
                             [name #".text"]
                             [type 'progbits]
                             [flags '(alloc exec)]
                             [address #x400000]
                             [content .text]))
         (.text-program-header (new elf-program-header%
                                    [type 'load]
                                    [flags '(read execute)]

                                    ))
         (bytes (send (new elf-file% [sections (list .text-section)]) get-bytes)))
    (call-with-output-file "out.elf"
      (lambda (out)
        (write-bytes bytes out))
      #:mode 'binary #:exists 'truncate/replace)
    (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))
  )