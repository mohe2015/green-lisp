(module main racket
  (require green-lisp/elf/section)
  (require green-lisp/elf/program-header)
  (require green-lisp/elf/file)
  (require green-lisp/new-x86-64)

  ;; Derived from: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
  ;; Licensed under /* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */

  (let-values ([(.text .rodata) (get-the-code 0 #x400278)])
    (let* ((.text-section (new elf-section%
                               [name #".text"]
                               [type 'progbits]
                               [flags '(alloc exec)]
                               [content .text]))
           (.rodata-section (new elf-section%
                                 [name #".rodata"]
                                 [type 'progbits]
                                 [flags '(alloc write)]
                                 [content .rodata]))
           (.text-program-header (new elf-program-header%
                                      [type 'load]
                                      [flags '(read execute)]
                                      [section .text-section]
                                      ))
           (.rodata-program-header (new elf-program-header%
                                        [type 'load]
                                        [flags '(read write)]
                                        [section .rodata-section]
                                        ))
           (bytes (send (new elf-file%
                             [sections (list .text-section .rodata-section)]
                             [program-headers (list .text-program-header .rodata-program-header)]) get-bytes)))
      (call-with-output-file "out.elf"
        (lambda (out)
          (write-bytes bytes out))
        #:mode 'binary #:exists 'truncate/replace)
      (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit))))
  )