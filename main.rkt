(module main racket
  (require green-lisp/utils)
  (require green-lisp/elf/section)
  (require green-lisp/elf/program-header)
  (require green-lisp/elf/file)
  (require green-lisp/x86-64/index)

  ;; Derived from: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
  ;; Licensed under /* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */

  ;; we could know the start of the sections before because we know the section count
  ;; but only for the first section
  ;; except when we map them in memory with a big distance
  ;; otherwise we can't because of alignment etc.
  ;; for the data we should try to use a constant address

  ;; TODO what about the data with PIE?

  (let ((rodata-base-address (+ BASE 128 (* 64 3) (* 56 2)))) ;; TODO change this design as there will be multiple sections
    (let-values ([(.text .rodata) (get-the-code 0 rodata-base-address)])
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
                               [sections (list .rodata-section .text-section)]
                               [program-headers (list .rodata-program-header .text-program-header)]) get-bytes)))
        (call-with-output-file "out.elf"
          (lambda (out)
            (write-bytes bytes out))
          #:mode 'binary #:exists 'truncate/replace)
        (file-or-directory-permissions "out.elf" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))))
  )