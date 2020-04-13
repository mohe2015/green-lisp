(module main racket
  (require green-lisp/utils)
  (require green-lisp/elf/section)
  (require green-lisp/elf/program-header)
  (require green-lisp/elf/file)
  (require green-lisp/elf/symbol)
  (require green-lisp/x86-64/index)

  ;; Derived from: https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
  ;; Licensed under /* SPDX-License-Identifier: GPL-2.0 WITH Linux-syscall-note */

  ;; we could know the start of the sections before because we know the section count
  ;; but only for the first section
  ;; except when we map them in memory with a big distance
  ;; otherwise we can't because of alignment etc.
  ;; for the data we should try to use a constant address

  ;; TODO what about the data with PIE?

  (let* ((rodata-base-address (+ BASE 128 (* 64 5) (* 56 2)))
         (aligned-rodata-base-address (+ rodata-base-address (get-byte-count-to-align 12 rodata-base-address)))) ;; TODO change this design as there will be multiple sections
    (match-let ([(list length symbols code-lambda rodata-lambda real-symbols-lambda) get-the-code])
      (let* ((.rodata (bytes-append* rodata-lambda))
             (code-base-address (+ aligned-rodata-base-address (bytes-length .rodata)))
             (aligned-code-base-address (+ code-base-address (get-byte-count-to-align 12 code-base-address)))
             (.text (code-lambda aligned-code-base-address aligned-rodata-base-address))
             (real-symbols (real-symbols-lambda aligned-code-base-address))
             (.text-section (new elf-section%
                                 [name #".text"]
                                 [type 'progbits]
                                 [flags '(alloc exec)]
                                 [alignment 12]
                                 [content .text]))
             (.rodata-section (new elf-section%
                                   [name #".rodata"]
                                   [type 'progbits]
                                   [flags '(alloc merge strings)]
                                   [alignment 12]
                                   [content .rodata]))
             (.text-program-header (new elf-program-header%
                                        [type 'load]
                                        [flags '(read execute)]
                                        [start-section .text-section]
                                        [end-section .text-section]
                                        [alignment 4096]
                                        ))
             (.rodata-program-header (new elf-program-header%
                                          [type 'load]
                                          [flags '(read)]
                                          [start-section .rodata-section]
                                          [end-section .rodata-section]
                                          [alignment 4096]
                                          ))
             (elf-file (new elf-file%
                               [sections (list .rodata-section .text-section)]
                               [program-headers (list .rodata-program-header .text-program-header)]
                               [symbols real-symbols]
                               ))
             
             (bytes (send elf-file get-bytes)))
        (call-with-output-file "libgreen-lisp.so"
          (lambda (out)
            (write-bytes bytes out))
          #:mode 'binary #:exists 'truncate/replace)
        (file-or-directory-permissions "libgreen-lisp.so" (bitwise-ior user-read-bit user-write-bit user-execute-bit)))))
  )