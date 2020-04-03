(module modrm racket
  (require racket/match)
  (provide mod11-to-binary)
  ;; (register eax)
  ;; (pointer (register eax))
  ;; (pointer (+ (register eax) displacement))
  ;; (register al) vs (register (part 1 eax)) or so
  ;; or maybe both

  ;; todo different reg size e.g. al and ax for eax
  (define reg-to-binary
    (lambda (value)
      (match value
        ['(register eax) 0]
        ['(register ecx) 1]
        ['(register edx) 2]
        ['(register ebx) 3]
        ['(register esp) 4]
        ['(register ebp) 5]
        ['(register esi) 6]
        ['(register edi) 7]
        [_ null]
        )))

  (define (mod11-rm-to-binary value)
    (match value
      ['(register eax) 0]
      ['(register ecx) 1]
      ['(register edx) 2]
      ['(register ebx) 3]
      ['(register esp) 4]
      ['(register ebp) 5]
      ['(register esi) 6]
      ['(register edi) 7]
      [_ null]))

  (define (mod11-to-binary reg1 reg2)
    (let* ((mod #b11000000)
           (rm (mod11-rm-to-binary reg1))
           (reg (reg-to-binary reg2)))
      (+ mod (arithmetic-shift reg 3) rm))))