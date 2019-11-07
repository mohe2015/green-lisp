#lang racket/base
;; https://docs.racket-lang.org/ts-guide/

(provide read-module)
  
;; https://docs.racket-lang.org/binary-class/index.html
(require racket/class)
(require binary-class)
  
;; http://webassembly.github.io/spec/core/binary/conventions.html
(define (valtype)
  (binary
   (λ (in) (read-byte in))
   (λ (out value) (write-byte value out))))

(define-binary-class module%
  ((_ (constant #"\0asm"))))

(define (read-module file)
  (call-with-input-file file
    (λ (in) (read-value module% in))))

;; TODO https://docs.racket-lang.org/optimization-coach/index.html

;; https://docs.racket-lang.org/guide/i_o.html
(define (write-file out)
  (write-byte 1 out))

(call-with-output-file "data" #:exists 'truncate write-file)

;; /home/moritz/Documents/green-lisp/webassembly/demo/example.wasm