#lang typed/racket/base
;; https://docs.racket-lang.org/ts-guide/
(module unsafe typed/racket/base

  ;; https://docs.racket-lang.org/binary-class/index.html
  (require typed/racket/class)
  (require/typed binary-class
                 [binary (-> (-> Input-Port Any) (-> Output-Port Integer Any) Any)])

  ;; http://webassembly.github.io/spec/core/binary/conventions.html
  (define (valtype)
    (binary
     (λ ([in : Input-Port]) (read-byte in))
     (λ ([out : Output-Port] [value : Integer]) (write-byte value out))))

  (define-binary-class id3-tag%
    ((major-version  u1)
     (revision       u1)
     (flags          u1))
    #:dispatch 
    nil)
  
  (define-binary-class module%
    ((_ (constant (list #x00 #x61 #x73 #x6D)))))

  (define (read-module file)
    (call-with-input-file file
      (λ (in) (read-value module% in))))

  ;; TODO https://docs.racket-lang.org/optimization-coach/index.html

  ;; https://docs.racket-lang.org/guide/i_o.html
  (: write-file (-> Output-Port Any))
  (define (write-file out)
    (write-byte 1 out))

  (call-with-output-file "data" #:exists 'truncate write-file))

  ;; /home/moritz/Documents/green-lisp/webassembly/demo/example.wasm