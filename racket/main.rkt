#lang typed/racket
;; https://docs.racket-lang.org/ts-guide/
  
;; http://webassembly.github.io/spec/core/binary/conventions.html

;; 79 e3           ldi     r23, 0x39       ; 57

'(define-assembly-instruction (ldi (d register) (k integer))
   (1 1 1 0 k k k k  d d d d k k k k)
   1
   (set! (register m d) k)
   (increment! (program-counter m))) ;; the program counter addresses words

(define (parse-ldi [byte1 : Byte] [byte2 : Byte])
  (if (= (bitwise-and (- (arithmetic-shift 1 8) (arithmetic-shift 1 4)) byte1)
           #b11100000)
        (let* ([d : Byte (arithmetic-shift byte2 -4)]
               [k : Integer (bitwise-ior
                             (bitwise-and (- (arithmetic-shift 1 8) (arithmetic-shift 1 4))
                                          (arithmetic-shift byte1 4))
                             (bitwise-and (sub1 (arithmetic-shift 1 4))
                                          byte2))])
          (list d k))
        null))

;; TODO big switch case
(define (detect-instruction [in : Input-Port])
  (let* ([byte2 : Byte (cast (read-byte in) Byte)]
         [byte1 : Byte (cast (read-byte in) Byte)]
         [byte4 : Byte (cast (read-byte in) Byte)]
         [byte3 : Byte (cast (read-byte in) Byte)])
    ;; maybe use a jump table?
    (if (bitwise-bit-set? byte1 0)
        null ;; some are at both locations
        null) ;; some only at one
  null))

(define (read-ldi [in : Input-Port])
  (let* ([byte2 : Byte (cast (read-byte in) Byte)] ;; little endian
         [byte1 : Byte (cast (read-byte in) Byte)])
    (parse-ldi byte1 byte2)))

;; this only needs bytes probably
'(define-binary-class module
   ((magic (constant #"\0asm"))
    (version (constant #"\1\0\0\0"))))

'(define (read-module file)
   (call-with-input-file file
     (λ (in) (read-value module% in))))

;; TODO https://docs.racket-lang.org/optimization-coach/index.html

;; https://docs.racket-lang.org/guide/i_o.html
(: write-file (-> Output-Port Any))
(define (write-file out)
  (write-byte 1 out))

(call-with-output-file "data" #:exists 'truncate write-file)

;; wasm-objdump -sxhd webassembly/demo/example.wasm
'(read-module "/home/moritz/Documents/green-lisp/webassembly/demo/example.wasm")