#lang racket
(require rackunit)
(provide gnu-hash)

;; https://flapenguin.me/elf-dt-gnu-hash
(define (gnu-hash-internal name h)
  (if (non-empty-string? name)
      (gnu-hash-internal
       (substring name 1)
       (bitwise-and
        #xffffffff
        (+ (arithmetic-shift h 5) h (char->integer (string-ref name 0)))))
      h))

(define (gnu-hash name)
  (gnu-hash-internal name 5381))

(check-equal? (number->string (gnu-hash "") 16) "1505")
(check-equal? (number->string (gnu-hash "printf") 16) "156b2bb8")
(check-equal? (number->string (gnu-hash "exit") 16) "7c967e3f")
(check-equal? (number->string (gnu-hash "syscall") 16) "bac212a0")
(check-equal? (number->string (gnu-hash "flapenguin.me") 16) "8ae9f18e")

;; https://www.gabriel.urdhr.fr/2015/09/28/elf-file-format/#hash-tables
;; struct Gnu_Hash_Header {
;;  uint32_t nbuckets;
;;  uint32_t symndx;    /* Index of the first accessible symbol in .dynsym */
;;  uint32_t maskwords; /* Nyumber of elements in the Bloom Filter */
;;  uint32_t shift2;    /* Shift count for the Bloom Filter */
;;  uintXX_t bloom_filter[maskwords];
;;  uint32_t buckets[nbuckets];
;;  uint32_t values[dynsymcount - symndx];
;;};
