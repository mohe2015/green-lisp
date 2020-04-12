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

(define gnu-hash%
  (class object%
    (init-field (nbuckets 4)
                (symoffset 1)
                (bloom_size 2)
                (bloom_shift 5)
                bloom_filter ;; length bloom-size
                buckets ;; length nbuckets
                chain) ;; length symcount - symoffset
    
    (define/public (get-bytes)
      (bytes-append



       ))))

;; https://www.gabriel.urdhr.fr/2015/09/28/elf-file-format/#hash-tables
;; struct Gnu_Hash_Header {
;;  uint32_t nbuckets;
;;  uint32_t symndx / symoffset;    /* Index of the first accessible symbol in .dynsym */
;;  uint32_t maskwords / bloom_size; /* Nyumber of elements in the Bloom Filter */
;;  uint32_t shift2 / bloom_shift;    /* Shift count for the Bloom Filter */
;;  uintXX_t bloom_filter/bloom[maskwords/bloom_size]; // 64 / 32
;;  uint32_t buckets[nbuckets];
;;  uint32_t values[dynsymcount - symndx] / chain[];
;;};


;; bloom[(hash / ELFCLASS_BITS) % bloom_size]
;; TODO just set every bit so it is ignored first
;; if bit hash % ELFCLASS_BITS and bit (hash >> bloom_shift) % ELFCLASS_BITS
;; is set the symbol may be in the hash table, otherwise it isn't


;; we have to skip the symbols which are not global etc.

;; symbol placed in hash % nbuckets bucket

;; last bit in chain set -> end of chain

;; bucket array holds index for first symbol in chain (bucket[foobar] - symoffset])

;; Chains being contiguous sequences imply that symbols within the same bucket must be stored contiguously.

;; A bucket element will contain the index 0 if there is no symbol in the hash table for the given value of N. As index 0 of the dynsym is a reserved value, this index cannot occur for a valid symbol, and is therefore non-ambiguous.

;; sort the symbols before based on their hash?

;; https://flapenguin.me/elf-dt-gnu-hash

;; https://blogs.oracle.com/solaris/gnu-hash-elf-sections-v2 may explain the bloom filter more accurate
;; on the other hand wikipedia should also contain information about the bloom filter