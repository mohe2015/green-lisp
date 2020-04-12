#lang racket
(require rackunit green-lisp/utils green-lisp/elf/symbol)
(require math/statistics)
(provide gnu-hash%)

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
    (super-new)
    (init-field (nbuckets 4)
                (symoffset 1)
                (bloom-size 0) ;; 2
                (bloom-shift 0)) ;; 5

    (define (get-buckets-and-chain symbols)
      (print symbols)
      (let* ((symbols-with-hash-and-bucket-index
              (map (lambda (s)
                     (list s (gnu-hash (get-field name s)) (modulo (gnu-hash (get-field name s)) nbuckets))) symbols))
             (sorted (sort symbols-with-hash-and-bucket-index < #:key (lambda (v) (third v))))
             (sorted2 (append (list (list 'null 0 -1)) sorted))
             (sorted-with-index (for/list ([y sorted2] [i (in-naturals)])
                                  (append y (list i))))
             (sorted-with-index-without-null (cdr sorted-with-index))
             (bins 
              ;; hack
              (bin-samples/key '(0 1 2 3 4) < (lambda (v) (third v)) sorted-with-index-without-null))
             (bins-values
              (map sample-bin-values bins))
             (aa (print bins-values))
             (bucket-indexes
              (map (lambda (v) (if (empty? v) 0 (fourth (first v)))) bins-values))
             (bucket (bytes-append*
                      (map (lambda (v) (unsigned 32 v)) bucket-indexes)))
             (chain-elements
              (map (lambda (l)
                     (if (empty? l)
                         '()
                         (let-values ([(left right) (split-at-right l 1)])
                           (append 
                            (map (lambda (v) (bitwise-and #xfffffffe (second v))) left)
                            (list (bitwise-ior 1 (second (car right)))))))) bins-values))
             (flattened-chain-elements (flatten chain-elements))
             (chain (bytes-append* (map (lambda (v) (unsigned 32 v)) flattened-chain-elements))))
        (bytes-append bucket chain)))
    
    (define/public (get-bytes symbols)
      (bytes-append
       (unsigned 32 nbuckets)
       (unsigned 32 symoffset)
       (unsigned 32 bloom-size)
       (unsigned 32 bloom-shift)
       ;; 64 bloom-filter length bloom-size

       (get-buckets-and-chain symbols)
       ;; 32 buckets length nbuckets
       ;; 32 chain length symcount - symoffset

       ))))

;; bloom[(hash / ELFCLASS_BITS) % bloom_size]
;; TODO just set every bit so it is ignored first
;; if bit hash % ELFCLASS_BITS and bit (hash >> bloom_shift) % ELFCLASS_BITS
;; is set the symbol may be in the hash table, otherwise it isn't

;; https://flapenguin.me/elf-dt-gnu-hash

;; https://blogs.oracle.com/solaris/gnu-hash-elf-sections-v2 may explain the bloom filter more accurate
;; on the other hand wikipedia should also contain information about the bloom filter