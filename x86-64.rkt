#lang typed/racket
(require green-lisp/label-interface green-lisp/modrm)
(provide
 syscall% syscall
 mov-imm8% mov-imm8
 mov-imm64% mov-imm64
 jmp% jmp
 push% push
 pop% pop
 call% call
 add% add
 )

(: REX.W Integer)
(define REX.W #b01001000)

(: unsigned (-> Integer Integer Bytes))
(define unsigned
  (lambda (bits value)
    (integer->integer-bytes value (arithmetic-shift bits -3) #f)))

(: syscall% (Class
             (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
             (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
             (length (-> Integer Integer))))
(define syscall%
  (class data-interface%
    (super-new)

    (define/override (get-label-addresses offset)
      (list))
      
    (define/override (get-bytes current-address label-addresses)
      (bytes #x0f #x05))
    
    (define/override (length offset)
      2)))

(define (syscall)
  (new syscall%))

(define-type mov-imm8-type
  (Class
   (init (register Integer) (value Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define mov-imm8%
  (class data-interface%
    (init [register : Integer] [value : Integer])
    (: the-register Integer)
    (define the-register register)
    (: the-value Integer)
    (define the-value value)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))
      
    (define/override (get-bytes current-address label-addresses)
      (bytes-append
       (if (= the-register 7)
           (unsigned 8 #x40)
           (bytes)) ; REX prefix to access dil instead of bh
       (bytes (bitwise-ior #xb0 the-register) (dynamic the-value label-addresses))))

    (define/override (length offset)
      (if (= the-register 7) 3 2))))

(: mov-imm8 (-> Integer Integer (Instance mov-imm8-type)))
(define (mov-imm8 register value)
  (new mov-imm8% [register register] [value value]))

(define-type mov-imm64-type
  (Class
   (init (register Integer) (value Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define mov-imm64%
  (class data-interface%
    (init [register : Integer] [value : Any])
    (: the-register Integer)
    (define the-register register)
    (: the-value Any)
    (define the-value value)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))
    
    (define/override (get-bytes current-address label-addresses)
      (bytes-append
       (unsigned 8 REX.W)
       (unsigned 8 (+ #xb8 the-register)) ;; opcode with register
       (unsigned 64 (dynamic the-value label-addresses)))) ;; value
    
    (define/override (length offset)
      10)))

(: mov-imm64 (-> Integer Any (Instance mov-imm64-type)))
(define (mov-imm64 register value)
  (new mov-imm64% [register register] [value value]))

(define-type jmp-type
  (Class
   (init (displacement Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define jmp%
  (class data-interface%
    (init [displacement : Integer])
    (: the-displacement Integer)
    (define the-displacement displacement)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))
    
    (define/override (get-bytes current-address label-addresses)
      (bytes-append (bytes #xeb) (integer->integer-bytes (dynamic the-displacement label-addresses) 1 #t)))
    
    (define/override (length offset)
      2)))

(: jmp (-> Integer (Instance jmp-type)))
(define (jmp displacement)
  (new jmp% [displacement displacement]))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1163&zoom=100,-7,754
(define-type push-type
  (Class
   (init (register Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define push%
  (class data-interface%
    (init [register : Integer])
    (: the-register Integer)
    (define the-register register)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))

    (define/override (get-bytes current-address label-addresses)
      (bytes (+ #x50 the-register)))

    (define/override (length offset)
      1)))

(: push (-> Integer (Instance push-type)))
(define (push register)
  (new push% [register register]))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=1037&zoom=auto,-17,727
(define-type pop-type
  (Class
   (init (register Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define pop%
  (class data-interface%
    (init [register : Integer])
    (: the-register Integer)
    (define the-register register)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))

    (define/override (get-bytes current-address label-addresses)
      (bytes (+ #x58 the-register)))
    ;; (bytes-append (bytes #x8f) (integer->integer-bytes the-register 1 #f)))

    (define/override (length offset)
      1)))

(: pop (-> Integer (Instance pop-type)))
(define (pop register)
  (new pop% [register register]))

;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=224&zoom=100,28,726
(define-type call-type
  (Class
   (init (address Integer))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define call%
  (class data-interface%
    (init [address : Any])
    (: the-address Any)
    (define the-address address)
    (super-new)
      
    (define/override (get-label-addresses offset)
      (list))

    (define/override (get-bytes current-address label-addresses)
      (bytes-append (bytes #xe8) (integer->integer-bytes (- (dynamic the-address label-addresses) current-address (length current-address)) 4 #t)))

    (define/override (length offset)
      5)))

(: call (-> Any (Instance call-type)))
(define (call address)
  (new call% [address address]))
  
;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=133&zoom=100,-7,726
(define-type add-type
  (Class
   (init (destination (Listof Symbol)) (source (Listof Symbol)))
   (get-bytes (-> Integer (Listof (List Symbol Integer)) Bytes))
   (get-label-addresses (-> Integer (Listof (List Symbol Integer))))
   (length (-> Integer Integer))))
(define add%
  (class data-interface%
    (init [destination : (Listof Symbol)] [source : (Listof Symbol)])
    (: the-destination (Listof Symbol))
    (define the-destination destination)
    (: the-source (Listof Symbol))
    (define the-source source)
    (super-new)

    (define/override (get-label-addresses offset)
      (list))

    (define/override (get-bytes current-address label-addresses)
      ;; https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf#page=40&zoom=100,28,745
      (bytes REX.W 01 (mod11-to-binary the-destination the-source)))

    (define/override (length offset)
      3)))

(: add (-> (Listof Symbol) (Listof Symbol) (Instance add-type)))
(define (add destination source)
  (new add% [destination destination] [source source]))