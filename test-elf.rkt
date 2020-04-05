#lang racket

;; code needs to be able to return strings and data it needs which should then be stored in .data and .rodata, ...
;; (use-string "test")
;; returns (values ... (list "test"))
;; but needs to somehow get the address
;; functional way? return all strings and a function that returns correct code when applied with addresses of strings

;; maybe different approach for the code:
;; toplevel macro that completely parses all chilren?

'(elf
  (ehdr) ;; if you order everything correctly this could be precalculated (or just entrypoint)
  (phdrs
   (.text-phdr) ;; depends on code
   (.rodata-phdr) ;; depends on code
   (.data-phdr)) ;; depends on code
  (shdrs
   (null-shdr) ;; precalculated
   (.shstrtab-shdr) ;; possibly precalculated (if all sections known)
   (.strtab-shdr) ;; symbol strings etc. (depends on symbols in code)
   (symtab-shdr) ;; symbols (depends on symbols in code)
   (.text-shdr) ;; depends on .text size and offset (code)
   (.rodata-shdr) ;; depends on code
   (.data-shdr)) ;; depends on code
  (.shstrtab) ;; possibly precalculated
  (.strtab) ;; depends on symbol strings
  (symbols
   (symbol0);; precalculated
   (some-other-symbol) ;; totally depends on code
   (another-symbol)) ;; totally depends on code
  (.text the-code) ;; this is the code
  (.rodata the-constants) ;; and this are the codes constants
  (.data the-data)) ;; and this the codes data

;; may don't implement it completely in macros so it can be optimized etc.
;; e.g. stripping
;; but it should also be really optimized (maybe this would be premature optimization?)

;; abstraction is also important, maybe the elf file can be specified in a clean way

;; TODO DWARF

;; also there should be an indermediate assembly language

;; code optimizations probably need objects for the assembly
;; also for dwarf it needs source information thats kept when optimizing

;; every instruction is an object that has properties like length, bytes, labels, debugging info, symbols, (dependent) types, global symbols, source location info, state change function, memory and time use
;; then we can work on these to transform them, e.g. inlining, constant propagation, state calculation

;; optimizations may be really hard: e.g dependent types
;; maybe try doing them at a higher level (also easier to reason about)
;; but like racket: first expand program to base macros / code and then try to optimize


;; first implement it quite abstracted, optimize later when language design is futher

;; create shared object for every file, load them to import the file, can be reloaded by reloading shared object (live code reload)

;; FUNCTIONAL CODE IS THE MOST IMPORTANT PART!!! (this is hard for elf file as it is quite complicated)

;; not really function but whatever
'(let ((Button test (new Button "Hello")))
   (text test))

'((set r1 (allocate-memory-in-region region0 16))
  (set (r1 0) BUTTON_TAG)
  (set (r1 1) "H") ;; also add string size prefix
  ...
  (ret (r1 1))) ;; tell caller to also deallocate this memory

'((;; x86 assembly
   ))