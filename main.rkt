#lang racket/base

;; some ideas

;; provable assembly instructions
(define-assembly-instruction ldi (register value)
  (binary-format ...)
  (state-update
   (lambda (state)
     (merge
      state
      (quasiquote (registers
                   (register
                    ((unquote register)
                    (unquote value)))))))))

(let (state (registers (register (r1 1))))
  (let (new-state (state-update (ldi r1 2) state))
    (prove (= 2 (r1 (register (registers new-state)))))))

;; more abstract proofing
;; https://hackernoon.com/writing-code-like-a-mathematical-proof-f5838fc27382

;; this is what we want to prove
(let (a integer)
  (let (b integer)
    (when-then (consecutive a b)
               (odd (+ a b)))))

(define consecutive a b
  (= b (+ a 1)))

(define odd a
  (let (k integer)
    (= a (+ (* 2 k) 1))))

(define + a b
  magic)

;; rewriting what we want to prove
(let (a integer)
  (let (b integer)
    (when-then (= b (+ a 1)
                  (odd (+ a a 1))))))

(let (a integer)
  (odd (+ (* 2 a) 1)))

(let (a integer)
  (let (k integer)
    (= (+ (* 2 a) 1) (+ (* 2 k) 1)))) ;; - 1

(let (a integer)
  (let (k integer)
    (= (* 2 a) (* 2 k)))) ;; / 2

(let (a integer)
  (let (k integer)
    (= a k))) ;; this is solvable -> true

#t

;; write an assembler in racket
(assemble-x86-64
 (ldi r1 7)
 (add r1 r1)
 (ret))

;; make a generic language on top of it with fake variables
(assemble
 (set test 7)
 (set test (+ test test))
 (return test))

;; use macros to create a basic programming language

;; first use stack for variables
;; TODO look how this is implemented in scheme (SICP should have some hints)
(defmacro let (variable value body)
  (push value)
  (bind-variable variable stack-pointer
              (progn body))
  (pop value))

(defmacro lambda (params body)
  (foreach param params
           (set (variable param)
                (value param)))
  (progn body))

(defmacro define (symbol value)
  (symbol name)
  value)