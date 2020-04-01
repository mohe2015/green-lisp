#lang racket

(print (+ 1 2)) ;; print has side effects
->
(push 1)
(push 2)
(add)
(print)


(* (+ 1 2) (- 3 1))
->
(push 1)
(push 2)
(add)
(push 3)
(push 1)
(subtract)
(multiply)