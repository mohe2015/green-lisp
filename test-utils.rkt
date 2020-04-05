#lang racket
(provide list->label-addresses)

;(: list->label-addresses (-> (Listof Symbol) (Listof Integer) (Listof Any) Integer (Values (Listof Any) (Listof Any))))
(define (list->label-addresses symbols sizes codes offset)
  (cond [(or (null? symbols) (null? sizes) (null? codes)) (values (list) (list))]
        [else
         (let* ([symbol (car symbols)] ;; syntax element
                [size #`(#,(car sizes) #,offset)] ;; syntax element of lambda call e.g. code that calculates alignment size
                [code (car codes)] ;; syntax element
                [current-element-symbol (car (generate-temporaries '(element)))]) ;; syntax element
           (let-values ([(cara carb) (if (eq? (syntax-e symbol) 'null)
                                         (values (list (list current-element-symbol offset))                      `(, #`(#,code #,current-element-symbol)))
                                         (values (list (list current-element-symbol offset) (list symbol current-element-symbol)) `(, #`(#,code #,current-element-symbol))))]
                        [(cdra cdrb) (list->label-addresses (cdr symbols) (cdr sizes) (cdr codes) #`(+ #,current-element-symbol #,size))])
             (values
              (append cara cdra)
              (append carb cdrb))))]))

;; (list->label-addresses `(,#'test) `(,#'(lambda (_) 1)) `(,#'(lambda (_) (bytes))) 0)
;; (list->label-addresses `(,#'a ,#'b) `(,#'(lambda (_) 1) ,#'(lambda (_) 2)) `(,#'(lambda (_) (bytes)) ,#'(lambda (_) (bytes))) 0)

;; I think this may work as the expressions don't get longer and longer that way
'(let* ((start 0)
        (element-0 (+ start     ((lambda (current-address) (get-byte-count-to-align alignment-bits current-address)) start)))
        (element-1 (+ element-0 ((lambda (_) 0) element-0)))
        (symbol element-1))
   ((lambda (current-address) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0)) element-0)
   ((lambda (current-address) (bytes)) element-1))