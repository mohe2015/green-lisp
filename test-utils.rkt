#lang racket
(provide list->label-addresses)

;(: list->label-addresses (-> (Listof Symbol) (Listof Integer) (Listof Any) Integer (Values (Listof Any) (Listof Any))))
(define (list->label-addresses symbols sizes codes offset)
  (cond [(or (null? symbols) (null? sizes) (null? codes)) (values (list) (list))]
        [else
         (let* ([symbol (car symbols)]
                [size ((car sizes) offset)]
                [code (car codes)])
           (let-values ([(cara carb) (if (eq? symbol 'null)
                                         (values (list)                      `((,code ,offset)))
                                         (values (list (list symbol offset)) `((,code ,offset))))]
                        [(cdra cdrb) (list->label-addresses (cdr symbols) (cdr sizes) (cdr codes) (+ offset size))])
             (values
              (append cara cdra)
              (append carb cdrb))))]))

'(data-list
  (data-align 12)
  (label symbol))
;; I think this may work as the expressions don't get longer and longer that way
'(let* ((start 0)
        (element-0 (+ start     ((lambda (current-address) (get-byte-count-to-align alignment-bits current-address)) start)))
        (element-1 (+ element-0 ((lambda (_) 0) element-0)))
        (symbol element-1))
   ((lambda (current-address) (make-bytes (get-byte-count-to-align alignment-bits current-address) 0)) elment-0)
   ((lambda (current-address) (bytes)) element-1))