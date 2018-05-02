;Realizar un programa que crea una lista de tamaño n
;y los valores de la lista son n a la n
;Utiliza funciones locales

(define (magic n) (local (
(define (nn n) (expt n n))
(define (magic n x) (cond [(= n 0) empty]
                        [else(cons x (magic (- n 1) x))])))

(magic n (nn n))))

(magic 3 )

;(check-expect (magic 3 (nn 3)) (list 27 27 27))
