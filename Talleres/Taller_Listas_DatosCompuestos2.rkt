;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Taller_Listas_DatosCompuestos2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;TALLER #3;;

;;PUNTO 1;;
;;Encontrar el mayor valor de una lista de datos;;
;;maxVal(number) -> number
;;PRUEBAS: (2 5 4 3 0 1) -> 5

(define a (list 2 5 4 3 0 1))
(define (maxVal a)
(if (empty? (rest a)) (first a)
(max (first a) (maxVal (rest a)))))

(check-expect (maxVal a) 5)

;;PUNTO 2;;
;;Encontrar el mayor promedio de una lista de datos;;
;;media(number) -> number
;;PRUEBAS: (2 5 4 3 0 1) -> 2.5

(define lista (list 2 5 4 3 0 1))

(define (size a)
(if(empty? a) 0 (+ 1 (size (rest a)))))

(define (sum a)
(if (empty? a) 0
(+ (first a) (sum (rest a)))))

(define (media a)
(/ (sum a) (size a)))

(check-expect (media a) 2.5)

;;PUNTO 3;;
;;Invierte el orden de una lista;;
;;invert(number) -> number
;;PRUEBAS: (2 5 4 3 0 1) -> 1 0 3 4 5 2

(define (invert a)
(if (empty? a) a
(append (invert (rest a)) (list(first a)))))

(check-expect (invert a)
(cons 1 (cons 0 (cons 3 (cons 4 (cons 5 (cons 2 '())))))) )

;PUNTO 4
;;NO REALIZAR;;

;;PUNTO 5;;
;;Genera una lista de los primeros 5 digitos de la serie fibonacchi;;
;;fibList (number) -> number
;;PRUEBAS:

(define (fibonacci n)
(cond [(= n 0) 0]
[(= n 1) 1]
[else (+ (fibonacci (- n 1))(fibonacci (- n 2)))]))

(define (listaFib n)
(if (= n 0) (list 0)
(append (listaFib (- n 1)) (list (fibonacci n)))))

(check-expect (listaFib 5) (list 0 1 1 2 3 5))
(check-expect (listaFib 8) (list 0 1 1 2 3 5 8 13 21))

;;PUNTO 8
;implementar una función que retorna el índice n de
;dónde se encuentra un número x dado, si existe, o -(n + 1 ), donde n es la
;posición en la cual se debería insertar x para mantener la lista ordenada

(define z (list 3 5 7))

(define (findn l x) (findx l x 0))

(define (findx l x n) (cond
[(empty? l) (- -1 n)]
[(< x (first l)) (- -1 n)]
[(= x (first l)) n]
[else (findx (rest l) x(+ n 1))]))

;PRUEBAS
(check-expect (findn z 1)-1)
(check-expect (findn z 3)0)
(check-expect (findn z 5)1)
(check-expect (findn z 7)2)
(check-expect (findn z 6)-3)
(check-expect (findn z 8)-4)

