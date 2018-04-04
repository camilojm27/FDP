;;PUNTO 1;;
;;CONTRATO;;
;;Datos_Persona: (a b c d e f g);;
;;Demuestra el nombre, email y fecha de nacimiento de una persona;;
;;Ejemplo: persona1 = (make-persona (make-nombre "Santiago" "Martinez" "Mesa") "aaa@gmail.com" (make-fecha 9 8 2000))

(define-struct nombre (nombre apellido1 apellido2))
(define-struct fecha (dia mes año))
(define-struct persona (nombre email fecha))


(define persona1 (make-persona (make-nombre "Santiago" "Martinez" "Mesa") "Santiago@gmail.com" (make-fecha 9 8 2001)))

;;PUNTO 2;;

(define persona2 (make-persona (make-nombre "Camilo" "Mezu" "Mina") "Camilo@gmail.com" (make-fecha 8 5 1998)))
(define persona3 (make-persona (make-nombre "Pablo" "Marin" "Rodriguez") "Pablo@gmail.com" (make-fecha 15 09 1999)))
(define persona4 (make-persona (make-nombre "Elver" "Andres" "Galarga") "Elver@gmail.com" (make-fecha 5 6 1995)))
(define persona5 (make-persona (make-nombre "Faruck" "Mezu" "Mina") "faruckmezu@gmail.com" (make-fecha 5 6 1995)))
;;PUNTO 3;;

(define-struct fechaReferencia (dia mes año))
(make-fechaReferencia 1 1 2000)

(define (esMayor? a)
(cond
[(>= (fechaReferencia-año(make-fechaReferencia 1 1 2000))(fecha-año(persona-fecha a))) #false]
[(< (fechaReferencia-año(make-fechaReferencia 1 1 2000))(fecha-año(persona-fecha a))) #true]))

(esMayor? persona1)

;PUNTO 4;;
;PUNTO 5;

(define (sonParientes? p1 p2)
  (cond
    [(string=? (nombre-apellido1(persona-nombre p1)) (nombre-apellido1(persona-nombre p2))) "Son parientes"]
    [(string=? (nombre-apellido2(persona-nombre p1)) (nombre-apellido2(persona-nombre p2))) "Son parientes"]

    [(string=? (nombre-apellido1(persona-nombre p1)) (nombre-apellido2(persona-nombre p2))) "Son parientes"]
    [(string=? (nombre-apellido2(persona-nombre p1)) (nombre-apellido1(persona-nombre p2))) "Son parientes"]

    [else "No son parientes"]
    

  ))
(sonParientes? persona2 persona5)

;PUNTO 6;;

(define-struct point2d (x y))

(define g (make-point2d 2 3))
(define h (make-point2d 4 6))
(define i (make-point2d 1 2))

(define-struct points (g h i))
(make-points g h i)

;PUNTO 7;;

(define-struct rect (g h))

(define rec1 (make-rect g h))
(define rec2 (make-rect i h))
(define rec3 (make-rect g i))

(define-struct rects (rec1 rec2 rec3))
(make-rects rec1 rec2 rec3)

;PUNTO 10;;
#lang racket
(define-struct point2d (x y) #:transparent)

(define point2d1 (make-point2d 5 6))
(define point2d2 (make-point2d 2 3))
(define point2d3 (make-point2d 4 5))
(define point2d4 (make-point2d 10 12))

(define-struct rect (x1 x2 y1 y2))

(define rect1 (make-rect 1 6 3 7))

(define (interv a)
  (cond
    [(and (>= (point2d-x a) 1) (<= (point2d-x a) 6))
          (and (>= (point2d-y a) 3) (<= (point2d-y a) 7))"esta dentro"]
    (else "esta fuera")))

(interv point2d4)

;;PUNTO 11 (SIN TERMINAR);;

#lang racket

(define-struct rect (x1 x2 y1 y2))

(define rect1 (make-rect 2 6 2 2))
(define rect2 (make-rect 4 8 2 2))

(define (overlap a b)
  (cond
    [(and (>= (rect-x1 a) (rect-x1 b)) (<= (rect-x2 a) (rect-x2 b))
