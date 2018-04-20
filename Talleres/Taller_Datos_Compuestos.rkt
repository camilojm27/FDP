;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Taller_Datos_Compuestos) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;Punto 8
(define x (make-point2d 2 3))
(define y (make-point2d 4 6))
(define z (make-point2d 1 2))

(define-struct point2 (x y z))
(make-points x y z)

;Punto 9

(define rec4 (make-rect x y))
(define rec5 (make-rect z y))
(define rec6 (make-rect x z))

(make-rects rec4 rec5 rec6)