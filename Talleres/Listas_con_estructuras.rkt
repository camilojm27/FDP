;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Listas_con_estructuras) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

;;PARTE 2 - TALLER LISTAS CON ESTRUCTURAS

;INTEGRANTES:
;Camilo José Mezú = 1824313-3743
;Pablo Andrés Marín Rodriguez = 1870116-3743
;Santiago Martínez Mesa = 1823107-3743

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;PRIMERA PARTE-PUNTO 1;;

;;CONTRATO: struct-elemento (nombre album artista duracion estrellas) -> struct
;;          objeto-struct-elemento (nombre album artista duracion estrellas) -> struct
;;FUNCION:;Crear una estructura que almacene los datos relevantes de las canciones
          ;Creacion de instancias de la estructura "canciones"
;;EJEMPLO: (define cancion8 (make-elemento "Titanium" "Nothing but the Beat" "David Guetta" (make-duracion 0 4 5) "5 estrellas")) -> (make-elemento "Titanium" "Nothing but the Beat" "David Guetta" (make-duracion 0 4 5) "5 estrellas")
;;CUERPO:
(define-struct cancion (nombre album artista duracion estrellas) )

;Creacion de instancias de la estructura "canciones"
(define song1 (make-cancion "in the end" "soft metal" "linkin park" (list 0 3 15) 1))
(define song2 (make-cancion "numb" "soft metal" "linkin park" (list 0 3 12) 5))
(define song3 (make-cancion "what i've done" "soft metal" "linkin park" (list 0 3 30) 3))
(define song4 (make-cancion "Ilussia" "Ilussia" "Mägo de Oz" (list 0 7 54) 1))
(define song5 (make-cancion "Animals" "V" "Maroon 5" (list 0 4 0) 4))
(define song6 (make-cancion "Light it up" "Cream Anthems 2016" "Major Lazer" (list 0 2 50) 4))
(define song7 (make-cancion "Bad" "Listen Again" "David Guetta" (list 0 2 50) 1))
(define song8 (make-cancion "Titanium" "Nothing but the Beat" "David Guetta" (list 0 4 5) 5))
(define song9 (make-cancion "Burn It Down" "soft metal" "linkin park" (list 0 3 15) 4))
(define song10 (make-cancion "The End" "A Shipwreck In The Sand" "Silverstein" (list 0 7 24) 1))

;Creacion de la lista de canciones
(define listaCanciones (list song1 song2 song3 song4 song5 song6 song7 song8 song9 song10))

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;PRIMERA PARTE-PUNTO 2;;

;nombre, un album, el artista, la duración en segundos de la pista y número de estrellas que usted les ha asignado(Entre 1 y 5).

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE-PUNTO 1;;

;CONTRATO: (string list) -> struct
;FUNCION: Crear una funcion que busque una cancion y la retorne o devuelva empty
;EJEMPLO: ((songSearch "Medusa" listaCanciones)) -> (make-cancion "Medusa" "Count Your Blessing" "Bring Me The Horizon" (make-duracion 0 5 39) 5)
;CUERPO:

(define (songSearch nombreCancion lista)
  (cond
    [(empty? lista) empty]
    [(equal? nombreCancion (cancion-nombre (first lista))) (first lista)]
    [else (songSearch nombreCancion (rest lista))]))

(songSearch "in the end" listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE-PUNTO 2;;

;CONTRATO: (string list) -> list
;FUNCION: Crear una funcion que realize una busqueda por artista y retorne todas las canciones de el o empty
;EJEMPLO: (searchArtist "Funeral For A Friend" listaCanciones) ->
;(list
 ;(make-cancion "1%" "Chapter And Verse" "Funeral For A Friend" (make-duracion 0 3 44) 4)
 ;(make-cancion "Escape Artits Never Die" "Your History Is Mine" "Funeral For A Friend" (make-duracion 0 5 18) 1))
;CUERPO:

(define (searchArtist artista lista)
  (cond
    [(empty? lista) empty]
    [(equal? artista (cancion-artista (first lista))) (cons (first lista) (searchArtist artista (rest lista)))]
    [else (searchArtist artista (rest lista))]))

(searchArtist "linkin park" listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE-PUNTO 3;;

;CONTRATO:  (list) -> list
;FUNCION: Crear una funcion que retorna la duración de la lista de reproducción en el formato “horas:minutos:segundos”
;EJEMPLO: (list (make-cancion "in the end" "soft metal" "linkin park" (list 0 3 15) 1))
;               (make-cancion "numb" "soft metal" "linkin park" (list 0 3 12) 5)) -> (0 6 30)
;CUERPO:

(define (listDurationFinale lista)
  (local
    ((define (listDuration2Sec lista)
      (cond
        [(empty? lista) 0]
        [else (+ (third (cancion-duracion (first lista))) (listDuration2Sec (rest lista)))]))
     (define (listDuration2Min lista)
       (cond
        [(empty? lista) 0]
        [else (+ (second (cancion-duracion (first lista))) (listDuration2Min (rest lista)))]))
     (define (listDuration2Hour lista)
       (cond
         [(empty? lista) 0]
         [else (+ (first (cancion-duracion (first lista))) (listDuration2Hour (rest lista)))])))
  (cond
    [(empty? lista) 0]
    [(and (<=(listDuration2Sec lista) 120)(>=(listDuration2Sec lista) 60))
          (append (list (listDuration2Hour lista)) (list (+ (listDuration2Min lista) 1)) (list (- (listDuration2Sec lista) 60)))]
    [(and (<=(listDuration2Sec lista) 180)(>=(listDuration2Sec lista) 120))
          (append (list (listDuration2Hour lista)) (list (+ (listDuration2Min lista) 2)) (list (- (listDuration2Sec lista) 120)))]
    [(and (<=(listDuration2Sec lista) 240)(>=(listDuration2Sec lista) 180))
          (append (list (listDuration2Hour lista)) (list (+ (listDuration2Min lista) 3)) (list (- (listDuration2Sec lista) 180)))]
    [(and (<=(listDuration2Sec lista) 300)(>=(listDuration2Sec lista) 240))
          (append (list (listDuration2Hour lista)) (list (+ (listDuration2Min lista) 4)) (list (- (listDuration2Sec lista) 240)))]
    [else (append (list (listDuration2Hour lista))(list (listDuration2Min lista))(list (listDuration2Sec lista)))])))

(listDurationFinale listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;SEGUNDA PARTE - PUNTO 4;;

;CONTRATO:  (list) -> list
;FUNCION: Retorna todas la canciones con almenos 2 estrellas
;EJEMPLO: (list (make-cancion "in the end" "soft metal" "linkin park" (list 0 3 15) 1))
;               (make-cancion "numb" "soft metal" "linkin park" (list 0 3 12) 2)) -> (list(make-cancion "numb" "soft metal" "linkin park" (list 0 3 12) 2)))
;CUERPO:

(define (2star lista)
  (if (empty? lista) lista
      (if (>= (cancion-estrellas (first lista)) 2) (append (list (first lista)) (2star (rest lista)))
          (2star (rest lista)))))

(2star listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE - PUNTO 5;;

;CONTRATO: list -> list
;FUNCION: Crear una funcion que imprima todas las canciones con 5 estrellas
;EJEMPLO: (5star listaCanciones) --->>> (make-cancion "Pumppet Master" "Dear Desolation" "Thy Art Is Murder" (make-duracion 0 3 16) 5)
;         (make-cancion "Viviendo" "A través de mí" "Nach" (make-duracion 0 5 15) 5)
;         (make-cancion "When I Was Done Dying" "Gliss Riffer" "Dan Deacon" (make-duracion 0 4 19) 5)
;         (make-cancion
;          "02 - Honey, this Mirror Isn't Big Enough for the Two of Us"
;          "I Brought You My Bullets, You Brought Me Your Love"
;          "My Chemical Romance"
;         (make-duracion 0 3 53)
;          5)
;         (make-cancion "Medusa" "Count Your Blessing" "Bring Me The Horizon" (make-duracion 0 5 39) 5)
;         (make-cancion "Reino de Tormentas" "Reino de Tormentas" "Deny" (make-duracion 0 3 43) 5)
;         (make-cancion "The End" "A Shipwreck In The Sand" "Silverstein" (make-duracion 0 7 24) 5))
;CUERPO:


(define (5star lista)
  (cond
    [(empty? lista) empty]
    [(equal? 5(cancion-estrellas (first lista)) )(cons (first lista) (5star (rest lista))) ]
    [else (5star (rest lista))]))

(5star listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE - PUNTO 6;;

;CONTRATO: list -> list
;FUNCION: Crear una funcion que imprima los titulos de las canciones y su duracion
;EJEMPLO: (list (cancion "in the end" "soft metal" "linkin park" '(0 3 15) 1)
;               (cancion "numb" "soft metal" "linkin park" '(0 3 12) 5)) -> '(("in the end" (0 3 15)) ("numb" (0 3 12)))
;CUERPO:

(define (titDur lista)
  (cond
    [(empty? lista) empty]
    [else  (cons (list (cancion-nombre (first lista)) (cancion-duracion (first lista))) (titDur(rest lista)))]))


(titDur listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE - PUNTO 7;;

;CONTRATO: list -> list
;FUNCION: Crear una funcion que creo una lista con  las canciones de mejor a peor
;EJEMPLO: (list (cancion "in the end" "soft metal" "linkin park" '(0 3 15) 1)
;               (cancion "numb" "soft metal" "linkin park" '(0 3 12) 5))
;               (cancion "Burn It Down" "soft metal" "linkin park" '(0 3 15) 4))
;            -> (cancion "numb" "soft metal" "linkin park" '(0 3 12) 5))
;               (cancion "Burn It Down" "soft metal" "linkin park" '(0 3 15) 4))
;               (cancion "in the end" "soft metal" "linkin park" '(0 3 15) 1)
;CUERPO:

;(define (bestToWorse lista)
;  (cond
;    [(=(length lista)1) lista]
;    [(> (cancion-estrellas (first lista)) (cancion-estrellas (first(rest lista)))) (cons (first (rest lista)) (bestToWorse(rest lista)))]
;    [(< (cancion-estrellas (first lista)) (cancion-estrellas (first(rest lista)))) (cons (first (rest lista)) (bestToWorse(rest lista)))]
;    [(= (cancion-estrellas (first lista)) (cancion-estrellas (first(rest lista)))) (cons (first (rest lista)) (bestToWorse(rest lista)))]
;    [else 0]))

;(bestToWorse2 listaCanciones)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;SEGUNDA PARTE - PUNTO 8;;

;CONTRATO: list -> list
;FUNCION: Crear una funcion que elimine la n-ésima canción
;EJEMPLO: (extraer_elemento 1 (list (cancion "in the end" "soft metal" "linkin park" '(0 3 15) 1)
;               (cancion "numb" "soft metal" "linkin park" '(0 3 12) 5))
;               (cancion "Burn It Down" "soft metal" "linkin park" '(0 3 15) 4))
;            -> (list (cancion "in the end" "soft metal" "linkin park" '(0 3 15) 1)
;               (cancion "Burn It Down" "soft metal" "linkin park" '(0 3 15) 4))
;CUERPO:
 
(define extraer_elemento (lambda (pos listado)
(letrec (
(funcion (lambda (contador posicion lista)
(if (null? lista)
'()
(if (= contador posicion)
(cdr lista) 
(cons (car lista) (funcion (+ contador 1) posicion (cdr lista)))
);if
);if
);lambda
);funcion
)
(funcion 1 pos listado));letrec 
);lambda
);define

(extraer_elemento 2 listaCanciones)


;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;TALLER LISTAS CON ESTRUCTURAS

;;TERCERA PARTE-PUNTO 1;;
;;CONTRATO: struct-elemento (nombre costo existencias) -> struct
;;FUNCION: Crea una estructura que almacena el nombre, costo y numero de elementos en existencia de un producto de inventario
;;EJEMPLO: (define e1 (make-elemento "aguacates" 1500 12) -> (make-elemento "aguacates" 1500 12)
;;CUERPO:


(define-struct producto (toy cost existencias))

(define p1 (make-producto "hotwheels" 300 200))
(define p2 (make-producto "trompo" 5 500))
(define p3 (make-producto "yoyo" 6 300))
(define p03 (make-producto "cartas" 11 0))
(define p4 (make-producto "nerf" 1000 150))
(define p5 (make-producto "ps4" 5000 60))
(define p6 (make-producto "tazos" 9 800))
(define p7 (make-producto "monopoli" 3000 0))

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;TERCERA PARTE-PUNTO 2;;
;;CONTRATO: (list item item item) -> list
;;FUNCION: Define una lista que representa el inventario de la tienda.
;;EJEMPLO: (define l1(list aguacates mangos peras manzanas) -> (list "aguacates" "mangos" "peras" "manzanas"))
;;CUERPO:

(define inventario (list p1 p2  p3 p03  p4 p5 p6 p7 ))


;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;;CUARTA PARTE-PUNTO 1;;
;;CONTRATO: (cheapList list) -> list
;;FUNCION: Lista los elementos que cuestan menos de $10
;;EJEMPLO: (cheapList inventario) -> list (aguacates moras peras); si (aguacates 8 12) (papayas 12 12) (moras 7 24) (peras 5 23) (piñas 20 12)
;;CUERPO:
(define (listMin10 lista)
  (cond
    [(empty? lista) empty]
    [(<= (producto-cost (first lista)) 10) (cons (first lista) (listMin10 (rest lista)))]
    [(> (producto-cost (first lista)) 10) (cons (listMin10 (rest lista)) empty)]))

(listMin10 inventario)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;CUARTA PARTE-PUNTO 2;;
;;CONTRATO: (givePrice item) -> number
;;FUNCION:  Da el precio del item con un nombre dado
;;EJEMPLO: (givePrice aguacate) -> 8 ; si (aguacates 8 12)
;;CUERPO:

(define (precio n lista)
  (cond
    [(empty? lista) false]
    [(string=? n(producto-toy (first lista)))(producto-cost (first lista))]
    [else(precio n(rest lista))]))

(precio "nerf" inventario)
;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;CUARTA PARTE-PUNTO 3;;
;;CONTRATO: (enoughUnits item) -> number
;;FUNCION:  Verificar si existen suficientes unidades del item con el nombre dado.
;;EJEMPLO: (enoughUnits aguacate inventario) -> 12 ; si (aguacates 8 12)
;;CUERPO:

(define (enoughUnits item lista)
  (cond
    [(empty? lista) "No tenemos existencias"]
    [(equal? item (producto-toy (first lista))) (producto-existencias (first lista))]
    [else (enoughUnits item (rest lista))]
    ))

(enoughUnits "yoyo" inventario)

;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;CUARTA PARTE-PUNTO 4;;
;;CONTRATO: (suprItem list) -> lista
;;FUNCION: Eliminar de la lista los items cuyas existencias sean iguales a 0
;;EJEMPLO: (suprItem lista) -> (list papayas moras peras piñas)
;;CUERPO:

;(define (reduce0 lista)
 ; (if (empty? lista) lista
  ;    (if (= (producto-existencias (first lista)) 0) (append (list (first (rest lista))) (reduce0 (rest lista)))
   ;       (append (list (first lista))(reduce0 (rest lista))))))

;(reduce0 inventario)


;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;CUARTA PARTE-PUNTO 5;;
;;CONTRATO: (precio-unidades number item) -> number
;;FUNCION: Da el precio total para la venta de n items del nombre dado
;;EJEMPLO: (precio-unidades "aguacate" lista 20) -> 160  ; si (aguacates 8 12)
;;CUERPO:

(define (precio-unidades name lista nrounidades)
  (cond
    [(empty? lista) false]
    [(string=? name(producto-toy (first lista)))(* (producto-cost (first lista)) nrounidades)]
    [else(*(precio name(rest lista))nrounidades)]))

(precio-unidades "ps4" inventario 200)

;--------------------------------END----OF---FILE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
