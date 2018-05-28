;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Listas_con_estructuras) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;PARTE 2 - TALLER LISTAS CON ESTRUCTURAS

;INTEGRANTES:
;Camilo José Mezú = 1824313-3743
;Pablo Andrés Marín Rodriguez = 1870116-3743
;Santiago Martínez Mesa = 1823107-3743



;;PRIMERA PARTE-PUNTO 1;;
;;CONTRATO: struct-elemento (nombre album artista duracion estrellas) -> struct
;;FUNCION:;Crear una estructura que almacene los datos relevantes de las canciones

;;CUERPO:

(define-struct duracion (horas minutos segundos))
(define-struct cancion (nombre album artista duracion estrellas))
;;PRIMERA PARTE-PUNTO 1;;
;Contrato: objeto-struct-elemento (nombre album artista duracion estrellas) -> struct
;Creacion de instancias de la estructura "canciones"
;;EJEMPLO: (define cancion8 (make-elemento "Titanium" "Nothing but the Beat" "David Guetta" (make-duracion 0 4 5) "5 estrellas")) -> (make-elemento "Titanium" "Nothing but the Beat" "David Guetta" (make-duracion 0 4 5) "5 estrellas")

(define song1 (make-cancion "Aquamarine" "Dead Reflection" "Silverstein" (make-duracion 0 3 44 ) 4))
(define song2 (make-cancion "Pumppet Master" "Dear Desolation" "Thy Art Is Murder" (make-duracion 0 3 16) 5))
(define song3 (make-cancion  "Viviendo" "A través de mí" "Nach" (make-duracion 0 5 15) 5 ))
(define song4 (make-cancion "When I Was Done Dying" "Gliss Riffer" "Dan Deacon" (make-duracion 0 4 19) 5 ))
(define song5(make-cancion "02 - Honey, this Mirror Isn't Big Enough for the Two of Us" "I Brought You My Bullets, You Brought Me Your Love" "My Chemical Romance" (make-duracion 0 3 53 )5 ))
(define song6 (make-cancion "1%" "Chapter And Verse" "Funeral For A Friend" (make-duracion 0 3 44) 4))
(define song7 (make-cancion "Escape Artits Never Die" "Your History Is Mine" "Funeral For A Friend" (make-duracion 0 5 18 )1 ))
(define song8 (make-cancion "Medusa" "Count Your Blessing" "Bring Me The Horizon" (make-duracion 0 5 39) 5))
(define song9 (make-cancion "Reino de Tormentas" "Reino de Tormentas" "Deny" (make-duracion 0 3 43) 5 ))
(define song10 (make-cancion "The End" "A Shipwreck In The Sand" "Silverstein" (make-duracion 0 7 24) 5))

;Crear una lista de reproduccion la cual almacene las canciones
(define listaCanciones (list song1 song2 song3 song4 song5 song6 song7 song8 song9 song10))



;---------------------------------------------------------------------------------------------------------------------------------------


;SEGUNDA PARTE-PUNTO 1;;
;CONTRATO: (string list) -> struct
;Crear una funcion que busque una cancion y la retorne o devuelva empty
;Ejemplo ((songSearch "Medusa" listaCanciones)) -> (make-cancion "Medusa" "Count Your Blessing" "Bring Me The Horizon" (make-duracion 0 5 39) 5)


(define (songSearch nombreCancion lista)
  (cond
    [(empty? lista) empty]
    [(equal? nombreCancion (cancion-nombre (first lista))) (first lista)]
    [else (songSearch nombreCancion (rest lista))]))

;(songSearch "The End" listaCanciones)
;---------------------------------------------------------------------------------------------------------------------------------------
;;SEGUNDA PARTE-PUNTO 2;;
;CONTRATO: (string list) -> list
;Crear una funcion que realize una busqueda por artista y retorne todas las canciones de el o empty
;Ejemplo (searchArtist "Funeral For A Friend" listaCanciones) ->
;(list
 ;(make-cancion "1%" "Chapter And Verse" "Funeral For A Friend" (make-duracion 0 3 44) 4)
 ;(make-cancion "Escape Artits Never Die" "Your History Is Mine" "Funeral For A Friend" (make-duracion 0 5 18) 1))


(define (searchArtist artista lista)
  (cond
    [(empty? lista) empty]
    [(equal? artista (cancion-artista (first lista))) (cons (first lista) (searchArtist artista (rest lista)))]
    [else (searchArtist artista (rest lista))]))

;(searchArtist "Silverstein" listaCanciones)
;---------------------------------------------------------------------------------------------------------------------------------------

;(searchArtist "Silverstein" listaCanciones)

;;SEGUNDA PARTE-PUNTO 3;;

;(define (listDuration lista)
;  (cond
 ;   [(empty? lista) empty]
  ;  [else (+ (duracion-segundos (duracion-first lista)) (listDuration (rest lista)))]))

;(listDuration listaCanciones)
;---------------------------------------------------------------------------------------------------------------------------------------
;PUNTO4

;(define (mayor2 list)
 ; (if (>= 2 (cancion-estrellas (first list))) (cons (first lista) (2star (rest lista)) "holi"))) 

;(define (2star lista)
 ; (cond
  ;  [(empty? lista) empty]
   ; [(filter mayor2  lista) (cons (first lista) (2star (rest lista)) )]
    ; ))

;(2star listaCanciones )

;---------------------------------------------------------------------------------------------------------------------------------------
;;SEGUNDA PARTE-PUNTO 5;;

;CONTRATO: list -> list
;Crear una funcion que imprima todas las canciones con 5 estrellas
;Ejemplo (5star listaCanciones) --->>> (make-cancion "Pumppet Master" "Dear Desolation" "Thy Art Is Murder" (make-duracion 0 3 16) 5)
;  (make-cancion "Viviendo" "A través de mí" "Nach" (make-duracion 0 5 15) 5)
; (make-cancion "When I Was Done Dying" "Gliss Riffer" "Dan Deacon" (make-duracion 0 4 19) 5)
; (make-cancion
;  "02 - Honey, this Mirror Isn't Big Enough for the Two of Us"
;  "I Brought You My Bullets, You Brought Me Your Love"
;  "My Chemical Romance"
;  (make-duracion 0 3 53)
;  5)
; (make-cancion "Medusa" "Count Your Blessing" "Bring Me The Horizon" (make-duracion 0 5 39) 5)
; (make-cancion "Reino de Tormentas" "Reino de Tormentas" "Deny" (make-duracion 0 3 43) 5)
; (make-cancion "The End" "A Shipwreck In The Sand" "Silverstein" (make-duracion 0 7 24) 5))

(define (5star lista)
  (cond
    [(empty? lista) empty]
    [(equal? 5(cancion-estrellas (first lista)) )(cons (first lista) (5star (rest lista))) ]
    [else (5star (rest lista))]))

;(5star listaCanciones)
;---------------------------------------------------------------------------------------------------------------------------------------
;;SEGUNDA PARTE-PUNTO 6;;

;CONTRATO: list -> list
;Crear una funcion que imprima los titulos de las canciones y su duracion
;Ejemplo

(define (titDur lista)
  (cond
    [(empty? lista) empty]
    [else  (append(list (cancion-nombre (first lista)) (duracion-horas (cancion-duracion (first lista)))
                        (duracion-minutos (cancion-duracion (first lista)))
                        (duracion-segundos (cancion-duracion (first lista)))) (titDur(rest lista)))]))


;(titDur listaCanciones)
;---------------------------------------------------------------------------------------------------------------------------------------
;;SEGUNDA PARTE-PUNTO 7;;

;CONTRATO: list -> list
;Crear una funcion que creo una lista con  las canciones de mejor a peor
;Ejemplo




;---------------------------------------------------------------------------------------------------------------------------------------
;;SEGUNDA PARTE-PUNTO 8;;

;CONTRATO: list -> list
;Crear una funcion que elimine la n-ésima canción
;Ejemplo





;;PARTE 2 - TALLER LISTAS CON ESTRUCTURAS

;;TERCERA PARTE-PUNTO 1;;
;;CONTRATO: struct-elemento (nombre costo existencias) -> struct
;;FUNCION: Crea una estructura que almacena el nombre, costo y numero de elementos en existencia de un producto de inventario
;;EJEMPLO: (define e1 (make-elemento "aguacates" 1.500 12) -> (make-elemento "aguacates" 1.5 12)
;;CUERPO:

;;TERCERA PARTE-PUNTO 2;;
;;CONTRATO: (list item item item) -> list
;;FUNCION: Define una lista que representa el inventario de la tienda.
;;EJEMPLO: (define l1(list aguacates mangos peras manzanas) -> (list "aguacates" "mangos" "peras" "manzanas"))
;;CUERPO:

;;CUARTA PARTE-PUNTO 1;;
;;CONTRATO: (cheapList list) -> list
;;FUNCION: Lista los elementos que cuestan menos de $10
;;EJEMPLO: (cheapList inventario) -> list (aguacates moras peras); si (aguacates 8 12) (papayas 12 12) (moras 7 24) (peras 5 23) (piñas 20 12)
;;CUERPO:

;;CUARTA PARTE-PUNTO 2;;
;;CONTRATO: (givePrice item) -> number
;;FUNCION:  Da el precio del item con un nombre dado
;;EJEMPLO: (givePrice aguacate) -> 8 ; si (aguacates 8 12)
;;CUERPO:

;;CUARTA PARTE-PUNTO 3;;
;;CONTRATO: (enoughUnits item) -> number
;;FUNCION:  Verificar si existen suficientes unidades del item con el nombre dado.
;;EJEMPLO: (enoughUnits aguacate) -> 12 ; si (aguacates 8 12)
;;CUERPO:

;;CUARTA PARTE-PUNTO 4;;
;;CONTRATO: (suprItem list) -> lista
;;FUNCION: Eliminar de la lista los items cuyas existencias sean iguales a 0
;;EJEMPLO: (suprItem aguacate) -> (list papayas moras peras piñas)
;;CUERPO:

;;CUARTA PARTE-PUNTO 5;;
;;CONTRATO: (totalPrice number item) -> number
;;FUNCION: Da el precio total para la venta de n items del nombre dado
;;EJEMPLO: (totalPrice 3 aguacate) -> 24
;;CUERPO: