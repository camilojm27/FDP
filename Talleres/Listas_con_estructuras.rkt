;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Listas_con_estructuras) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;PARTE 2 - TALLER LISTAS CON ESTRUCTURAS

;INTEGRANTES:
;Camilo José Mezú = 1824313-3743
;Pablo Andrés Marín Rodriguez = 1870116-3743
;Santiago Martínez Mesa = 1823107-3743


;Crear una estructura que almacene los datos relevantes de las canciones
(define-struct duracion (horas minutos segundos))
(define-struct cancion (nombre album artista duracion estrellas))
;Creacion de instancias de la estructura "canciones"
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

(define listaCanciones (list song1 song2 song3 song4 song5 song6 song7 song8 song9 song10))



;;PRIMERA PARTE-PUNTO 2;;

;nombre, un album, el artista, la duración en segundos de la pista y número de estrellas que usted les ha asignado(Entre 1 y 5).


;;SEGUNDA PARTE-PUNTO 1;;


(define (songSearch nombreCancion lista)
  (cond
    [(empty? lista) empty]
    [(equal? nombreCancion (cancion-nombre (first lista))) (first lista)]
    [else (songSearch nombreCancion (rest lista))]))

(songSearch "Titanium" listaCanciones)

;;SEGUNDA PARTE-PUNTO 2;;

(define (searchArtist artista lista)
  (cond
    [(empty? lista) empty]
    [(equal? artista (cancion-artista (first lista))) (cons (first lista) (searchArtist artista (rest lista)))]
    [else (searchArtist artista (rest lista))]))

;(searchArtist "Silverstein" listaCanciones)

;;SEGUNDA PARTE-PUNTO 3;;

;(define (listDuration lista)
;  (cond
 ;   [(empty? lista) empty]
  ;  [else (+ (duracion-segundos (duracion-first lista)) (listDuration (rest lista)))]))

;(listDuration listaCanciones)

;PUNTO4

;(define (mayor2 list)
 ; (if (>= 2 (cancion-estrellas (first list))) (cons (first lista) (2star (rest lista)) "holi"))) 

;(define (2star lista)
 ; (cond
  ;  [(empty? lista) empty]
   ; [(filter mayor2  lista) (cons (first lista) (2star (rest lista)) )]
    ; ))

;(2star listaCanciones )


;Punto 5


(define (5star lista)
  (cond
    [(empty? lista) empty]
    [(equal? 5(cancion-estrellas (first lista)) )(cons (first lista) (5star (rest lista))) ]
    [else (5star (rest lista))]))

;(5star listaCanciones)

;Punto 6

(define (titDur lista)
  (cond
    [(empty? lista) empty]
    [else  (append(list (cancion-nombre (first lista)) (duracion-horas (cancion-duracion (first lista)))
                        (duracion-minutos (cancion-duracion (first lista)))
                        (duracion-segundos (cancion-duracion (first lista)))) (titDur(rest lista)))]))


;(titDur listaCanciones)


;(not (empty? lista))