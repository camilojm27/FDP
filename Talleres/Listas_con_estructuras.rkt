;INTEGRANTES:
;Camilo José Mezú = 1824313-3743
;Pablo Andrés Marín Rodriguez = 1870116-3743
;Santiago Martínez Mesa = 1823107-3743


;Crear una estructura que almacene los datos relevantes de las canciones

(define-struct canciones (nombre album artista duracion estrella))
;Creacion de instancias de la estructura "canciones"
(define song1 (make-canciones "Aquamarine" "Dead Reflection" "Silverstein" 3.44 4))
(define song2 (make-canciones "Pumppet Master" "Dear Desolation" "Thy Art Is Murder" 3.16 5))
(define song3 (make-canciones  "Viviendo" "A través de mí" "Nach" 5.15 5 ))
(define song4 (make-canciones "When I Was Done Dying" "Gliss Riffer" "Dan Deacon" 4.19 5 ))
(define song5(make-canciones "02 - Honey, this Mirror Isn't Big Enough for the Two of Us" "I Brought You My Bullets, You Brought Me Your Love" "My Chemical Romance" 3.53 5 ))
(define song6 (make-canciones "1%" "Chapter And Verse" "Funeral For A Friend" 3.44 4))
(define song7 (make-canciones "Escape Artits Never Die" "Your History Is Mine" "Funeral For A Friend" 5.18 1 ))
(define song8 (make-canciones "Medusa" "Count Your Blessing" "Bring Me The Horizon" 5.39 5))
(define song9 (make-canciones "Reino de Tormentas" "Reino de Tormentas" "Deny" 3.43 5 ))
(define song10 (make-canciones "The End" "A Shipwreck In The Sand" "Silverstein" 7.24 5))

;Lista de reproduccion de las canciones (indexadas)
(define lista-fav (list song1 song2 song3 song4 song5 song6 song7 song8 song9 song10))

;Contrato: Realizar una funcion que busque si una cancion esta en la lista
;list, string -> struct
(define (searchArtist artista lista)
  (cond
    [(empty? lista) empty]
    [(equal? artista (canciones-artista (first lista))) (cons (first lista) (searchArtist artista (rest lista)))]
    [else (searchArtist artista (rest lista))]))


;R
;

(define (busqueda-Art song-list buscar)
  (cond
    [(empty? song-list) empty]
    [(equal? buscar (canciones-artista (first song-list))) (first song-list)]
    [(busqueda-Art  (rest song-list) buscar) (first song-list)]))



