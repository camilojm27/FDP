;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

(define (precio n lista)
  (cond
    [(empty? lista) false]
    [(string=? n(producto-toy (first lista)))(producto-cost (first lista))]
    [else(precio n(rest lista))]))

(precio "nerf" inventario)

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
