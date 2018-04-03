;CONTRATO  number -> binario
;Proposito: un programa que reciba un numero y lo combierta a binario

(define (dec2bin n) (cond[(= n 0) "0"]
                         [else
(string-append (dec2bin (quotient n 2)) (number->string (remainder n 2)))]))


(dec2bin 324); -> "101000100"
(dec2bin 0); -> "0"
(dec2bin 1); -> "01"
(dec2bin 2); -> "10"
(dec2bin 3); -> "011"
