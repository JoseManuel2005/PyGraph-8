#lang eopl

;Proyecto del curso de FLP
;URL del repositorio: 

;Integrantes:
;Carlos Daniel Corrales Arango (2122878)
;Jose Manuel Palma (2125182)

;***********************************************************

;;   La definición BNF para las expresiones del lenguaje:

;    <programa> :=  <expresion>
;                   un-programa (exp)

;    <expresion> := <numero>
;                   numero-lit (num)

;                := "\""<texto> "\""
;                   texto-lit (txt)

;                := '\''<caracter> '\''
;                   caracter-lit (crc)

;                := <identificador>
;                   var-exp (id)

;                := var (<identificador> = <expresion> (,)) { <expresion> }
;                   variableMutable-exp (ids exps cuerpo)

;                := const (<identificador> = <expresion> (,)) { <expresion> }
;                   variableNoMutable-exp (ids exps cuerpo)

;                ::= rec {<identificador> (<identificador>* (;)) = <expresion>}* en <expresion>
;                   rec-exp (proc-nombres idss exps cuerpodecrec)

;******************************************************************************************

;Especificación Léxica

(define spec-lexica
'((white-sp
   (whitespace) skip)
  
  (comentario
   ("%" (arbno (not #\newline))) skip)

  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  
  (texto
   ((or letter "-") (arbno (or letter digit "-" ":"))) string)

  (caracter
   ((or letter "-") (arbno (or letter digit "-" ":"))) string)
  
  (numero
   (digit (arbno digit)) number)
  
  (numero
   ("-" digit (arbno digit)) number)

  (numero
   (digit (arbno digit) "." (arbno digit)) number)

  (numero
   ("-" digit (arbno digit) "." (arbno digit)) number)

  )
)

;Especificación Sintáctica (gramática)

(define spec-gramatica
  '((programa (expresion) un-programa)
    
    (expresion (numero) numero-lit)
    (expresion ("\""texto"\"") texto-lit)
    (expresion ("\''caracter'\'") caracter-lit)
    (expression ("false") false-exp)
    (expression ("true") true-exp)
    (expresion (identificador) var-exp)
    (expresion ("var" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableMutable-exp)
    (expresion ("const" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableNoMutable-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ";") ")" "=" expresion) "en" expresion) rec-exp)
    
    ))