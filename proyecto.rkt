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

;                := <identificador>
;                   var-exp (id)

;                := var (<identificador> = <expresion> (,)) { <expresion> }
;                   variableMutable-exp (ids exps cuerpo)

;                := const (<identificador> = <expresion> (,)) { <expresion> }
;                   variableNoMutable-exp (ids exps cuerpo)

;                ::= rec {<identificador> (<identificador>* (;)) = <expresion>}* en <expresion>
;                   rec-exp (proc-nombres idss exps cuerpodecrec)

;******************************************************************************************