#lang eopl

;Proyecto del curso de FLP
;URL del repositorio: 

;Integrantes:
;Carlos Daniel Corrales Arango (2122878)
;Jose Manuel Palma (2125182)

;***********************************************************

;;Valores denotados: Enteros, flotantes, caracteres, cadenas de caracteres, booleanos (true, false),
;;                   procedimientos, listas, registros, vectores, Grafos no dirigidos, vertices, ejes

;;Valores expresados: Enteros, flotantes, caracteres, cadenas de caracteres, booleanos (true, false),
;;                    procedimientos, listas, registros, vectores, Grafos no dirigidos, vertices, ejes.

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

;                := (<expresion> <primitiva-binaria> <expresion>)
;                   primapp-bin-exp (exp1 prim-binaria exp2)

;                := <primitiva-unaria> (<expresion>)
;                   primapp-un-exp (prim-unaria exp)

;    <primitiva-binaria> :=  + (primitiva-suma)
;                        :=  ~ (primitiva-resta)
;                        :=  / (primitiva-div)
;                        :=  * (primitiva-multi)
;                        :=  % (primitiva-residuo)
;                        :=  concat (primitiva-concat)

;    <primitiva-unaria>  :=  longitud (primitiva-longitud)
;                        :=  add1 (primitiva-add1)
;                        :=  sub1 (primitiva-sub1)

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

  ;(caracter
   ;((or letter "-") (arbno (or letter digit "-" ":"))) string)
  
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
    ;(expresion ("\'"caracter"\'") caracter-lit)
    (expresion (identificador) var-exp)
    (expresion ("var" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableMutable-exp)
    (expresion ("const" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableNoMutable-exp)
    (expresion ("rec" (arbno identificador "(" (separated-list identificador ";") ")" "=" expresion) "en" expresion "finRec") rec-exp)
    (expresion ("false") false-exp)
    (expresion ("true") true-exp)

    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("%") primitiva-residuo)
    (primitiva-binaria ("concat") primitiva-concat)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ))

;datatypes con SLLGEN
(sllgen:make-define-datatypes spec-lexica spec-gramatica);crear datatypes

(define show-the-datatypes;mostrar datatypes
  (lambda () (sllgen:list-define-datatypes spec-lexica spec-gramatica)))

;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser spec-lexica spec-gramatica))

;El Analizador Léxico (Scanner) -> genera los tokens

(define just-scan
  (sllgen:make-string-scanner spec-lexica spec-gramatica))

;**************************************************************

;Ambientes -> auxiliares

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env ambiente?))
  )

(define scheme-value? (lambda (v) #t))

;empty-env:
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;función que busca una variable dado un ambiente, si no esta retorna error, si esta retorna el valor de la variable
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable ~s no existe" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env sym))))
      )
    ))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;***************************************

;ambiente inicial
(define init-env
  (extend-env '(@a @b @c @d @e)
              '(1 2 3 "hola" "FLP")
  (empty-env)))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;datatype para los procedimientos
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa  pgm))
    (sllgen:make-stream-parser 
      spec-lexica
      spec-gramatica)))

;********************************

;evaluacion de funciones


;exp(program) -> numero o texto
;Proposito:Evalua un programa llamando a evaluar-expresion en su cuerpo con un entorno inicial y devuelve el resultado de evaluar la expresión del cuerpo del programa.
(define evaluar-programa
  (lambda (exp)
    (cases programa exp
      (un-programa (body)
                   (evaluar-expresion body init-env))
      )
    ))

;exp(expresion), env(ambiente) -> numero o texto
;Proposito: Evalua una expresion en el ambiente dado y retorna el resultado de la evaluación dada segun la especificacion gramatical
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (n) n)
      (var-exp (id) (buscar-variable env id))
      (texto-lit (text) text)
      (variableMutable-exp (ids exps cuerpo)
                         (let ((args(evaluar-operandos exps env)))
                           (evaluar-expresion cuerpo
                                  (extend-env ids args env))
                         ))
      (variableNoMutable-exp (ids exps cuerpo)
                         (let ((args(evaluar-operandos exps env)))
                           (evaluar-expresion cuerpo
                                  (extend-env ids args env))
                         ))
      (rec-exp (proc-nombres idss exps cuerpodecrec)
                  (evaluar-expresion cuerpodecrec
                                   (extend-env-recursively
                                     proc-nombres idss exps env)))
      (true-exp ()
                #t)
      (false-exp ()
                 #f)

      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let(
                            (operando1 (evaluar-expresion exp1 env))
                            (operando2 (evaluar-expresion exp2 env))
                            )
                         (evaluar-primitiva-binaria prim-binaria operando1 operando2)
                         ))
      (primapp-un-exp (prim-unaria exp)
                      (let (
                            (arg (evaluar-expresion exp env))
                            )
                           (evaluar-primitiva-unaria  prim-unaria arg)
                           ))
      )))

;prim(primitiva-binaria) rand1(texto o numero) rand2(texto o numero) -> texto o numero
;Proposito: Aplica la operacion binaria dada los dos operandos y retorna el resultado dde la operacion segun sea el caso que se aplique
(define evaluar-primitiva-binaria
  (lambda(prim rand1 rand2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ rand1 rand2))
      (primitiva-resta () (- rand1 rand2))
      (primitiva-div () (/ rand1 rand2))
      (primitiva-multi () (* rand1 rand2))
      (primitiva-residuo () (modulo rand1 rand2))
      (primitiva-concat () (string-append rand1 rand2))
      )))

;prim(primitiva-unaria) args(texto o numero) -> numero
;Proposito: Aplica la primitiva unaria al operando dado y retorna el resultado de la operacion segun sea el caso que se aplique
(define evaluar-primitiva-unaria
  (lambda(prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length args))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      )))


; funciones auxiliares para aplicar evaluar-expresion a cada elemento de una lista de operandos (expresiones)

;operandos, env (ambiente) -> list
;Proposito: recibe una lista de operandos y los manda a evaluar uno por uno a evaluar-operando. Retorna finalmente la union de cada operando ya evaluado
(define evaluar-operandos
  (lambda (operandos env)
    (map (lambda (x) (evaluar-operando x env)) operandos)))

;operando, env (ambiente) -> numero o texto
;Proposito: recibe cada uno de los operandos por separado de evaluar-operandos y los evalua en evaluar-expresion y retorna el valor de esa evaluación.
(define evaluar-operando
  (lambda (operando env)
    (evaluar-expresion operando env)))

;int, int -> numero
;proposito: Calcula el residuo de la division de los numeros a entre b
(define residuo
  (lambda(a b)
    (if (< a b)
        a
        (modulo (- a b) b))))