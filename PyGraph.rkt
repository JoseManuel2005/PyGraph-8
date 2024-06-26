#lang eopl

;Proyecto del curso de FLP
;URL del repositorio: https://github.com/JoseManuel2005/Proyecto-FLP

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

;                := rec {<identificador> (<identificador>* (;)) = <expresion>}* en <expresion>
;                    rec-exp (proc-nombres idss exps cuerpodecrec)

;                := begin {<expresion>}+(;) end
;                    begin-exp (exp exps)

;                := set <identificador> = <expresion>
;                    <set-exp (id rhsexp)>

;                := Si <expresion> entonces <expresion>  sino <expresion> 
;                    condicional-exp (test-exp true-exp false-exp)

;                := procedimiento (<identificador>*',') { <expresion> }
;                   procedimiento-exp (ids cuerpo)

;                := evaluar expresion (expresion ",")* finEval
;                   app-exp(exp exps)

;                := (<expresion> <primitiva-binaria> <expresion>)
;                   primapp-bin-exp (exp1 prim-binaria exp2)

;                := <primitiva-unaria> (<expresion>)
;                   primapp-un-exp (prim-unaria exp)

;                := lista [{<expresion>}* (,)]
;                := lista-exp (exp)

;                := vector [{<expresion>}* (,)]
;                := vector-exp (exp)

;                := registro { {<identificador>} = <expresion>}+ (;)}
;                := registro-exp (id exp)

;                := ref-vector ( <expresion> , <expresion> )
;                := set-vector-exp (vec pos)

;                := set-vector ( <expresion> , <expresion> , <expresion> )
;                := set-vector-exp (vec pos val)

;                := <predicado-primitivo> ( <expresion> , <expresion>)
;                := pred-prim-exp (exp1 exp2)

;                := <operacion-booleana> ( <expresion> , <expresion> )
;                := oper−bin−bool (exp1 exp2)

;                := <operacion-unaria-booleana> (<expresion>)
;                := oper−un−bool (exp)

;                := print ( <Expresion> )
;                := print-exp (exp)

;    <primitiva-binaria> :=  + (primitiva-suma)
;                        :=  ~ (primitiva-resta)
;                        :=  / (primitiva-div)
;                        :=  * (primitiva-multi)
;                        :=  % (primitiva-residuo)
;                        :=  concat (primitiva-concat)
;                        :=  append (primitiva-append)

;    <primitiva-unaria>  :=  longitud (primitiva-longitud)
;                        :=  add1 (primitiva-add1)
;                        :=  sub1 (primitiva-sub1)

;    <predicado-primitivo> := < (menor-que)
;                          := > (mayor-que)
;                          := <= (menorIgual-que)
;                          := >= (mayorIgual-que)
;                          := == (igual-que)
;                          := <> (diferente-de)

;    <operacion-booleana> := and (and-exp)
;                         := or (or-exp)

;    <operacion-unaria-booleana> := not(not-exp)
;                                := zero? (zero-exp)
;                                := vacio? (vacio-validacion-exp)
;                                := lista? (lista-validacion-exp)
;                                := cabeza (cabeza-validacion-exp)
;                                := resto (resto-validacion-exp)
;                                := vector? (vector-validacion-exp)

;******************************************************************************************

;Especificación Léxica

(define spec-lexica
  '((white-sp
     (whitespace) skip)
  
    (comentario
     ("//" (arbno (not #\newline))) skip)

    (identificador
     ("@" letter (arbno (or letter digit "?"))) symbol)
  
    (texto
     ((or letter "-") (arbno (or letter digit "-" ":" "?"))) string)

    (caracter
     ("'" letter) symbol)
  
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
    (expresion (caracter) caracter-lit)
    (expresion (identificador) var-exp)

    (expresion ("var" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableMutable-exp)
    (expresion ("const" "(" (separated-list identificador "=" expresion ",") ")" "{" expresion "}") variableNoMutable-exp)

    (expresion ("rec" (arbno identificador "(" (separated-list identificador ";") ")" "=" expresion) "en" expresion "finRec") rec-exp)

    (expresion ("begin" "{" expresion (arbno ";" expresion) "}" "end")  begin-exp)
    (expresion ("set" identificador "=" expresion)set-exp)
    
    (expresion ("si" expresion "entonces" expresion "sino" expresion) condicional-exp)
    (expresion ("procedimiento" "(" (separated-list identificador ",") ")" "{" expresion "}") procedimiento-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)


    (expresion ("false") false-exp)
    (expresion ("true") true-exp)
    (expresion ("vacio") vacio-exp)

    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    (expresion ("lista" "[" (separated-list expresion ",") "]") lista-exp)
    (expresion ("vector" "[" (separated-list expresion ",") "]") vector-exp)
    (expresion ("registro" "{" identificador "=" expresion (arbno ";" identificador "=" expresion)  "}") registro-exp)

    (expresion ("ref-vector" "(" expresion "," expresion ")") ref-vector-exp)
    (expresion ("set-vector" "(" expresion "," expresion "," expresion ")") set-vector-exp)

    (expresion ("while" "(" expresion ")" "do" expresion "end") while-exp)

    (expresion(predicado-primitivo "(" expresion "," expresion ")") pred-prim-exp)
    (expresion(operacion-booleana "(" expresion "," expresion ")") oper−bin−bool)
    (expresion(operacion-unaria-booleana "(" expresion ")") oper−un−bool)

    (expresion ("print" "(" expresion ")") print-exp)

    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("%") primitiva-residuo)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria ("append") primitiva-append)

    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)

    (predicado-primitivo ("<") menor-que)
    (predicado-primitivo (">") mayor-que)
    (predicado-primitivo ("<=") menorIgual-que)
    (predicado-primitivo (">=") mayorIgual-que)
    (predicado-primitivo ("==") igual-que)
    (predicado-primitivo ("<>") diferente-de)

    (operacion-booleana("and") and-exp)
    (operacion-booleana("or") or-exp)
    
    (operacion-unaria-booleana("not") not-exp)
    (operacion-unaria-booleana ("zero?") zero-exp)
    (operacion-unaria-booleana ("vacio?") vacio-validacion-exp)
    (operacion-unaria-booleana ("lista?") lista-validacion-exp)
    (operacion-unaria-booleana ("cabeza") cabeza-validacion-exp)
    (operacion-unaria-booleana ("resto") resto-validacion-exp)
    (operacion-unaria-booleana ("vector?") vector-validacion-exp)
    ))

;;;;;;;;;;;interpretador;;;;;;;;;;;;;;

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

;Ambientes

;definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vec vector?)
                       (env ambiente?))
  (extended-env-const (syms (list-of symbol?))
                      (val vector?)
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
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-const
  (lambda (syms vals env)
    (extended-env-const syms (list->vector vals) env)))

;función que busca una variable dado un ambiente, si no esta retorna error, si esta retorna el valor de la variable
(define buscar-variable
  (lambda (env sym)
    (deref (buscar-variable-ref env sym))))

(define buscar-variable-ref
  (lambda (env sym)
    (cases ambiente env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable ~s no existe" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (buscar-variable-ref old-env sym))))
      (extended-env-const (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-const pos vals)
              (buscar-variable-ref env sym))))
      )
    ))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (cerradura ids body env)))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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

;*******************************************************************************************
;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?))
  (a-const (position integer?)
           (vec vector?))
  )

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos))
      (a-const (pos vec)
               (vector-ref vec pos))
      )))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val))
      (a-const (pos vec)
        (eopl:error 'setref! "No se puede cambiar el valor de una constante"))
      )))

;***************************************

;ambiente inicial
(define init-env
  (extend-env '(@a @b @c @d @e)
              '(1 2 3 "hola" "FLP")
              (empty-env)))

;valor-verdad?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define valor-verdad?
  (lambda (x)
    (equal? (not #f) x) 
    )
  )

;datatype para los procedimientos
(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))


;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body env)
                 (evaluar-expresion body (extend-env ids args env))))))

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
      (caracter-lit (car) car)
      (variableMutable-exp (ids exps cuerpo)
                           (let ((args(evaluar-operandos exps env)))
                             (evaluar-expresion cuerpo
                                                (extend-env ids args env))
                             ))
      (variableNoMutable-exp (ids exps cuerpo)
                             (let ((args(evaluar-operandos exps env)))
                               (evaluar-expresion cuerpo
                                                  (extend-env-const ids args env))
                               ))
      (rec-exp (proc-nombres idss exps cuerpodecrec)
               (evaluar-expresion cuerpodecrec
                                  (extend-env-recursively
                                   proc-nombres idss exps env)))
      (begin-exp (exp exps) 
                 (let loop ((acc (evaluar-expresion exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (evaluar-expresion (car exps) 
                                                env)
                             (cdr exps)))))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (buscar-variable-ref env id)
                  (evaluar-expresion rhs-exp env))
                 'seteo-confirmado))
      (condicional-exp (test-exp true-exp false-exp)
                       (if (valor-verdad? (evaluar-expresion test-exp env))
                           (evaluar-expresion true-exp env)
                           (evaluar-expresion false-exp env)))
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo env))
      (app-exp (exp exps)
               (let ((proc (evaluar-expresion exp env))
                     (args (evaluar-operandos exps env)))
                 (if (procVal? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))

      (true-exp ()
                #t)
      (false-exp ()
                 #f)
      (vacio-exp ()
                 '()
                 )

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

      (lista-exp (exp)
                 (map(lambda(expr) (evaluar-expresion expr env)) exp)
                 )

      (vector-exp (exp)
                  (list->vector(map(lambda(expr) (evaluar-expresion expr env)) exp))
                  )

      (registro-exp (id exp ids exps)
                    exp
                    )

      (ref-vector-exp (vec pos)
                      (let ((vector (evaluar-expresion vec env))
                            (posicion (evaluar-expresion pos env)))
                        (vector-ref vector posicion))
                      )

      (set-vector-exp (vec pos val)
                      (let ((vector (evaluar-expresion vec env))
                            (posicion (evaluar-expresion pos env))
                            (valor (evaluar-expresion val env))
                            )
                        (vector-set! vector posicion valor)
                        vector
                        )
                      )
      
      (while-exp (test-exp body) 
                 (let loop ((condicion test-exp)
                            (expr body))
                   (if (evaluar-expresion condicion env) 
                       (loop condicion (evaluar-expresion body env))
                       'listo)
                   )
                 )

      (pred-prim-exp(prim exp1 exp2)
                    (let(
                         (expr1 (evaluar-expresion exp1 env))
                         (expr2 (evaluar-expresion exp2 env))
                         )
                      (evaluar-predicado-primitivo prim expr1 expr2)
                      ))

      (oper−bin−bool(operacion exp1 exp2)
                    (evaluar-operacion-booleana operacion (evaluar-expresion exp1 env) (evaluar-expresion exp2 env))
                    )

      (oper−un−bool(oper exp)
                   (evaluar-operacion-unaria-booleana oper (evaluar-expresion exp env))
                   )

      (print-exp(exp)
                (let(
                     (expr(evaluar-expresion exp env))
                     )
                  (display expr)
                  (newline)
                  )
                )
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
      (primitiva-residuo () (remainder rand1 rand2))
      (primitiva-concat () (string-append rand1 rand2))
      (primitiva-append() (if (list? rand1)
                              (append rand1 (list rand2))
                              (list rand1 rand2)
                              ))
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

;prim(predicado-primitivo) exp1(numero) exp2(numero) -> bool
;Proposito: Aplica el predicado primitivo al operando dado y retorna el resultado de la operacion segun sea el caso que se aplique
(define evaluar-predicado-primitivo
  (lambda(prim exp1 exp2)
    (cases predicado-primitivo prim
      (menor-que () (< exp1 exp2))
      (mayor-que () (> exp1 exp2))
      (menorIgual-que () (<= exp1 exp2))
      (mayorIgual-que () (>= exp1 exp2))
      (igual-que () (equal? exp1 exp2))
      (diferente-de () (not(equal? exp1 exp2)))
      )))

;ope(operacion-booleana) exp1(expresion) exp2(expresion) -> bool
;Proposito: Aplica la operacion and/or depende el caso segun lo que se pase en la exp1 y exp2
(define evaluar-operacion-booleana
  (lambda(ope exp1 exp2)
    (cases operacion-booleana ope
      (and-exp () (and exp1 exp2))
      (or-exp () (or exp1 exp2))
      )))

;ope(operacion-booleana) exp(expresion) -> bool
;Proposito: Aplica la operacion not (negacion) a lo que sea que retorne exp ya evaluado
(define evaluar-operacion-unaria-booleana
  (lambda(ope exp)
    (cases operacion-unaria-booleana ope
      (not-exp () (not exp))
      (zero-exp() (zero? exp))
      (vacio-validacion-exp() (null? exp))
      (lista-validacion-exp() (list? exp))
      (cabeza-validacion-exp() (car exp))
      (resto-validacion-exp() (cdr exp))
      (vector-validacion-exp() (vector? exp))
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
