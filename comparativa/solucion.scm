; ========================================================
; FUNCIÓN: transicion_scheme
; NATURALEZA: Pura (Dado las vars color-actual cambiar-a, siempre va a devolver una lista, sin efectos secundarios)
; ESTRATEGIA: Es una Funcion de Selección Simple (Se basa en la funcion COND y predicados logicos)
; IMPACTO: No destructiva
; ========================================================
(define (transicion_scheme color-actual cambiar-a) ; Pocas diferecnias a Trancion-lisp
	(cond										   ; Se sigue utilizando una estructura cond para las comparaciones entre simbolos
		((and (eq? cambiar-a 'rojo) (eq? color-actual 'en-verde)) (list color-actual "cambiar-a-rojo")) ; Apesar de que se sigue usando el and
		((and (eq? cambiar-a 'verde) (eq? color-actual 'en-rojo)) (list color-actual "cambiar-a-verde")) ; la diferencia radica en eq? que es la funcion de comparacion de simbolos en scheme
		((and (eq? cambiar-a 'amarillo) (eq? color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo"))
		(else (list color-actual "accion-por-defecto")))) ; otra diferencia radica en el reemplazo de "t" por "else" en la accion por defecto

; ========================================================
;; FUNCION: timer
;; REQUERIMIENTO 2: Temporizador  version .scm
;; NATURALEZA: Pura (Dado un timestamp siempre retorna el mismo color)
;; ESTRATEGIA: Funcion Predicado / Logica Condicional
;; IMPACTO: No destructiva
;; ========================================================
(define (timer timestamp)                     ; Se utiliza 'define' en vez de 'defun' para declarar la funcion en Scheme
  (cond                                       ; Se mantiene la estructura condicional 'cond' para evaluar los rangos de tiempo
     ((< (modulo timestamp 216) 90) 'rojo)    ; La diferencia esta en la primitiva matematica, se usa 'modulo' en vez de 'mod' abreviado de Lisp
     ((< (modulo timestamp 216) 96) 'amarillo)
     (else 'verde)))                          ; Al igual que arriba, se reemplaza la 't' (True) de Lisp por la palabra 'else'
