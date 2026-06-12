; ========================================================
; FUNCIÓN: transicion_scheme
; NATURALEZA: Pura (Dado las vars color-actual cambiar-a, siempre va a devolver una lista, sin efectos secundarios)
; ESTRATEGIA: Es una Funcion de Selección Simple (Se basa en la funcion COND y predicados logicos)
; IMPACTO: No destructiva
; ========================================================
(define (transicion_scheme color-actual cambiar-a) ; Pocas diferecnias a Trancion-lisp
	(cond										   ; Se sigue utilizando una estructura cond para las comparaciones entre simbolos
		((and (eq? cambiar-a 'verde) (eq? color-actual 'en-rojo)) (list color-actual "cambiar-a-verde")) ; Apesar de que se sigue usando el and
		((and (eq? cambiar-a 'amarillo) (eq? color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo")) ; la diferencia radica en eq? que es la funcion de comparacion de simbolos en scheme
		((and (eq? cambiar-a 'rojo) (eq? color-actual 'en-amarillo)) (list color-actual "cambiar-a-rojo"))
		(else (list color-actual "accion-por-defecto")))) ; otra diferencia radica en el reemplazo de "t" por "else" en la accion por defecto

; ========================================================
;; FUNCIÓN: timer.scm
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO (VERSIÓN SCHEME)
;; NATURALEZA: Pura (Dado un timestamp siempre retorna el mismo color)
;; ESTRATEGIA: Función Predicado / Lógica Condicional
;; IMPACTO: No destructiva
;; ========================================================
(define (timer timestamp)
  (cond
    ((< (modulo timestamp 216) 90) 'rojo)
    ((< (modulo timestamp 216) 96) 'amarillo)
    (else 'verde)))