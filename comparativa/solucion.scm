; ========================================================
; FUNCIÓN: transicion_scheme
; NATURALEZA: Pura (Dado las vars color-actual cambiar-a, siempre va a devolver una lista, sin efectos secundarios)
; ESTRATEGIA: Es una Funcion de Selección Simple (Se basa en la funcion COND y predicados logicos)
; IMPACTO: No destructiva
; ========================================================
(define (transicion_scheme color-actual cambiar-a)
	(cond
		((and (eq? cambiar-a 'verde) (eq? color-actual 'en-rojo)) (list color-actual "cambiar-a-verde"))
		((and (eq? cambiar-a 'amarillo) (eq? color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo"))
		((and (eq? cambiar-a 'rojo) (eq? color-actual 'en-amarillo)) (list color-actual "cambiar-a-rojo"))
		(else (list color-actual "accion-por-defecto"))))