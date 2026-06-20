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
		(else (list color-actual "accion-por-defecto")) ; otra diferencia radica en el reemplazo de "t" por "else" en la accion por defecto
		; de la misma manera que funcion transicion, aqui actualizaremos para las intermitencias en lenguaje scheme
		((and (eq? cambiar-a 'rojo-intermitente) (eq? color-actual 'en-verde)) (list color-actual "rojo-intermitente")) ; las diferencias en esta actualizacion
		((and (eq? cambiar-a 'amarillo-intermitente) (eq? color-actual 'en-rojo)) (list color-actual "amarillo-intermitente")) ; a simple vista son nulas
		((and (eq? cambiar-a 'verde-intermitente) (eq? color-actual 'en-amarillo)) (list color-actual "verde-intermitente")))) ; pues vemos grandes similitudes
;requerimiento 2
;;=========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Función Condicional
;; IMPACTO: No destructiva
;;=========================================================

(define (timer timestamp)                               ; Se usa "define" en vez de "defun" para declarar la funcion en Scheme.
  (cond                                                 ; Se mantiene la estructura condicional 'cond' para evaluar los rangos de tiempo
     ;; ciclo rojo: 87s fijo + 3s
     ((< (modulo timestamp 225) 87) 'rojo)              ; Se usa la primitiva "modulo" en vez de "mod" de Lisp. 
     ((< (modulo timestamp 225) 90) 'rojo-intermitente)
     
     ;; ciclo verde: 117s fijo + 3s
     ((< (modulo timestamp 225) 207) 'verde)
     ((< (modulo timestamp 225) 210) 'verde-intermitente)
     
     ;; ciclo amarillo: 3s fijo + 3s por descarte
     ((< (modulo timestamp 225) 213) 'amarillo)
     (else 'amarillo-intermitente)))                    ; Se reemplaza la "t" de Lisp por la palabra "else" propia de Scheme
