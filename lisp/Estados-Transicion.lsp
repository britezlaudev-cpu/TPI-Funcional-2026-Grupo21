;; ========================================================
;; FUNCIoN: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (color-actual cambiar-a) ; recibe dos parametros, el color actual del semaforo y el color al que se cambie
  (cond                                    ; una estructura condiconal para evaluar las transiciones validas entre colores
     ((and (equal cambiar-a 'rojo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-rojo")) ; and para evaluar ambas condiciones y equal para comparar los simbolos
     ((and (equal cambiar-a 'verde) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-verde")) ; de ser verdadero retorna una lista con el color actual y la accion a realizar
     ((and (equal cambiar-a 'amarillo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo"))
     (t (list color-actual "accion-por-defecto")))) ; si todas las condicones son falsas, retorna una accion por defecto