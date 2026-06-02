;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (color-actual cambiar-a)
  (cond
     ((and (equal cambiar-a 'verde) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-verde")) 
     ((and (equal cambiar-a 'amarillo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo")) 
     ((and (equal cambiar-a 'rojo) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-rojo"))
     (t (list color-actual "accion-por-defecto"))))