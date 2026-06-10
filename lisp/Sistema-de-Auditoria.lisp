


;; ========================================================
;; FUNCIÓN: registrar-cambio
;; NATURALEZA: Impura, porque imprime información en pantalla.
;; ESTRATEGIA: Función simple.
;; IMPACTO: No destructiva, no modifica datos existentes.
;; ========================================================

(defun registrar-cambio (epoch color-anterior color-nuevo)
  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          epoch
          color-anterior
          color-nuevo))
