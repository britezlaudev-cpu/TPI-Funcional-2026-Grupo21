;; ========================================================
;; FUNCIÓN: registrar-cambio
;; NATURALEZA: Impura, porque imprime información en pantalla.
;; ESTRATEGIA: Función simple.
;; IMPACTO: No destructiva, no modifica datos existentes.
;; ========================================================
(ql:quickload :local-time)
(defun registrar-cambio (epoch color-anterior color-nuevo)
  (format t "Horario del cambio: [~a], la luz ha cambiado de ~A a ~A~%" 
            (local-time:format-timestring nil (local-time:unix-to-timestamp epoch) 
                                              :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)) 
            color-anterior color-nuevo))

