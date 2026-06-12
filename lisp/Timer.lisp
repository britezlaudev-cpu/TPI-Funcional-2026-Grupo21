
;; Nombre de la funcion: timer
;; Su naturaleza: Pura
;; Estrategia: Función Predicado / logica Condicional
;; Impacto: No destructiva

(defun timers (timestamp)
  (cond
    ((< (mod timestamp 216) 87) 'rojo)
    ((< (mod timestamp 216) 90) 'rojo-intermitente)
    ((< (mod timestamp 216) 93) 'amarillo)
    ((< (mod timestamp 216) 96) 'amarillo-intermitente)
    ((< (mod timestamp 216) 213) 'verde)
    (t 'verde-intermitente)))