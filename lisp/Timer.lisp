
;; Nombre de la funcion: timer
;; Su naturaleza: Pura
;; Estrategia: Función Predicado / logica Condicional
;; Impacto: No destructiva

(defun timer (timestamp)
  (cond
    ((< (mod timestamp 216) 90) 'rojo)
    ((< (mod timestamp 216) 96) 'amarillo)
    (t 'verde)))