
;; REQUERIMIENTO 2: TEMPORIZADOR AUTOMÁTICO (VERSIÓN SCHEME)
;; NATURALEZA: Pura (Dado un timestamp siempre retorna el mismo color)
;; ESTRATEGIA: Función Predicado / Lógica Condicional
;; IMPACTO: No destructiva

(define (timer timestamp)
  (cond
    ((< (modulo timestamp 216) 90) 'rojo)
    ((< (modulo timestamp 216) 96) 'amarillo)
    (else 'verde)))