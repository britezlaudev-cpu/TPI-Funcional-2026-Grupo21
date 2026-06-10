;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (color-actual cambiar-a)
  (cond
     ((and (equal cambiar-a 'rojo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-rojo")) 
     ((and (equal cambiar-a 'amarillo) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-amarillo")) 
     ((and (equal cambiar-a 'verde) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-verde"))
     (t (list color-actual "accion-por-defecto"))))
;; ========================================================
;; Ejemplo de uso normal: (transicion 'en-verde 'rojo) --> (EN-VERDE "cambiar-a-rojo")
;;                        (transicion 'en-rojo 'amarillo) --> (EN-ROJO "cambiar-a-amarillo")
;;                        (transicion 'en-amarillo 'verde) --> (EN-AMARILLO "cambiar-a-verde")
;; Ejemplo de casos de error: (transicion 'en-rojo 'verde) --> (EN-ROJO "accion-por-defecto")
;;                            (transicion 'en-verde 'amarillo) --> (EN-VERDE "accion-por-defecto")
;;                            (transicion 'en-amarillo 'rojo) --> (EN-AMARILLO "accion-por-defecto")
;;                            (transicion 'en-rojo 'azul) --> (EN-ROJO "accion-por-defecto")
;; ========================================================

;REQUERIMIENTO 5
;; ========================================================
;; FUNCION: ciclos-por-tiempo
;; NATURALEZA: Pura 
;; ESTRATEGIA: Calculo Aritmetico (Implementada mediante operaciones matematicas y TRUNCATE)
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
   (if (numberp minutos)
       (truncate (/ (* minutos 60) 216))
      "ingrese un dato valido"
   )
)

;; ========================================================
;; Ejemplo de uso normal:
;; (ciclos-por-tiempo 15) --> 4
;; (ciclos-por-tiempo 60) --> 16
;; (ciclos-por-tiempo 30) --> 8
;;
;; Ejemplo de casos de error:
;; (ciclos-por-tiempo 'hola) --> "ingrese un dato valido"
;; (ciclos-por-tiempo '(15)) --> "ingrese un dato valido"
;; (ciclos-por-tiempo nil) --> "ingrese un dato valido"
;; ========================================================


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




;; ========================================================
;; EJEMPLOS DE USO - Sistema de Auditoría
;; ========================================================

;; Caso normal:
;; (registrar-cambio 1000 'en-rojo 'en-verde)
;; Resultado esperado:
;; Tiempo 1000: la luz ha cambiado de EN-ROJO a EN-VERDE

;; Caso alternativo:
;; (registrar-cambio 1090 'en-verde 'en-amarillo)
;; Resultado esperado:
;; Tiempo 1090: la luz ha cambiado de EN-VERDE a EN-AMARILLO

;; Caso con error de sintaxis:
;; (registrar-cambio 1000 en-rojo 'en-verde)
;; Error esperado:
;; The variable EN-ROJO is unbound.
