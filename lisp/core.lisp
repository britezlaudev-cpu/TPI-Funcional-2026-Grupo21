;(en-rojo -> verde)
;(en-verde -> amarillo)
;(en-amarillo -> rojo)

;REQUERIMIENTO 1
;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a) ; recibe dos parametros, el color actual del semaforo y el color al que se cambie
  (cond  								   ; una estructura condiconal para evaluar las transiciones validas entre colores
	((and (equal cambiar-a 'verde) (equal color-actual 'en-rojo)); and para evaluar ambas condiciones y equal para comparar los simbolos
        (list color-actual "cambiar-a-verde")); de ser verdadero retorna una lista con el color actual y la accion a realizar
	((and (equal cambiar-a 'amarillo) (equal color-actual 'en-verde))
  	    (list color-actual "cambiar-a-amarillo"))
	((and (equal cambiar-a 'rojo) (equal color-actual 'en-amarillo))
  		(list color-actual "cambiar-a-rojo"))
	(t (list color-actual "accion-por-defecto")) ; si todas las condicones son falsas, retorna una accion por defecto
   )
)

;; ========================================================
;; ;; Ejemplo de uso normal:
;; (transicion 'en-rojo 'verde) --> (EN-ROJO "cambiar-a-verde")
;; (transicion 'en-verde 'amarillo) --> (EN-VERDE "cambiar-a-amarillo")
;; (transicion 'en-amarillo 'rojo) --> (EN-AMARILLO "cambiar-a-rojo")
;;
;; Ejemplo de casos de error:
;; (transicion 'en-rojo 'amarillo) --> (EN-ROJO "accion-por-defecto")
;; (transicion 'en-verde 'rojo) --> (EN-VERDE "accion-por-defecto")
;; (transicion 'en-amarillo 'verde) --> (EN-AMARILLO "accion-por-defecto")
;; (transicion 'en-rojo 'azul) --> (EN-ROJO "accion-por-defecto")
;; ========================================================

;REQUERIMIENTO 2
;;=========================================================
;; FUNCION: timer
;; NATURALEZA: Pura
;; ESTRATEGIA: Función Condicional
;; IMPACTO: No destructiva
;;=========================================================

(defun timers (timestamp)
  (cond
    ((< (mod timestamp 216) 90) 'rojo)
    ((< (mod timestamp 216) 96) 'amarillo)
    (t 'verde)))

;; ========================================================
;; Ejemplo de uso normal:
;; (timers 0) --> ROJO          (Inicio del ciclo)
;; (timers 92) --> AMARILLO     (Entra en los 6 segundos de transición)
;; (timers 150) --> VERDE       (Supera los 96 segundos iniciales)
;; (timers 216) --> ROJO        (Se cumple un ciclo exacto y reinicia)
;;
;; Ejemplo de casos de error:
;; (timers "hola") --> Error: The value "hola" is not of type NUMBER.
;; (timers 'veintidos) --> Error: The value VEINTIDOS is not of type NUMBER.
;; ========================================================

;REQUERIMIENTO 3
;; ========================================================
;; FUNCION: registrar-cambio
;; NATURALEZA: Impura, porque imprime información en pantalla
;; ESTRATEGIA: Función simple
;; IMPACTO: No destructiva, no modifica datos existentes
;; ========================================================

(ql:quickload :local-time)
(defun registrar-cambio (epoch color-anterior color-nuevo)
  (format t "Horario del cambio: [~a], la luz ha cambiado de ~A a ~A~%" 
            (local-time:format-timestring nil (local-time:unix-to-timestamp epoch) 
                                              :format '(:year "-" :month "-" :day " " :hour ":" :min ":" :sec)) 
            color-anterior color-nuevo))

;; ========================================================
;; EJEMPLOS DE USO - Sistema de Auditoría
;; ========================================================

;; Caso normal:
;; (registrar-cambio 1000 'en-rojo 'en-verde)
;; Resultado esperado:
;; Horario del cambio: [1970-01-01 00:16:40], la luz ha cambiado de EN-ROJO a EN-VERDE
;; Caso alternativo:
;; (registrar-cambio 1090 'en-verde 'en-amarillo)
;; Resultado esperado:
;; Horario del cambio: [1970-01-01 00:18:10], la luz ha cambiado de EN-VERDE a EN-AMARILLO
;; Caso con error de sintaxis:
;; (registrar-cambio 1000 en-rojo 'en-verde)
;; Error esperado:
;; The variable EN-ROJO is unbound.


;REQUERIMIENTO 4 (A-B)
;; ========================================================
;;FUNCION: duracion-ciclo
;;NATURALEZA: Impura
;;ESTRATEGIA: Condicional no recursiva
;;IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo (lista-segundos)
	(cond
		((and (consp lista-segundos) (integerp (car lista-segundos)) (integerp (cadr lista-segundos)) (integerp (caddr lista-segundos)))
			(cond
				((or (> (length lista-segundos) 3) (< (length lista-segundos) 3))
					(pprint "Error de parametro. La cantidad de numeros que representan a los segundos deben ser de 3 (rojo->amarillo->verde)"))
				(t (+ (car lista-segundos) (cadr lista-segundos) (caddr lista-segundos))))
		)
		(t (pprint "Error, el parametro recibido no es una lista o alguno de sus elementos no es un numero entero"))
	)
)

;; ========================================================
;; Ejemplo de uso normal:
;; (duracion-ciclo '(90 6 120)) --> 216
;; (duracion-ciclo '(60 5 90)) --> 155
;;
;; Ejemplo de casos de error:
;; (duracion-ciclo '(90 6)) --> "Error de parametro..."
;; (duracion-ciclo '(90 6 120 10)) --> "Error de parametro..."
;; (duracion-ciclo '(90 a 120)) --> "Error, el parametro recibido no es una lista o alguno de sus elementos no es un numero entero"
;; (duracion-ciclo 'hola) --> "Error, el parametro recibido no es una lista o alguno de sus elementos no es un numero entero"
;; ========================================================



;; ========================================================
;;FUNCION: recomendacion-ciclo
;;NATURALEZA: Impura
;;ESTATEGIA: Condicional no recursiva
;;IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion-ciclo-segs)
	(cond
		((integerp duracion-ciclo-segs)
			(cond
				((< duracion-ciclo-segs 35) (pprint "La duracion del ciclo esta por debajo del recomendado, debe ser mayor a los 35 segundos"))
				((> duracion-ciclo-segs 150) (pprint "La duracion del ciclo esta por encima del recomendado, debe ser menor a los 150 segundos"))
				(t (pprint "La duracion del ciclo se encuentra dentro de los estandares de ingenieria de trafico"))
			)
		)
		(t (pprint "Error, el parametro recibido no es un numero"))
	)
)

;; ========================================================
;; Ejemplo de uso normal:
;; (recomendacion-ciclo 30)
;; --> "La duracion del ciclo esta por debajo del recomendado, debe ser mayor a los 35 segundos"
;;
;; (recomendacion-ciclo 100)
;; --> "La duracion del ciclo se encuentra dentro de los estandares de ingenieria de trafico"
;;
;; (recomendacion-ciclo 200)
;; --> "La duracion del ciclo esta por encima del recomendado, debe ser menor a los 150 segundos"
;;
;; Ejemplo de casos de error:
;; (recomendacion-ciclo 'hola)
;; --> "Error, el parametro recibido no es un numero"
;;
;; (recomendacion-ciclo '(90))
;; --> "Error, el parametro recibido no es un numero"
;; ========================================================

;REQUERIMIENTO 5
;; ========================================================
;;FUNCION; ciclos-por-tiempo
;;NATURALEZA; Pura
;;ESTRATEGIA: Calculo Aritmetico
;;IMPACTO: No destructiva
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


;REQUERIMIENTO 6
;; ========================================================
;;Funcion: distribucion-temporal
;;Naturaleza: impura (Ademas de calcular y devolver un valor, utiliza "pprint" para mostrar mensajes al usuario)
;;Estrategia de Control: Condicional no recursiva (Emplea la funcion "if" para validar el parámetro, y no realiza llamadas recursivas a sí misma)
;;Impacto en Memoria: No destructiva (No modifica la lista recibida como parámetro sino que crea nuevas listas mediante "list" y "mapcar")
;; ========================================================

(defun distribucion-temporal (regla-de-ciclo)
	(if (and (consp regla-de-ciclo) (= (length regla-de-ciclo) 3))
		(let ((total (reduce '+ regla-de-ciclo)))
			(list "Formato: ROJO --> AMARILLO --> VERDE"
				(mapcar (lambda (regla-de-ciclo)
							(list (round (/ (* regla-de-ciclo 100) total)) '%)
						)
					regla-de-ciclo
				)
			)
		)
		(pprint "El parametro recibido no cumple con alguna condicion: 
			1) Ser una lista 2) Tener tres elementos")
	)
)

;; ========================================================
;; Ejemplo de uso normal:
;;
;; (distribucion-temporal '(90 6 120))
;; --> ("Formato: ROJO --> AMARILLO --> VERDE"
;;      ((42 %) (3 %) (56 %)))
;;
;; (distribucion-temporal '(60 30 60))
;; --> ("Formato: ROJO --> AMARILLO --> VERDE"
;;      ((40 %) (20 %) (40 %)))
;;
;; Ejemplo de casos de error:
;;
;; (distribucion-temporal '(90 6))
;; --> "El parametro recibido no cumple con alguna condicion..."
;;
;; (distribucion-temporal '(90 6 120 10))
;; --> "El parametro recibido no cumple con alguna condicion..."
;;
;; (distribucion-temporal 'hola)
;; --> "El parametro recibido no cumple con alguna condicion..."
;;
;; (distribucion-temporal nil)
;; --> "El parametro recibido no cumple con alguna condicion..."
;; ========================================================
