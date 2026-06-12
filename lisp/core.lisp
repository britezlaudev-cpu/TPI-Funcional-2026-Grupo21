;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================
(defun transicion (color-actual cambiar-a) ; recibe dos parametros, el color actual del semaforo y el color al que se cambie
  (cond                                    ; una estructura condiconal para evaluar las transiciones validas entre colores
     ((and (equal cambiar-a 'rojo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-rojo")) ; and para evaluar ambas condiciones y equal para comparar los simbolos
     ((and (equal cambiar-a 'amarillo) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-amarillo")) ; de ser verdadero retorna una lista con el color actual y la accion a realizar
     ((and (equal cambiar-a 'verde) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-verde"))
     ((and (equal cambiar-a 'rojo-intermitente) (equal color-actual 'en-verde)) (list color-actual "rojo-intermitente")) 
     ((and (equal cambiar-a 'amarillo-intermitente) (equal color-actual 'en-rojo)) (list color-actual "amarillo-intermitente")) 
     ((and (equal cambiar-a 'verde-intermitente) (equal color-actual 'en-amarillo)) (list color-actual "verde-intermitente"))
     (t (list color-actual "accion-por-defecto")))) ; si todas las condicones son falsas, retorna una accion por defecto

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

;;-------------------------------------------------
;;Funcion: duracion-ciclo
;;Naturaleza: Impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;;-------------------------------------------------
(defun duracion-ciclo (lista-segundos)
	(cond
		((and (consp lista-segundos) (integerp (car lista-segundos)) (integerp (cadr lista-segundos)) (integerp (caddr lista-segundos)))
			(cond
				((or (> (length lista-segundos) 3) (< (length lista-segundos) 3))
					(pprint "Error de parametro. La cantidad de numeros que representan a los segundos deben ser de 3 (rojo->amarillo->verde)"))
				(t (+ (car lista-segundos) (cadr lista-segundos) (caddr lista-segundos) 9)))
		)
		(t (pprint "Error, el parametro recibido no es una lista o alguno de sus elementos no es un numero entero"))
	)
)

;;--------------------------------------------------
;;Funcion: recomendacion-ciclo
;;Naturaleza: Impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;;--------------------------------------------------
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

;;--------------------------------------------------
;;Funcion: distribucion-temporal
;;Naturaleza: Impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;;--------------------------------------------------
(defun distribucion-temporal (regla-de-ciclo)
	(if (and (consp regla-de-ciclo) (= (length regla-de-ciclo) 3))
		(let ((total (reduce '+ regla-de-ciclo)))
			(list "Formato:  ROJO --> ROJO-INT --> AMARILLO --> AMARILLO-INT --> VERDE --> VERDE-INT"
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