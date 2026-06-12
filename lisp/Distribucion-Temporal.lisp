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