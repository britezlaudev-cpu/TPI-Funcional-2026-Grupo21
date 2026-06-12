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