;;===================================
;;FUNCION; ciclos-por-tiempo
;;NATURALEZA; pura
;;ESTRATEGIA: calculo
;;IMPACTO: no destructiva
;;===================================

(defun ciclos-por-tiempo (minutos)
	(if (numberp minutos)
		(truncate (/ (* minutos 60) 216))
		"ingrese un dato valido"
	)
)