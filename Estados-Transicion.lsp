(defun transicion (color-actual cambiar-a)
	(cond
		((and (equal cambiar-a 'rojo) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-rojo")) 
		((and (equal cambiar-a 'amarillo) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-amarillo")) 
		((and (equal cambiar-a 'verde) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-verde"))
		(t (list color-actual "accion-por-defecto"))))

