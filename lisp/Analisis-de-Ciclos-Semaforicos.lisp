  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49 (2010-07-07) <http://clisp.cons.org/>

Copyright (c) Bruno Haible, Michael Stoll 1992, 1993
Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
Copyright (c) Bruno Haible, Sam Steingold 1999-2000
Copyright (c) Sam Steingold, Bruno Haible 2001-2010

Type :h and hit Enter for context help.

[1]> ;;--------------------------------------------
;;Funcion: duracion-ciclo
;;Naturaleza: Impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;;--------------------------------------------------
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

DURACION-CICLO
[2]> ;;--------------------------------------------
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

RECOMENDACION-CICLO
[3]> 