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
;;Funcion: distribucion-temporal
;;Naturaleza: Impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;;--------------------------------------------------
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

DISTRIBUCION-TEMPORAL
[2]> 