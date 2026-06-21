;; requerimiento 1
;; ========================================================
;; FUNCION: transicion
;; NATURALEZA: Pura (Debido a LIST, siempre retornara una lista limpia, dejando los datos originales intactos)
;; ESTRATEGIA: Es una Funcion Condicional (Implementada mediante COND y Predicados Logicos)
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a) ; recibe dos parametros, el color actual del semaforo y el color al que se cambie
  (cond                                    ; una estructura condiconal para evaluar las transiciones validas entre colores
     ((and (equal cambiar-a 'verde) (equal color-actual 'en-rojo)) (list color-actual "cambiar-a-verde")) ; and para evaluar ambas condiciones y equal para comparar los simbolos
     ((and (equal cambiar-a 'amarillo) (equal color-actual 'en-verde)) (list color-actual "cambiar-a-amarillo")) ; de ser verdadero retorna una lista con el color actual y la accion a realizar
     ((and (equal cambiar-a 'rojo) (equal color-actual 'en-amarillo)) (list color-actual "cambiar-a-rojo"))
     ; para complementar se actualizo la funcion transicion para incluir los requerimientos de la iteracion2|extension1
     ((and (equal cambiar-a 'rojo-intermitente) (equal color-actual 'en-verde)) (list color-actual "rojo-intermitente")) ; esto funcionaria mejor con la ayuda del timer
     ((and (equal cambiar-a 'amarillo-intermitente) (equal color-actual 'en-rojo)) (list color-actual "amarillo-intermitente")) 
     ((and (equal cambiar-a 'verde-intermitente) (equal color-actual 'en-amarillo)) (list color-actual "verde-intermitente"))
     (t (list color-actual "accion-por-defecto")))) ; si todas las condicones son falsas, retorna una accion por defecto, claro que tambien
                                                    ; retornaria un color intermediario de ser necesario
;; ========================================================
;; Ejemplos de uso normal:
;; (transicion 'en-rojo 'verde) -->(EN-ROJO "cambiar-a-verde")
;; (transicion 'en-verde 'amarillo) -->(EN-VERDE "cambiar-a-amarillo")
;; (transicion 'en-amarillo 'rojo) -->(EN-AMARILLO "cambiar-a-rojo")
;; ========================================================
;; Uso de las intermitencias:
;; (transicion 'en-verde 'rojo-intermitente) -->(EN-VERDE "rojo-intermitente")
;; (transicion 'en-rojo 'amarillo-intermitente) -->(EN-ROJO "amarillo-intermitente")
;; (transicion 'en-amarillo 'verde-intermitente) -->(EN-AMARILLO "verde-intermitente")
;; ========================================================
;; Ejemplos casos de mal uso o de errores:
;; (transicion 'en-rojo 'amarillo) -->(EN-ROJO "accion-por-defecto")
;; (transicion 'en-amarillo 'rojo-intermitente) -->(EN-AMARILLO "accion-por-defecto")
;; (transicion 'en-rojo 'azul) -->(EN-ROJO "accion-por-defecto")
;; ========================================================


;requerimiento 2
;; ========================================================
;; FUNCION: temporizador
;; NATURALEZA: Pura
;; ESTRATEGIA: Función Condicional
;; IMPACTO: No destructiva
;; ========================================================

(defun temporizador (timestamp)
  (cond
    ;; ciclo rojo: 87s + 3s 
    ((< (mod timestamp 225) 87) 'rojo)
    ((< (mod timestamp 225) 90) 'rojo-intermitente)
    ;; ciclo verde: 117s + 3s 
    ((< (mod timestamp 225) 207) 'verde)
    ((< (mod timestamp 225) 210) 'verde-intermitente)
    ;; ciclo amarillo: 3s + 3s por descarte
    ((< (mod timestamp 225) 213) 'amarillo)
    (t 'amarillo-intermitente)))

;; ========================================================
;; Ejemplo de uso normal:
;; (temporizador 50) --> ROJO
;; (temporizador 100) --> VERDE
;; (temporizador 211) --> AMARILLO
;; ========================================================
;; Ejemplos con intermitencia:
;; (temporizador 88) --> ROJO-INTERMITENTE
;; (temporizador 208) --> VERDE-INTERMITENTE
;; (temporizador 224) --> AMARILLO-INTERMITENTE
;; ========================================================


;requerimiento 3 
;; ========================================================
;; FUNCION: informe
;; NATURALEZA: Impura
;; ESTRATEGIA: Escritura en archivo 
;; IMPACTO: Genera/modifica un archivo de texto
;; ========================================================

(ql:quickload :local-time)
(defun informe (datos)
  (with-open-file
      (stream "informe-ejecucion-semaforo.txt"
              :direction :output ;abre el archivo para escribir
              :if-exists :supersede) ;si ya existe, se reemplaza por completo

    (format stream "Informe de Ejecucion del Sistema Semaforico~%") ; escribe una linea en el archivo
    (format stream "==========================~%")

    (mapcar
      (lambda (x)
        (format stream "~a - Transicion ~A --> ~A~%"
          (local-time:format-timestring
            nil ;para que devuelva la fecha y hora como una cadena de texto, con el local-time:format-timestring
            (local-time:unix-to-timestamp (car x)) ;convierte el tiempo Unix a el formato fecha y hora
            :format '(:year "-" :month "-" :day
                      " "
                      :hour ":" :min ":" :sec))
          (cadr x) ;color anterior
          (caddr x))) ;color nuevo
      datos)

    (format stream "~%--- Fin del Informe ---")))
;; ========================================================
;; Ejemplo de uso normal:
;; (informe '((1717511415 en-rojo verde)               
;;    (1717511500 en-verde amarillo)             
;;    (1717511506 en-amarillo rojo)              
;;    (1717511600 en-verde rojo-intermitente)    
;;    (1717511700 en-rojo amarillo-intermitente) 
;;    (1717511800 en-amarillo verde-intermitente)
;;    (1717511900 en-rojo accion-por-defecto)    
;;   )) 
;; ========================================================


;requerimiento 4 (a-b)
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
				(t (+ (car lista-segundos) (cadr lista-segundos) (caddr lista-segundos) 9)))
		)
		(t (pprint "Error, el parametro recibido no es una lista o alguno de sus elementos no es un numero entero"))
	)
)

;; ========================================================
;; Ejemplo de uso normal:
;; (duracion-ciclo '(87 117 3)) --> 216
;; ========================================================
;; Ejemplo de casos de error:
;; (duracion-ciclo '(87 117)) --> "Error de parametro. La cantidad de numeros que representan a los segundos deben ser de 3 (rojo->amarillo->verde)"
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
;; (recomendacion-ciclo 30) --> "La duracion del ciclo esta por debajo del recomendado, debe ser mayor a los 35 segundos"
;; (recomendacion-ciclo 100) --> "La duracion del ciclo se encuentra dentro de los estandares de ingenieria de trafico"
;; (recomendacion-ciclo 200) --> "La duracion del ciclo esta por encima del recomendado, debe ser menor a los 150 segundos"
;; ========================================================
;; Ejemplo de casos de error:
;; (recomendacion-ciclo 'hola) --> "Error, el parametro recibido no es un numero"
;; (recomendacion-ciclo '(90)) --> "Error, el parametro recibido no es un numero"
;; ========================================================


;requerieminto 5
;; ========================================================
;; FUNCION: ciclos-por-tiempo-iteracion2
;; NATURALEZA: Pura 
;; ESTRATEGIA: Calculo Aritmetico (Implementada mediante operaciones matematicas y TRUNCATE)
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
   (if (numberp minutos)
       (truncate (/ (* minutos 60) 225)) ;suma los 9s(216, 225)
      "ingrese un dato valido"
   )
)

;; ========================================================
;; Ejemplo de uso normal:
;; (ciclos-por-tiempo 15) --> 4
;; (ciclos-por-tiempo 35) --> 9
;; (ciclos-por-tiempo 60) --> 16
;; ========================================================
;; Ejemplo de casos de error:
;; (ciclos-por-tiempo 'hola) --> "ingrese un dato valido"
;; (ciclos-por-tiempo '(15)) --> "ingrese un dato valido"
;; (ciclos-por-tiempo nil) --> "ingrese un dato valido"
;; ========================================================


;requeriminto 6
;; ========================================================
;;Funcion: distribucion-temporal
;;Naturaleza: impura
;;Estrategia de Control: Condicional no recursiva
;;Impacto en Memoria: No destructiva
;; ========================================================

(defun distribucion-temporal (regla-de-ciclo)
	(if (and (consp regla-de-ciclo) (= (length regla-de-ciclo) 3))
		(let ((total (reduce '+ regla-de-ciclo)))
			(list "Formato:  ROJO --> ROJO-INT --> VERDE --> VERDE-INT --> AMARILLO --> AMARILLO-INT"
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
;; (distribucion-temporal '(87 117 3)) --> ("Formato: ROJO --> ROJO-INT --> VERDE --> VERDE-INT --> AMARILLO --> AMARILLO-INT" ((42 %) (57 %) (1 %)))
;; ========================================================
;; Ejemplo de casos de error:
;; (distribucion-temporal '(87 117)) --> "El parametro recibido no cumple con alguna condicion: 1) Ser una lista 2) Tener tres elementos"
;; (distribucion-temporal 'hola) --> "El parametro recibido no cumple con alguna condicion: 1) Ser una lista 2) Tener tres elementos"
;; ========================================================
