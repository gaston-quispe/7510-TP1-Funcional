(ns logical-interpreter)
(require '[clojure.string :as str])

;;;;;;;;;;;;;;;;;;PARSER;;;;;;;;;;;;;;;;;;
;Nombre de regla/definicion: Solo letras en minusculas
;Variables: Solo letras mayusculas
;"  hija    (   X   ,    Y   )  :-   mujer   (  X  )   ,    hija    (    X  ,    Y  )      "
(defn texto-regla-es-valido [texto-regla]
	(re-matches #"^\s*[a-z_]+\s*\((( *[A-Z]+ *),)*(( *[A-Z]+ *))\)\s*:-\s*([a-z_]+\s*\(((\s*[A-Z]+\s*),\s*)*((\s*[A-Z]+\s*))\)\s*,\s*)*[a-z_]+\s*\(((\s*[A-Z]+\s*),)*((\s*[A-Z]+\s*))\)\s*$" texto-regla))

;Valores: Solo letras en minusculas
;"       madre      (      juan  ,      maria  )        "
(defn texto-definicion-es-valido [texto-definicion]
	(re-matches #"^\s*[a-z_]+\s*\(((\s*[a-z_]+\s*),)*((\s*[a-z_]+\s*))\)\s*$" texto-definicion))

;Valores: Solo letras en minusculas
;"       madre      (      juan  ,      maria  )
(defn texto-consulta-es-valido [texto-consulta]
	(re-matches #"^\s*[a-z_]+\s*\(((\s*[a-z_]+\s*),)*((\s*[a-z_]+\s*))\)\s*$" texto-consulta))

(defn parsear-texto-regla
[texto]
(if-not (texto-regla-es-valido texto)
	nil
	(let [
		texto-limpio	(str/replace texto #"\s" "")
		tokens		(str/split texto-limpio #":-")
		izq		(get tokens 0)
		der		(get tokens 1)
		nombre		(get (str/split izq #"\(") 0)
		parametros	(re-seq #"[A-Z]" izq)
		consultas	(re-seq #"[a-z_]+\([A-Z,]*\)"der)
		]
		;Consdero que los las reglas estan formadas
		;por consultas parametrizadas (X,Y,Z,...)
		(hash-map
			:nombre nombre
			:parametros parametros
			:consultas consultas))))

(defn parsear-texto-definicion
[texto]
(if-not (texto-consulta-es-valido texto)
	nil
	(let [
		texto-limpio	(str/replace texto #"\s" "")
		tokens		(str/split texto-limpio #"\(|,|\)")
	]
	(hash-map
		:nombre		(first tokens)
		:valores	(subvec tokens 1)))))

(defn parsear-texto-consulta
[texto]
(if-not (texto-consulta-es-valido texto)
	nil
	(let [
		texto-limpio 	(str/replace texto #"\s" "")
		tokens 		(str/split texto-limpio #"\(|,|\)")
	]
	(hash-map
		:nombre		(first tokens)
		:valores	(subvec tokens 1)))))


(defn parsear-texto-database
[texto]
(let [	texto-limpio		(str/replace texto #"\t|\n|\s|\.$" "")
	lineas			(str/split  texto-limpio #"\.")
	lineas-definiciones	(filter	(fn [l] (texto-definicion-es-valido l)) lineas)
	lineas-reglas		(filter	(fn [l] (texto-regla-es-valido l)) lineas)]

	;Si todas las lineas de la base de datos son validas...
	(if-not (= (+ (count lineas-definiciones) (count lineas-reglas)) (count lineas))
		nil
		(hash-map
			:definiciones 	(map (fn [d] (parsear-texto-definicion d)) lineas-definiciones)
			:reglas		(map (fn [r] (parsear-texto-regla r)) lineas-reglas)))))

;;;;;;;;;;;;;;;;FIN PARSER;;;;;;;;;;;;;;;;

;Remplaza parametros X,Y... de la definicion de una regla por valores
(defn reemplazar-parametros-de-consulta
[consulta-con-parametros parametros valores]
(let [	parametro-valor	(apply merge (map
			(fn [x,y] (hash-map x y))
			parametros valores))]

	(str/replace consulta-con-parametros #"[A-Z]" parametro-valor)))

;Devuelve true si la consulta cumple con alguna de las definiciones
(defn consulta-cumple-alguna-de-las-definiciones
[consulta lista-definiciones]
	(some (fn [d] (= d consulta)) lista-definiciones))

;Devuelve true si todas las consultas a comprobar existen en la lista de definiciones
(defn lista-de-consultas-cumple-definiciones
[lista-consultas-a-comprobar lista-definiciones]
	(every?
	(fn[c] (consulta-cumple-alguna-de-las-definiciones c lista-definiciones))
	lista-consultas-a-comprobar))

;Genera un listado de consultas a partir de una regla
(defn convertir-regla-en-consultas
[regla consulta-con-valores]
	;Si la cantidad de parametros X,Y,Z,... no coindice con la cantidad de valoes pepe,juan,toto,..
	(if-not (= (count (regla :parametros)) (count (consulta-con-valores :valores)))
		false
		(map parsear-texto-consulta
			(map
			(fn [consulta-con-parametros] (reemplazar-parametros-de-consulta consulta-con-parametros (regla :parametros) (consulta-con-valores :valores)))
			(regla :consultas)))))

;Convierte la regla en consultas y devuelve TRUE si
;todas las consultas se encuentran en la lista de definiciones
(defn evaluar-regla
[regla consulta lista-definiciones]
(let [lista-consultas-a-evaluar (convertir-regla-en-consultas regla consulta)]
	(if-not lista-consultas-a-evaluar
		false
		(lista-de-consultas-cumple-definiciones lista-consultas-a-evaluar lista-definiciones))))

;Si se encuentra devuelve la regla, sino devuelve nil
(defn buscar-regla
[nombre-regla lista-reglas]
	(first (filter (fn [r] (= (r :nombre) nombre-regla)) lista-reglas)))

;Devuelve true si la consulta cumple con alguna de las reglas
(defn consulta-cumple-alguna-de-las-reglas
[consulta lista-reglas lista-definiciones]
(let [
	nombre-regla 		(consulta :nombre)
	regla-encontrada	(buscar-regla nombre-regla lista-reglas)]

	(if regla-encontrada
		(evaluar-regla regla-encontrada consulta lista-definiciones)
		false)))

(defn evaluate-query
        "Returns true if the rules and facts in database imply query, false if not. If
        either input can't be parsed, returns nil"
        [database query]
        (let [
                definiciones-reglas	(parsear-texto-database database)
		consulta		(parsear-texto-consulta query)]

		(if-not definiciones-reglas
			nil
			(if-not consulta
				nil
                		(if (consulta-cumple-alguna-de-las-definiciones consulta (definiciones-reglas :definiciones))
					true
					(consulta-cumple-alguna-de-las-reglas consulta (definiciones-reglas :reglas) (definiciones-reglas :definiciones)))
				))))
