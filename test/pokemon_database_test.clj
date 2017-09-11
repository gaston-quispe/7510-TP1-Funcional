(ns pokemon-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def pokemon-database "
	varon(ash).
	varon(tracey).
	mujer(misty).
	mujer(yenni).
	entrenador(ash).
	entrenador(misty).
	pokemon(pikachu).
	pokemon(raichu).
	pokemon(bulbasaur).
	pokemon(charmander).
	pokemon(starmie).
	pokemon(psyduck).
	pokemon(gyarados).
	evolucion(pikachu, raichu).
	duenio(ash, pikachu).
	duenio(ash, balbasour).
	duenio(ash, charmander).
	duenio(misty, staryu).
	duenio(misty, psyduck).
	duenio(misty, goldeen).
	tipo(electrico).
	tipo(planta).
	tipo(fuego).
	tipo(agua).
	tipo(psiquico).
	tipo(volador).
	pokemon_tipo(pikachu, electrico).
	pokemon_tipo(raichu, electrico).
	pokemon_tipo(bulbasaur, planta).
	pokemon_tipo(charmander, fuego).
	pokemon_tipo(starmie, agua).
	pokemon_tipo(starmie, psiquico).
	pokemon_tipo(psyduck, agua).
	pokemon_tipo(gyarados, agua).
	pokemon_tipo(gyarados, volador).
	fuerte_contra(electrico, agua).
	fuerte_contra(agua, fuego).
	fuerte_contra(fuego, planta).
	fuerte_contra(volador, planta).
	fuerte_contra(volador, fuego).
	es_entrenador_mujer(X) :- mujer(X), entrenador(X).
	es_entrenador_varon(X) :- varon(X), entrenador(X).
	entrenador_x_tiene_pokemon_y_de_tipo_z(X, Y, Z) :- entrenador(X), pokemon(Y), tipo(Z), pokemon_tipo(Y,Z), duenio(X, Y).
	pokemon_x_de_tipo_y_es_fuerte_contra_tipo_z(X,Y,Z) :- pokemon(X), tipo(Y), tipo(Z), fuerte_contra(Y, Z).
	los_dos_pokemones_son_del_tipo_z(X,Y,Z) :- pokemon(X), pokemon(Y), pokemon_tipo(X,Z), pokemon_tipo(Y,Z).
	pokemon_x_evoluciona_a_y_de_tipo_z(X, Y, Z) :- pokemon(X), pokemon(Y), evolucion(X, Y), pokemon_tipo(Y,Z).
")

(deftest pokemon-database-fact-test
	(testing "entrenador(misty) should be true"
		(is (= (evaluate-query pokemon-database "entrenador(misty)")
		true)))
	(testing "entrenador(tracey) should be false"
		(is (= (evaluate-query pokemon-database "entrenador(tracey)")
		false)))
	(testing "fuerte_contra(agua, fuego) should be true"
		(is (= (evaluate-query pokemon-database "fuerte_contra(agua, fuego)")
		true)))
	(testing "     fuerte_contra      (     agua    ,    fuego    )    should be true"
		(is (= (evaluate-query pokemon-database "     fuerte_contra      (     agua    ,    fuego    )   ")
		true))))

(deftest pokemon-database-rule-test
	(testing "es_entrenador_mujer(misty) should be true"
		(is (= (evaluate-query pokemon-database "es_entrenador_mujer(misty)")
		true)))
	(testing "es_entrenador_mujer(ash) should be false"
		(is (= (evaluate-query pokemon-database "es_entrenador_mujer(ash)")
		false)))
	(testing "entrenador_x_tiene_pokemon_y_de_tipo_z(ash, pikachu, electrico) should be true"
		(is (= (evaluate-query pokemon-database "entrenador_x_tiene_pokemon_y_de_tipo_z(ash, pikachu, electrico)")
		true)))
	(testing "pokemon_x_de_tipo_y_es_fuerte_contra_tipo_z(ash, humano, gaviotas) should be false"
		(is (= (evaluate-query pokemon-database "pokemon_x_de_tipo_y_es_fuerte_contra_tipo_z(ash, humano, gaviotas)")
		false)))
	(testing "los_dos_pokemones_son_del_tipo_z(psyduck, gyarados, agua) should be true"
		(is (= (evaluate-query pokemon-database "los_dos_pokemones_son_del_tipo_z(psyduck, gyarados, agua)")
		true)))
	(testing "   los_dos_pokemones_son_del_tipo_z   (     psyduck   ,   gyarados,     agua    )    should be true"
		(is (= (evaluate-query pokemon-database "   los_dos_pokemones_son_del_tipo_z   (     psyduck   ,   gyarados,     agua    )   ")
		true)))
	(testing "pokemon_x_evoluciona_a_y_de_tipo_z(pikachu, raichu, electrico) should be true"
		(is (= (evaluate-query pokemon-database "pokemon_x_evoluciona_a_y_de_tipo_z(pikachu, raichu, electrico)")
		true)))
	(testing "pokemon_x_evoluciona_a_y_de_tipo_z(pikachu, raichu, agua) should be false"
		(is (= (evaluate-query pokemon-database "pokemon_x_evoluciona_a_y_de_tipo_z(pikachu, raichu, agua)")
		false))))

(deftest pokemon-database-errors-in-querys-test
	(testing "entrenador(no_estoy_en_la_base) should be false"
		(is (= (evaluate-query pokemon-database "entrenador(no_estoy_en_la_base)")
		false)))
	(testing "es_entrenador_mujer(no_estoy_en_la_base) should be false"
		(is (= (evaluate-query pokemon-database "entrenador(no_estoy_en_la_base)")
		false)))
	(testing "no_estoy_en_la_base(ash) should be false"
		(is (= (evaluate-query pokemon-database "fact_que_no_esta_en_la_base(ash)")
		false)))
	(testing "no_estoy_en_la_base(tampoco_estoy_en_la_base) should be false"
		(is (= (evaluate-query pokemon-database "fact_que_no_esta_en_la_base(tampoco_estoy_en_la_base)")
		false)))
	(testing "cosas_sin_formato_asdasd3$·$·$DASDWQW)EQWE)(()())  should be nil"
		(is (= (evaluate-query pokemon-database "cosas_sin_formato_asdasd3$·$·$DASDWQW)EQWE)(()())")
		nil)))
	(testing "es_entrenador_mujer(maYuscula) should be nil"
		(is (= (evaluate-query pokemon-database "es_entrenador_mujer(maYuscula)")
		nil)))
	(testing "maYuscua(ash) should be nil"
		(is (= (evaluate-query pokemon-database "maYuscua(ash)")
		nil)))
	(testing "entrenador(maYuscua) should be nil"
		(is (= (evaluate-query pokemon-database "entrenador(maYuscua)")
		nil)))
	(testing "num3ro5(ash) should be nil"
		(is (= (evaluate-query pokemon-database "num3ro5(ash)")
		nil)))
	(testing "entrenador(num3ro5) should be nil"
		(is (= (evaluate-query pokemon-database "entrenador(num3ro5)")
		nil)))
	(testing "entrenador() should be nil"
		(is (= (evaluate-query pokemon-database "entrenador() ")
		nil)))
	(testing "entrenador(,,) should be nil"
		(is (= (evaluate-query pokemon-database "entrenador(,,)")
		nil)))
	(testing "entrenador_x_tiene_pokemon_y_de_tipo_z(ash, pikachu) should be false"
		(is (= (evaluate-query pokemon-database "entrenador_x_tiene_pokemon_y_de_tipo_z(ash, pikachu)")
		false))))
