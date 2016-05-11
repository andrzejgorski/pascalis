# pascalis

#####Za pomocą poleceń `make gramma`, oraz `make test`można zbudować i przetestować cały projekt.

#####Zostały do projektu dostawione 2 pliki napisane w pythonie, które służą pomocy w pisaniu:
1. `add/_exps.py` - program do wygenerowanej przez bnfc gramatyki dodaje część kodu do pliku AbsPascalis.hs,
którego nie udało mi się wstawić poprzez gramatykę.  (nie jest to ładne rozwiązanie)
2. `run_tests.py` - program uruchamia każdy plik z folderu tests jako program napisany w języku pascalis.
w pliku znajduje się duży słownik, który mówi jaki output powinien być wypisany dla każdego testu.
#####Część testów wymaga podania inputu, który jest podawany w plików w folderze inputs.

#####Zostało zrobione:
#####Dla każdego z punktów dopiszę nazwy testów znajdujących sie w katalogu tests, które pokazują
#####działanie danej rzeczy w programie.


#### na 8 punktów
1. typ int
(program)
2. zimenne typu int
(test_decl_int, test_eq_var_int, test_variables_strange)
3. if
(test_if, test\_if_false, test_if_el, test_if_el_false)
4. while
(test_while)
5. wyrażenia z arytmetyką + - */ ()
(test_x_plus_1, test_x_minus_1, test_10_minus_1)
6. Porównowanie intów
(test_eq_int_\*, test_lt_\*, test_gt_\*, test_le_\*, test_ge_\*, test_neq_\*)

#### na 12 punktów
7. funkcje i procedury z parametrami przez wartość i zmeinną z rekurencją.
(test_function_recursive\*, test_procedure_\*)

#### na 16 punktów
1. drugi typ wyrażeń (bool)
(test_decl_bool)
2. arytmetyka (porównań boolowskich)
(test_eq_int, test_eq_int_false, test_neq_int, test_neq_int_false, test_eq_bool\*(4), test_bool\*(4), test_and_\*(4), test_or_\*(4), test_bool_not)

-- 3. o while if else już napisałem

-- 4. Funckje i procedury już wspomniałem

5. instrukcja print umiejąca wypisywać stringi, chary, boole, tablice (tylko pełne)
(test_decl\*, test_array_write)
6. a) dwa sposoby przekazywania parametrów do funkcji i procedur.
(test_procedure_\*, test_function_\*)
   b) pętla for jak w pascalu
   (test_loop_for)
   c) typ String.
(test_eq_string_true, test_string_\*, test_decl_str, test_change_string_content, test_change_one_letter_string, test_sub_str_var)
(miały być 2 z 4, a jest 3)

#### na 20 punktów
1. przesłanianie indentyfikatorów (test_block_variables, test_global_variables)
2. (brak) Statycznego typowania
3. Jawnie obsłużone dynamiczne błędy wykonania (nieotestowane, nie przerywają działania programu, niewielka część błędów)
4. Funkcje zwracające wartość
(test_function_return_0)
5. b) tablice int -> int (test_array_construct)
   (test_dict_\*)
   c) słowniki int -> int
   (2 z 5)

#### na 24 punkty
1. Dowolne zagnieżdżanie definicji funkcji / procedur z zachowaniem poprawności statycznego wiązania indentyfikatorów (test_function_declaration_in_function)
2. ..h) operacje na stringach s[:], s[e:], s[:e], s[e1:e2] - na wzór pythonowych list. (test_python_str_\*)


#### Co zostało zrobione ponad to?
1. funkcja lege() (read) która wczytuje i rozpoznaje z wejścia typy int string i char. (test_lege\*)
2. funkcja ord() char -> int (test_char_ord)
3. funkcja longitudo() (length) (test_array_length)
4. zmienne typu char wraz całą arytmetyką porównań (test_eq_char_false, test_eq_char_true, test_decl_char)
5. Deklaracje jako instrukcje (test_stmt_decl)
6. framework do testów napisany w pythonie



