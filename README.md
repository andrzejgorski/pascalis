# pascalis

Za pomocą poleceń `make gramma`, oraz `make test`można zbudować i przetestować cały projekt.

Zostały do projektu dostawione 2 pliki napisane w pythonie, które służą pomocy w pisaniu:
`add/_exps.py` - program do wygenerowanej przez bnfc gramatyki dodaje część kodu do pliku AbsPascalis.hs,
którego nie udało mi się wstawić poprzez gramatykę.
`run_tests.py` - program uruchamia każdy plik z folderu tests jako program napisany w języku pascalis.
w pliku znajduje się duży słownik, który mówi jaki output powinien być wypisany dla każdego testu.
Część testów wymaga podania inputu, który jest podawany w plików w folderze inputs.

Zostało zrobione:
Dla każdego z punktów dopiszę nazwy testów znajdujących sie w katalogu tests, które pokazują
działanie danej rzeczy w programie.


#### na 8 punktów
1. typ int
(program)
2. zimenne typu int
(test\_decl\_int, test\_eq\_var\_int, test\_variables\_strange)
3. if
(test\_if, test\_if\_false, test\_if\_el, test\_if\_el\_false)
4. while
(test\_while)
5. wyrażenia z arytmetyką + - */ ()
(test\_x\_plus\_1, test\_x\_minus\_1, test\_10\_minus\_1)
6. Porównowanie intów
(test\_eq\_int\_*, test\_lt\_*, test\_gt\_*, test\_le\_*, test\_ge\_*, test\_neq\_*)

#### na 12 punktów
7. funkcje i procedury z parametrami przez wartość i zmeinną z rekurencją.
(test\_function\_recursive*, test\_procedure\_*)

#### na 16 punktów
1. drugi typ wyrażeń (bool)
(test\_decl\_bool)
2. arytmetyka (porównań boolowskich)
(test\_eq\_int, test\_eq\_int\_false, test\_neq\_int, test\_neq\_int\_false, test\_eq\_bool*(4), test\_bool*(4), test\_and\_*(4), test\_or\_*(4), test\_bool\_not)
#####-- 3. o while if else już napisałem
#####-- 4. Funckje i procedury już wspomniałem
5. instrukcja print umiejąca wypisywać stringi, chary, boole, tablice (tylko pełne)
(test\_decl*, test\_array\_write)
6. a) dwa sposoby przekazywania parametrów do funkcji i procedur.
(test\_procedure\_*, test\_function\_*)
   b) pętla for jak w pascalu
   (test\_loop\_for)
   c) typ String.
(test\_eq\_string\_true, test\_string\_*, test\_decl\_str, test\_change\_string\_content, test\_change\_one\_letter\_string, test\_sub\_str\_var)
(miały być 2 z 4, a jest 3)

#### na 20 punktów
1. przesłanianie indentyfikatorów (test\_block\_variables, test\_global\_variables)
2. (brak) Statycznego typowania
3. Jawnie obsłużone dynamiczne błędy wykonania (nieotestowane, nie przerywają działania programu, niewielka część błędów)
4. Funkcje zwracające wartość
(test\_function\_return\_0)
5. b) tablice int -> int (test\_array\_construct)
   (test\_dict\_*)
   c) słowniki int -> int
   (2 z 5)

#### na 24 punkty
1. Dowolne zagnieżdżanie definicji funkcji / procedur z zachowaniem poprawności statycznego wiązania indentyfikatorów (test\_function\_declaration\_in\_function)
2. ..h) operacje na stringach s[:], s[e:], s[:e], s[e1:e2] - na wzór pythonowych list. (test\_python\_str\_*)


Co zostało zrobione ponad to?
1. funkcja lege() (read) która wczytuje i rozpoznaje z wejścia typy int string i char. (test\_lege*)
2. funkcja ord() char -> int (test\_char\_ord)
3. funkcja longitudo() (length) (test\_array\_length)
4. zmienne typu char wraz całą arytmetyką porównań (test\_eq\_char\_false, test\_eq\_char\_true, test\_decl\_char)
5. Deklaracje jako instrukcje (test\_stmt\_decl)
6. framework do testów napisany w pythonie



