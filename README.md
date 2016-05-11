# pascalis

Za pomocą poleceń `make gramma`, oraz `make test`można zbudować i przetestować cały projekt.

Zostały do projektu dostawione 2 pliki napisane w pythonie, które służą pomocy w pisaniu:
`add/_exps.py` - program do wygenerowanej przez bnfc gramatyki dodaje część kodu do pliku AbsPascalis.hs,
którego nie udało mi się wstawić poprzez gramatykę.
`run_tests.py` - program uruchamia każdy plik z folderu tests jako program napisany w języku pascalis.
w pliku znajduje się duży słownik, który mówi jaki output powinien być wypisany dla każdego testu.
Część testów wymaga podania inputu, który jest podawany w plików w folderze inputs.


Zostało zrobione:

##### na 8 punktów
1. typ int
2. zimenne typu int
3. if
4. while
5. wyrażenia z arytmetyką + - */ ()
6. Porównowanie intów

##### na 12 punktów
7. funkcje i procedury z parametrami przez wartość i zmeinną z rekurencją.

##### na 16 punktów
1. drugi typ wyrażeń (bool)
2. arytmetyka (porównań boolowskich)
3. while if else
-- 4. Funckje i procedury już wspomniałem
5. instrukcja print umiejąca wypisywać stringi, chary, boole (incribo w moim języku)
6. a) dwa sposoby przekazywania parametrów do funkcji i procedur.
   b) pętla for jak w pascalu
   c) typ String.
(miały być 2 z 4, a jest 3)

##### na 20 punktów
1. przesłanianie indentyfikatorów
2. (brak) Statycznego typowania
3. Jawnie obsłużone dynamiczne błędy wykonania (nieotestowane, nie przerywają działania programu)
4. Funkcje zwracające wartość
5. b) tablice int -> int
   c) słowniki int -> int
   (2 z 5)

##### na 24 punkty
1. Dowolne zagnieżdżanie definicji funkcji / procedur z zachowaniem poprawności statycznego wiązania indentyfikatorów
2. ..h) operacje na stringach s[:], s[e:], s[:e], s[e1:e2] - na wzór pythonowych list.


Co zostało zrobione ponad to?
wbudowane funkcje:
1. funkcja lege() (read) która wczytuje i rozpoznaje z wejścia typy int string i char.
2. funkcja ord() char -> int
3. funkcja longitudo() (length)
4. zmienne typu char wraz całą arytmetyką porównań
5. zmienne typu string
6. framework do testów napisany w pythonie



