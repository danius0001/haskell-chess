# Szachy

Stan projektu przed zmianami został wrzucony w pierwszym commicie.

Co zostało zrobione:
- naprawiona strategia minimax (drobny błąd sprawiał, że wybierany był najgorszy możliwy ruch zamiast najlepszego)
- wybór losowego ruchu z puli najlepszych możliwych ruchów (wcześniej był to zawsze pierwszy z brzegu)
- refactor pliku Main.hs z użyciem Monad Transformerów (oraz drobny refactor pozostałych modułów) 
- zrównoleglenie obliczania minimax na drzewie stanów (tu okazało się że wykorzystanie prostego parMap działa najlepiej -> każdy możliwy ruch gracza to jeden SPARK)
- dwie strategie do generowania drzewa równolegle (nie było potrzeby ich wykorzystania, bo nie dawały już żadnego przyspieszenia)

