# Inteligencja-Obliczeniowa
## Uniwersytet Gdański
## Studia magisterskie: Informatyka rok.1 sem.1, 2018/19
### Repozytorium zawiera projekty tworzone na laboratoria Intelignecji obliczeniowej
Projekty zaimplementowane za pomocą języka R.
### Projekt1 - Algorytm genetyczny
Celem projektu było zaprojektowanie funckji fitness odpowiedzialnej za szukanie rozwiązania wybranego problemu, w moim przypadku był to nonogram.
Zostały zaimplementowane dwa sposoby karania za błedne bity w chromosomie.
### Projekt2 - Zgłębianie danych
Celem projektu było na podstawie bazy danych użycie klasyfikatorów i sprawdzenie ich skutecznosci, w tym celu baza była obrabiana do poprawnych danych
i podzielona na części treningową i testową. Skuteczność to np. przewidywanie zachorowania na podstawie wyników pacjenta. Klasyfikatory C4.5/ID3 (drzewo), kNN, naiveBayes, SVM.
Projekt zawierał też użycie metody grupowania k-średnich oraz znalezienie regół asocjacji.
### Projekt3 - Sieci neuronowe
Sieć neuronowa z wykorzystaniem neuralnet. Celem było zebranie danych z gry [Tanks](https://inf.ug.edu.pl/~gmadejsk/tanks/tanks.html) i wybranie według siebie dobrych do nauki danych wyjściowych, które by posłużyły do szkolenia sieci w celu podejmowania odpowiednich decyzji dla danych wejściowych np. jedź prosto, strzelaj. Ostatecznie okazało się że mnogość danych( zapis z gry co tik zegara procesora ) nie nadawały się do dobrego szkolenia jedynie mogły zdezorientować taką sieć.
skrypt.js służy do automatycznego sterowania czołgiem w grze na podstawie wag z sieci neuronowej.
### Projekt4 - Przetwarzanie obrazu
Celem projektu jest dokonanie klasyfikacji na wybranym zbiorze. Należy przetestować kilka
klasyfikatorów. Zbiór do klasyfikacji zawierał zdjęcia RTG zapalania płuc i zdrowych płuc. Wybrane klasyfikatory to CNN ( biblioteka MXNet ), Random Forest ( biblioteka randomForest ), Gradient boosting ( biblioteka gbm ), SVM ( biblioteka e1701 ). Klasyfikatory miały rozpoznawać zapalenie płuc, ostatecznie dobrze rozpoznawały dobrze zapalenie płuc, lecz jednocześnie mocno myliły zdrowych z chorymi.


