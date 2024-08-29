PROJEKT
Diese Arbeit evaluiert die Performance von SVM-Algorithmen für die binäre Klassifikation unter verschiedenen Datensituationen. Der Code generiert synthetische Datensätze und führt Analysen durch, um die Effizienz von SVM in  verschiedenen Szenarien zu bewerten.

ORDNERSTRUKTUR
Code/: Enthält den Code für die Datengenerierung und Analyse, unterteilt nach den drei Arten von Entscheidungsgrenzen:
- hyperplane = Code für die lineare Form der Entscheidungsgrenze
- quadratic hyperplane = Code für die polynomiale Form der Entscheidungsgrenze
- hypersphere = Code für die radiale Form der Entscheidungsgrenze

Abgabe/: Enthält die Hausarbeit und zugehörige Dokumente:
- Hausarbeit.Rmd: das RMarkdown-Dokument der Hausarbeit
- Hausarbeit.pdf: die Hausarbeit in PDF-Format
- Literatur.bib: die Literatur in BibTeX-Format
- titlepage.tex: die Titelseite als TeX-Datei
- header.tex: die Präambel mit weiteren Latex Packages
- Ordner: einzelne Kapitel der Hausarbeit

BENÖTIGTE PAKETE
- class
- e1071
- glmnet
- gridExtra
- knitr
- pROC
- rBayesianOptimization
- rgl
- xtable

REPRODUKTION DER ANALYSE
Die Daten wurden abgespeichert und können im "Datenimport"-Kapitel geladen werden.
Aufgrund der hohen Rechenzeit wurden die Hyperparameter, die durch die Bayesian Optimization ermittelt wurden abgespeichert und können zu Beginn des "Modelle tunen"-Abschnitts geladen werden.