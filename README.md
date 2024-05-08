# Kod för kandidatarbete i matematisk statistik (GU : MSG900)

## Bow.RData
Simulerad data (upp till 100000 ms) som ej tillhör vår, eller orginalstudien från Jyväskylä University.
Syftet är att kunna köra koden utan att behöva orginaldatan. Som konsekvens blir
"jumplength" ej gamma fördelad som gör att MLE metoden ej konvergerar för model4.R.

## imageAndBow.R
Innehåller variabler som alla modeller använder sig av, dessa inkluderar:
* image : Konstverket som betraktas
* Bow : Data för personens blick på konstverket

## simulation.R
Används för att jämföra processerna över tid.

## plots.R
Används för att plotta jämnförelsestatistikor.

## model1.R
Homogen Poissonprocess, test av CSR.

## model2.R
Icke-homogen Poissonprocess.

## model3.R
Icke-homogen Poissonprocess med tidsvarierande intensitet. Icke-homogen
i tid och rum

## model4.R
Spatio-remporal modell med beroende mellan fixeringar utifrån reglering av extrema sackadlängder. Fungerar ej på grund av att "jumplength" ej är gammafördelad i "fake" datan.
