# Kod för kandidatarbete i matematisk statistik (GU : MSG900)

## Bow.RData
Simulerad data (upp till 100000 ms) som ej tillhör vårt kandidatarbete, eller originalstudien, från Jyväskylä
universitet. Syftet är att kunna köra koden utan att behöva originaldatan. Som konsekvens blir variabeln
jumplength ej gammafördelad och model4.R är därför modifierad med ett förbestämt värde på variabeln r.

## imageAndBow.R
Innehåller variabler som alla modeller använder sig av, dessa inkluderar:
* image : Konstverket som betraktas.
* Bow : Data för deltagarnas blickar på konstverket.

## simulation.R
Används för alla modeller för att jämföra processerna över tid.

## plots.R
Används för att visualisera jämförelsestatistikor för alla modeller.

## model1.R
Homogen Poissonprocess, test av CSR.

## model2.R
Icke-homogen Poissonprocess.

## model3.R
Icke-homogen Poissonprocess med tidsvarierande intensitet. Icke-homogen
i tid och rum.

## model4.R
Spatio-temporal modell med beroende mellan fixeringar utifrån reglering av extrema sackadlängder.
