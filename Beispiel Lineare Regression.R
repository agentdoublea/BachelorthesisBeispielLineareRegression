#Import noetiger Pakete
library(openxlsx)
library(data.table)

#Laden des generierten Dummy-Datensatzes
alleDatensaetze <- data.frame(read.xlsx("./Beispiel Lineare Regression.xlsx"))

#Festlegen eines Seeds faer ein reproduzierbares Ergebnis
set.seed(5555)

#Mischen der Datensaetze
alleDatensaetze <- alleDatensaetze[sample(nrow(alleDatensaetze)),]

#Aufteilen der Daten in Trainings- und Testdaten
trainingDatensaetze <- alleDatensaetze[1:80,]
testDatensaetze <- alleDatensaetze[81:100,]

#Genereierung des linearen Modells aus den Traings-Datensaetze mit der vorgefertigten Funktion lm() 
model <- lm(Einstiegsgehalt ~ Abschlussnote, data = trainingDatensaetze)
summary(model)

#Anwenden des Modells auf die Test-Datensaetze und Berechnung der Abweichung der Prognose von den tatsaechlichen Werten
prognose <- data.frame(predict(model, newdata=testDatensaetze))
names(prognose) <- c("Einstiegsgehalt.Prognose")

testDatensaetzeMitPrognose <- data.frame(cbind(testDatensaetze, prognose))
testDatensaetzeMitPrognose$Einstiegsgehalt.Prognose <- round(testDatensaetzeMitPrognose$Einstiegsgehalt.Prognose, digits = 0)
testDatensaetzeMitPrognose$AbweichungInProzent <- with(
  testDatensaetzeMitPrognose, round(((Einstiegsgehalt.Prognose-Einstiegsgehalt)/Einstiegsgehalt)*100, digits=2))
testDatensaetzeMitPrognose$Fehlerterm <- with(
  testDatensaetzeMitPrognose, round(Einstiegsgehalt-Einstiegsgehalt.Prognose), digits = 0)

