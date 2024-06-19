#einlesen
library(haven)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
class(allb)
str(allb)
length(unique(allb$respid))

allb$dg03
str(allb$dg03)

str(allb$splt21)

#Datensatz nach jedem Split aufgeteilt
Split_A <- allb[allb$splt21 == 1, ]
Split_B <- allb[allb$splt21 == 2, ]
Split_C <- allb[allb$splt21 == 3, ]

#Spalten in denen alle Antworten gleich sind identifiziert
equal_A <- names(which(sapply(Split_A, function(x) length(unique(x)) == 1)))
equal_B <- names(which(sapply(Split_B, function(x) length(unique(x)) == 1)))
equal_C <- names(which(sapply(Split_C, function(x) length(unique(x)) == 1)))

#Spalten identifizieren die nicht durch alle Splits beantwortet wurden
nicht_alle_Splits <- unique(c(equal_A, equal_B, equal_C))

behalten <- setdiff(names(allb), nicht_alle_Splits)

#Datensatz bereinigt um Spalten, die nicht von allen bantwortet wurden
allb_red1 <- allb[, behalten]



NA_Anteil <- sapply(allb_red1, function(x) sum(is.na(x))/length(x))
summary(NA_Anteil)
