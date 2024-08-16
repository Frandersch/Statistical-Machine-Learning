#einlesen
library(haven)
library(purrr)
library(e1071)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
str(allb)

########################Datenpräparation###################################

Datenpräparation_Allbus <- function(Allbus) {
  #Datensatz nach jedem Split aufgeteilt
  Split_A <- allb[allb$splt21 == 1,]
  Split_B <- allb[allb$splt21 == 2,]
  Split_C <- allb[allb$splt21 == 3,]
  
  #Spalten in denen alle Antworten gleich sind identifiziert
  equal_A <-
    names(which(sapply(Split_A, function(x)
      length(unique(
        x
      )) == 1)))
  equal_B <-
    names(which(sapply(Split_B, function(x)
      length(unique(
        x
      )) == 1)))
  equal_C <-
    names(which(sapply(Split_C, function(x)
      length(unique(
        x
      )) == 1)))
  
  #Spalten identifizieren die nicht durch alle Splits beantwortet wurden
  nicht_alle_Splits <- unique(c(equal_A, equal_B, equal_C))
  
  behalten <- setdiff(names(allb), nicht_alle_Splits)
  
  #Datensatz bereinigt um Spalten, die nicht von allen bantwortet wurden
  allb_red1 <- allb[, behalten]
  
  
  #aussortieren von Spalten mit mehr als 5% NA
  NA_Anteil <- sapply(allb_red1, function(x)
    sum(is.na(x)) / length(x))
  Threshold <- 0.05
  allb_red2 <- subset(allb_red1, select = NA_Anteil < Threshold)
  
  #administrative Spalten entfernen
  allb_red3 <- allb_red2[,-c(1:4)]
  
  #Spalten mit Gewichten entfernen
  allb_red4 <- allb_red3[, -c(grep("wght", names(allb_red3)))]
  
  #alle Observations mit NA entfernen
  allb_red5 <- na.omit(allb_red4)
  
  #nur relevante Fälle behalten (-42 Datenfehler, -9 keine Angabe,
  #18 Fruehere Dt. Ostgeb. und 95 Sonstiges Land entfernen) und faktorisieren
  allb_red6 <- allb_red5[allb_red5$dg10 == 1 | allb_red5$dg10 == 2,]
  allb_red6$dg10 <- factor(allb_red6$dg10, labels = c("Alte BL", "Neue BL"))
  
  #letzte Datenbearbeitung ausgeben
  allb_red6
}

allb_prep <- Datenpräparation_Allbus(allb)
allb_prep$dg10
write.csv2(allb_prep,"Code/Daten/Allbus_prep.csv",row.names = F)

