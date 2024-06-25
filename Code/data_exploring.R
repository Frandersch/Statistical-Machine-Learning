#einlesen
library(haven)
library(purrr)
library(e1071)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
class(allb)
str(allb)
length(unique(allb$respid))

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
  
  #alle Observations mit NA entfernen
  allb_red4 <- na.omit(allb_red3)
  
  #nur relevante Fälle behalten (-42 Datenfehler, -9 keine Angabe,
  #18 Fruehere Dt. Ostgeb. und 95 Sonstiges Land entfernen) und faktorisieren
  allb_red4 <- allb_red4[allb_red4$dg10 == 1 | allb_red4$dg10 == 2,]
  allb_red4$dg10 <- factor(allb_red4$dg10)
  
  #letzte Datenbearbeitung ausgeben
  allb_red4
}

allb_prep <- Datenpräparation_Allbus(allb)


################################Auswertung###################################


#test und train Daten definieren
set.seed(123)
train <- sample(1:nrow(allb_red4), round(nrow(allb_red4)*0.7))
allb_train <- allb_red4[train, ]
allb_test <- allb_red4[-train, ]

#support vector classifier mit c = 1
svc_model <- svm(dg10 ~ ., data = allb_train, kernel = "linear", cost = 1)
summary(svc_model)

predictions <- predict(svc_model, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)


#verschiedene Cost-Budgets
tuning_c <- function(costs) {
  for (i in costs) {
    svc_model <-
      svm(dg10 ~ .,
          data = allb_train,
          kernel = "linear",
          cost = i)
    predictions <- predict(svc_model, allb_test)
    confusion_matrix <-
      table(predicted = predictions, real = allb_test$dg10)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    cat("Cost of ", i, " has an accuracy of ", accuracy, "\n", sep = "")
  }
}

tuning_c(c(0.001, 0.01, 0.1, 1))

#support vector machine mit radial Kernel
svm_model <- svm(dg10 ~ ., data = allb_train, kernel = "radial")
summary(svm_model)

predictions <- predict(svm_model, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene C und Gamma für radial Kernel
tuning_c_gamma <- function(costs, gammas) {
  for (i in costs) {
    for (j in gammas) {
      svm_model <-
        svm(
          dg10 ~ .,
          data = allb_train,
          kernel = "radial",
          cost = i,
          gamma = j
        )
      predictions <- predict(svm_model, allb_test)
      confusion_matrix <-
        table(predicted = predictions, real = allb_test$dg10)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      cat("Cost of ",
          i,
          " and gamma of ",
          j,
          " has an accuracy of ",
          accuracy,
          "\n",
          sep = "")
    }
  }
}

tuning_c_gamma(1, c(0.001, 0.01))
