#einlesen
library(haven)
library(purrr)
library(e1071)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
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

################################ Auswertung #################################


#test und train Daten definieren
set.seed(123)
train <- sample(1:nrow(allb_prep), round(nrow(allb_prep)*0.7))
allb_train <- allb_prep[train, ]
allb_test <- allb_prep[-train, ]

#support vector classifier mit c = 1
svc_model <- svm(dg10 ~ ., data = allb_train, kernel = "linear", cost = 1)
summary(svc_model)

predictions <- predict(svc_model, allb_test)

confusion_matrix <- table(predictions, allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene Cost-Budgets
tuning_c <- function(costs) {
 matrixcosts <- matrix(rep(NA,2*length(costs)),nrow = length(costs),ncol=2)
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
    matrixcosts[i,1] <- costs[i]
    matrixcosts[i,2] <- accuracy
   }
 return(matrixcosts)
}

tuning_c(c(0.1, 1, 10))
#best for cost of 1

#support vector machine mit polynomial Kernel
svm_model_polynomial <- svm(dg10 ~ ., data = allb_train, kernel = "polynomial")
summary(svm_model)

predictions <- predict(svm_model, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene C und degree für polynomial Kernel
tuning_c_degree <- function(costs, degree) {
  for (i in costs) {
    for (j in degree) {
      svm_model <-
        svm(
          dg10 ~ .,
          data = allb_train,
          kernel = "polynomial",
          cost = i,
          degree = j
        )
      predictions <- predict(svm_model, allb_test)
      confusion_matrix <-
        table(predicted = predictions, real = allb_test$dg10)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      cat("Cost of ",
          i,
          " and degree of ",
          j,
          " has an accuracy of ",
          accuracy,
          "\n",
          sep = "")
    }
  }
}

tuning_c_degree(c(1, 10, 20), c(2, 3, 4))
#best accuracy for cost of 10 and degree of 3

#support vector machine mit radial Kernel
svm_model_radial <- svm(dg10 ~ ., data = allb_train, kernel = "radial")
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

tuning_c_gamma(c(1, 10), c(0.001, 0.01, 0.1, 2))
#best for cost of 1 and gamma of 0.01


######################## Comparison to LDA ###################################


library(MASS)
lda_model <- lda(dg10 ~ ., data = allb_train)
lda_model

prediction <- predict(lda_model, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)


############### ROC curves versch. SVM Modelle ##############################


library(ROCR)

#Funktion zum erzeugen des ROC Plots
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

#Modell mit bestmöglichen Parametern
svm_model_opt <- svm(
  dg10 ~ .,
  data = allb_train,
  kernel = "radial",
  gamma = 0.01,
  cost = 1,
  decision.values = TRUE
)

fitted_opt <-
  attributes(predict(svm_model_opt, allb_test, decision.values = TRUE))$decision.values

par(mfrow = c(1, 2))
rocplot(fitted_opt, allb_test$dg10, main = "Test Data")

# Modell mit flexibleren Parametern
svm_model_flex <- svm(
  dg10 ~ .,
  data = allb_train,
  kernel = "radial",
  gamma = 0.1,
  cost = 10,
  decision.values = TRUE
)

fitted_flex <-
  attributes(predict(svm_model_flex, allb_test, decision.values = TRUE))$decision.values

rocplot(fitted_flex, allb_test$dg10, main = "Test Data")


################## ROC curves SVM und LDA ##################################

library(pROC)
par(mfrow = c(1, 1))

# LDA Modell plotten
lda_model_roc <- lda(dg10 ~ ., data = allb_train)

lda_predictions <- predict(lda_model_opt, allb_test)

lda_prob <- lda_predictions$posterior[, 2]

roc_lda <- roc(allb_test$dg10, lda_prob, plot = TRUE, col = "blue", main = "ROC Curves for LDA and SVM")

# SVM Modell hinzufügen
svm_model_roc <- svm(
  dg10 ~ .,
  data = allb_train,
  kernel = "radial",
  gamma = 0.01,
  cost = 1,
  probability = TRUE
)

svm_predictions <- predict(svm_model_opt, allb_test, probability = TRUE)

svm_prob <- attr(svm_predictions, "probabilities")[, 2]

roc_svm <- roc(allb_test$dg10, svm_prob, plot = TRUE, col = "red", add = TRUE)

# Legende hinzufügen
legend("bottomright", legend = c("LDA", "SVM"), col = c("blue", "red"), lwd = 2)

# AUC Scores

auc_lda <- auc(roc_lda)
auc_svm <- auc(roc_svm)

cat("AUC for LDA:", auc_lda, "\nAUC for SVM:", auc_svm)


#################### Sensitivitätsanalyse ##################################

library(caret)
library(iml)

svm_model_opt <- svm_model_opt <- svm(
  dg10 ~ .,
  data = allb_train,
  kernel = "radial",
  gamma = 0.01,
  cost = 1,
)

# noch fortführen
