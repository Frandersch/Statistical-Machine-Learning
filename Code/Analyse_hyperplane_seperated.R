# Pakete laden
library(e1071)
library(glmnet)
library(class)
library(rBayesianOptimization)
library(gridExtra)
library(pROC)

## Szenario 1: p << n (n = 1000, p = 10)

## Datenimport

# laden der Daten für Szenario 1
load(file = "Abgabe/Ergebnisse/Daten/Data_S1.RData")

## Modelle tunen

# laden der durch das Tuning ermittelten besten Parameter je Modell
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S1.RData")

# definieren des Modells, aller zu tunenden Hyperparameter und der Maßzahl
S1_tune_linear <- function(cost) {
  # Model mit Trainingsdaten fitten
  model <-
    svm(y ~ .,
        data = S1_data_train,
        kernel = "linear",
        cost = cost)
  # Prediction anhand der Testdaten
  prediction <- predict(model, S1_data_test)
  # Festlegen der Genauigkeit als Maßzahl
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

# durchführen des Tunings mit Hilfe der Bayesian Optimization
set.seed(11)
S1_opt_param_linear <- BayesianOptimization(
  # festlegen der Funktion
  FUN = S1_tune_linear,
  # obere und untere Grenzen aller Parameter
  bounds = list(cost = c(0.01, 100)),
  # initiale Ziehungen vor Beginn der Bayesian Optimization
  init_points = 10,
  # Anzahl der Iterationen
  n_iter = 15,
  # Upper Confidence Bound als Akqusitionsfunktion festlegen
  acq = "ucb",
  # unterdrückt das printen der Ausgabe jeder Iteration
  verbose = FALSE
)

S1_tune_polynomial <- function(cost, gamma, degree) {
  model <-
    svm(
      y ~ .,
      data = S1_data_train,
      kernel = "polynomial",
      cost = cost,
      gamma = gamma,
      degree = degree
    )
  prediction <- predict(model, S1_data_test)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

set.seed(12)
S1_opt_param_polynomial <- BayesianOptimization(
  FUN = S1_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S1_tune_radial <- function(cost, gamma) {
  model <-
    svm(
      y ~ .,
      data = S1_data_train,
      kernel = "radial",
      cost = cost,
      gamma = gamma
    )
  prediction <- predict(model, S1_data_test)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

set.seed(13)
S1_opt_param_radial <- BayesianOptimization(
  FUN = S1_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S1_tune_logR <- function(alpha, lambda) {
  model <-
    glmnet(
      as.matrix(S1_data_train[, setdiff(names(S1_data_train), "y")]),
      S1_data_train[, "y"],
      family = "binomial",
      alpha = alpha,
      lambda = lambda
    )
  probabilities <-
    predict(model,
            as.matrix(S1_data_test[, setdiff(names(S1_data_test), "y")]),
            s = lambda,
            type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

set.seed(16)
S1_opt_param_logR <- BayesianOptimization(
  FUN = S1_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S1_tune_k_NN <- function(k) {
  prediction <-
    knn(S1_data_train[, setdiff(names(S1_data_train), "y")],
        S1_data_test[, setdiff(names(S1_data_test), "y")],
        S1_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

set.seed(15)
S1_opt_param_k_NN <- BayesianOptimization(
  FUN = S1_tune_k_NN,
  bounds = list(k = c(1, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

# speichern der durch das Tuning ermittelten besten Parameter je Modell für
# zukünftige Berechnungen
save(
  S1_opt_param_linear,
  S1_opt_param_polynomial,
  S1_opt_param_radial,
  S1_opt_param_logR,
  S1_opt_param_k_NN,
  file = "Abgabe/Ergebnisse/Parameter/Parameter_S1.RData"
)

## Modelle fitten

# für svm Paket e1071 verwendet
S1_svm_linear <-
  svm(y ~ .,
      data = S1_data_train,
      kernel = "linear",
      cost = S1_opt_param_linear$Best_Par)
S1_svm_polynomial <-
  svm(
    y ~ .,
    data = S1_data_train,
    kernel = "polynomial",
    cost = S1_opt_param_polynomial$Best_Par["cost"],
    gamma = S1_opt_param_polynomial$Best_Par["gamma"],
    degree = S1_opt_param_polynomial$Best_Par["degree"]
  )
S1_svm_radial <-
  svm(
    y ~ .,
    data = S1_data_train,
    kernel = "radial",
    cost = S1_opt_param_radial$Best_Par["cost"],
    gamma = S1_opt_param_radial$Best_Par["gamma"]
  )
# für logR Paket glmnet verwendet
S1_logR <-
  glmnet(
    as.matrix(S1_data_train[, setdiff(names(S1_data_train), "y")]),
    S1_data_train[, "y"],
    family = "binomial",
    alpha = S1_opt_param_logR$Best_Par["alpha"],
    lambda = S1_opt_param_logR$Best_Par["lambda"]
  )
# für k-nn Paket class
S1_k_NN <- knn(S1_data_train[, setdiff(names(S1_data_train), "y")],
               S1_data_test[, setdiff(names(S1_data_test), "y")],
               S1_data_train[, "y"],
               k = S1_opt_param_k_NN$Best_Par["k"])
## Predictions

S1_prediction_linear <- predict(S1_svm_linear, S1_data_test)
S1_prediction_polynomial <- predict(S1_svm_polynomial, S1_data_test)
S1_prediction_radial <- predict(S1_svm_radial, S1_data_test)
S1_probabilities_logR <-
  predict(S1_logR,
          as.matrix(S1_data_test[, setdiff(names(S1_data_test), "y")]),
          s = S1_opt_param_logR$Best_Par["lambda"],
          type = "response")
S1_prediction_logR <- ifelse(S1_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

## Accuracy

# erstellen der Confusion Matrix durch die Predictions und die tatsächlichen
# Testwerte
S1_confusion_matrix_linear <- table(S1_prediction_linear, S1_data_test$y)
# berchnen der Genauigkeit als Anteil der diagonalen an der gesamten Anzahl
S1_accuracy_linear <-
  sum(diag(S1_confusion_matrix_linear)) / sum(S1_confusion_matrix_linear)

S1_confusion_matrix_polynomial <- table(S1_prediction_polynomial, S1_data_test$y)
S1_accuracy_polynomial <-
  sum(diag(S1_confusion_matrix_polynomial)) / sum(S1_confusion_matrix_polynomial)

S1_confusion_matrix_radial <- table(S1_prediction_radial, S1_data_test$y)
S1_accuracy_radial <-
  sum(diag(S1_confusion_matrix_radial)) / sum(S1_confusion_matrix_radial)

S1_confusion_matrix_logR <- table(S1_prediction_logR, S1_data_test$y)
S1_accuracy_logR <-
  sum(diag(S1_confusion_matrix_logR)) / sum(S1_confusion_matrix_logR)

S1_confusion_matrix_k_NN <- table(S1_k_NN, S1_data_test$y)
S1_accuracy_k_NN <-
  sum(diag(S1_confusion_matrix_k_NN)) / sum(S1_confusion_matrix_k_NN)

## F1-Score

# berechnen des F1-Score aus der Präzision und Sensitivität
S1_F1_linear <- 
  2/(1/(S1_confusion_matrix_linear[1, 1]/
          sum(S1_confusion_matrix_linear[1, ])) + 
       1/(S1_confusion_matrix_linear[1, 1]/
            sum(S1_confusion_matrix_linear[, 1])))
S1_F1_polynomial <- 
  2/(1/(S1_confusion_matrix_polynomial[1, 1]/
          sum(S1_confusion_matrix_polynomial[1, ])) + 
       1/(S1_confusion_matrix_polynomial[1, 1]/
            sum(S1_confusion_matrix_polynomial[, 1])))
S1_F1_radial <- 
  2/(1/(S1_confusion_matrix_radial[1, 1]/
          sum(S1_confusion_matrix_radial[1, ])) + 
       1/(S1_confusion_matrix_radial[1, 1]/
            sum(S1_confusion_matrix_radial[, 1])))
S1_F1_logR <- 
  2/(1/(S1_confusion_matrix_logR[1, 1]/
          sum(S1_confusion_matrix_logR[1, ])) + 
       1/(S1_confusion_matrix_logR[1, 1]/
            sum(S1_confusion_matrix_logR[, 1])))
S1_F1_k_NN <- 
  2/(1/(S1_confusion_matrix_k_NN[1, 1]/
          sum(S1_confusion_matrix_k_NN[1, ])) + 
       1/(S1_confusion_matrix_k_NN[1, 1]/
            sum(S1_confusion_matrix_k_NN[, 1])))

## ROC/AUC

# Berechnung des Modells mit Ausgabe der Wahrscheinlichkeit (notwendig für ROC)
S1_svm_linear_probs <-
  svm(
    y ~ .,
    data = S1_data_train,
    kernel = "linear",
    cost = S1_opt_param_linear$Best_Par["cost"],
    probability = TRUE
  )
# Prediction anhand der Testdaten
S1_prob_svm_linear <-
  predict(S1_svm_linear_probs, S1_data_test, probability = TRUE)
# Extraktion der Klasse 1
S1_prediction_probs_linear <-
  attr(S1_prob_svm_linear, "probabilities")[, 1]
# Berechnung des ROC mit Hilfe des pROC-Paket
S1_roc_linear <-
  roc(S1_data_test$y,
      S1_prediction_probs_linear, 
      levels = rev(levels(S1_data_test$y)))

S1_svm_polynomial_probs <-
  svm(
    y ~ .,
    data = S1_data_train,
    kernel = "polynomial",
    cost = S1_opt_param_polynomial$Best_Par["cost"],
    gamma = S1_opt_param_polynomial$Best_Par["gamma"],
    degree = S1_opt_param_polynomial$Best_Par["degree"],
    probability = TRUE
  )
S1_prob_svm_polynomial <-
  predict(S1_svm_polynomial_probs, S1_data_test, probability = TRUE)
S1_prediction_probs_polynomial <-
  attr(S1_prob_svm_polynomial, "probabilities")[, 1]
S1_roc_polynomial <-
  roc(S1_data_test$y,
      S1_prediction_probs_polynomial,
      levels = rev(levels(S1_data_test$y)))

S1_svm_radial_probs <-
  svm(
    y ~ .,
    data = S1_data_train,
    kernel = "radial",
    cost = S1_opt_param_radial$Best_Par["cost"],
    gamma = S1_opt_param_radial$Best_Par["gamma"],
    probability = TRUE
  )
S1_prob_svm_radial <-
  predict(S1_svm_radial_probs, S1_data_test, probability = TRUE)
S1_prediction_probs_radial <-
  attr(S1_prob_svm_radial, "probabilities")[, 1]
S1_roc_radial <-
  roc(S1_data_test$y, 
      S1_prediction_probs_radial, 
      levels = rev(levels(S1_data_test$y)))

S1_prob_logR <-
  predict(S1_logR, 
          as.matrix(S1_data_test[, setdiff(names(S1_data_test), "y")]), 
          type = "response")
S1_roc_logR <-
  roc(S1_data_test$y, 
      as.numeric(S1_prob_logR), 
      levels = rev(levels(S1_data_test$y)))

S1_k_NN_probs <-
  knn(
    S1_data_train[, setdiff(names(S1_data_train), "y")],
    S1_data_test[, setdiff(names(S1_data_test), "y")],
    S1_data_train[, "y"],
    k = S1_opt_param_k_NN$Best_Par["k"],
    prob = TRUE
  )
S1_prediction_probs_k_NN <- attr(S1_k_NN_probs, "prob")
S1_prediction_probs_k_NN <-
  ifelse(
    S1_k_NN_probs == levels(S1_data_train$y)[1],
    S1_prediction_probs_k_NN,
    1 - S1_prediction_probs_k_NN
  )
S1_roc_k_NN <-
  roc(S1_data_test$y, 
      S1_prediction_probs_k_NN, 
      levels = rev(levels(S1_data_test$y)))

# speichern der ROC-Daten für ROC-Kurven und AUC-Werte
save(S1_roc_linear,
     S1_roc_polynomial,
     S1_roc_radial,
     S1_roc_logR,
     S1_roc_k_NN,
     file = "Abgabe/Ergebnisse/ROC/S1_roc.RData")

## Szenario 4: p = n (p = 50, n = 50)

# Datenimport

# laden der Daten für Szenario 4
load(file = "Abgabe/Ergebnisse/Daten/Data_S4.RData")

## Modelle tunen

# laden der durch das Tuning ermittelten besten Parameter je Modell
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S4.RData")

S4_tune_linear <- function(cost) {
  model <-
    svm(y ~ .,
        data = S4_data_train,
        kernel = "linear",
        cost = cost)
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

set.seed(41)
S4_opt_param_linear <- BayesianOptimization(
  FUN = S4_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

# alle Ziehungen des Cost-Parameter liefern die selbe Genauigkeit
# es gibt folglich keinen optimalen Parameter
# Default-Wert wird für den Cost-Parameter festgelegt
S4_opt_param_linear <- list(Best_Par = data.frame(cost = 1))

S4_tune_polynomial <- function(cost, gamma, degree) {
  model <-
    svm(
      y ~ .,
      data = S4_data_train,
      kernel = "polynomial",
      cost = cost,
      gamma = gamma,
      degree = degree
    )
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

set.seed(42)
S4_opt_param_polynomial <- BayesianOptimization(
  FUN = S4_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S4_tune_radial <- function(cost, gamma) {
  model <-
    svm(
      y ~ .,
      data = S4_data_train,
      kernel = "radial",
      cost = cost,
      gamma = gamma
    )
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

set.seed(43)
S4_opt_param_radial <- BayesianOptimization(
  FUN = S4_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S4_tune_logR <- function(alpha, lambda) {
  model <-
    glmnet(
      as.matrix(S4_data_train[, setdiff(names(S4_data_train), "y")]),
      S4_data_train[, "y"],
      family = "binomial",
      alpha = alpha,
      lambda = lambda
    )
  probabilities <-
    predict(model,
            as.matrix(S4_data_test[, setdiff(names(S4_data_test), "y")]),
            s = lambda,
            type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

set.seed(44)
S4_opt_param_logR <- BayesianOptimization(
  FUN = S4_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S4_tune_k_NN <- function(k) {
  prediction <-
    knn(S4_data_train[, setdiff(names(S4_data_train), "y")],
        S4_data_test[, setdiff(names(S4_data_test), "y")],
        S4_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

set.seed(45)
S4_opt_param_k_NN <- BayesianOptimization(
  FUN = S4_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

# speichern der durch das Tuning ermittelten besten Parameter je Modell für
# zukünftige Berechnungen
save(
  S4_opt_param_linear,
  S4_opt_param_polynomial,
  S4_opt_param_radial,
  S4_opt_param_logR,
  S4_opt_param_k_NN,
  file = "Abgabe/Ergebnisse/Parameter/Parameter_S4.RData"
)

## Modelle fitten

S4_svm_linear <-
  svm(y ~ .,
      data = S4_data_train,
      kernel = "linear",
      cost = S4_opt_param_linear$Best_Par["cost"])
S4_svm_polynomial <-
  svm(
    y ~ .,
    data = S4_data_train,
    kernel = "polynomial",
    cost = S4_opt_param_polynomial$Best_Par["cost"],
    gamma = S4_opt_param_polynomial$Best_Par["gamma"],
    degree = S4_opt_param_polynomial$Best_Par["degree"]
  )
S4_svm_radial <-
  svm(
    y ~ .,
    data = S4_data_train,
    kernel = "radial",
    cost = S4_opt_param_radial$Best_Par["cost"],
    gamma = S4_opt_param_radial$Best_Par["gamma"]
  )
S4_logR <-
  glmnet(
    as.matrix(S4_data_train[, setdiff(names(S4_data_train), "y")]),
    S4_data_train[, "y"],
    family = "binomial",
    alpha = S4_opt_param_logR$Best_Par["alpha"],
    lambda = S4_opt_param_logR$Best_Par["lambda"]
  )
S4_k_NN <- knn(S4_data_train[, setdiff(names(S4_data_train), "y")],
               S4_data_test[, setdiff(names(S4_data_test), "y")],
               S4_data_train[, "y"],
               k = S4_opt_param_k_NN$Best_Par["k"])

## Predictions

S4_prediction_linear <- predict(S4_svm_linear, S4_data_test)
S4_prediction_polynomial <- predict(S4_svm_polynomial, S4_data_test)
S4_prediction_radial <- predict(S4_svm_radial, S4_data_test)
S4_probabilities_logR <-
  predict(S4_logR,
          as.matrix(S4_data_test[, setdiff(names(S4_data_test), "y")]),
          s = S4_opt_param_logR$Best_Par["lambda"],
          type = "response")
S4_prediction_logR <- ifelse(S4_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

## Accuracy

S4_confusion_matrix_linear <- table(S4_prediction_linear, S4_data_test$y)
S4_accuracy_linear <-
  sum(diag(S4_confusion_matrix_linear)) / sum(S4_confusion_matrix_linear)

S4_confusion_matrix_polynomial <- table(S4_prediction_polynomial, S4_data_test$y)
S4_accuracy_polynomial <-
  sum(diag(S4_confusion_matrix_polynomial)) / sum(S4_confusion_matrix_polynomial)

S4_confusion_matrix_radial <- table(S4_prediction_radial, S4_data_test$y)
S4_accuracy_radial <-
  sum(diag(S4_confusion_matrix_radial)) / sum(S4_confusion_matrix_radial)

S4_confusion_matrix_logR <- table(S4_prediction_logR, S4_data_test$y)
S4_accuracy_logR <-
  sum(diag(S4_confusion_matrix_logR)) / sum(S4_confusion_matrix_logR)

S4_confusion_matrix_k_NN <- table(S4_k_NN, S4_data_test$y)
S4_accuracy_k_NN <-
  sum(diag(S4_confusion_matrix_k_NN)) / sum(S4_confusion_matrix_k_NN)

## F1-Score

S4_F1_linear <- 
  2/(1/(S4_confusion_matrix_linear[1, 1]/
          sum(S4_confusion_matrix_linear[1, ])) + 
       1/(S4_confusion_matrix_linear[1, 1]/
            sum(S4_confusion_matrix_linear[, 1])))
S4_F1_polynomial <- 
  2/(1/(S4_confusion_matrix_polynomial[1, 1]/
          sum(S4_confusion_matrix_polynomial[1, ])) + 
       1/(S4_confusion_matrix_polynomial[1, 1]/
            sum(S4_confusion_matrix_polynomial[, 1])))
S4_F1_radial <- 
  2/(1/(S4_confusion_matrix_radial[1, 1]/
          sum(S4_confusion_matrix_radial[1, ])) + 
       1/(S4_confusion_matrix_radial[1, 1]/
            sum(S4_confusion_matrix_radial[, 1])))
S4_F1_logR <- 
  2/(1/(S4_confusion_matrix_logR[1, 1]/
          sum(S4_confusion_matrix_logR[1, ])) + 
       1/(S4_confusion_matrix_logR[1, 1]/
            sum(S4_confusion_matrix_logR[, 1])))
S4_F1_k_NN <- 
  2/(1/(S4_confusion_matrix_k_NN[1, 1]/
          sum(S4_confusion_matrix_k_NN[1, ])) + 
       1/(S4_confusion_matrix_k_NN[1, 1]/
            sum(S4_confusion_matrix_k_NN[, 1])))

## ROC/AUC

S4_svm_linear_probs <-
  svm(
    y ~ .,
    data = S4_data_train,
    kernel = "linear",
    cost = S4_opt_param_linear$Best_Par["cost"],
    probability = TRUE
  )
S4_prob_svm_linear <-
  predict(S4_svm_linear_probs, S4_data_test, probability = TRUE)
S4_prediction_probs_linear <-
  attr(S4_prob_svm_linear, "probabilities")[, 1]
S4_roc_linear <-
  roc(S4_data_test$y, 
      S4_prediction_probs_linear, 
      levels = rev(levels(S4_data_test$y)))

S4_svm_polynomial_probs <-
  svm(
    y ~ .,
    data = S4_data_train,
    kernel = "polynomial",
    cost = S4_opt_param_polynomial$Best_Par["cost"],
    gamma = S4_opt_param_polynomial$Best_Par["gamma"],
    degree = S4_opt_param_polynomial$Best_Par["degree"],
    probability = TRUE
  )
S4_prob_svm_polynomial <-
  predict(S4_svm_polynomial_probs, S4_data_test, probability = TRUE)
S4_prediction_probs_polynomial <-
  attr(S4_prob_svm_polynomial, "probabilities")[, 1]
S4_roc_polynomial <-
  roc(S4_data_test$y,
      S4_prediction_probs_polynomial,
      levels = rev(levels(S4_data_test$y)))

S4_svm_radial_probs <-
  svm(
    y ~ .,
    data = S4_data_train,
    kernel = "radial",
    cost = S4_opt_param_radial$Best_Par["cost"],
    gamma = S4_opt_param_radial$Best_Par["gamma"],
    probability = TRUE
  )
S4_prob_svm_radial <-
  predict(S4_svm_radial_probs, S4_data_test, probability = TRUE)
S4_prediction_probs_radial <-
  attr(S4_prob_svm_radial, "probabilities")[, 1]
S4_roc_radial <-
  roc(S4_data_test$y,
      S4_prediction_probs_radial, 
      levels = rev(levels(S4_data_test$y)))

S4_prob_logR <-
  predict(S4_logR, 
          as.matrix(S4_data_test[, setdiff(names(S4_data_test), "y")]), 
          type = "response")
S4_roc_logR <-
  roc(S4_data_test$y,
      as.numeric(S4_prob_logR), 
      levels = rev(levels(S4_data_test$y)))

S4_k_NN_probs <-
  knn(
    S4_data_train[, setdiff(names(S4_data_train), "y")],
    S4_data_test[, setdiff(names(S4_data_test), "y")],
    S4_data_train[, "y"],
    k = S4_opt_param_k_NN$Best_Par["k"],
    prob = TRUE
  )
S4_prediction_probs_k_NN <- attr(S4_k_NN_probs, "prob")
S4_prediction_probs_k_NN <-
  ifelse(
    S4_k_NN_probs == levels(S4_data_train$y)[1],
    S4_prediction_probs_k_NN,
    1 - S4_prediction_probs_k_NN
  )
S4_roc_k_NN <-
  roc(S4_data_test$y, 
      S4_prediction_probs_k_NN, 
      levels = rev(levels(S4_data_test$y)))

## Szenario 7: p >> n (p = 200, n = 50)

## Datenimport

# laden der Daten für Szenario 7
load(file = "Abgabe/Ergebnisse/Daten/Data_S7.RData")

## Modelle tunen

# laden der durch das Tuning ermittelten besten Parameter je Modell
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S7.RData")

S7_tune_linear <- function(cost) {
  model <-
    svm(y ~ .,
        data = S7_data_train,
        kernel = "linear",
        cost = cost)
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

set.seed(71)
S7_opt_param_linear <- BayesianOptimization(
  FUN = S7_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

# alle Ziehungen des Cost-Parameter liefern die selbe Genauigkeit
# es gibt folglich keinen optimalen Parameter
# Default-Wert wird für den Cost-Parameter festgelegt
S7_opt_param_linear <- list(Best_Par = data.frame(cost = 1))

S7_tune_polynomial <- function(cost, gamma, degree) {
  model <-
    svm(
      y ~ .,
      data = S7_data_train,
      kernel = "polynomial",
      cost = cost,
      gamma = gamma,
      degree = degree
    )
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

set.seed(72)
S7_opt_param_polynomial <- BayesianOptimization(
  FUN = S7_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S7_tune_radial <- function(cost, gamma) {
  model <-
    svm(
      y ~ .,
      data = S7_data_train,
      kernel = "radial",
      cost = cost,
      gamma = gamma
    )
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

set.seed(73)
S7_opt_param_radial <- BayesianOptimization(
  FUN = S7_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S7_tune_logR <- function(alpha, lambda) {
  model <-
    glmnet(
      as.matrix(S7_data_train[, setdiff(names(S7_data_train), "y")]),
      S7_data_train[, "y"],
      family = "binomial",
      alpha = alpha,
      lambda = lambda
    )
  probabilities <-
    predict(model,
            as.matrix(S7_data_test[, setdiff(names(S7_data_test), "y")]),
            s = lambda,
            type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

set.seed(74)
S7_opt_param_logR <- BayesianOptimization(
  FUN = S7_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S7_tune_k_NN <- function(k) {
  prediction <-
    knn(S7_data_train[, setdiff(names(S7_data_train), "y")],
        S7_data_test[, setdiff(names(S7_data_test), "y")],
        S7_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

set.seed(75)
S7_opt_param_k_NN <- BayesianOptimization(
  FUN = S7_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

# speichern der durch das Tuning ermittelten besten Parameter je Modell für
# zukünftige Berechnungen
save(
  S7_opt_param_linear,
  S7_opt_param_polynomial,
  S7_opt_param_radial,
  S7_opt_param_logR,
  S7_opt_param_k_NN,
  file = "Abgabe/Ergebnisse/Parameter/Parameter_S7.RData"
)

## Modelle fitten

S7_svm_linear <-
  svm(y ~ .,
      data = S7_data_train,
      kernel = "linear",
      cost = S7_opt_param_linear$Best_Par["cost"])
S7_svm_polynomial <-
  svm(
    y ~ .,
    data = S7_data_train,
    kernel = "polynomial",
    cost = S7_opt_param_polynomial$Best_Par["cost"],
    gamma = S7_opt_param_polynomial$Best_Par["gamma"],
    degree = S7_opt_param_polynomial$Best_Par["degree"]
  )
S7_svm_radial <-
  svm(
    y ~ .,
    data = S7_data_train,
    kernel = "radial",
    cost = S7_opt_param_radial$Best_Par["cost"],
    gamma = S7_opt_param_radial$Best_Par["gamma"]
  )
S7_logR <-
  glmnet(
    as.matrix(S7_data_train[, setdiff(names(S7_data_train), "y")]),
    S7_data_train[, "y"],
    family = "binomial",
    alpha = S7_opt_param_logR$Best_Par["alpha"],
    lambda = S7_opt_param_logR$Best_Par["lambda"]
  )
S7_k_NN <- knn(S7_data_train[, setdiff(names(S7_data_train), "y")],
               S7_data_test[, setdiff(names(S7_data_test), "y")],
               S7_data_train[, "y"],
               k = S7_opt_param_k_NN$Best_Par["k"])

## Predictions

S7_prediction_linear <- predict(S7_svm_linear, S7_data_test)
S7_prediction_polynomial <- predict(S7_svm_polynomial, S7_data_test)
S7_prediction_radial <- predict(S7_svm_radial, S7_data_test)
S7_probabilities_logR <-
  predict(S7_logR,
          as.matrix(S7_data_test[, setdiff(names(S7_data_test), "y")]),
          s = S7_opt_param_logR$Best_Par["lambda"],
          type = "response")
S7_prediction_logR <- ifelse(S7_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

## Accuracy

S7_confusion_matrix_linear <- table(S7_prediction_linear, S7_data_test$y)
S7_accuracy_linear <-
  sum(diag(S7_confusion_matrix_linear)) / sum(S7_confusion_matrix_linear)

S7_confusion_matrix_polynomial <- table(S7_prediction_polynomial, S7_data_test$y)
S7_accuracy_polynomial <-
  sum(diag(S7_confusion_matrix_polynomial)) / sum(S7_confusion_matrix_polynomial)

S7_confusion_matrix_radial <- table(S7_prediction_radial, S7_data_test$y)
S7_accuracy_radial <-
  sum(diag(S7_confusion_matrix_radial)) / sum(S7_confusion_matrix_radial)

S7_confusion_matrix_logR <- table(S7_prediction_logR, S7_data_test$y)
S7_accuracy_logR <-
  sum(diag(S7_confusion_matrix_logR)) / sum(S7_confusion_matrix_logR)

S7_confusion_matrix_k_NN <- table(S7_k_NN, S7_data_test$y)
S7_accuracy_k_NN <-
  sum(diag(S7_confusion_matrix_k_NN)) / sum(S7_confusion_matrix_k_NN)

## F1-Score

S7_F1_linear <- 
  2/(1/(S7_confusion_matrix_linear[1, 1]/
          sum(S7_confusion_matrix_linear[1, ])) + 
       1/(S7_confusion_matrix_linear[1, 1]/
            sum(S7_confusion_matrix_linear[, 1])))
S7_F1_polynomial <- 
  2/(1/(S7_confusion_matrix_polynomial[1, 1]/
          sum(S7_confusion_matrix_polynomial[1, ])) + 
       1/(S7_confusion_matrix_polynomial[1, 1]/
            sum(S7_confusion_matrix_polynomial[, 1])))
S7_F1_radial <- 
  2/(1/(S7_confusion_matrix_radial[1, 1]/
          sum(S7_confusion_matrix_radial[1, ])) + 
       1/(S7_confusion_matrix_radial[1, 1]/
            sum(S7_confusion_matrix_radial[, 1])))
S7_F1_logR <- 
  2/(1/(S7_confusion_matrix_logR[1, 1]/
          sum(S7_confusion_matrix_logR[1, ])) + 
       1/(S7_confusion_matrix_logR[1, 1]/
            sum(S7_confusion_matrix_logR[, 1])))
S7_F1_k_NN <- 
  2/(1/(S7_confusion_matrix_k_NN[1, 1]/
          sum(S7_confusion_matrix_k_NN[1, ])) + 
       1/(S7_confusion_matrix_k_NN[1, 1]/
            sum(S7_confusion_matrix_k_NN[, 1])))

## ROC/AUC

S7_svm_linear_probs <-
  svm(
    y ~ .,
    data = S7_data_train,
    kernel = "linear",
    cost = S7_opt_param_linear$Best_Par["cost"],
    probability = TRUE
  )
S7_prob_svm_linear <-
  predict(S7_svm_linear_probs, S7_data_test, probability = TRUE)
S7_prediction_probs_linear <-
  attr(S7_prob_svm_linear, "probabilities")[, 1]
S7_roc_linear <-
  roc(S7_data_test$y, 
      S7_prediction_probs_linear, 
      levels = rev(levels(S7_data_test$y)))

S7_svm_polynomial_probs <-
  svm(
    y ~ .,
    data = S7_data_train,
    kernel = "polynomial",
    cost = S7_opt_param_polynomial$Best_Par["cost"],
    gamma = S7_opt_param_polynomial$Best_Par["gamma"],
    degree = S7_opt_param_polynomial$Best_Par["degree"],
    probability = TRUE
  )
S7_prob_svm_polynomial <-
  predict(S7_svm_polynomial_probs, S7_data_test, probability = TRUE)
S7_prediction_probs_polynomial <-
  attr(S7_prob_svm_polynomial, "probabilities")[, 1]
S7_roc_polynomial <-
  roc(S7_data_test$y,
      S7_prediction_probs_polynomial,
      levels = rev(levels(S7_data_test$y)))

S7_svm_radial_probs <-
  svm(
    y ~ .,
    data = S7_data_train,
    kernel = "radial",
    cost = S7_opt_param_radial$Best_Par["cost"],
    gamma = S7_opt_param_radial$Best_Par["gamma"],
    probability = TRUE
  )
S7_prob_svm_radial <-
  predict(S7_svm_radial_probs, S7_data_test, probability = TRUE)
S7_prediction_probs_radial <-
  attr(S7_prob_svm_radial, "probabilities")[, 1]
S7_roc_radial <-
  roc(S7_data_test$y, 
      S7_prediction_probs_radial,
      levels = rev(levels(S7_data_test$y)))

S7_prob_logR <-
  predict(S7_logR,
          as.matrix(S7_data_test[, setdiff(names(S7_data_test), "y")]),
          type = "response")
S7_roc_logR <-
  roc(S7_data_test$y,
      as.numeric(S7_prob_logR),
      levels = rev(levels(S7_data_test$y)))

S7_k_NN_probs <-
  knn(
    S7_data_train[, setdiff(names(S7_data_train), "y")],
    S7_data_test[, setdiff(names(S7_data_test), "y")],
    S7_data_train[, "y"],
    k = S7_opt_param_k_NN$Best_Par["k"],
    prob = TRUE
  )
S7_prediction_probs_k_NN <- attr(S7_k_NN_probs, "prob")
S7_prediction_probs_k_NN <-
  ifelse(
    S7_k_NN_probs == levels(S7_data_train$y)[1],
    S7_prediction_probs_k_NN,
    1 - S7_prediction_probs_k_NN
  )
S7_roc_k_NN <-
  roc(S7_data_test$y, 
      S7_prediction_probs_k_NN,
      levels = rev(levels(S7_data_test$y)))

## erstellen der zusammenfassenden Tabellen

# alle Maßzahlen in einem Dataframe zusammenfassen
S1_Tabelle <-
  data.frame(
    ACC = c(
      S1_accuracy_linear,
      S1_accuracy_polynomial,
      S1_accuracy_radial,
      S1_accuracy_logR,
      S1_accuracy_k_NN
    ),
    AUC = c(
      auc(S1_roc_linear),
      auc(S1_roc_polynomial),
      auc(S1_roc_radial),
      auc(S1_roc_logR),
      auc(S1_roc_k_NN)
    ),
    F1 = c(
      S1_F1_linear,
      S1_F1_polynomial,
      S1_F1_radial,
      S1_F1_logR,
      S1_F1_k_NN
    ),
    row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")
  )
# Berechnung der Summe über alle Maßzahlen als Basis für den Rang
S1_index <- rowSums(S1_Tabelle)
# Festlegen und Hinzufügen des Rangs
S1_rank <- data.frame(Rang = rank(-S1_index, ties.method = "min"))
S1_Tabelle <- round(cbind(S1_Tabelle, S1_rank), 3)


S4_Tabelle <-
  data.frame(
    ACC = c(
      S4_accuracy_linear,
      S4_accuracy_polynomial,
      S4_accuracy_radial,
      S4_accuracy_logR,
      S4_accuracy_k_NN
    ),
    AUC = c(
      auc(S4_roc_linear),
      auc(S4_roc_polynomial),
      auc(S4_roc_radial),
      auc(S4_roc_logR),
      auc(S4_roc_k_NN)
    ),
    F1 = c(
      S4_F1_linear,
      S4_F1_polynomial,
      S4_F1_radial,
      S4_F1_logR,
      S4_F1_k_NN
    ),
    row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")
  )
S4_index <- rowSums(S4_Tabelle)
S4_rank <- data.frame(Rang = rank(-S4_index, ties.method = "min"))
S4_Tabelle <- round(cbind(S4_Tabelle, S4_rank), 3)

S7_Tabelle <-
  data.frame(
    ACC = c(
      S7_accuracy_linear,
      S7_accuracy_polynomial,
      S7_accuracy_radial,
      S7_accuracy_logR,
      S7_accuracy_k_NN
    ),
    AUC = c(
      auc(S7_roc_linear),
      auc(S7_roc_polynomial),
      auc(S7_roc_radial),
      auc(S7_roc_logR),
      auc(S7_roc_k_NN)
    ),
    F1 = c(
      S7_F1_linear,
      S7_F1_polynomial,
      S7_F1_radial,
      S7_F1_logR,
      S7_F1_k_NN
    ),
    row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")
  )
S7_index <- rowSums(S7_Tabelle)
S7_rank <- data.frame(Rang = rank(-S7_index, ties.method = "min"))
S7_Tabelle <- round(cbind(S7_Tabelle, S7_rank), 3)

# speichern der Tabellen
save(S1_Tabelle, S4_Tabelle, S7_Tabelle, 
     file = "Abgabe/Ergebnisse/Tabellen/Tabellen_S1_S4_S7.RData")
