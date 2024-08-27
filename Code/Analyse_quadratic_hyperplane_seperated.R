# Pakete
library(e1071)
library(glmnet)
library(class)
library(rBayesianOptimization)
library(gridExtra)
library(pROC)

## Szenario 2: p << n (n = 1000, p = 10)

# Datenimport

load(file = "Abgabe/Ergebnisse/Daten/Data_S2.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S2.RData")

S2_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S2_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S2_data_test)
  accuracy <- mean(prediction == S2_data_test$y)
  list(Score = accuracy)
}

set.seed(21)
S2_opt_param_linear <- BayesianOptimization(
  FUN = S2_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S2_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S2_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S2_data_test)
  accuracy <- mean(prediction == S2_data_test$y)
  list(Score = accuracy)
}

set.seed(22)
S2_opt_param_polynomial <- BayesianOptimization(
  FUN = S2_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S2_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S2_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S2_data_test)
  accuracy <- mean(prediction == S2_data_test$y)
  list(Score = accuracy)
}

set.seed(23)
S2_opt_param_radial <- BayesianOptimization(
  FUN = S2_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S2_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S2_data_train[, setdiff(names(S2_data_train), "y")]), S2_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S2_data_test[, setdiff(names(S2_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S2_data_test$y)
  list(Score = accuracy)
}

set.seed(24)
S2_opt_param_logR <- BayesianOptimization(
  FUN = S2_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S2_tune_k_NN <- function(k) {
  prediction <-
    knn(S2_data_train[, setdiff(names(S2_data_train), "y")],
        S2_data_test[, setdiff(names(S2_data_test), "y")],
        S2_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S2_data_test$y)
  list(Score = accuracy)
}

set.seed(25)
S2_opt_param_k_NN <- BayesianOptimization(
  FUN = S2_tune_k_NN,
  bounds = list(k = c(1, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

save(S2_opt_param_linear, S2_opt_param_polynomial, S2_opt_param_radial, S2_opt_param_logR, S2_opt_param_k_NN, file = "Abgabe/Ergebnisse/Parameter/Parameter_S2.RData")

# Modelle fitten

S2_svm_linear <- svm(y ~., data = S2_data_train, kernel = "linear", cost = S2_opt_param_linear$Best_Par["cost"])
S2_svm_polynomial <- svm(y ~., data = S2_data_train, kernel = "polynomial", cost = S2_opt_param_polynomial$Best_Par["cost"], gamma = S2_opt_param_polynomial$Best_Par["gamma"], degree = S2_opt_param_polynomial$Best_Par["degree"])
S2_svm_radial <- svm(y ~., data = S2_data_train, kernel = "radial", cost = S2_opt_param_radial$Best_Par["cost"], gamma = S2_opt_param_radial$Best_Par["gamma"])
S2_logR <- glmnet(as.matrix(S2_data_train[, setdiff(names(S2_data_train), "y")]), S2_data_train[, "y"], family = "binomial", alpha = S2_opt_param_logR$Best_Par["alpha"], lambda = S2_opt_param_logR$Best_Par["lambda"])
S2_k_NN <- knn(S2_data_train[, setdiff(names(S2_data_train), "y")],
               S2_data_test[, setdiff(names(S2_data_test), "y")],
               S2_data_train[, "y"],
               k = S2_opt_param_k_NN$Best_Par["k"])

# Predictions

S2_prediction_linear <- predict(S2_svm_linear, S2_data_test)
S2_prediction_polynomial <- predict(S2_svm_polynomial, S2_data_test)
S2_prediction_radial <- predict(S2_svm_radial, S2_data_test)
S2_probabilities_logR <- predict(S2_logR, as.matrix(S2_data_test[, setdiff(names(S2_data_test), "y")]), s = S2_opt_param_logR$Best_Par["lambda"], type = "response")
S2_prediction_logR <- ifelse(S2_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S2_confusion_matrix_linear <- table(S2_prediction_linear, S2_data_test$y)
S2_accuracy_linear <- sum(diag(S2_confusion_matrix_linear))/sum(S2_confusion_matrix_linear)

S2_confusion_matrix_polynomial <- table(S2_prediction_polynomial, S2_data_test$y)
S2_accuracy_polynomial <- sum(diag(S2_confusion_matrix_polynomial))/sum(S2_confusion_matrix_polynomial)

S2_confusion_matrix_radial <- table(S2_prediction_radial, S2_data_test$y)
S2_accuracy_radial <- sum(diag(S2_confusion_matrix_radial))/sum(S2_confusion_matrix_radial)

S2_confusion_matrix_logR <- table(S2_prediction_logR, S2_data_test$y)
S2_accuracy_logR <- sum(diag(S2_confusion_matrix_logR))/sum(S2_confusion_matrix_logR)

S2_confusion_matrix_k_NN <- table(S2_k_NN, S2_data_test$y)
S2_accuracy_k_NN <- sum(diag(S2_confusion_matrix_k_NN))/sum(S2_confusion_matrix_k_NN)

# F1-Score

S2_F1_linear <- 2/(1/(S2_confusion_matrix_linear[1, 1]/sum(S2_confusion_matrix_linear[1, ])) + 1/(S2_confusion_matrix_linear[1, 1]/sum(S2_confusion_matrix_linear[, 1])))
S2_F1_polynomial <- 2/(1/(S2_confusion_matrix_polynomial[1, 1]/sum(S2_confusion_matrix_polynomial[1, ])) + 1/(S2_confusion_matrix_polynomial[1, 1]/sum(S2_confusion_matrix_polynomial[, 1])))
S2_F1_radial <- 2/(1/(S2_confusion_matrix_radial[1, 1]/sum(S2_confusion_matrix_radial[1, ])) + 1/(S2_confusion_matrix_radial[1, 1]/sum(S2_confusion_matrix_radial[, 1])))
S2_F1_logR <- 2/(1/(S2_confusion_matrix_logR[1, 1]/sum(S2_confusion_matrix_logR[1, ])) + 1/(S2_confusion_matrix_logR[1, 1]/sum(S2_confusion_matrix_logR[, 1])))
S2_F1_k_NN <- 2/(1/(S2_confusion_matrix_k_NN[1, 1]/sum(S2_confusion_matrix_k_NN[1, ])) + 1/(S2_confusion_matrix_k_NN[1, 1]/sum(S2_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S2_svm_linear_probs <- svm(y ~., data = S2_data_train, kernel = "linear", cost = S2_opt_param_linear$Best_Par["cost"], probability = TRUE)
S2_prob_svm_linear <- predict(S2_svm_linear_probs, S2_data_test, probability = TRUE)
S2_prediction_probs_linear <- attr(S2_prob_svm_linear, "probabilities")[, 1]
S2_roc_linear <- roc(S2_data_test$y, S2_prediction_probs_linear, levels = rev(levels(S2_data_test$y)))

S2_svm_polynomial_probs <- svm(y ~., data = S2_data_train, kernel = "polynomial", cost = S2_opt_param_polynomial$Best_Par["cost"], gamma = S2_opt_param_polynomial$Best_Par["gamma"], degree = S2_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S2_prob_svm_polynomial <- predict(S2_svm_polynomial_probs, S2_data_test, probability = TRUE)
S2_prediction_probs_polynomial <- attr(S2_prob_svm_polynomial, "probabilities")[, 1]
S2_roc_polynomial <- roc(S2_data_test$y, S2_prediction_probs_polynomial, levels = rev(levels(S2_data_test$y)))

S2_svm_radial_probs <- svm(y ~., data = S2_data_train, kernel = "radial", cost = S2_opt_param_radial$Best_Par["cost"], gamma = S2_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S2_prob_svm_radial <- predict(S2_svm_radial_probs, S2_data_test, probability = TRUE)
S2_prediction_probs_radial <- attr(S2_prob_svm_radial, "probabilities")[, 1]
S2_roc_radial <- roc(S2_data_test$y, S2_prediction_probs_radial, levels = rev(levels(S2_data_test$y)))

S2_prob_logR <- predict(S2_logR, as.matrix(S2_data_test[, setdiff(names(S2_data_test), "y")]), type = "response")
S2_roc_logR <- roc(S2_data_test$y, as.numeric(S2_prob_logR), levels = rev(levels(S2_data_test$y)))

S2_k_NN_probs <- knn(S2_data_train[, setdiff(names(S2_data_train), "y")],
                     S2_data_test[, setdiff(names(S2_data_test), "y")],
                     S2_data_train[, "y"],
                     k = S2_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S2_prediction_probs_k_NN <- attr(S2_k_NN_probs, "prob")
S2_prediction_probs_k_NN <- ifelse(S2_k_NN_probs == levels(S2_data_train$y)[1], S2_prediction_probs_k_NN, 1 - S2_prediction_probs_k_NN)
S2_roc_k_NN <- roc(S2_data_test$y, S2_prediction_probs_k_NN, levels = rev(levels(S2_data_test$y)))

#speichern der ROC-Daten für ROC-Kurven
save(S2_roc_linear, S2_roc_polynomial, S2_roc_radial, S2_roc_logR, S2_roc_k_NN, file = "Abgabe/Ergebnisse/ROC/S2_roc.RData")

## Szenario 5: p = n (p = 50, n = 50)

# Datengenerierung

load(file = "Abgabe/Ergebnisse/Daten/Data_S5.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S5.RData")

S5_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S5_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S5_data_test)
  accuracy <- mean(prediction == S5_data_test$y)
  list(Score = accuracy)
}

set.seed(51)
S5_opt_param_linear <- BayesianOptimization(
  FUN = S5_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)
# kein tuning möglich, da kein optimaler Parameter vorhanden -> default Werte
S5_opt_param_linear <- list(Best_Par = data.frame(cost = 1))

S5_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S5_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S5_data_test)
  accuracy <- mean(prediction == S5_data_test$y)
  list(Score = accuracy)
}

set.seed(52)
S5_opt_param_polynomial <- BayesianOptimization(
  FUN = S5_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S5_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S5_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S5_data_test)
  accuracy <- mean(prediction == S5_data_test$y)
  list(Score = accuracy)
}

set.seed(53)
S5_opt_param_radial <- BayesianOptimization(
  FUN = S5_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S5_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S5_data_train[, setdiff(names(S5_data_train), "y")]), S5_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S5_data_test[, setdiff(names(S5_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S5_data_test$y)
  list(Score = accuracy)
}

set.seed(54)
S5_opt_param_logR <- BayesianOptimization(
  FUN = S5_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S5_tune_k_NN <- function(k) {
  prediction <-
    knn(S5_data_train[, setdiff(names(S5_data_train), "y")],
        S5_data_test[, setdiff(names(S5_data_test), "y")],
        S5_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S5_data_test$y)
  list(Score = accuracy)
}

set.seed(55)
S5_opt_param_k_NN <- BayesianOptimization(
  FUN = S5_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

save(S5_opt_param_linear, S5_opt_param_polynomial, S5_opt_param_radial, S5_opt_param_logR, S5_opt_param_k_NN, file = "Abgabe/Ergebnisse/Parameter/Parameter_S5.RData")

# Modelle fitten

S5_svm_linear <- svm(y ~., data = S5_data_train, kernel = "linear", cost = S5_opt_param_linear$Best_Par["cost"])
S5_svm_polynomial <- svm(y ~., data = S5_data_train, kernel = "polynomial", cost = S5_opt_param_polynomial$Best_Par["cost"], gamma = S5_opt_param_polynomial$Best_Par["gamma"], degree = S5_opt_param_polynomial$Best_Par["degree"])
S5_svm_radial <- svm(y ~., data = S5_data_train, kernel = "radial", cost = S5_opt_param_radial$Best_Par["cost"], gamma = S5_opt_param_radial$Best_Par["gamma"])
S5_logR <- glmnet(as.matrix(S5_data_train[, setdiff(names(S5_data_train), "y")]), S5_data_train[, "y"], family = "binomial", alpha = S5_opt_param_logR$Best_Par["alpha"], lambda = S5_opt_param_logR$Best_Par["lambda"])
S5_k_NN <- knn(S5_data_train[, setdiff(names(S5_data_train), "y")],
               S5_data_test[, setdiff(names(S5_data_test), "y")],
               S5_data_train[, "y"],
               k = S5_opt_param_k_NN$Best_Par["k"])

# Predictions

S5_prediction_linear <- predict(S5_svm_linear, S5_data_test)
S5_prediction_polynomial <- predict(S5_svm_polynomial, S5_data_test)
S5_prediction_radial <- predict(S5_svm_radial, S5_data_test)
S5_probabilities_logR <- predict(S5_logR, as.matrix(S5_data_test[, setdiff(names(S5_data_test), "y")]), s = S5_opt_param_logR$Best_Par["lambda"], type = "response")
S5_prediction_logR <- ifelse(S5_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S5_confusion_matrix_linear <- table(S5_prediction_linear, S5_data_test$y)
S5_accuracy_linear <- sum(diag(S5_confusion_matrix_linear))/sum(S5_confusion_matrix_linear)

S5_confusion_matrix_polynomial <- table(S5_prediction_polynomial, S5_data_test$y)
S5_accuracy_polynomial <- sum(diag(S5_confusion_matrix_polynomial))/sum(S5_confusion_matrix_polynomial)

S5_confusion_matrix_radial <- table(S5_prediction_radial, S5_data_test$y)
S5_accuracy_radial <- sum(diag(S5_confusion_matrix_radial))/sum(S5_confusion_matrix_radial)

S5_confusion_matrix_logR <- table(S5_prediction_logR, S5_data_test$y)
S5_accuracy_logR <- sum(diag(S5_confusion_matrix_logR))/sum(S5_confusion_matrix_logR)

S5_confusion_matrix_k_NN <- table(S5_k_NN, S5_data_test$y)
S5_accuracy_k_NN <- sum(diag(S5_confusion_matrix_k_NN))/sum(S5_confusion_matrix_k_NN)

# F1-Score

S5_F1_linear <- 2/(1/(S5_confusion_matrix_linear[1, 1]/sum(S5_confusion_matrix_linear[1, ])) + 1/(S5_confusion_matrix_linear[1, 1]/sum(S5_confusion_matrix_linear[, 1])))
S5_F1_polynomial <- 2/(1/(S5_confusion_matrix_polynomial[1, 1]/sum(S5_confusion_matrix_polynomial[1, ])) + 1/(S5_confusion_matrix_polynomial[1, 1]/sum(S5_confusion_matrix_polynomial[, 1])))
S5_F1_radial <- 2/(1/(S5_confusion_matrix_radial[1, 1]/sum(S5_confusion_matrix_radial[1, ])) + 1/(S5_confusion_matrix_radial[1, 1]/sum(S5_confusion_matrix_radial[, 1])))
S5_F1_logR <- 2/(1/(S5_confusion_matrix_logR[1, 1]/sum(S5_confusion_matrix_logR[1, ])) + 1/(S5_confusion_matrix_logR[1, 1]/sum(S5_confusion_matrix_logR[, 1])))
S5_F1_k_NN <- 2/(1/(S5_confusion_matrix_k_NN[1, 1]/sum(S5_confusion_matrix_k_NN[1, ])) + 1/(S5_confusion_matrix_k_NN[1, 1]/sum(S5_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S5_svm_linear_probs <- svm(y ~., data = S5_data_train, kernel = "linear", cost = S5_opt_param_linear$Best_Par["cost"], probability = TRUE)
S5_prob_svm_linear <- predict(S5_svm_linear_probs, S5_data_test, probability = TRUE)
S5_prediction_probs_linear <- attr(S5_prob_svm_linear, "probabilities")[, 1]
S5_roc_linear <- roc(S5_data_test$y, S5_prediction_probs_linear, levels = rev(levels(S5_data_test$y)))

S5_svm_polynomial_probs <- svm(y ~., data = S5_data_train, kernel = "polynomial", cost = S5_opt_param_polynomial$Best_Par["cost"], gamma = S5_opt_param_polynomial$Best_Par["gamma"], degree = S5_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S5_prob_svm_polynomial <- predict(S5_svm_polynomial_probs, S5_data_test, probability = TRUE)
S5_prediction_probs_polynomial <- attr(S5_prob_svm_polynomial, "probabilities")[, 1]
S5_roc_polynomial <- roc(S5_data_test$y, S5_prediction_probs_polynomial, levels = rev(levels(S5_data_test$y)))

S5_svm_radial_probs <- svm(y ~., data = S5_data_train, kernel = "radial", cost = S5_opt_param_radial$Best_Par["cost"], gamma = S5_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S5_prob_svm_radial <- predict(S5_svm_radial_probs, S5_data_test, probability = TRUE)
S5_prediction_probs_radial <- attr(S5_prob_svm_radial, "probabilities")[, 1]
S5_roc_radial <- roc(S5_data_test$y, S5_prediction_probs_radial, levels = rev(levels(S5_data_test$y)))

S5_prob_logR <- predict(S5_logR, as.matrix(S5_data_test[, setdiff(names(S5_data_test), "y")]), type = "response")
S5_roc_logR <- roc(S5_data_test$y, as.numeric(S5_prob_logR), levels = rev(levels(S5_data_test$y)))

S5_k_NN_probs <- knn(S5_data_train[, setdiff(names(S5_data_train), "y")],
                     S5_data_test[, setdiff(names(S5_data_test), "y")],
                     S5_data_train[, "y"],
                     k = S5_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S5_prediction_probs_k_NN <- attr(S5_k_NN_probs, "prob")
S5_prediction_probs_k_NN <- ifelse(S5_k_NN_probs == levels(S5_data_train$y)[1], S5_prediction_probs_k_NN, 1 - S5_prediction_probs_k_NN)
S5_roc_k_NN <- roc(S5_data_test$y, S5_prediction_probs_k_NN, levels = rev(levels(S5_data_test$y)))

## Szenario 8: p >> n (p = 200, n = 50)

# Datengenerierung

load(file = "Abgabe/Ergebnisse/Daten/Data_S8.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Abgabe/Ergebnisse/Parameter/Parameter_S8.RData")

S8_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S8_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S8_data_test)
  accuracy <- mean(prediction == S8_data_test$y)
  list(Score = accuracy)
}

set.seed(81)
S8_opt_param_linear <- BayesianOptimization(
  FUN = S8_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)
# kein tuning möglich, da kein optimaler Parameter vorhanden -> default Werte
S8_opt_param_linear <- list(Best_Par = data.frame(cost = 1))

S8_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S8_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S8_data_test)
  accuracy <- mean(prediction == S8_data_test$y)
  list(Score = accuracy)
}

set.seed(82)
S8_opt_param_polynomial <- BayesianOptimization(
  FUN = S8_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S8_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S8_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S8_data_test)
  accuracy <- mean(prediction == S8_data_test$y)
  list(Score = accuracy)
}

set.seed(83)
S8_opt_param_radial <- BayesianOptimization(
  FUN = S8_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S8_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S8_data_train[, setdiff(names(S8_data_train), "y")]), S8_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S8_data_test[, setdiff(names(S8_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S8_data_test$y)
  list(Score = accuracy)
}

set.seed(84)
S8_opt_param_logR <- BayesianOptimization(
  FUN = S8_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S8_tune_k_NN <- function(k) {
  prediction <-
    knn(S8_data_train[, setdiff(names(S8_data_train), "y")],
        S8_data_test[, setdiff(names(S8_data_test), "y")],
        S8_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S8_data_test$y)
  list(Score = accuracy)
}

set.seed(85)
S8_opt_param_k_NN <- BayesianOptimization(
  FUN = S8_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

save(S8_opt_param_linear, S8_opt_param_polynomial, S8_opt_param_radial, S8_opt_param_logR, S8_opt_param_k_NN, file = "Abgabe/Ergebnisse/Parameter/Parameter_S8.RData")

# Modelle fitten

# default Werte für linear, da kein optimaler Parameter vorhanden
S8_svm_linear <- svm(y ~., data = S8_data_train, kernel = "linear", cost = S8_opt_param_linear$Best_Par["cost"])
S8_svm_polynomial <- svm(y ~., data = S8_data_train, kernel = "polynomial", cost = S8_opt_param_polynomial$Best_Par["cost"], gamma = S8_opt_param_polynomial$Best_Par["gamma"], degree = S8_opt_param_polynomial$Best_Par["degree"])
S8_svm_radial <- svm(y ~., data = S8_data_train, kernel = "radial", cost = S8_opt_param_radial$Best_Par["cost"], gamma = S8_opt_param_radial$Best_Par["gamma"])
S8_logR <- glmnet(as.matrix(S8_data_train[, setdiff(names(S8_data_train), "y")]), S8_data_train[, "y"], family = "binomial", alpha = S8_opt_param_logR$Best_Par["alpha"], lambda = S8_opt_param_logR$Best_Par["lambda"])
S8_k_NN <- knn(S8_data_train[, setdiff(names(S8_data_train), "y")],
               S8_data_test[, setdiff(names(S8_data_test), "y")],
               S8_data_train[, "y"],
               k = S8_opt_param_k_NN$Best_Par["k"])

# Predictions

S8_prediction_linear <- predict(S8_svm_linear, S8_data_test)
S8_prediction_polynomial <- predict(S8_svm_polynomial, S8_data_test)
S8_prediction_radial <- predict(S8_svm_radial, S8_data_test)
S8_probabilities_logR <- predict(S8_logR, as.matrix(S8_data_test[, setdiff(names(S8_data_test), "y")]), s = S8_opt_param_logR$Best_Par["lambda"], type = "response")
S8_prediction_logR <- ifelse(S8_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S8_confusion_matrix_linear <- table(S8_prediction_linear, S8_data_test$y)
S8_accuracy_linear <- sum(diag(S8_confusion_matrix_linear))/sum(S8_confusion_matrix_linear)

S8_confusion_matrix_polynomial <- table(S8_prediction_polynomial, S8_data_test$y)
S8_accuracy_polynomial <- sum(diag(S8_confusion_matrix_polynomial))/sum(S8_confusion_matrix_polynomial)

S8_confusion_matrix_radial <- table(S8_prediction_radial, S8_data_test$y)
S8_accuracy_radial <- sum(diag(S8_confusion_matrix_radial))/sum(S8_confusion_matrix_radial)

S8_confusion_matrix_logR <- table(S8_prediction_logR, S8_data_test$y)
S8_accuracy_logR <- sum(diag(S8_confusion_matrix_logR))/sum(S8_confusion_matrix_logR)

S8_confusion_matrix_k_NN <- table(S8_k_NN, S8_data_test$y)
S8_accuracy_k_NN <- sum(diag(S8_confusion_matrix_k_NN))/sum(S8_confusion_matrix_k_NN)

# F1-Score

S8_F1_linear <- 2/(1/(S8_confusion_matrix_linear[1, 1]/sum(S8_confusion_matrix_linear[1, ])) + 1/(S8_confusion_matrix_linear[1, 1]/sum(S8_confusion_matrix_linear[, 1])))
S8_F1_polynomial <- 2/(1/(S8_confusion_matrix_polynomial[1, 1]/sum(S8_confusion_matrix_polynomial[1, ])) + 1/(S8_confusion_matrix_polynomial[1, 1]/sum(S8_confusion_matrix_polynomial[, 1])))
S8_F1_radial <- 2/(1/(S8_confusion_matrix_radial[1, 1]/sum(S8_confusion_matrix_radial[1, ])) + 1/(S8_confusion_matrix_radial[1, 1]/sum(S8_confusion_matrix_radial[, 1])))
S8_F1_logR <- 2/(1/(S8_confusion_matrix_logR[1, 1]/sum(S8_confusion_matrix_logR[1, ])) + 1/(S8_confusion_matrix_logR[1, 1]/sum(S8_confusion_matrix_logR[, 1])))
S8_F1_k_NN <- 2/(1/(S8_confusion_matrix_k_NN[1, 1]/sum(S8_confusion_matrix_k_NN[1, ])) + 1/(S8_confusion_matrix_k_NN[1, 1]/sum(S8_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S8_svm_linear_probs <- svm(y ~., data = S8_data_train, kernel = "linear", cost = S8_opt_param_linear$Best_Par["cost"], probability = TRUE)
S8_prob_svm_linear <- predict(S8_svm_linear_probs, S8_data_test, probability = TRUE)
S8_prediction_probs_linear <- attr(S8_prob_svm_linear, "probabilities")[, 1]
S8_roc_linear <- roc(S8_data_test$y, S8_prediction_probs_linear, levels = rev(levels(S8_data_test$y)))

S8_svm_polynomial_probs <- svm(y ~., data = S8_data_train, kernel = "polynomial", cost = S8_opt_param_polynomial$Best_Par["cost"], gamma = S8_opt_param_polynomial$Best_Par["gamma"], degree = S8_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S8_prob_svm_polynomial <- predict(S8_svm_polynomial_probs, S8_data_test, probability = TRUE)
S8_prediction_probs_polynomial <- attr(S8_prob_svm_polynomial, "probabilities")[, 1]
S8_roc_polynomial <- roc(S8_data_test$y, S8_prediction_probs_polynomial, levels = rev(levels(S8_data_test$y)))

S8_svm_radial_probs <- svm(y ~., data = S8_data_train, kernel = "radial", cost = S8_opt_param_radial$Best_Par["cost"], gamma = S8_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S8_prob_svm_radial <- predict(S8_svm_radial_probs, S8_data_test, probability = TRUE)
S8_prediction_probs_radial <- attr(S8_prob_svm_radial, "probabilities")[, 1]
S8_roc_radial <- roc(S8_data_test$y, S8_prediction_probs_radial, levels = rev(levels(S8_data_test$y)))

S8_prob_logR <- predict(S8_logR, as.matrix(S8_data_test[, setdiff(names(S8_data_test), "y")]), type = "response")
S8_roc_logR <- roc(S8_data_test$y, as.numeric(S8_prob_logR), levels = rev(levels(S8_data_test$y)))

S8_k_NN_probs <- knn(S8_data_train[, setdiff(names(S8_data_train), "y")],
                     S8_data_test[, setdiff(names(S8_data_test), "y")],
                     S8_data_train[, "y"],
                     k = S8_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S8_prediction_probs_k_NN <- attr(S8_k_NN_probs, "prob")
S8_prediction_probs_k_NN <- ifelse(S8_k_NN_probs == levels(S8_data_train$y)[1], S8_prediction_probs_k_NN, 1 - S8_prediction_probs_k_NN)
S8_roc_k_NN <- roc(S8_data_test$y, S8_prediction_probs_k_NN, levels = rev(levels(S8_data_test$y)))

# erstellen der Einzeltabellen

S2_Tabelle <- data.frame(ACC = c(S2_accuracy_linear, S2_accuracy_polynomial, S2_accuracy_radial, S2_accuracy_logR, S2_accuracy_k_NN),
                         AUC = c(auc(S2_roc_linear), auc(S2_roc_polynomial), auc(S2_roc_radial), auc(S2_roc_logR), auc(S2_roc_k_NN)),
                         F1 = c(S2_F1_linear, S2_F1_polynomial, S2_F1_radial, S2_F1_logR, S2_F1_k_NN),
                         row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN"))
S2_index <- rowSums(S2_Tabelle)
S2_rank <- data.frame(Rang = rank(-S2_index, ties.method = "min"))
S2_Tabelle <- round(cbind(S2_Tabelle, S2_rank), 3)


S5_Tabelle <- data.frame(ACC = c(S5_accuracy_linear, S5_accuracy_polynomial, S5_accuracy_radial, S5_accuracy_logR, S5_accuracy_k_NN),
                         AUC = c(auc(S5_roc_linear), auc(S5_roc_polynomial), auc(S5_roc_radial), auc(S5_roc_logR), auc(S5_roc_k_NN)),
                         F1 = c(S5_F1_linear, S5_F1_polynomial, S5_F1_radial, S5_F1_logR, S5_F1_k_NN),
                         row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN"))
S5_index <- rowSums(S5_Tabelle)
S5_rank <- data.frame(Rang = rank(-S5_index, ties.method = "min"))
S5_Tabelle <- round(cbind(S5_Tabelle, S5_rank), 3)

S8_Tabelle <- data.frame(ACC = c(S8_accuracy_linear, S8_accuracy_polynomial, S8_accuracy_radial, S8_accuracy_logR, S8_accuracy_k_NN),
                         AUC = c(auc(S8_roc_linear), auc(S8_roc_polynomial), auc(S8_roc_radial), auc(S8_roc_logR), auc(S8_roc_k_NN)),
                         F1 = c(S8_F1_linear, S8_F1_polynomial, S8_F1_radial, S8_F1_logR, S8_F1_k_NN),
                         row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN"))
S8_index <- rowSums(S8_Tabelle)
S8_rank <- data.frame(Rang = rank(-S8_index, ties.method = "min"))
S8_Tabelle <- round(cbind(S8_Tabelle, S8_rank), 3)

save(S2_Tabelle, S5_Tabelle, S8_Tabelle, file = "Abgabe/Ergebnisse/Tabellen/Tabellen_S2_S5_S8.RData")
