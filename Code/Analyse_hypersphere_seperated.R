# Pakete
library(e1071)
library(glmnet)
library(class)
library(rBayesianOptimization)
library(gridExtra)
library(pROC)

## Szenario 3: p << n (n = 1000, p = 10)

# Datenimport

load(file = "Code/Daten/Data_S3.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Code/Parameter/Parameter_S3.RData")

S3_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S3_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S3_data_test)
  accuracy <- mean(prediction == S3_data_test$y)
  list(Score = accuracy)
}

set.seed(31)
S3_opt_param_linear <- BayesianOptimization(
  FUN = S3_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S3_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S3_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S3_data_test)
  accuracy <- mean(prediction == S3_data_test$y)
  list(Score = accuracy)
}

set.seed(32)
S3_opt_param_polynomial <- BayesianOptimization(
  FUN = S3_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S3_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S3_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S3_data_test)
  accuracy <- mean(prediction == S3_data_test$y)
  list(Score = accuracy)
}

set.seed(33)
S3_opt_param_radial <- BayesianOptimization(
  FUN = S3_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S3_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S3_data_train[, setdiff(names(S3_data_train), "y")]), S3_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S3_data_test[, setdiff(names(S3_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S3_data_test$y)
  list(Score = accuracy)
}

set.seed(34)
S3_opt_param_logR <- BayesianOptimization(
  FUN = S3_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S3_tune_k_NN <- function(k) {
  prediction <-
    knn(S3_data_train[, setdiff(names(S3_data_train), "y")],
        S3_data_test[, setdiff(names(S3_data_test), "y")],
        S3_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S3_data_test$y)
  list(Score = accuracy)
}

set.seed(35)
S3_opt_param_k_NN <- BayesianOptimization(
  FUN = S3_tune_k_NN,
  bounds = list(k = c(1, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

save(S3_opt_param_linear, S3_opt_param_polynomial, S3_opt_param_radial, S3_opt_param_logR, S3_opt_param_k_NN, file = "Code/Parameter/Parameter_S3.RData")

# Modelle fitten

S3_svm_linear <- svm(y ~., data = S3_data_train, kernel = "linear", cost = S3_opt_param_linear$Best_Par["cost"])
S3_svm_polynomial <- svm(y ~., data = S3_data_train, kernel = "polynomial", cost = S3_opt_param_polynomial$Best_Par["cost"], gamma = S3_opt_param_polynomial$Best_Par["gamma"], degree = S3_opt_param_polynomial$Best_Par["degree"])
S3_svm_radial <- svm(y ~., data = S3_data_train, kernel = "radial", cost = S3_opt_param_radial$Best_Par["cost"], gamma = S3_opt_param_radial$Best_Par["gamma"])
S3_logR <- glmnet(as.matrix(S3_data_train[, setdiff(names(S3_data_train), "y")]), S3_data_train[, "y"], family = "binomial", alpha = S3_opt_param_logR$Best_Par["alpha"], lambda = S3_opt_param_logR$Best_Par["lambda"])
S3_k_NN <- knn(S3_data_train[, setdiff(names(S3_data_train), "y")],
               S3_data_test[, setdiff(names(S3_data_test), "y")],
               S3_data_train[, "y"],
               k = S3_opt_param_k_NN$Best_Par["k"])

# Predictions

S3_prediction_linear <- predict(S3_svm_linear, S3_data_test)
S3_prediction_polynomial <- predict(S3_svm_polynomial, S3_data_test)
S3_prediction_radial <- predict(S3_svm_radial, S3_data_test)
S3_probabilities_logR <- predict(S3_logR, as.matrix(S3_data_test[, setdiff(names(S3_data_test), "y")]), s = S3_opt_param_logR$Best_Par["lambda"], type = "response")
S3_prediction_logR <- ifelse(S3_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S3_confusion_matrix_linear <- table(S3_prediction_linear, S3_data_test$y)
S3_accuracy_linear <- sum(diag(S3_confusion_matrix_linear))/sum(S3_confusion_matrix_linear)

S3_confusion_matrix_polynomial <- table(S3_prediction_polynomial, S3_data_test$y)
S3_accuracy_polynomial <- sum(diag(S3_confusion_matrix_polynomial))/sum(S3_confusion_matrix_polynomial)

S3_confusion_matrix_radial <- table(S3_prediction_radial, S3_data_test$y)
S3_accuracy_radial <- sum(diag(S3_confusion_matrix_radial))/sum(S3_confusion_matrix_radial)

S3_confusion_matrix_logR <- table(S3_prediction_logR, S3_data_test$y)
S3_accuracy_logR <- sum(diag(S3_confusion_matrix_logR))/sum(S3_confusion_matrix_logR)

S3_confusion_matrix_k_NN <- table(S3_k_NN, S3_data_test$y)
S3_accuracy_k_NN <- sum(diag(S3_confusion_matrix_k_NN))/sum(S3_confusion_matrix_k_NN)

S3_Accuracy <- data.frame(linear = c(as.character(round(S3_accuracy_linear, 4)), as.character(round(S3_opt_param_linear$Best_Par["cost"], 4)), "/", "/", "/", "/", "/"),
                          polynomial = c(as.character(round(S3_accuracy_polynomial, 4)), as.character(round(S3_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S3_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S3_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
                          radial = c(as.character(round(S3_accuracy_radial, 4)), as.character(round(S3_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S3_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
                          logR = c(as.character(round(S3_accuracy_logR, 4)), "/", "/", "/", as.character(round(S3_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S3_opt_param_logR$Best_Par["lambda"], 4)), "/"),
                          k_NN = c(as.character(round(S3_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S3_opt_param_k_NN$Best_Par["k"], 4))),
                          row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S3_Accuracy_Tabelle <- tableGrob(S3_Accuracy)
grid.arrange(S3_Accuracy_Tabelle)

# F1-Score

S3_F1_linear <- 2/(1/(S3_confusion_matrix_linear[1, 1]/sum(S3_confusion_matrix_linear[1, ])) + 1/(S3_confusion_matrix_linear[1, 1]/sum(S3_confusion_matrix_linear[, 1])))
S3_F1_polynomial <- 2/(1/(S3_confusion_matrix_polynomial[1, 1]/sum(S3_confusion_matrix_polynomial[1, ])) + 1/(S3_confusion_matrix_polynomial[1, 1]/sum(S3_confusion_matrix_polynomial[, 1])))
S3_F1_radial <- 2/(1/(S3_confusion_matrix_radial[1, 1]/sum(S3_confusion_matrix_radial[1, ])) + 1/(S3_confusion_matrix_radial[1, 1]/sum(S3_confusion_matrix_radial[, 1])))
S3_F1_logR <- 2/(1/(S3_confusion_matrix_logR[1, 1]/sum(S3_confusion_matrix_logR[1, ])) + 1/(S3_confusion_matrix_logR[1, 1]/sum(S3_confusion_matrix_logR[, 1])))
S3_F1_k_NN <- 2/(1/(S3_confusion_matrix_k_NN[1, 1]/sum(S3_confusion_matrix_k_NN[1, ])) + 1/(S3_confusion_matrix_k_NN[1, 1]/sum(S3_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S3_svm_linear_probs <- svm(y ~., data = S3_data_train, kernel = "linear", cost = S3_opt_param_linear$Best_Par["cost"], probability = TRUE)
S3_prob_svm_linear <- predict(S3_svm_linear_probs, S3_data_test, probability = TRUE)
S3_prediction_probs_linear <- attr(S3_prob_svm_linear, "probabilities")[, 1]
S3_roc_linear <- roc(S3_data_test$y, S3_prediction_probs_linear, levels = rev(levels(S3_data_test$y)))

S3_svm_polynomial_probs <- svm(y ~., data = S3_data_train, kernel = "polynomial", cost = S3_opt_param_polynomial$Best_Par["cost"], gamma = S3_opt_param_polynomial$Best_Par["gamma"], degree = S3_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S3_prob_svm_polynomial <- predict(S3_svm_polynomial_probs, S3_data_test, probability = TRUE)
S3_prediction_probs_polynomial <- attr(S3_prob_svm_polynomial, "probabilities")[, 1]
S3_roc_polynomial <- roc(S3_data_test$y, S3_prediction_probs_polynomial, levels = rev(levels(S3_data_test$y)))

S3_svm_radial_probs <- svm(y ~., data = S3_data_train, kernel = "radial", cost = S3_opt_param_radial$Best_Par["cost"], gamma = S3_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S3_prob_svm_radial <- predict(S3_svm_radial_probs, S3_data_test, probability = TRUE)
S3_prediction_probs_radial <- attr(S3_prob_svm_radial, "probabilities")[, 1]
S3_roc_radial <- roc(S3_data_test$y, S3_prediction_probs_radial, levels = rev(levels(S3_data_test$y)))

S3_prob_logR <- predict(S3_logR, as.matrix(S3_data_test[, setdiff(names(S3_data_test), "y")]), type = "response")
S3_roc_logR <- roc(S3_data_test$y, as.numeric(S3_prob_logR), levels = rev(levels(S3_data_test$y)))

S3_k_NN_probs <- knn(S3_data_train[, setdiff(names(S3_data_train), "y")],
                     S3_data_test[, setdiff(names(S3_data_test), "y")],
                     S3_data_train[, "y"],
                     k = S3_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S3_prediction_probs_k_NN <- attr(S3_k_NN_probs, "prob")
S3_prediction_probs_k_NN <- ifelse(S3_k_NN_probs == levels(S3_data_train$y)[1], S3_prediction_probs_k_NN, 1 - S3_prediction_probs_k_NN)
S3_roc_k_NN <- roc(S3_data_test$y, S3_prediction_probs_k_NN, levels = rev(levels(S3_data_test$y)))

plot(S3_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S3_roc_polynomial, col = "red", add = TRUE)
plot(S3_roc_radial, col = "green", add = TRUE)
plot(S3_roc_logR, col = "violet", add = TRUE)
plot(S3_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S3_roc_linear), ")"), paste("polynomial (AUC:", auc(S3_roc_polynomial), ")"), paste("radial (AUC:", auc(S3_roc_radial), ")"), paste("logR (AUC:", auc(S3_roc_logR), ")"), paste("k_NN (AUC:", auc(S3_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 6: p = n (p = 50, n = 50)

# Datengenerierung

load(file = "Code/Daten/Data_S6.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Code/Parameter/Parameter_S6.RData")

S6_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S6_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S6_data_test)
  accuracy <- mean(prediction == S6_data_test$y)
  list(Score = accuracy)
}

set.seed(61)
S6_opt_param_linear <- BayesianOptimization(
  FUN = S6_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S6_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S6_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S6_data_test)
  accuracy <- mean(prediction == S6_data_test$y)
  list(Score = accuracy)
}

set.seed(62)
S6_opt_param_polynomial <- BayesianOptimization(
  FUN = S6_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S6_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S6_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S6_data_test)
  accuracy <- mean(prediction == S6_data_test$y)
  list(Score = accuracy)
}

set.seed(63)
S6_opt_param_radial <- BayesianOptimization(
  FUN = S6_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S6_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S6_data_train[, setdiff(names(S6_data_train), "y")]), S6_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S6_data_test[, setdiff(names(S6_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S6_data_test$y)
  list(Score = accuracy)
}

set.seed(64)
S6_opt_param_logR <- BayesianOptimization(
  FUN = S6_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

S6_tune_k_NN <- function(k) {
  prediction <-
    knn(S6_data_train[, setdiff(names(S6_data_train), "y")],
        S6_data_test[, setdiff(names(S6_data_test), "y")],
        S6_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S6_data_test$y)
  list(Score = accuracy)
}

set.seed(65)
S6_opt_param_k_NN <- BayesianOptimization(
  FUN = S6_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
  verbose = FALSE
)

save(S6_opt_param_linear, S6_opt_param_polynomial, S6_opt_param_radial, S6_opt_param_logR, S6_opt_param_k_NN, file = "Code/Parameter/Parameter_S6.RData")

# Modelle fitten

S6_svm_linear <- svm(y ~., data = S6_data_train, kernel = "linear", cost = S6_opt_param_linear$Best_Par["cost"])
S6_svm_polynomial <- svm(y ~., data = S6_data_train, kernel = "polynomial", cost = S6_opt_param_polynomial$Best_Par["cost"], gamma = S6_opt_param_polynomial$Best_Par["gamma"], degree = S6_opt_param_polynomial$Best_Par["degree"])
S6_svm_radial <- svm(y ~., data = S6_data_train, kernel = "radial", cost = S6_opt_param_radial$Best_Par["cost"], gamma = S6_opt_param_radial$Best_Par["gamma"])
S6_logR <- glmnet(as.matrix(S6_data_train[, setdiff(names(S6_data_train), "y")]), S6_data_train[, "y"], family = "binomial", alpha = S6_opt_param_logR$Best_Par["alpha"], lambda = S6_opt_param_logR$Best_Par["lambda"])
S6_k_NN <- knn(S6_data_train[, setdiff(names(S6_data_train), "y")],
               S6_data_test[, setdiff(names(S6_data_test), "y")],
               S6_data_train[, "y"],
               k = S6_opt_param_k_NN$Best_Par["k"])

# Predictions

S6_prediction_linear <- predict(S6_svm_linear, S6_data_test)
S6_prediction_polynomial <- predict(S6_svm_polynomial, S6_data_test)
S6_prediction_radial <- predict(S6_svm_radial, S6_data_test)
S6_probabilities_logR <- predict(S6_logR, as.matrix(S6_data_test[, setdiff(names(S6_data_test), "y")]), s = S6_opt_param_logR$Best_Par["lambda"], type = "response")
S6_prediction_logR <- ifelse(S6_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S6_confusion_matrix_linear <- table(S6_prediction_linear, S6_data_test$y)
S6_accuracy_linear <- sum(diag(S6_confusion_matrix_linear))/sum(S6_confusion_matrix_linear)

S6_confusion_matrix_polynomial <- table(S6_prediction_polynomial, S6_data_test$y)
S6_accuracy_polynomial <- sum(diag(S6_confusion_matrix_polynomial))/sum(S6_confusion_matrix_polynomial)

S6_confusion_matrix_radial <- table(S6_prediction_radial, S6_data_test$y)
S6_accuracy_radial <- sum(diag(S6_confusion_matrix_radial))/sum(S6_confusion_matrix_radial)

S6_confusion_matrix_logR <- table(S6_prediction_logR, S6_data_test$y)
S6_accuracy_logR <- sum(diag(S6_confusion_matrix_logR))/sum(S6_confusion_matrix_logR)

S6_confusion_matrix_k_NN <- table(S6_k_NN, S6_data_test$y)
S6_accuracy_k_NN <- sum(diag(S6_confusion_matrix_k_NN))/sum(S6_confusion_matrix_k_NN)

S6_Accuracy <- data.frame(linear = c(as.character(round(S6_accuracy_linear, 4)), as.character(round(S6_opt_param_linear$Best_Par["cost"], 4)), "/", "/", "/", "/", "/"),
                          polynomial = c(as.character(round(S6_accuracy_polynomial, 4)), as.character(round(S6_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S6_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S6_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
                          radial = c(as.character(round(S6_accuracy_radial, 4)), as.character(round(S6_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S6_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
                          logR = c(as.character(round(S6_accuracy_logR, 4)), "/", "/", "/", as.character(round(S6_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S6_opt_param_logR$Best_Par["lambda"], 4)), "/"),
                          k_NN = c(as.character(round(S6_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S6_opt_param_k_NN$Best_Par["k"], 4))),
                          row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S6_Accuracy_Tabelle <- tableGrob(S6_Accuracy)
grid.arrange(S6_Accuracy_Tabelle)

# F1-Score

S6_F1_linear <- 2/(1/(S6_confusion_matrix_linear[1, 1]/sum(S6_confusion_matrix_linear[1, ])) + 1/(S6_confusion_matrix_linear[1, 1]/sum(S6_confusion_matrix_linear[, 1])))
S6_F1_polynomial <- 2/(1/(S6_confusion_matrix_polynomial[1, 1]/sum(S6_confusion_matrix_polynomial[1, ])) + 1/(S6_confusion_matrix_polynomial[1, 1]/sum(S6_confusion_matrix_polynomial[, 1])))
S6_F1_radial <- 2/(1/(S6_confusion_matrix_radial[1, 1]/sum(S6_confusion_matrix_radial[1, ])) + 1/(S6_confusion_matrix_radial[1, 1]/sum(S6_confusion_matrix_radial[, 1])))
S6_F1_logR <- 2/(1/(S6_confusion_matrix_logR[1, 1]/sum(S6_confusion_matrix_logR[1, ])) + 1/(S6_confusion_matrix_logR[1, 1]/sum(S6_confusion_matrix_logR[, 1])))
S6_F1_k_NN <- 2/(1/(S6_confusion_matrix_k_NN[1, 1]/sum(S6_confusion_matrix_k_NN[1, ])) + 1/(S6_confusion_matrix_k_NN[1, 1]/sum(S6_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S6_svm_linear_probs <- svm(y ~., data = S6_data_train, kernel = "linear", cost = S6_opt_param_linear$Best_Par["cost"], probability = TRUE)
S6_prob_svm_linear <- predict(S6_svm_linear_probs, S6_data_test, probability = TRUE)
S6_prediction_probs_linear <- attr(S6_prob_svm_linear, "probabilities")[, 1]
S6_roc_linear <- roc(S6_data_test$y, S6_prediction_probs_linear, levels = rev(levels(S6_data_test$y)))

S6_svm_polynomial_probs <- svm(y ~., data = S6_data_train, kernel = "polynomial", cost = S6_opt_param_polynomial$Best_Par["cost"], gamma = S6_opt_param_polynomial$Best_Par["gamma"], degree = S6_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S6_prob_svm_polynomial <- predict(S6_svm_polynomial_probs, S6_data_test, probability = TRUE)
S6_prediction_probs_polynomial <- attr(S6_prob_svm_polynomial, "probabilities")[, 1]
S6_roc_polynomial <- roc(S6_data_test$y, S6_prediction_probs_polynomial, levels = rev(levels(S6_data_test$y)))

S6_svm_radial_probs <- svm(y ~., data = S6_data_train, kernel = "radial", cost = S6_opt_param_radial$Best_Par["cost"], gamma = S6_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S6_prob_svm_radial <- predict(S6_svm_radial_probs, S6_data_test, probability = TRUE)
S6_prediction_probs_radial <- attr(S6_prob_svm_radial, "probabilities")[, 1]
S6_roc_radial <- roc(S6_data_test$y, S6_prediction_probs_radial, levels = rev(levels(S6_data_test$y)))

S6_prob_logR <- predict(S6_logR, as.matrix(S6_data_test[, setdiff(names(S6_data_test), "y")]), type = "response")
S6_roc_logR <- roc(S6_data_test$y, as.numeric(S6_prob_logR), levels = rev(levels(S6_data_test$y)))

S6_k_NN_probs <- knn(S6_data_train[, setdiff(names(S6_data_train), "y")],
                     S6_data_test[, setdiff(names(S6_data_test), "y")],
                     S6_data_train[, "y"],
                     k = S6_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S6_prediction_probs_k_NN <- attr(S6_k_NN_probs, "prob")
S6_prediction_probs_k_NN <- ifelse(S6_k_NN_probs == levels(S6_data_train$y)[1], S6_prediction_probs_k_NN, 1 - S6_prediction_probs_k_NN)
S6_roc_k_NN <- roc(S6_data_test$y, S6_prediction_probs_k_NN, levels = rev(levels(S6_data_test$y)))

plot(S6_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S6_roc_polynomial, col = "red", add = TRUE)
plot(S6_roc_radial, col = "green", add = TRUE)
plot(S6_roc_logR, col = "violet", add = TRUE)
plot(S6_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S6_roc_linear), ")"), paste("polynomial (AUC:", auc(S6_roc_polynomial), ")"), paste("radial (AUC:", auc(S6_roc_radial), ")"), paste("logR (AUC:", auc(S6_roc_logR), ")"), paste("k_NN (AUC:", auc(S6_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 9: p >> n (p = 200, n = 50)

# Datengenerierung

load(file = "Code/Daten/Data_S9.RData")

# Modelle tunen

# Tuning durchgeführt. Parameter hier abrufbar:
load(file = "Code/Parameter/Parameter_S9.RData")

S9_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S9_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S9_data_test)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy)
}

set.seed(91)
S9_opt_param_linear <- BayesianOptimization(
  FUN = S9_tune_linear,
  bounds = list(cost = c(0.01, 100)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S9_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S9_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S9_data_test)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy)
}

set.seed(92)
S9_opt_param_polynomial <- BayesianOptimization(
  FUN = S9_tune_polynomial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10), degree = c(1, 5)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S9_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S9_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S9_data_test)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy)
}

set.seed(93)
S9_opt_param_radial <- BayesianOptimization(
  FUN = S9_tune_radial,
  bounds = list(cost = c(0.01, 100), gamma = c(0.001, 10)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S9_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S9_data_train[, setdiff(names(S9_data_train), "y")]), S9_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S9_data_test[, setdiff(names(S9_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy)
}

set.seed(94)
S9_opt_param_logR <- BayesianOptimization(
  FUN = S9_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

S9_tune_k_NN <- function(k) {
  prediction <-
    knn(S9_data_train[, setdiff(names(S9_data_train), "y")],
        S9_data_test[, setdiff(names(S9_data_test), "y")],
        S9_data_train[, "y"],
        k = k)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy)
}

set.seed(96)
S9_opt_param_k_NN <- BayesianOptimization(
  FUN = S9_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 15,
  n_iter = 25,
  acq = "ucb",
  verbose = FALSE
)

save(S9_opt_param_linear, S9_opt_param_polynomial, S9_opt_param_radial, S9_opt_param_logR, S9_opt_param_k_NN, file = "Code/Parameter/Parameter_S9.RData")

# Modelle fitten

S9_svm_linear <- svm(y ~., data = S9_data_train, kernel = "linear", cost = S9_opt_param_linear$Best_Par["cost"])
S9_svm_polynomial <- svm(y ~., data = S9_data_train, kernel = "polynomial", cost = S9_opt_param_polynomial$Best_Par["cost"], gamma = S9_opt_param_polynomial$Best_Par["gamma"], degree = S9_opt_param_polynomial$Best_Par["degree"])
S9_svm_radial <- svm(y ~., data = S9_data_train, kernel = "radial", cost = S9_opt_param_radial$Best_Par["cost"], gamma = S9_opt_param_radial$Best_Par["gamma"])
S9_logR <- glmnet(as.matrix(S9_data_train[, setdiff(names(S9_data_train), "y")]), S9_data_train[, "y"], family = "binomial", alpha = S9_opt_param_logR$Best_Par["alpha"], lambda = S9_opt_param_logR$Best_Par["lambda"])
S9_k_NN <- knn(S9_data_train[, setdiff(names(S9_data_train), "y")],
               S9_data_test[, setdiff(names(S9_data_test), "y")],
               S9_data_train[, "y"],
               k = S9_opt_param_k_NN$Best_Par["k"])

# Predictions

S9_prediction_linear <- predict(S9_svm_linear, S9_data_test)
S9_prediction_polynomial <- predict(S9_svm_polynomial, S9_data_test)
S9_prediction_radial <- predict(S9_svm_radial, S9_data_test)
S9_probabilities_logR <- predict(S9_logR, as.matrix(S9_data_test[, setdiff(names(S9_data_test), "y")]), s = S9_opt_param_logR$Best_Par["lambda"], type = "response")
S9_prediction_logR <- ifelse(S9_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S9_confusion_matrix_linear <- table(S9_prediction_linear, S9_data_test$y)
S9_accuracy_linear <- sum(diag(S9_confusion_matrix_linear))/sum(S9_confusion_matrix_linear)

S9_confusion_matrix_polynomial <- table(S9_prediction_polynomial, S9_data_test$y)
S9_accuracy_polynomial <- sum(diag(S9_confusion_matrix_polynomial))/sum(S9_confusion_matrix_polynomial)

S9_confusion_matrix_radial <- table(S9_prediction_radial, S9_data_test$y)
S9_accuracy_radial <- sum(diag(S9_confusion_matrix_radial))/sum(S9_confusion_matrix_radial)

S9_confusion_matrix_logR <- table(S9_prediction_logR, S9_data_test$y)
S9_accuracy_logR <- sum(diag(S9_confusion_matrix_logR))/sum(S9_confusion_matrix_logR)

S9_confusion_matrix_k_NN <- table(S9_k_NN, S9_data_test$y)
S9_accuracy_k_NN <- sum(diag(S9_confusion_matrix_k_NN))/sum(S9_confusion_matrix_k_NN)

S9_Accuracy <- data.frame(linear = c(as.character(round(S9_accuracy_linear, 4)), as.character(round(S9_opt_param_linear$Best_Par["cost"], 4)), "/", "/", "/", "/", "/"),
                          polynomial = c(as.character(round(S9_accuracy_polynomial, 4)), as.character(round(S9_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S9_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S9_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
                          radial = c(as.character(round(S9_accuracy_radial, 4)), as.character(round(S9_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S9_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
                          logR = c(as.character(round(S9_accuracy_logR, 4)), "/", "/", "/", as.character(round(S9_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S9_opt_param_logR$Best_Par["lambda"], 4)), "/"),
                          k_NN = c(as.character(round(S9_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S9_opt_param_k_NN$Best_Par["k"], 4))),
                          row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S9_Accuracy_Tabelle <- tableGrob(S9_Accuracy)
grid.arrange(S9_Accuracy_Tabelle)

# F1-Score

S9_F1_linear <- 2/(1/(S9_confusion_matrix_linear[1, 1]/sum(S9_confusion_matrix_linear[1, ])) + 1/(S9_confusion_matrix_linear[1, 1]/sum(S9_confusion_matrix_linear[, 1])))
S9_F1_polynomial <- 2/(1/(S9_confusion_matrix_polynomial[1, 1]/sum(S9_confusion_matrix_polynomial[1, ])) + 1/(S9_confusion_matrix_polynomial[1, 1]/sum(S9_confusion_matrix_polynomial[, 1])))
S9_F1_radial <- 2/(1/(S9_confusion_matrix_radial[1, 1]/sum(S9_confusion_matrix_radial[1, ])) + 1/(S9_confusion_matrix_radial[1, 1]/sum(S9_confusion_matrix_radial[, 1])))
S9_F1_logR <- 2/(1/(S9_confusion_matrix_logR[1, 1]/sum(S9_confusion_matrix_logR[1, ])) + 1/(S9_confusion_matrix_logR[1, 1]/sum(S9_confusion_matrix_logR[, 1])))
S9_F1_k_NN <- 2/(1/(S9_confusion_matrix_k_NN[1, 1]/sum(S9_confusion_matrix_k_NN[1, ])) + 1/(S9_confusion_matrix_k_NN[1, 1]/sum(S9_confusion_matrix_k_NN[, 1])))

# ROC/AUC

# alle Modelle mit Ausgabe der Wahrscheinlichkeit fitten, da ROC auf Wahrscheinlichkeiten basiert
S9_svm_linear_probs <- svm(y ~., data = S9_data_train, kernel = "linear", cost = S9_opt_param_linear$Best_Par["cost"], probability = TRUE)
S9_prob_svm_linear <- predict(S9_svm_linear_probs, S9_data_test, probability = TRUE)
S9_prediction_probs_linear <- attr(S9_prob_svm_linear, "probabilities")[, 1]
S9_roc_linear <- roc(S9_data_test$y, S9_prediction_probs_linear, levels = rev(levels(S9_data_test$y)))

S9_svm_polynomial_probs <- svm(y ~., data = S9_data_train, kernel = "polynomial", cost = S9_opt_param_polynomial$Best_Par["cost"], gamma = S9_opt_param_polynomial$Best_Par["gamma"], degree = S9_opt_param_polynomial$Best_Par["degree"], probability = TRUE)
S9_prob_svm_polynomial <- predict(S9_svm_polynomial_probs, S9_data_test, probability = TRUE)
S9_prediction_probs_polynomial <- attr(S9_prob_svm_polynomial, "probabilities")[, 1]
S9_roc_polynomial <- roc(S9_data_test$y, S9_prediction_probs_polynomial, levels = rev(levels(S9_data_test$y)))

S9_svm_radial_probs <- svm(y ~., data = S9_data_train, kernel = "radial", cost = S9_opt_param_radial$Best_Par["cost"], gamma = S9_opt_param_radial$Best_Par["gamma"], probability = TRUE)
S9_prob_svm_radial <- predict(S9_svm_radial_probs, S9_data_test, probability = TRUE)
S9_prediction_probs_radial <- attr(S9_prob_svm_radial, "probabilities")[, 1]
S9_roc_radial <- roc(S9_data_test$y, S9_prediction_probs_radial, levels = rev(levels(S9_data_test$y)))

S9_prob_logR <- predict(S9_logR, as.matrix(S9_data_test[, setdiff(names(S9_data_test), "y")]), type = "response")
S9_roc_logR <- roc(S9_data_test$y, as.numeric(S9_prob_logR), levels = rev(levels(S9_data_test$y)))

S9_k_NN_probs <- knn(S9_data_train[, setdiff(names(S9_data_train), "y")],
                     S9_data_test[, setdiff(names(S9_data_test), "y")],
                     S9_data_train[, "y"],
                     k = S9_opt_param_k_NN$Best_Par["k"],
                     prob = TRUE)
S9_prediction_probs_k_NN <- attr(S9_k_NN_probs, "prob")
S9_prediction_probs_k_NN <- ifelse(S9_k_NN_probs == levels(S9_data_train$y)[1], S9_prediction_probs_k_NN, 1 - S9_prediction_probs_k_NN)
S9_roc_k_NN <- roc(S9_data_test$y, S9_prediction_probs_k_NN, levels = rev(levels(S9_data_test$y)))

plot(S9_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S9_roc_polynomial, col = "red", add = TRUE)
plot(S9_roc_radial, col = "green", add = TRUE)
plot(S9_roc_logR, col = "violet", add = TRUE)
plot(S9_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S9_roc_linear), ")"), paste("polynomial (AUC:", auc(S9_roc_polynomial), ")"), paste("radial (AUC:", auc(S9_roc_radial), ")"), paste("logR (AUC:", auc(S9_roc_logR), ")"), paste("k_NN (AUC:", auc(S9_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)
# erstellen der Einzeltabellen

S3_Tabelle <- round(data.frame(Accuracy = c(S3_accuracy_linear, S3_accuracy_polynomial, S3_accuracy_radial, S3_accuracy_logR, S3_accuracy_k_NN),
                               AUC = c(auc(S3_roc_linear), auc(S3_roc_polynomial), auc(S3_roc_radial), auc(S3_roc_logR), auc(S3_roc_k_NN)),
                               F_1_Score = c(S3_F1_linear, S3_F1_polynomial, S3_F1_radial, S3_F1_logR, S3_F1_k_NN),
                               row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")), 2)

S6_Tabelle <- round(data.frame(Accuracy = c(S6_accuracy_linear, S6_accuracy_polynomial, S6_accuracy_radial, S6_accuracy_logR, S6_accuracy_k_NN),
                               AUC = c(auc(S6_roc_linear), auc(S6_roc_polynomial), auc(S6_roc_radial), auc(S6_roc_logR), auc(S6_roc_k_NN)),
                               F_1_Score = c(S6_F1_linear, S6_F1_polynomial, S6_F1_radial, S6_F1_logR, S6_F1_k_NN),
                               row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")), 2)

S9_Tabelle <- round(data.frame(Accuracy = c(S9_accuracy_linear, S9_accuracy_polynomial, S9_accuracy_radial, S9_accuracy_logR, S9_accuracy_k_NN),
                               AUC = c(auc(S9_roc_linear), auc(S9_roc_polynomial), auc(S9_roc_radial), auc(S9_roc_logR), auc(S9_roc_k_NN)),
                               F_1_Score = c(S9_F1_linear, S9_F1_polynomial, S9_F1_radial, S9_F1_logR, S9_F1_k_NN),
                               row.names = c("SVM-L", "SVM-P", "SVM-R", "LogR", "K-NN")), 2)

save(S3_Tabelle, S6_Tabelle, S9_Tabelle, file = "Code/Tabellen/Tabellen_S3_S6_S9.RData")
