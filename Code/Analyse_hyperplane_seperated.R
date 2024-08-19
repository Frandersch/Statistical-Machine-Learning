# Pakete
library(e1071)
library(glmnet)
library(class)
library(rBayesianOptimization)
library(gridExtra)
library(pROC)
library(ggplot2)

## Szenario 1: p << n (n = 1000, p = 10)

# Datenimport

load(file = "Code/Daten/Data_S1.RData")

# Modelle tunen

S1_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S1_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S1_data_test)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

S1_opt_param_linear <- BayesianOptimization(
  FUN = S1_tune_linear,
  bounds = list(cost = c(0.01, 50)),
  init_points = 5,
  n_iter = 20,
  acq = "ucb",
)

S1_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S1_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S1_data_test)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

S1_opt_param_polynomial <- BayesianOptimization(
  FUN = S1_tune_polynomial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10), degree = c(1, 10)),
  init_points = 5,
  n_iter = 20,
  acq = "ucb"
)

S1_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S1_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S1_data_test)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

S1_opt_param_radial <- BayesianOptimization(
  FUN = S1_tune_radial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10)),
  init_points = 5,
  n_iter = 20,
  acq = "ucb"
)

S1_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S1_data_train[, setdiff(names(S1_data_train), "y")]), S1_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S1_data_test[, setdiff(names(S1_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S1_data_test$y)
  list(Score = accuracy)
}

S1_opt_param_logR <- BayesianOptimization(
  FUN = S1_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
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

S1_opt_param_k_NN <- BayesianOptimization(
  FUN = S1_tune_k_NN,
  bounds = list(k = c(1, 100)),
  init_points = 5,
  n_iter = 20,
  acq = "ucb"
)

# Modelle fitten

S1_svm_linear <- svm(y ~., data = S1_data_train, kernel = "linear", cost = S1_opt_param_linear$Best_Par)
S1_svm_polynomial <- svm(y ~., data = S1_data_train, kernel = "polynomial", cost = S1_opt_param_polynomial$Best_Par["cost"], gamma = S1_opt_param_polynomial$Best_Par["gamma"], degree = S1_opt_param_polynomial$Best_Par["degree"])
S1_svm_radial <- svm(y ~., data = S1_data_train, kernel = "radial", cost = S1_opt_param_radial$Best_Par["cost"], gamma = S1_opt_param_radial$Best_Par["gamma"])
S1_logR <- glmnet(as.matrix(S1_data_train[, setdiff(names(S1_data_train), "y")]), S1_data_train[, "y"], family = "binomial", alpha = S1_opt_param_logR$Best_Par["alpha"], lambda = S1_opt_param_logR$Best_Par["lambda"])
S1_k_NN <- knn(S1_data_train[, setdiff(names(S1_data_train), "y")],
            S1_data_test[, setdiff(names(S1_data_test), "y")],
            S1_data_train[, "y"],
            k = S1_opt_param_k_NN$Best_Par["k"])

# Predictions

S1_prediction_linear <- predict(S1_svm_linear, S1_data_test)
S1_prediction_polynomial <- predict(S1_svm_polynomial, S1_data_test)
S1_prediction_radial <- predict(S1_svm_radial, S1_data_test)
S1_probabilities_logR <- predict(S1_logR, as.matrix(S1_data_test[, setdiff(names(S1_data_test), "y")]), s = S1_opt_param_logR$Best_Par["lambda"], type = "response")
S1_prediction_logR <- ifelse(S1_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S1_confusion_matrix_linear <- table(S1_prediction_linear, S1_data_test$y)
S1_accuracy_linear <- sum(diag(S1_confusion_matrix_linear))/sum(S1_confusion_matrix_linear)

S1_confusion_matrix_polynomial <- table(S1_prediction_polynomial, S1_data_test$y)
S1_accuracy_polynomial <- sum(diag(S1_confusion_matrix_polynomial))/sum(S1_confusion_matrix_polynomial)

S1_confusion_matrix_radial <- table(S1_prediction_radial, S1_data_test$y)
S1_accuracy_radial <- sum(diag(S1_confusion_matrix_radial))/sum(S1_confusion_matrix_radial)

S1_confusion_matrix_logR <- table(S1_prediction_logR, S1_data_test$y)
S1_accuracy_logR <- sum(diag(S1_confusion_matrix_logR))/sum(S1_confusion_matrix_logR)

S1_confusion_matrix_k_NN <- table(S1_k_NN, S1_data_test$y)
S1_accuracy_k_NN <- sum(diag(S1_confusion_matrix_k_NN))/sum(S1_confusion_matrix_k_NN)

S1_Accuracy <- data.frame(linear = c(as.character(round(S1_accuracy_linear), 4), as.character(round(S1_opt_param_linear$Best_Par["cost"], 4)), "/", "/", "/", "/", "/"),
           polynomial = c(as.character(round(S1_accuracy_polynomial, 4)), as.character(round(S1_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S1_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S1_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
           radial = c(as.character(round(S1_accuracy_radial, 4)), as.character(round(S1_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S1_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
           logR = c(as.character(round(S1_accuracy_logR, 4)), "/", "/", "/", as.character(round(S1_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S1_opt_param_logR$Best_Par["lambda"], 4)), "/"),
           k_NN = c(as.character(round(S1_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S1_opt_param_k_NN$Best_Par["k"], 4))),
           row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S1_Accuracy_Tabelle <- tableGrob(S1_Accuracy)
grid.arrange(S1_Accuracy_Tabelle)

# ROC/AUC

S1_roc_linear <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_linear))
S1_roc_polynomial <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_polynomial))
S1_roc_radial <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_radial))
S1_roc_logR <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_logR))
S1_roc_k_NN <- roc(response = S1_data_test$y, predictor = as.numeric(S1_k_NN))

ggplot() +
  geom_line(aes(x = S1_roc_linear$specificities, y = S1_roc_linear$sensitivities, color = "linear"), linewidth = 1) +
  geom_line(aes(x = S1_roc_polynomial$specificities, y = S1_roc_polynomial$sensitivities, color = "polynomial"), linewidth = 1) +
  geom_line(aes(x = S1_roc_radial$specificities, y = S1_roc_radial$sensitivities, color = "radial"), linewidth = 1) +
  geom_line(aes(x = S1_roc_logR$specificities, y = S1_roc_logR$sensitivities, color = "logR"), linewidth = 1) +
  geom_line(aes(x = S1_roc_k_NN$specificities, y = S1_roc_k_NN$sensitivities, color = "k_NN"), linewidth = 1) +
  geom_abline(slope = 1, intercept = 1, color = "black", linewidth = 0.5) +
  labs(title = "ROC-Kurven Szenario 1", x = "Spezifität", y = "Sensitivität") +
  scale_color_manual(name = "Modelle",
                     values = c("linear" = "blue", "polynomial" = "red", "radial" = "green", "logR" = "violet", "k_NN" = "orange"),
                     labels = c("linear" = paste("linear (AUC:", round(auc(S1_roc_linear), 4), ")"),
                                "polynomial" = paste("polynomial (AUC:", round(auc(S1_roc_polynomial), 4), ")"),
                                "radial" = paste("radial (AUC:", round(auc(S1_roc_radial), 4), ")"),
                                "logR" = paste("logR (AUC:", round(auc(S1_roc_logR), 4), ")"),
                                "k_NN" = paste("k_NN (AUC:", round(auc(S1_roc_k_NN), 4), ")")),
                     breaks = c("linear", "polynomial", "radial", "logR", "k_NN")) +
  scale_x_reverse() +
  theme_minimal()

## Szenario 4: p = n (p = 200, n = 200)

# Datengenerierung

load(file = "Code/Daten/Data_S4.RData")

# Modelle tunen

S4_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S4_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

S4_opt_param_linear <- BayesianOptimization(
  FUN = S4_tune_linear,
  bounds = list(cost = c(0.01, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
)
# kein tuning möglich, da kein optimaler Parameter vorhanden -> default Werte

S4_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S4_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

S4_opt_param_polynomial <- BayesianOptimization(
  FUN = S4_tune_polynomial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10), degree = c(1, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

S4_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S4_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S4_data_test)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

S4_opt_param_radial <- BayesianOptimization(
  FUN = S4_tune_radial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

S4_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S4_data_train[, setdiff(names(S4_data_train), "y")]), S4_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S4_data_test[, setdiff(names(S4_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S4_data_test$y)
  list(Score = accuracy)
}

S4_opt_param_logR <- BayesianOptimization(
  FUN = S4_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
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

S4_opt_param_k_NN <- BayesianOptimization(
  FUN = S4_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

# Modelle fitten

# default Werte für linear, da kein optimaler Parameter vorhanden
S4_svm_linear <- svm(y ~., data = S4_data_train, kernel = "linear")
S4_svm_polynomial <- svm(y ~., data = S4_data_train, kernel = "polynomial", cost = S4_opt_param_polynomial$Best_Par["cost"], gamma = S4_opt_param_polynomial$Best_Par["gamma"], degree = S4_opt_param_polynomial$Best_Par["degree"])
S4_svm_radial <- svm(y ~., data = S4_data_train, kernel = "radial", cost = S4_opt_param_radial$Best_Par["cost"], gamma = S4_opt_param_radial$Best_Par["gamma"])
S4_logR <- glmnet(as.matrix(S4_data_train[, setdiff(names(S4_data_train), "y")]), S4_data_train[, "y"], family = "binomial", alpha = S4_opt_param_logR$Best_Par["alpha"], lambda = S4_opt_param_logR$Best_Par["lambda"])
S4_k_NN <- knn(S4_data_train[, setdiff(names(S4_data_train), "y")],
               S4_data_test[, setdiff(names(S4_data_test), "y")],
               S4_data_train[, "y"],
               k = S4_opt_param_k_NN$Best_Par["k"])

# Predictions

S4_prediction_linear <- predict(S4_svm_linear, S4_data_test)
S4_prediction_polynomial <- predict(S4_svm_polynomial, S4_data_test)
S4_prediction_radial <- predict(S4_svm_radial, S4_data_test)
S4_probabilities_logR <- predict(S4_logR, as.matrix(S4_data_test[, setdiff(names(S4_data_test), "y")]), s = S4_opt_param_logR$Best_Par["lambda"], type = "response")
S4_prediction_logR <- ifelse(S4_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S4_confusion_matrix_linear <- table(S4_prediction_linear, S4_data_test$y)
S4_accuracy_linear <- sum(diag(S4_confusion_matrix_linear))/sum(S4_confusion_matrix_linear)

S4_confusion_matrix_polynomial <- table(S4_prediction_polynomial, S4_data_test$y)
S4_accuracy_polynomial <- sum(diag(S4_confusion_matrix_polynomial))/sum(S4_confusion_matrix_polynomial)

S4_confusion_matrix_radial <- table(S4_prediction_radial, S4_data_test$y)
S4_accuracy_radial <- sum(diag(S4_confusion_matrix_radial))/sum(S4_confusion_matrix_radial)

S4_confusion_matrix_logR <- table(S4_prediction_logR, S4_data_test$y)
S4_accuracy_logR <- sum(diag(S4_confusion_matrix_logR))/sum(S4_confusion_matrix_logR)

S4_confusion_matrix_k_NN <- table(S4_k_NN, S4_data_test$y)
S4_accuracy_k_NN <- sum(diag(S4_confusion_matrix_k_NN))/sum(S4_confusion_matrix_k_NN)

S4_Accuracy <- data.frame(linear = c(as.character(round(S4_accuracy_linear, 4)), "/", "/", "/", "/", "/", "/"),
                          polynomial = c(as.character(round(S4_accuracy_polynomial, 4)), as.character(round(S4_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S4_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S4_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
                          radial = c(as.character(round(S4_accuracy_radial, 4)), as.character(round(S4_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S4_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
                          logR = c(as.character(round(S4_accuracy_logR, 4)), "/", "/", "/", as.character(round(S4_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S4_opt_param_logR$Best_Par["lambda"], 4)), "/"),
                          k_NN = c(as.character(round(S4_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S4_opt_param_k_NN$Best_Par["k"], 4))),
                          row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S4_Accuracy_Tabelle <- tableGrob(S4_Accuracy)
grid.arrange(S4_Accuracy_Tabelle)

# ROC/AUC

S4_roc_linear <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_linear))
S4_roc_polynomial <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_polynomial))
S4_roc_radial <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_radial))
S4_roc_logR <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_logR))
S4_roc_k_NN <- roc(response = S4_data_test$y, predictor = as.numeric(S4_k_NN))

ggplot() +
  geom_line(aes(x = S4_roc_linear$specificities, y = S4_roc_linear$sensitivities, color = "linear"), linewidth = 1) +
  geom_line(aes(x = S4_roc_polynomial$specificities, y = S4_roc_polynomial$sensitivities, color = "polynomial"), linewidth = 1) +
  geom_line(aes(x = S4_roc_radial$specificities, y = S4_roc_radial$sensitivities, color = "radial"), linewidth = 1) +
  geom_line(aes(x = S4_roc_logR$specificities, y = S4_roc_logR$sensitivities, color = "logR"), linewidth = 1) +
  geom_line(aes(x = S4_roc_k_NN$specificities, y = S4_roc_k_NN$sensitivities, color = "k_NN"), linewidth = 1) +
  geom_abline(slope = 1, intercept = 1, color = "black", linewidth = 0.5) +
  labs(title = "ROC-Kurven Szenario 4", x = "Spezifität", y = "Sensitivität") +
  scale_color_manual(name = "Modelle",
                     values = c("linear" = "blue", "polynomial" = "red", "radial" = "green", "logR" = "violet", "k_NN" = "orange"),
                     labels = c("linear" = paste("linear (AUC:", round(auc(S4_roc_linear), 4), ")"),
                                "polynomial" = paste("polynomial (AUC:", round(auc(S4_roc_polynomial), 4), ")"),
                                "radial" = paste("radial (AUC:", round(auc(S4_roc_radial), 4), ")"),
                                "logR" = paste("logR (AUC:", round(auc(S4_roc_logR), 4), ")"),
                                "k_NN" = paste("k_NN (AUC:", round(auc(S4_roc_k_NN), 4), ")")),
                     breaks = c("linear", "polynomial", "radial", "logR", "k_NN")) +
  scale_x_reverse() +
  theme_minimal()

## Szenario 7: p >> n (p = 200, n = 50)

# Datengenerierung

load(file = "Code/Daten/Data_S7.RData")

# hier noch: Modelle tunen

S7_tune_linear <- function(cost) {
  model <- svm(y ~ ., data = S7_data_train, kernel = "linear", cost = cost)
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

S7_opt_param_linear <- BayesianOptimization(
  FUN = S7_tune_linear,
  bounds = list(cost = c(0.01, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb",
)
# kein tuning möglich, da kein optimaler Parameter vorhanden -> default Werte

S7_tune_polynomial <- function(cost, gamma, degree) {
  model <- svm(y ~ ., data = S7_data_train, kernel = "polynomial", cost = cost, gamma = gamma, degree = degree)
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

S7_opt_param_polynomial <- BayesianOptimization(
  FUN = S7_tune_polynomial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10), degree = c(1, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

S7_tune_radial <- function(cost, gamma) {
  model <- svm(y ~ ., data = S7_data_train, kernel = "radial", cost = cost, gamma = gamma)
  prediction <- predict(model, S7_data_test)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

S7_opt_param_radial <- BayesianOptimization(
  FUN = S7_tune_radial,
  bounds = list(cost = c(0.01, 50), gamma = c(0.01, 10)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

S7_tune_logR <- function(alpha, lambda) {
  model <- glmnet(as.matrix(S7_data_train[, setdiff(names(S7_data_train), "y")]), S7_data_train[, "y"], family = "binomial", alpha = alpha, lambda = lambda)
  probabilities <- predict(model, as.matrix(S7_data_test[, setdiff(names(S7_data_test), "y")]), s = lambda, type = "response")
  prediction <- ifelse(probabilities > 0.5, 2, 1)
  accuracy <- mean(prediction == S7_data_test$y)
  list(Score = accuracy)
}

S7_opt_param_logR <- BayesianOptimization(
  FUN = S7_tune_logR,
  bounds = list(alpha = c(0, 1), lambda = c(0.001, 1)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
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

S7_opt_param_k_NN <- BayesianOptimization(
  FUN = S7_tune_k_NN,
  bounds = list(k = c(1, 50)),
  init_points = 10,
  n_iter = 15,
  acq = "ucb"
)

# Modelle fitten

# default Werte für linear, da kein optimaler Parameter vorhanden
S7_svm_linear <- svm(y ~., data = S7_data_train, kernel = "linear")
S7_svm_polynomial <- svm(y ~., data = S7_data_train, kernel = "polynomial", cost = S7_opt_param_polynomial$Best_Par["cost"], gamma = S7_opt_param_polynomial$Best_Par["gamma"], degree = S7_opt_param_polynomial$Best_Par["degree"])
S7_svm_radial <- svm(y ~., data = S7_data_train, kernel = "radial", cost = S7_opt_param_radial$Best_Par["cost"], gamma = S7_opt_param_radial$Best_Par["gamma"])
S7_logR <- glmnet(as.matrix(S7_data_train[, setdiff(names(S7_data_train), "y")]), S7_data_train[, "y"], family = "binomial", alpha = S7_opt_param_logR$Best_Par["alpha"], lambda = S7_opt_param_logR$Best_Par["lambda"])
S7_k_NN <- knn(S7_data_train[, setdiff(names(S7_data_train), "y")],
               S7_data_test[, setdiff(names(S7_data_test), "y")],
               S7_data_train[, "y"],
               k = S7_opt_param_k_NN$Best_Par["k"])

# Predictions

S7_prediction_linear <- predict(S7_svm_linear, S7_data_test)
S7_prediction_polynomial <- predict(S7_svm_polynomial, S7_data_test)
S7_prediction_radial <- predict(S7_svm_radial, S7_data_test)
S7_probabilities_logR <- predict(S7_logR, as.matrix(S7_data_test[, setdiff(names(S7_data_test), "y")]), s = S7_opt_param_logR$Best_Par["lambda"], type = "response")
S7_prediction_logR <- ifelse(S7_probabilities_logR > 0.5, 2, 1)
# für k_NN wird direkt beim Model-fit predicted

# Accuracy

S7_confusion_matrix_linear <- table(S7_prediction_linear, S7_data_test$y)
S7_accuracy_linear <- sum(diag(S7_confusion_matrix_linear))/sum(S7_confusion_matrix_linear)

S7_confusion_matrix_polynomial <- table(S7_prediction_polynomial, S7_data_test$y)
S7_accuracy_polynomial <- sum(diag(S7_confusion_matrix_polynomial))/sum(S7_confusion_matrix_polynomial)

S7_confusion_matrix_radial <- table(S7_prediction_radial, S7_data_test$y)
S7_accuracy_radial <- sum(diag(S7_confusion_matrix_radial))/sum(S7_confusion_matrix_radial)

S7_confusion_matrix_logR <- table(S7_prediction_logR, S7_data_test$y)
S7_accuracy_logR <- sum(diag(S7_confusion_matrix_logR))/sum(S7_confusion_matrix_logR)

S7_confusion_matrix_k_NN <- table(S7_k_NN, S7_data_test$y)
S7_accuracy_k_NN <- sum(diag(S7_confusion_matrix_k_NN))/sum(S7_confusion_matrix_k_NN)

S7_Accuracy <- data.frame(linear = c(as.character(round(S7_accuracy_linear, 4)), "/", "/", "/", "/", "/", "/"),
                          polynomial = c(as.character(round(S7_accuracy_polynomial, 4)), as.character(round(S7_opt_param_polynomial$Best_Par["cost"], 4)), as.character(round(S7_opt_param_polynomial$Best_Par["gamma"], 4)), as.character(round(S7_opt_param_polynomial$Best_Par["degree"], 4)), "/", "/", "/"),
                          radial = c(as.character(round(S7_accuracy_radial, 4)), as.character(round(S7_opt_param_radial$Best_Par["cost"], 4)), as.character(round(S7_opt_param_radial$Best_Par["gamma"], 4)), "/", "/", "/", "/"),
                          logR = c(as.character(round(S7_accuracy_logR, 4)), "/", "/", "/", as.character(round(S7_opt_param_logR$Best_Par["alpha"], 4)), as.character(round(S7_opt_param_logR$Best_Par["lambda"], 4)), "/"),
                          k_NN = c(as.character(round(S7_accuracy_k_NN, 4)), "/", "/", "/", "/", "/", as.character(round(S7_opt_param_k_NN$Best_Par["k"], 4))),
                          row.names = c("Accuracy", "Cost", "Gamma", "Degree", "Alpha", "Lambda", "K"))

S7_Accuracy_Tabelle <- tableGrob(S7_Accuracy)
grid.arrange(S7_Accuracy_Tabelle)

# ROC/AUC

S7_roc_linear <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_linear))
S7_roc_polynomial <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_polynomial))
S7_roc_radial <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_radial))
S7_roc_logR <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_logR))
S7_roc_k_NN <- roc(response = S7_data_test$y, predictor = as.numeric(S7_k_NN))

ggplot() +
  geom_line(aes(x = S7_roc_linear$specificities, y = S7_roc_linear$sensitivities, color = "linear"), linewidth = 1) +
  geom_line(aes(x = S7_roc_polynomial$specificities, y = S7_roc_polynomial$sensitivities, color = "polynomial"), linewidth = 1) +
  geom_line(aes(x = S7_roc_radial$specificities, y = S7_roc_radial$sensitivities, color = "radial"), linewidth = 1) +
  geom_line(aes(x = S7_roc_logR$specificities, y = S7_roc_logR$sensitivities, color = "logR"), linewidth = 1) +
  geom_line(aes(x = S7_roc_k_NN$specificities, y = S7_roc_k_NN$sensitivities, color = "k_NN"), linewidth = 1) +
  geom_abline(slope = 1, intercept = 1, color = "black", linewidth = 0.5) +
  labs(title = "ROC-Kurven Szenario 7", x = "Spezifität", y = "Sensitivität") +
  scale_color_manual(name = "Modelle",
                     values = c("linear" = "blue", "polynomial" = "red", "radial" = "green", "logR" = "violet", "k_NN" = "orange"),
                     labels = c("linear" = paste("linear (AUC:", round(auc(S7_roc_linear), 4), ")"),
                                "polynomial" = paste("polynomial (AUC:", round(auc(S7_roc_polynomial), 4), ")"),
                                "radial" = paste("radial (AUC:", round(auc(S7_roc_radial), 4), ")"),
                                "logR" = paste("logR (AUC:", round(auc(S7_roc_logR), 4), ")"),
                                "k_NN" = paste("k_NN (AUC:", round(auc(S7_roc_k_NN), 4), ")")),
                     breaks = c("linear", "polynomial", "radial", "logR", "k_NN")) +
  scale_x_reverse() +
  theme_minimal()
