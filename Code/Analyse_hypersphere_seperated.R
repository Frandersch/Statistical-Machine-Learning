# Pakete
library(e1071)
library(class)

## Szenario 3: p << n (p = 1000, n = 10)

# Datengenerierung

load(file = "Code/Funktionen/hypersphere_seperated.RData")

# geringe Distance, da sonst alle Algorithmen perfekt klassifizieren
S3_data_train <- generate_dataset(1000, 10, 2)
S3_data_test <- generate_dataset(1000, 10, 2)

# hier noch: Modelle tunen

# Modelle fitten

S3_svm_linear <- svm(y ~., data = S3_data_train, kernel = "linear")
S3_svm_polynomial <- svm(y ~., data = S3_data_train, kernel = "polynomial")
S3_svm_radial <- svm(y ~., data = S3_data_train, kernel = "radial")
S3_logR <- glm(y ~., data = S3_data_train, family = binomial())
S3_k_NN <- knn(S3_data_train[, setdiff(names(S3_data_train), "y")],
               S3_data_test[, setdiff(names(S3_data_test), "y")],
               S3_data_train[, "y"],
               k = 10)

# Predictions

S3_prediction_linear <- predict(S3_svm_linear, S3_data_test)
S3_prediction_polynomial <- predict(S3_svm_polynomial, S3_data_test)
S3_prediction_radial <- predict(S3_svm_radial, S3_data_test)
S3_probabilities_logR <- predict(S3_logR, S3_data_test, type = "response")
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

data.frame(linear = S3_accuracy_linear, polynomial = S3_accuracy_polynomial, radial = S3_accuracy_radial, logR = S3_accuracy_logR, k_NN = S3_accuracy_k_NN, row.names = "S3_Accuracy")

# ROC/AUC
library(pROC)
S3_roc_linear <- roc(response = S3_data_test$y, predictor = as.numeric(S3_prediction_linear))
S3_roc_polynomial <- roc(response = S3_data_test$y, predictor = as.numeric(S3_prediction_polynomial))
S3_roc_radial <- roc(response = S3_data_test$y, predictor = as.numeric(S3_prediction_radial))
S3_roc_logR <- roc(response = S3_data_test$y, predictor = as.numeric(S3_prediction_logR))
S3_roc_k_NN <- roc(response = S3_data_test$y, predictor = as.numeric(S3_k_NN))

plot(S3_roc_linear, col = "blue", main = "ROC-Kurven Szenario 3")
plot(S3_roc_polynomial, col = "red", add = TRUE)
plot(S3_roc_radial, col = "green", add = TRUE)
plot(S3_roc_logR, col = "violet", add = TRUE)
plot(S3_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S3_roc_linear), ")"), paste("polynomial (AUC:", auc(S3_roc_polynomial), ")"), paste("radial (AUC:", auc(S3_roc_radial), ")"), paste("logR (AUC:", auc(S3_roc_logR), ")"), paste("k_NN (AUC:", auc(S3_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 6: p = n (p = 200, n = 200)

# Datengenerierung

load(file = "Code/Funktionen/hypersphere_seperated.RData")

# geringe Distance, da sonst alle Algorithmen perfekt klassifizieren
S6_data_train <- generate_dataset(200, 200, 2)
S6_data_test <- generate_dataset(200, 200, 2)

# hier noch: Modelle tunen

# Modelle fitten

S6_svm_linear <- svm(y ~., data = S6_data_train, kernel = "linear")
S6_svm_polynomial <- svm(y ~., data = S6_data_train, kernel = "polynomial")
S6_svm_radial <- svm(y ~., data = S6_data_train, kernel = "radial")
S6_logR <- glm(y ~., data = S6_data_train, family = binomial())
S6_k_NN <- knn(S6_data_train[, setdiff(names(S6_data_train), "y")],
               S6_data_test[, setdiff(names(S6_data_test), "y")],
               S6_data_train[, "y"],
               k = 10)

# Predictions

S6_prediction_linear <- predict(S6_svm_linear, S6_data_test)
S6_prediction_polynomial <- predict(S6_svm_polynomial, S6_data_test)
S6_prediction_radial <- predict(S6_svm_radial, S6_data_test)
S6_probabilities_logR <- predict(S6_logR, S6_data_test, type = "response")
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

data.frame(linear = S6_accuracy_linear, polynomial = S6_accuracy_polynomial, radial = S6_accuracy_radial, logR = S6_accuracy_logR, k_NN = S6_accuracy_k_NN, row.names = "S6_Accuracy")

# ROC/AUC
library(pROC)
S6_roc_linear <- roc(response = S6_data_test$y, predictor = as.numeric(S6_prediction_linear))
S6_roc_polynomial <- roc(response = S6_data_test$y, predictor = as.numeric(S6_prediction_polynomial))
S6_roc_radial <- roc(response = S6_data_test$y, predictor = as.numeric(S6_prediction_radial))
S6_roc_logR <- roc(response = S6_data_test$y, predictor = as.numeric(S6_prediction_logR))
S6_roc_k_NN <- roc(response = S6_data_test$y, predictor = as.numeric(S6_k_NN))

plot(S6_roc_linear, col = "blue", main = "ROC-Kurven Szenario 6")
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

load(file = "Code/Funktionen/hypersphere_seperated.RData")

# geringe Distance, da sonst alle Algorithmen perfekt klassifizieren
S9_data_train <- generate_dataset(50, 200, 2)
S9_data_test <- generate_dataset(50, 200, 2)

# anzupassen: Modelle tunen

library("rBayesianOptimization")
optimize_function <- function(C, gamma) {
  model <- svm(y ~ ., data = S9_data_train, kernel = "radial", cost = C, gamma = gamma)
  prediction <- predict(model, S9_data_test)
  accuracy <- mean(prediction == S9_data_test$y)
  list(Score = accuracy, Pred = 0)
}

bayes_opt_result <- BayesianOptimization(
  FUN = optimize_function,
  bounds = list(C = c(0.01, 50), gamma = c(0.01, 10)),
  init_points = 5,
  n_iter = 20,
  acq = "ucb"
)

# Modelle fitten

S9_svm_linear <- svm(y ~., data = S9_data_train, kernel = "linear")
S9_svm_polynomial <- svm(y ~., data = S9_data_train, kernel = "polynomial")
S9_svm_radial <- svm(y ~., data = S9_data_train, kernel = "radial")
S9_logR <- glm(y ~., data = S9_data_train, family = binomial())
S9_k_NN <- knn(S9_data_train[, setdiff(names(S9_data_train), "y")],
               S9_data_test[, setdiff(names(S9_data_test), "y")],
               S9_data_train[, "y"],
               k = 10)

# Predictions

S9_prediction_linear <- predict(S9_svm_linear, S9_data_test)
S9_prediction_polynomial <- predict(S9_svm_polynomial, S9_data_test)
S9_prediction_radial <- predict(S9_svm_radial, S9_data_test)
S9_probabilities_logR <- predict(S9_logR, S9_data_test, type = "response")
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

data.frame(linear = S9_accuracy_linear, polynomial = S9_accuracy_polynomial, radial = S9_accuracy_radial, logR = S9_accuracy_logR, k_NN = S9_accuracy_k_NN, row.names = "S9_Accuracy")

# ROC/AUC
library(pROC)
S9_roc_linear <- roc(response = S9_data_test$y, predictor = as.numeric(S9_prediction_linear))
S9_roc_polynomial <- roc(response = S9_data_test$y, predictor = as.numeric(S9_prediction_polynomial))
S9_roc_radial <- roc(response = S9_data_test$y, predictor = as.numeric(S9_prediction_radial))
S9_roc_logR <- roc(response = S9_data_test$y, predictor = as.numeric(S9_prediction_logR))
S9_roc_k_NN <- roc(response = S9_data_test$y, predictor = as.numeric(S9_k_NN))

plot(S9_roc_linear, col = "blue", main = "ROC-Kurven Szenario 9")
plot(S9_roc_polynomial, col = "red", add = TRUE)
plot(S9_roc_radial, col = "green", add = TRUE)
plot(S9_roc_logR, col = "violet", add = TRUE)
plot(S9_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S9_roc_linear), ")"), paste("polynomial (AUC:", auc(S9_roc_polynomial), ")"), paste("radial (AUC:", auc(S9_roc_radial), ")"), paste("logR (AUC:", auc(S9_roc_logR), ")"), paste("k_NN (AUC:", auc(S9_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)
