# Pakete
library(e1071)
library(class)

## Szenario 1: p << n (p = 1000, n = 10)

# Datengenerierung

load(file = "Code/Funktionen/quadratic_hyperplane_seperated.RData")

S2_data_train <- generate_dataset(1000, 3, 10, 2, 2024)
S2_data_test <- generate_dataset(1000, 3, 10, 2, 2024)

plot3d(S2_data_train$V1, S2_data_train$V2, S2_data_train$V3, col = S2_data_train$y)

# hier noch: Modelle tunen

# Modelle fitten

S2_svm_linear <- svm(y ~., data = S2_data_train, kernel = "linear")
S2_svm_polynomial <- svm(y ~., data = S2_data_train, kernel = "polynomial")
S2_svm_radial <- svm(y ~., data = S2_data_train, kernel = "radial")
S2_logR <- glm(y ~., data = S2_data_train, family = binomial())
S2_k_NN <- knn(S2_data_train[, setdiff(names(S2_data_train), "y")],
               S2_data_test[, setdiff(names(S2_data_test), "y")],
               S2_data_train[, "y"],
               k = 10)

# Predictions

S2_prediction_linear <- predict(S2_svm_linear, S2_data_test)
S2_prediction_polynomial <- predict(S2_svm_polynomial, S2_data_test)
S2_prediction_radial <- predict(S2_svm_radial, S2_data_test)
S2_probabilities_logR <- predict(S2_logR, S2_data_test, type = "response")
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

data.frame(linear = S2_accuracy_linear, polynomial = S2_accuracy_polynomial, radial = S2_accuracy_radial, logR = S2_accuracy_logR, k_NN = S2_accuracy_k_NN, row.names = "S2_Accuracy")

# ROC/AUC
library(pROC)
S2_roc_linear <- roc(response = S2_data_test$y, predictor = as.numeric(S2_prediction_linear))
S2_roc_polynomial <- roc(response = S2_data_test$y, predictor = as.numeric(S2_prediction_polynomial))
S2_roc_radial <- roc(response = S2_data_test$y, predictor = as.numeric(S2_prediction_radial))
S2_roc_logR <- roc(response = S2_data_test$y, predictor = as.numeric(S2_prediction_logR))
S2_roc_k_NN <- roc(response = S2_data_test$y, predictor = as.numeric(S2_k_NN))

plot(S2_roc_linear, col = "blue", main = "ROC-Kurven Szenario 2")
plot(S2_roc_polynomial, col = "red", add = TRUE)
plot(S2_roc_radial, col = "green", add = TRUE)
plot(S2_roc_logR, col = "violet", add = TRUE)
plot(S2_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S2_roc_linear), ")"), paste("polynomial (AUC:", auc(S2_roc_polynomial), ")"), paste("radial (AUC:", auc(S2_roc_radial), ")"), paste("logR (AUC:", auc(S2_roc_logR), ")"), paste("k_NN (AUC:", auc(S2_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 5: p = n (p = 200, n = 200)

# Datengenerierung

load(file = "Code/Funktionen/quadratic_hyperplane_seperated.RData")

S5_data_train <- generate_dataset(200, 200, 20, 2, 2024)
S5_data_test <- generate_dataset(200, 200, 20, 2, 2024)

# hier noch: Modelle tunen

# Modelle fitten

S5_svm_linear <- svm(y ~., data = S5_data_train, kernel = "linear")
S5_svm_polynomial <- svm(y ~., data = S5_data_train, kernel = "polynomial")
S5_svm_radial <- svm(y ~., data = S5_data_train, kernel = "radial")
S5_logR <- glm(y ~., data = S5_data_train, family = binomial())
S5_k_NN <- knn(S5_data_train[, setdiff(names(S5_data_train), "y")],
               S5_data_test[, setdiff(names(S5_data_test), "y")],
               S5_data_train[, "y"],
               k = 10)

# Predictions

S5_prediction_linear <- predict(S5_svm_linear, S5_data_test)
S5_prediction_polynomial <- predict(S5_svm_polynomial, S5_data_test)
S5_prediction_radial <- predict(S5_svm_radial, S5_data_test)
S5_probabilities_logR <- predict(S5_logR, S5_data_test, type = "response")
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

data.frame(linear = S5_accuracy_linear, polynomial = S5_accuracy_polynomial, radial = S5_accuracy_radial, logR = S5_accuracy_logR, k_NN = S5_accuracy_k_NN, row.names = "S5_Accuracy")

# ROC/AUC
library(pROC)
S5_roc_linear <- roc(response = S5_data_test$y, predictor = as.numeric(S5_prediction_linear))
S5_roc_polynomial <- roc(response = S5_data_test$y, predictor = as.numeric(S5_prediction_polynomial))
S5_roc_radial <- roc(response = S5_data_test$y, predictor = as.numeric(S5_prediction_radial))
S5_roc_logR <- roc(response = S5_data_test$y, predictor = as.numeric(S5_prediction_logR))
S5_roc_k_NN <- roc(response = S5_data_test$y, predictor = as.numeric(S5_k_NN))

plot(S5_roc_linear, col = "blue", main = "ROC-Kurven Szenario 5")
plot(S5_roc_polynomial, col = "red", add = TRUE)
plot(S5_roc_radial, col = "green", add = TRUE)
plot(S5_roc_logR, col = "violet", add = TRUE)
plot(S5_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S5_roc_linear), ")"), paste("polynomial (AUC:", auc(S5_roc_polynomial), ")"), paste("radial (AUC:", auc(S5_roc_radial), ")"), paste("logR (AUC:", auc(S5_roc_logR), ")"), paste("k_NN (AUC:", auc(S5_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 8: p >> n (p = 200, n = 50)

# Datengenerierung

load(file = "Code/Funktionen/quadratic_hyperplane_seperated.RData")

S8_data_train <- generate_dataset(50, 200, 20, 2, 2024)
S8_data_test <- generate_dataset(50, 200, 20, 2, 2024)

# hier noch: Modelle tunen

# Modelle fitten

S8_svm_linear <- svm(y ~., data = S8_data_train, kernel = "linear")
S8_svm_polynomial <- svm(y ~., data = S8_data_train, kernel = "polynomial")
S8_svm_radial <- svm(y ~., data = S8_data_train, kernel = "radial")
S8_logR <- glm(y ~., data = S8_data_train, family = binomial())
S8_k_NN <- knn(S8_data_train[, setdiff(names(S8_data_train), "y")],
               S8_data_test[, setdiff(names(S8_data_test), "y")],
               S8_data_train[, "y"],
               k = 10)

# Predictions

S8_prediction_linear <- predict(S8_svm_linear, S8_data_test)
S8_prediction_polynomial <- predict(S8_svm_polynomial, S8_data_test)
S8_prediction_radial <- predict(S8_svm_radial, S8_data_test)
S8_probabilities_logR <- predict(S8_logR, S8_data_test, type = "response")
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

data.frame(linear = S8_accuracy_linear, polynomial = S8_accuracy_polynomial, radial = S8_accuracy_radial, logR = S8_accuracy_logR, k_NN = S8_accuracy_k_NN, row.names = "S8_Accuracy")

# ROC/AUC
library(pROC)
S8_roc_linear <- roc(response = S8_data_test$y, predictor = as.numeric(S8_prediction_linear))
S8_roc_polynomial <- roc(response = S8_data_test$y, predictor = as.numeric(S8_prediction_polynomial))
S8_roc_radial <- roc(response = S8_data_test$y, predictor = as.numeric(S8_prediction_radial))
S8_roc_logR <- roc(response = S8_data_test$y, predictor = as.numeric(S8_prediction_logR))
S8_roc_k_NN <- roc(response = S8_data_test$y, predictor = as.numeric(S8_k_NN))

plot(S8_roc_linear, col = "blue", main = "ROC-Kurven Szenario 8")
plot(S8_roc_polynomial, col = "red", add = TRUE)
plot(S8_roc_radial, col = "green", add = TRUE)
plot(S8_roc_logR, col = "violet", add = TRUE)
plot(S8_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S8_roc_linear), ")"), paste("polynomial (AUC:", auc(S8_roc_polynomial), ")"), paste("radial (AUC:", auc(S8_roc_radial), ")"), paste("logR (AUC:", auc(S8_roc_logR), ")"), paste("k_NN (AUC:", auc(S8_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)
