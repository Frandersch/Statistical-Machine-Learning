# Pakete
library(e1071)
library(class)

## Szenario 1: p << n (n = 1000, p = 10)

# Datengenerierung

load(file = "Code/Funktionen/hyperplane_seperated.RData")

# geringe Distance, da sonst alle perfekt klassifizieren
S1_data_train <- generate_dataset(1000, 10, 0.5, 0.3, 2024)
S1_data_test <- generate_dataset(1000, 10, 0.5, 0.3, 2024)

# hier noch: Modelle tunen

# Modelle fitten

S1_svm_linear <- svm(y ~., data = S1_data_train, kernel = "linear")
S1_svm_polynomial <- svm(y ~., data = S1_data_train, kernel = "polynomial")
S1_svm_radial <- svm(y ~., data = S1_data_train, kernel = "radial")
S1_logR <- glm(y ~., data = S1_data_train, family = binomial())
S1_k_NN <- knn(S1_data_train[, setdiff(names(S1_data_train), "y")],
            S1_data_test[, setdiff(names(S1_data_test), "y")],
            S1_data_train[, "y"],
            k = 10)

# Predictions

S1_prediction_linear <- predict(S1_svm_linear, S1_data_test)
S1_prediction_polynomial <- predict(S1_svm_polynomial, S1_data_test)
S1_prediction_radial <- predict(S1_svm_radial, S1_data_test)
S1_probabilities_logR <- predict(S1_logR, S1_data_test, type = "response")
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

data.frame(linear = S1_accuracy_linear, polynomial = S1_accuracy_polynomial, radial = S1_accuracy_radial, logR = S1_accuracy_logR, k_NN = S1_accuracy_k_NN, row.names = "S1_Accuracy")

# ROC/AUC
library(pROC)
S1_roc_linear <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_linear))
S1_roc_polynomial <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_polynomial))
S1_roc_radial <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_radial))
S1_roc_logR <- roc(response = S1_data_test$y, predictor = as.numeric(S1_prediction_logR))
S1_roc_k_NN <- roc(response = S1_data_test$y, predictor = as.numeric(S1_k_NN))

plot(S1_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S1_roc_polynomial, col = "red", add = TRUE)
plot(S1_roc_radial, col = "green", add = TRUE)
plot(S1_roc_logR, col = "violet", add = TRUE)
plot(S1_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S1_roc_linear), ")"), paste("polynomial (AUC:", auc(S1_roc_polynomial), ")"), paste("radial (AUC:", auc(S1_roc_radial), ")"), paste("logR (AUC:", auc(S1_roc_logR), ")"), paste("k_NN (AUC:", auc(S1_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 4: p = n (p = 200, n = 200)

# Datengenerierung

load(file = "Code/Funktionen/hyperplane_seperated.RData")

S4_data_train <- generate_dataset(50, 50, 3, 0.5, 2024)
S4_data_test <- generate_dataset(50, 50, 3, 0.5, 2024)

# hier noch: Modelle tunen

# Modelle fitten

S4_svm_linear <- svm(y ~., data = S4_data_train, kernel = "linear")
S4_svm_polynomial <- svm(y ~., data = S4_data_train, kernel = "polynomial")
S4_svm_radial <- svm(y ~., data = S4_data_train, kernel = "radial")
S4_logR <- glm(y ~., data = S4_data_train, family = binomial())
S4_k_NN <- knn(S4_data_train[, setdiff(names(S4_data_train), "y")],
               S4_data_test[, setdiff(names(S4_data_test), "y")],
               S4_data_train[, "y"],
               k = 10)

# Predictions

S4_prediction_linear <- predict(S4_svm_linear, S4_data_test)
S4_prediction_polynomial <- predict(S4_svm_polynomial, S4_data_test)
S4_prediction_radial <- predict(S4_svm_radial, S4_data_test)
S4_probabilities_logR <- predict(S4_logR, S4_data_test, type = "response")
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

data.frame(linear = S4_accuracy_linear, polynomial = S4_accuracy_polynomial, radial = S4_accuracy_radial, logR = S4_accuracy_logR, k_NN = S4_accuracy_k_NN, row.names = "S4_Accuracy")

# ROC/AUC
library(pROC)
S4_roc_linear <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_linear))
S4_roc_polynomial <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_polynomial))
S4_roc_radial <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_radial))
S4_roc_logR <- roc(response = S4_data_test$y, predictor = as.numeric(S4_prediction_logR))
S4_roc_k_NN <- roc(response = S4_data_test$y, predictor = as.numeric(S4_k_NN))

plot(S4_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S4_roc_polynomial, col = "red", add = TRUE)
plot(S4_roc_radial, col = "green", add = TRUE)
plot(S4_roc_logR, col = "violet", add = TRUE)
plot(S4_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S4_roc_linear), ")"), paste("polynomial (AUC:", auc(S4_roc_polynomial), ")"), paste("radial (AUC:", auc(S4_roc_radial), ")"), paste("logR (AUC:", auc(S4_roc_logR), ")"), paste("k_NN (AUC:", auc(S4_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)

## Szenario 7: p >> n (p = 200, n = 50)

# Datengenerierung

load(file = "Code/Funktionen/hyperplane_seperated.RData")

S7_data_train <- generate_dataset(50, 200, 3, 0.5, 2024)
S7_data_test <- generate_dataset(50, 200, 3, 0.5, 2024)

# hier noch: Modelle tunen

# Modelle fitten

S7_svm_linear <- svm(y ~., data = S7_data_train, kernel = "linear")
S7_svm_polynomial <- svm(y ~., data = S7_data_train, kernel = "polynomial")
S7_svm_radial <- svm(y ~., data = S7_data_train, kernel = "radial")
S7_logR <- glm(y ~., data = S7_data_train, family = binomial())
S7_k_NN <- knn(S7_data_train[, setdiff(names(S7_data_train), "y")],
               S7_data_test[, setdiff(names(S7_data_test), "y")],
               S7_data_train[, "y"],
               k = 10)

# Predictions

S7_prediction_linear <- predict(S7_svm_linear, S7_data_test)
S7_prediction_polynomial <- predict(S7_svm_polynomial, S7_data_test)
S7_prediction_radial <- predict(S7_svm_radial, S7_data_test)
S7_probabilities_logR <- predict(S7_logR, S7_data_test, type = "response")
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

data.frame(linear = S7_accuracy_linear, polynomial = S7_accuracy_polynomial, radial = S7_accuracy_radial, logR = S7_accuracy_logR, k_NN = S7_accuracy_k_NN, row.names = "S7_Accuracy")

# ROC/AUC
library(pROC)
S7_roc_linear <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_linear))
S7_roc_polynomial <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_polynomial))
S7_roc_radial <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_radial))
S7_roc_logR <- roc(response = S7_data_test$y, predictor = as.numeric(S7_prediction_logR))
S7_roc_k_NN <- roc(response = S7_data_test$y, predictor = as.numeric(S7_k_NN))

plot(S7_roc_linear, col = "blue", main = "ROC-Kurven Szenario 1")
plot(S7_roc_polynomial, col = "red", add = TRUE)
plot(S7_roc_radial, col = "green", add = TRUE)
plot(S7_roc_logR, col = "violet", add = TRUE)
plot(S7_roc_k_NN, col = "orange", add = TRUE)
legend("bottomright",
       legend = c(paste("linear (AUC:", auc(S7_roc_linear), ")"), paste("polynomial (AUC:", auc(S7_roc_polynomial), ")"), paste("radial (AUC:", auc(S7_roc_radial), ")"), paste("logR (AUC:", auc(S7_roc_logR), ")"), paste("k_NN (AUC:", auc(S7_roc_k_NN), ")")),
       col = c("blue", "red", "green", "violet", "orange"),
       lwd = 2)
