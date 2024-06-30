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

lda_predictions <- predict(lda_model_roc, allb_test)

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

