######################## Comparison to LDA ###################################


library(MASS)
lda_model <- lda(dg10 ~ ., data = allb_train)
lda_model

prediction <- predict(lda_model, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)



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