load("Code/Daten/hyperplane_seperated_data_train.RData")
load("Code/Daten/hyperplane_seperated_data_test.RData")
library(e1071)

svm_linear <- svm(Y ~., data = hyperplane_seperated_data_train, kernel = "linear")
svm_polynomial <- svm(Y ~., data = hyperplane_seperated_data_train, kernel = "polynomial")
svm_radial <- svm(Y ~., data = hyperplane_seperated_data_train, kernel = "radial")

prediction_linear <- predict(svm_linear, hyperplane_seperated_data_test)
prediction_polynomial <- predict(svm_polynomial, hyperplane_seperated_data_test)
prediction_radial <- predict(svm_radial, hyperplane_seperated_data_test)

confusion_matrix_linear <- table(prediction_linear, hyperplane_seperated_data_test$Y)
sum(diag(confusion_matrix_linear))/sum(confusion_matrix_linear)

confusion_matrix_polynomial <- table(prediction_polynomial, hyperplane_seperated_data_test$Y)
sum(diag(confusion_matrix_polynomial))/sum(confusion_matrix_polynomial)

confusion_matrix_radial <- table(prediction_radial, hyperplane_seperated_data_test$Y)
sum(diag(confusion_matrix_radial))/sum(confusion_matrix_radial)
