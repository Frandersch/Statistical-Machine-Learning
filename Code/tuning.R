allb_prep <- Datenpräparation_Allbus(allb)
#test und train Daten definieren
set.seed(123)
train <- sample(1:nrow(allb_prep), round(nrow(allb_prep)*0.7))
allb_train <- allb_prep[train, ]
allb_test <- allb_prep[-train, ]

#support vector classifier mit c = 1
svc_model <- svm(dg10 ~ ., data = allb_train, kernel = "linear", cost = 1)
summary(svc_model)

predictions <- predict(svc_model, allb_test)

confusion_matrix <- table(predictions, allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene Cost-Budgets
tuning_c <- function(costs) {
  matrixcosts <- matrix(rep(NA,2*length(costs)),nrow = length(costs),ncol=2)
  for (i in 1:length(costs)) {
    svc_model <-
      svm(dg10 ~ .,
          data = allb_train,
          kernel = "linear",
          cost = costs[i])
    predictions <- predict(svc_model, allb_test)
    confusion_matrix <-
      table(predicted = predictions, real = allb_test$dg10)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    matrixcosts[i,1] <- costs[i]
    matrixcosts[i,2] <- accuracy
  }
  colnames(matrixcosts) <- c("cost","accuracy")
  return(matrixcosts)
}

tuning_c(c(0.1, 1,10))
#best for cost of 1

#support vector machine mit polynomial Kernel
svm_model_polynomial <- svm(dg10 ~ ., data = allb_train, kernel = "polynomial")
summary(svm_model_polynomial)

predictions <- predict(svm_model_polynomial, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene C und degree für polynomial Kernel
tuning_c_degree <- function(costs, degree) {
  tuning_grid <- matrix(rep(NA,length(costs)*length(degree)*3),
                        nrow = length(costs)*length(degree),
                        ncol =3)
  row_index <- 1
  for (i in 1:length(costs)) {
    for (j in 1:length(degree)) {
      svm_model <-
        svm(
          dg10 ~ .,
          data = allb_train,
          kernel = "polynomial",
          cost = costs[i],
          degree = degree[j]
        )
      predictions <- predict(svm_model, allb_test)
      confusion_matrix <-
        table(predicted = predictions, real = allb_test$dg10)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      tuning_grid[row_index,1] <-costs[i]
      tuning_grid[row_index,2] <- degree[j]
      tuning_grid[row_index,3] <- accuracy
      row_index <- row_index+1
    }
  }
  colnames(tuning_grid) <- c("cost","degree","accuracy")
  return(tuning_grid)
}

tuning_c_degree(c(1, 10, 20), c(2, 3, 4))
#best accuracy for cost of 10 and degree of 3

#support vector machine mit radial Kernel
svm_model_radial <- svm(dg10 ~ ., data = allb_train, kernel = "radial")
summary(svm_model_radial)

predictions <- predict(svm_model_radial, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene C und Gamma für radial Kernel
tuning_c_gamma <- function(costs, gammas) {
  tuning_grid <- matrix(rep(NA,length(costs)*length(gammas)*3),
                        nrow = length(costs)*length(gammas),
                        ncol =3)
  row_index <- 1
  for (i in 1:length(costs)) {
    for (j in 1:length(gammas)) {
      svm_model <-
        svm(
          dg10 ~ .,
          data = allb_train,
          kernel = "radial",
          cost = costs[i],
          gamma = gammas[j]
        )
      predictions <- predict(svm_model, allb_test)
      confusion_matrix <-
        table(predicted = predictions, real = allb_test$dg10)
      accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
      tuning_grid[row_index,1] <-costs[i]
      tuning_grid[row_index,2] <- gammas[j]
      tuning_grid[row_index,3] <- accuracy
      row_index <- row_index+1
    }
  }
  colnames(tuning_grid) <- c("cost","gamma","accuracy")
  return(tuning_grid)
}

tuning_c_gamma(c(1, 10), c(0.001, 0.01, 0.1, 2))
#best for cost of 1 and gamma of 0.01

#Support Vector machines with sigmoid kernel
svm_model_sigmoid <- svm(dg10 ~ ., data = allb_train, kernel = "sigmoid")
summary(svm_model_sigmoid)

predictions <- predict(svm_model_sigmoid, allb_test)

confusion_matrix <- table(predicted = predictions, real = allb_test$dg10)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)

#verschiedene C, gamma und coef0 für sigmoid kernel

tuning_c_gamma_coef0 <- function(costs, gammas,coef0s) {
  tuning_grid <- matrix(rep(NA,length(costs)*length(gammas)*length(coef0s)*4),
                        nrow = length(costs)*length(gammas)*length(coef0s),
                        ncol =4)
  row_index <- 1
  for (i in 1:length(costs)) {
    for (j in 1:length(gammas)) {
      for(k in 1:length(coef0s)){
        svm_model <-
          svm(
            dg10 ~ .,
            data = allb_train,
            kernel = "sigmoid",
            cost = costs[i],
            gamma = gammas[j],
            coef0= coef0s[k]
          )
        predictions <- predict(svm_model, allb_test)
        confusion_matrix <-
          table(predicted = predictions, real = allb_test$dg10)
        accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
        tuning_grid[row_index,1] <-costs[i]
        tuning_grid[row_index,2] <- gammas[j]
        tuning_grid[row_index,3] <- coef0s[k]
        tuning_grid[row_index,4] <- accuracy
        row_index <- row_index+1 
      }
    }
  }
  colnames(tuning_grid) <- c("cost","gamma","coef0","accuracy")
  return(tuning_grid)
}

tuning_c_gamma_coef0(c(0.1, 1, 10),c(0.001, 0.01, 0.1),c(0.001, 0.01, 0.1))

#iterative tuning

#adaptive tuning