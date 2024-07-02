library(e1071)
library(ggplot2)

##############################################################################
########################### lineare Daten ####################################
##############################################################################

# Funktion zum Generieren von Daten
generate_linear_data <- function(n, p, mean1, mean2, sd1, sd2) {
  X <- data.frame(matrix(nrow = n, ncol = p))
  y <- as.factor(c(rep(0, n/2), rep(1, n/2)))
  
  for (i in 1:p) {
    X[,i] <- c(rnorm(n/2, mean = mean1, sd = sd1), rnorm(n/2, mean = mean2, sd = sd2))
  }
  
  cbind(y, X)
}

#generiere lineare Daten
set.seed(42)
linear_data_sep <- generate_linear_data(200, 2, 0, 4, 1, 1)

linear_data_nonsep <- generate_linear_data(200, 2, 0, 2, 1, 1)

#plotte ersten beiden Variablen

ggplot(linear_data_sep, aes(x = X2, y = X1, color = y)) +
  geom_point(size = 2) +
  labs(title = "Linear Data", x = "x1", y = "x2") +
  theme_minimal()

ggplot(linear_data_nonsep, aes(x = X2, y = X1, color = y)) +
  geom_point(size = 2) +
  labs(title = "Linear Data", x = "x1", y = "x2") +
  theme_minimal()

#Hyperparameter tunen
tune_linear_sep <- tune(svm,
                    y ~ .,
                    data = linear_data_sep,
                    kernel = "linear",
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

tune_linear_nonsep <- tune(svm,
                        y ~ .,
                        data = linear_data_nonsep,
                        kernel = "linear",
                        ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

tune_radial_nonsep <- tune(svm,
                          y ~ .,
                          data = linear_data_nonsep,
                          kernel = "radial",
                          ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                                        gamma = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

#bestes model finden
summary(tune_linear_sep)
tune_linear_sep$best.parameters

summary(tune_linear_nonsep)
tune_linear_nonsep$best.parameters

summary(tune_radial_nonsep)
tune_radial_nonsep$best.parameters

#bestes Model erstellen
model1 <- svm(y ~., data = linear_data_sep, kernel = "linear", cost = tune_linear_sep$best.parameters$cost)

model2 <- svm(y ~., data = linear_data_nonsep, kernel = "linear", cost = tune_linear_nonsep$best.parameters$cost)

model3 <- svm(y ~., data = linear_data_nonsep, kernel = "radial", cost = tune_radial_nonsep$best.parameters$cost, gamma = tune_radial_nonsep$best.parameters$gamma)

#Model plotten
plot(model1, linear_data_sep)

plot(model2, linear_data_nonsep)

plot(model3, linear_data_nonsep)

#Anzahl der Support Vectors
summary(model1)

summary(model2)

summary(model3)

#Testdaten kreieren
linear_testdata_sep <- generate_linear_data(200, 2, 0, 4, 1, 1)
linear_testdata_nonsep <- generate_linear_data(200, 2, 0, 2, 1, 1)

# predictions auf Testdaten
prediction_linear_sep <- predict(model1, linear_testdata_sep)
prediction_linear_nonsep <- predict(model2, linear_testdata_nonsep)
prediction_radial_nonsep <- predict(model3, linear_testdata_nonsep)

# Accuracy
confusion_matrix_linear_sep <- table(prediction_linear_sep, linear_testdata_sep$y)
sum(diag(confusion_matrix_linear_sep))/sum(confusion_matrix_linear_sep)

confusion_matrix_linear_nonsep <- table(prediction_linear_nonsep, linear_testdata_nonsep$y)
sum(diag(confusion_matrix_linear_nonsep))/sum(confusion_matrix_linear_nonsep)

confusion_matrix_radial_nonsep <- table(prediction_radial_nonsep, linear_testdata_nonsep$y)
sum(diag(confusion_matrix_radial_nonsep))/sum(confusion_matrix_radial_nonsep)

##############################################################################
####################### nichtlineare Daten, für Radial #######################
##############################################################################

# Funktion zur Erzeugung des Datensatzes
generate_nonlinear_separable_data <- function(n, p) {
  theta <- runif(n, 0, 2 * pi)
  r1 <- 1 + 0.2 * rnorm(n / 2)
  r2 <- 1.5 + 0.2 * rnorm(n / 2)
  
  X <- data.frame(matrix(nrow = n, ncol = p))
  
  for (i in 1:p) {
    if (i %% 2 != 0) {
      X[,i] <- c(r1 * cos(theta[1:(n / 2)]), r2 * cos(theta[(n / 2 + 1):n]))
    } else {
      X[,i] <- c(r1 * sin(theta[1:(n / 2)]), r2 * sin(theta[(n / 2 + 1):n]))
    }
    colnames(X)[i] <- paste0('x', i)
  }
  
  y <- as.factor(c(rep(0, n / 2), rep(1, n / 2)))
  
  cbind(y, X)
}

# kreiere Datensatz
set.seed(24)
data <- generate_nonlinear_separable_data(100, 2)
testdata <- generate_nonlinear_separable_data(100, 2)

# Plotte die ersten beiden Variablen
ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2) +
  labs(title = "Non-linear but Well-Separable Data", x = "x1", y = "x2") +
  theme_minimal()

#hyperparameter tuning
tune_linear <- tune(svm,
                    y ~ .,
                    data = data,
                    kernel = "linear",
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune_radial <- tune(
  svm,
  y ~ .,
  data = data,
  kernel = "radial",
  ranges = list(
    cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
    gamma = c(0.001, 0.01, 0.1, 1, 5, 10, 100)
  )
)

#bestes model finden
summary(tune_linear)
tune_linear$best.parameters
summary(tune_radial)
tune_radial$best.parameters

# Modelle mit besten Parametern erstellen
model_linear <-
  svm(
    y ~ .,
    data = data,
    kernel = "linear",
    cost = tune_linear$best.parameters$cost
  )
model_radial <-
  svm(
    y ~ .,
    data = data,
    kernel = "radial",
    cost = tune_radial$best.parameters$cost,
    gamma = tune_radial$best.parameters$gamma
  )

# Model plotten
plot(model_linear, data)
plot(model_radial, data)

# Anzahl Support Vektoren
summary(model_linear)
summary(model_radial)

# auf Testdaten anwenden
prediction_linear <- predict(model_linear, testdata)
prediction_radial <- predict(model_radial, testdata)

# Accuracy
confusion_matrix_linear <- table(prediction_linear, testdata$y)
sum(diag(confusion_matrix_linear))/sum(confusion_matrix_linear)

confusion_matrix_radial <- table(prediction_radial, testdata$y)
sum(diag(confusion_matrix_radial))/sum(confusion_matrix_radial)
