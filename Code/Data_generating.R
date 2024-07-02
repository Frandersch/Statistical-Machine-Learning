library(e1071)
library(ggplot2)
# zu verschiedenen Graden linear separable Daten

generate_linear_data <- function(n, p, mean1, mean2, sd1, sd2) {
  X <- data.frame(matrix(nrow = n, ncol = p))
  y <- as.factor(c(rep(0, n/2), rep(1, n/2)))
  
  for (i in 1:p) {
    X[,i] <- c(rnorm(n/2, mean = mean1, sd = sd1), rnorm(n/2, mean = mean2, sd = sd2))
  }
  
  cbind(y, X)
}

linear_data <- generate_linear_data(200, 10, 0, 4, 1, 1)

# plotte ersten beiden Variablen

ggplot(linear_data, aes(x = X1, y = X2, color = y)) +
  geom_point(size = 2) +
  labs(title = "Linear Data", x = "x1", y = "x2") +
  theme_minimal()


# nichtlineare Daten generieren

generate_nonlinear_data <- function(n, p, mean1, mean2, sd1, sd2) {
  X <- data.frame(matrix(nrow = n, ncol = p))
  y <- as.factor(c(rep(0, n/2), rep(1, n/2)))
  
  for (i in 1:p) {
    X[, i] <- c(rnorm(n/2, mean = mean1, sd = sd1), rnorm(n/2, mean = mean2, sd = sd2))
    if (i %% 2 == 0) {
      X[, i] <- X[, i] + X[, i] * 3
    } else {
      X[, i] <- X[, i] + X[, i] ^ 2
    }
  }
  
  cbind(y, X)
}

nonlinear_data <- generate_nonlinear_data(100, 4, 0, 2, 1, 1)

# plotte ersten beiden Variablen

ggplot(nonlinear_data, aes(x = X1, y = X2, color = y)) +
  geom_point(size = 2) +
  labs(title = "Non-linear Data", x = "x1", y = "x2") +
  theme_minimal()


####################### nichtlineare Daten, für Radial #######################

# Funktion zur Erzeugung des Datensatzes
generate_nonlinear_separable_data <- function(n, p) {
  theta <- runif(n, 0, 2 * pi)
  r1 <- 1 + 0.1 * rnorm(n / 2)
  r2 <- 2 + 0.1 * rnorm(n / 2)
  
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
data <- generate_nonlinear_separable_data(100, 6)
testdata <- generate_nonlinear_separable_data(100, 6)

# Plotte die ersten beiden Variablen
ggplot(data$X, aes(x = x1, y = x2, color = data$y)) +
  geom_point(size = 2) +
  labs(title = "Non-linear but Well-Separable Data", x = "x1", y = "x2") +
  theme_minimal()

#hyperparameter tuning
set.seed(1)
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
model_linear <- svm(y ~ ., data = data, kernel = "linear", cost = 0.1)
model_radial <- svm(y ~ ., data = data, kernel = "radial", cost = 10, gamma = 0.01)

# auf Testdaten anwenden
prediction_linear <- predict(model_linear, testdata)
prediction_radial <- predict(model_radial, testdata)

# Accuracy
confusion_matrix_linear <- table(prediction_linear, testdata$y)
sum(diag(confusion_matrix_linear))/sum(confusion_matrix_linear)

confusion_matrix_radial <- table(prediction_radial, testdata$y)
sum(diag(confusion_matrix_radial))/sum(confusion_matrix_radial)
