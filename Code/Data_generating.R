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
plot(linear_data$X1, linear_data$X2, col = linear_data$y)

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


# nichtlineare Daten, aber gut separiert

#set.seed(42)

generate_nonlinear_separable_data <- function(n) {
  theta <- runif(n, 0, 2*pi)
  r1 <- 1 + 0.1 * rnorm(n/2)
  r2 <- 2 + 0.1 * rnorm(n/2)
  
  x1 <- c(r1 * cos(theta[1:(n/2)]), r2 * cos(theta[(n/2 + 1):n]))
  x2 <- c(r1 * sin(theta[1:(n/2)]), r2 * sin(theta[(n/2 + 1):n]))
  
  X <- data.frame(x1 = x1, x2 = x2)
  y <- as.factor(c(rep(0, n/2), rep(1, n/2)))
  
  cbind(y, X)
}

data <- generate_nonlinear_separable_data(200)

#plotte Variablen
ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2) +
  labs(title = "Non-linear but Well-Separable Data", x = "x1", y = "x2") +
  theme_minimal()

model1 <- svm(y ~., data = data, kernel = "linear")
model2 <- svm(y ~., data = data, kernel = "radial")
summary(model1)
summary(model2)
