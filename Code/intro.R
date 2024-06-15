#Intro
library(e1071)
#data generating
set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ]+1
plot(x,col = (3-y))
dat <- data.frame(x = x, y = as.factor(y))

#model fitten
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10, scale = F)
#überprüfen
plot(svmfit, dat)
summary(svmfit)

#hyperparameter variieren (cost)
svmfit <- svm(y ~ .,data = dat, kernel = "linear", cost = 0.1, scale = F)
#überprüfen
