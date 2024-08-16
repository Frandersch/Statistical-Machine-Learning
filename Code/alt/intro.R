#Intro
library(e1071)
#SUPPORT VECTOR CLASSIFIER

#data generating
set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ]+1 # schiebt die mit y=-1 etwas weiter weg von den mit y=1
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
plot(svmfit,dat)
summary(svmfit)

#hyperparameter tuning
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear", 
                 ranges = list(cost = c(0.001,0.01,0.1, 1, 5, 10, 100)))
#bestes model finden
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

#testdaten schaffen
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1,1), 20, replace=TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
plot(xtest, col = (3-ytest))
testdat <- data.frame(x = xtest, y = as.factor(ytest))

#predicten
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)


#perfect linear separable dataset
x[y==1,] <- x[y == 1,]+0.5
plot(x, col = (y+5)/2, pch = 19)
dat <- data.frame(x =x, y = as.factor(y))
#fitten mit hohem cost value für kleinen margin
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit,dat)

#SUPPORT VECTOR MACHINES
 #generiere nonlinear seperated data
set.seed(1)
x <- matrix(rnorm(200*2), ncol =2)
x[1:100, ] <- x[1:100,]+2 #schieb beobachtung 1 bis 100 nach oben rechts
x[101:150, ] <- x[101:150, ]-2 #schieb beobachtung 101 bos 150 nach unten links
y <- c(rep(1, 150),rep(2, 50))
dat <- data.frame(x=x,y = as.factor(y))
plot(x, col = y)

#fitten mit radial kernel
train <- sample(200, 100)
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost =1)
plot(svmfit, dat[train,])

#hyperparametertuning mit cost und gamma
set.seed(1)
tune.out <- tune(svm, y ~., data = dat[train,],
                 kernel = "radial",
                 ranges = list(
                   cost = c(0.1,1,10,100,1000),
                   gamma = c(0.5,1,2,3,4)
                 ))
summary(tune.out)

#anwenden
table(true = dat[-train, "y"],
      pred = predict(tune.out$best.model, newdata = dat[-train,]))
