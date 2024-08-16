#quadratic Hyperplane in 3 dimensional space
#z=ax^2+by^2+c,a=b=0.5,c=0
x <- rnorm(100,0,2)
y <- rnorm(100,0,2)
z <- 0.5*x^2+0.5*y^2
df <- data.frame(x=x,y=y,z=z)
library(rgl)
plot3d(x=df$x,y=df$y,z=df$z)
#finde normalenvektor
df2 <- data.frame(x=rep(0,100),
                  y=rep(0,100),
                  z=rep(0,100))
for(i in 1:nrow(df2)){
  df2[i,]<- unlist(df[i,])+(rnorm(1,3,1)*c(0,0,1))
}
df3 <- data.frame(x=rep(0,100),
                     y=rep(0,100),
                     z=rep(0,100))
for(i in 1:nrow(df3)){
  df3[i,]<- unlist(df[i,])+(rnorm(1,-3,1)*c(0,0,1))
}
df_final <- rbind(df2,df3)
df_final$group <- c(rep(1,100),rep(2,100))
plot3d(x=df_final$x,y=df_final$y,z=df_final$z,col=df_final$group)

#erweiterung in n dimensionen
generate_dataset <- function(obs,variables,distance,jitter,coef_min=-100,coef_max=100,center=0,range=3){
  coefficients <- runif(2*variables-1,coef_min,coef_max)
  xses <- matrix(rnorm(obs,center,range),nrow=obs,ncol=1)
 for(i in 2:(variables-1)){
   xses <- cbind(xses,rnorm(obs,center,range))
 }
  combinations<- xses%*%coefficients[1:(variables-1)]
  quadcombinations <- xses^2%*%coefficients[variables:(2*variables-2)]
  z <- combinations+quadcombinations+rep(coefficients[2+variables-1],obs)+rnorm(obs,distance,jitter)
  return(as.data.frame(cbind(xses,z)))
}
