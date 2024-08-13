##radial structured data
#vorgehensweise in 3 dimensionen
#äußerere sphäre
azimutalw <- runif(500,0,2*pi)
polarw <- runif(500,0,2*pi)
r <- 2+rgamma(500,scale= 1,shape=2)
x1 <- r*sin(polarw)*cos(azimutalw)
y1 <- r*sin(polarw)*sin(azimutalw)
z1 <- r*cos(polarw)

#innerer sphäre
azimutalw <- runif(500,0,2*pi)
polarw <- runif(500,0,2*pi)
r2 <- runif(500,0,2)
x2 <- r2*sin(polarw)*cos(azimutalw)
y2 <- r2*sin(polarw)*sin(azimutalw)
z2 <- r2*cos(polarw)

group <- c(rep(1,500),rep(2,500))
df <- data.frame(x=c(x1,x2),y=c(y1,y2),z=c(z1,z2),group = group)
cor(df)
library(rgl)
plot3d(df$x,df$y,df$z,col = group)

#erweiterung in mehrere Dimensionen

generate_observation <- function(n, distance, jitter = 0.5) {
  #library(copula)
  #myCop <- normalCopula(param = rep(correl,((n-1)*(n-2))/2), dim = n-1, dispstr = "un")
  #paramMargins <- replicate(n-2, list(min = 0, max = pi), simplify = FALSE)
  #paramMargins[[n-1]] <- list(min=0,max =2*pi)
  #myMvd <- mvdc(copula = myCop, 
  #              margins = rep("unif",n-1),
  #              paramMargins = paramMargins)
  #winkel <- as.vector(rMvdc(1,myMvd))
  winkel <- runif(n - 1, 0, pi)
  winkel[n - 1] <- runif(1, 0, 2 * pi)
  radius <- rnorm(1, distance, jitter)
  x <- vector(mode = "numeric", length = n)
  
  for (i in 1:(n-1)) {
    if (i == 1) {
      x[i] <- radius * cos(winkel[i])
    } else {
      x[i] <- radius * prod(sin(winkel[1:(i-1)])) * cos(winkel[i])
    }
  }
  x[n] <- radius * prod(sin(winkel))
  
  return(x)
}
generate_observation(20,3)

generate_dataset <- function(obs,variables,distance,jitter=0.2){
  dat <- matrix(0,nrow = obs,ncol = variables)
  for(i in 1:obs){
    dat[i,] <- generate_observation(variables,distance,jitter = jitter)
  }
  return(as.data.frame(dat))
}

dat <- generate_dataset(20,20,3,correl = 0.5)
cov(dat)