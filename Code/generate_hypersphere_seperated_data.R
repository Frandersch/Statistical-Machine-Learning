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

generate_observation <- function(n, distance, jitter, radius, seed) {
  set.seed(seed)
  winkel <- runif(n - 1, 0, pi)
  winkel[n - 1] <- runif(1, 0, 2 * pi)
  x <- vector(mode = "numeric", length = n)
  
  for (i in 1:(n-1)) {
    if (i == 1) {
      x[i] <- radius * cos(winkel[i])
    } else {
      x[i] <- radius * prod(sin(winkel[1:(i-1)])) * cos(winkel[i])
    }
  }
  x[n] <- radius * prod(sin(winkel))
  x <- x+rnorm(1,distance,jitter)*(x/sqrt(sum(x^2)))
  return(x)
}

generate_subdatasets <- function(obs,variables,distance,jitter,radius,seed){
  
  dat <- matrix(0,nrow = obs,ncol = variables)
  for(i in 1:obs){
    dat[i,] <- generate_observation(variables,distance=distance,jitter = jitter,radius=radius,seed=seed+i)
  }
  return(as.data.frame(dat))
}

generate_dataset <- function(obs,variables,distance,jitter,radius, seed1, seed2){
  group1 <- generate_subdatasets(obs = obs/2, variables = variables, distance = distance, jitter = jitter,radius = radius, seed = seed1)
  group1_y <- rep(1, times = obs/2)
  group1 <- cbind(group1, y = as.factor(group1_y))
  group2 <- generate_subdatasets(obs = obs/2, variables = variables, distance = (-distance), jitter = jitter,radius = radius, seed = seed2)
  group2_y <- rep(2, times = obs/2)
  group2 <- cbind(group2, y = as.factor(group2_y))
  dataset <- rbind(group1, group2)
  dataset
}


S3_data_train <- generate_dataset(obs = 1000, variables = 10, distance = 0.5,jitter = 0.3, radius = 10, seed1 = 1000, seed2 = 2000)
S3_data_test <- generate_dataset(obs = 1000, variables = 10, distance = 0.5,jitter = 0.3, radius = 10, seed1 = 3000, seed2 = 4000)

S6_data_train <- generate_dataset(obs = 50, variables = 50, distance = 3,jitter = 1.5, radius = 10, seed1 = 100, seed2 = 200)
S6_data_test <- generate_dataset(obs = 50, variables = 50, distance = 3,jitter = 1.5, radius = 10, seed1 = 300, seed2 = 400)

S9_data_train <- generate_dataset(obs = 50, variables = 200, distance = 3,jitter = 1.5, radius = 10, seed1 = 100, seed2 = 200)
S9_data_test <- generate_dataset(obs = 50, variables = 200, distance = 3,jitter = 1.5, radius = 10, seed1 = 300, seed2 = 400)

save(S3_data_train, S3_data_test, file = "Code/Daten/Data_S3.RData")
save(S6_data_train, S6_data_test, file = "Code/Daten/Data_S6.RData")
save(S9_data_train, S9_data_test, file = "Code/Daten/Data_S9.RData")
