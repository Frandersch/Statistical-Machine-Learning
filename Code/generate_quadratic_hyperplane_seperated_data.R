#quadratische Hyperebene im 3dimensionalen Raum
#z=ax^2+by^2+c,a=b=0.5,c=0
x <- rnorm(100,0,2)
y <- rnorm(100,0,2)
z <- 0.5*x^2+0.5*y^2
df <- data.frame(x=x,y=y,z=z)
library(rgl)
plot3d(x=df$x,y=df$y,z=df$z)
#verschiebung in richtung z-Achse nach oben
df2 <- data.frame(x=rep(0,100),
                  y=rep(0,100),
                  z=rep(0,100))
for(i in 1:nrow(df2)){
  df2[i,]<- unlist(df[i,])+(rnorm(1,3,1)*c(0,0,1))
}
#und nach unten
df3 <- data.frame(x=rep(0,100),
                     y=rep(0,100),
                     z=rep(0,100))
for(i in 1:nrow(df3)){
  df3[i,]<- unlist(df[i,])+(rnorm(1,-3,1)*c(0,0,1))
}
df_final <- rbind(df2,df3)
df_final$group <- c(rep(1,100),rep(2,100))
plot3d(x=df_final$x,y=df_final$y,z=df_final$z,col=df_final$group)

#erweiterung in p dimensionen
generate_subdatasets <- function(obs,variables,distance,jitter,seed,coefficients,center=0,range=3){
  set.seed(seed)
  #ziehe werte für X_1 bis X_{p-1}
  xses <- matrix(rnorm(obs,center,range),nrow=obs,ncol=1)
 for(i in 2:(variables-1)){
   set.seed(seed+i)
   xses <- cbind(xses,rnorm(obs,center,range))
 }
  #stelle nach X_p um
  combinations<- xses%*%coefficients[1:(variables-1)]
  quadcombinations <- xses^2%*%coefficients[variables:(2*variables-2)]
  #berechne X_p inklusive verschiebung
  z <- combinations+quadcombinations+rep(coefficients[2*variables-1],obs)+rnorm(obs,distance,jitter)
  as.data.frame(cbind(xses,z))
}
#fasse zu klassengetrennten daten zusammen
generate_dataset <- function(obs,variables,distance,jitter,seed1,seed2,coefficients,center=0,range=3){
  group1 <- generate_subdatasets(obs = obs/2, variables = variables, distance = distance, jitter = jitter, seed = seed1,coefficients=coefficients,center=center,range=range)
  group1_y <- rep(1, times = obs/2)
  group1 <- cbind(group1, y = as.factor(group1_y))
  group2 <- generate_subdatasets(obs = obs/2, variables = variables, distance = (-distance), jitter = jitter, seed = seed2,coefficients=coefficients,center=center,range=range)
  group2_y <- rep(2, times = obs/2)
  group2 <- cbind(group2, y = as.factor(group2_y))
  dataset <- rbind(group1, group2)
  dataset
}
#erstelle Szenarien (wir mussten die Distance etwas erhöhen als ausgleich, da vergessen wurde \beta_p mit einzuberechnen)
set.seed(100)
S2_coefficients <- runif(2*10-1,-100,100)
S2_data_train <- generate_dataset(obs = 1000, variables = 10, distance = 1000, jitter = 500, seed1 = 1000, seed2 = 2000, coefficients = S2_coefficients)
S2_data_test <- generate_dataset(obs = 1000, variables = 10, distance = 1000, jitter = 500, seed1 = 3000, seed2 = 4000, coefficients = S2_coefficients)

set.seed(200)
S5_coefficients <- runif(2*50-1,-100,100)
S5_data_train <- generate_dataset(obs = 50, variables = 50, distance = 1000, jitter = 500, seed1 = 1000, seed2 = 2000, coefficients = S5_coefficients)
S5_data_test <- generate_dataset(obs = 50, variables = 50, distance = 1000, jitter = 500, seed1 = 3000, seed2 = 4000, coefficients = S5_coefficients)

set.seed(300)
S8_coefficients <- runif(2*200-1,-100,100)
S8_data_train <- generate_dataset(obs = 50, variables = 200, distance = 1000, jitter = 500, seed1 = 1000, seed2 = 2000, coefficients = S8_coefficients)
S8_data_test <- generate_dataset(obs = 50, variables = 200, distance = 1000, jitter = 500, seed1 = 3000, seed2 = 4000, coefficients = S8_coefficients)

save(S2_data_train, S2_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S2.RData")
save(S5_data_train, S5_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S5.RData")
save(S8_data_train, S8_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S8.RData")
