#linear getrennte daten in 3 dimensionen für f(x,y)=3

#sample zufällige punkte auf ebene
x <- rnorm(100,0,2)
y <- rnorm(100,0,2)
z <- rep(2,100)
#verschiebung entlang der z-Achse nach oben

for(i in 1:length(z)){
  z[i] <- z[i]+rnorm(1,3,1)
}
#sample nochmal punkte für unter der ebene 
x2 <- rnorm(100,0,2)
y2 <- rnorm(100,0,2)
z2 <- rep(2,100)
#verschiebung entlang der z-Achse nach unten
for(i in 1:length(z2)){
  z2[i] <- z2[i]-rnorm(1,3,1)
}
#darstellung
group <- c(rep(1,100),rep(2,100))
df <- data.frame(x=c(x,x2),y=c(y,y2),z=c(z,z2),group = group)
cor(df)
library(rgl)
plot3d(df$x,df$y,df$z,col = group)

#definiere Ebene der Form ax+by+cz+d=0,a=2,b=2,c=3,d=2
x3 <- rnorm(100,0,3)
y3 <- rnorm(100,0,3)
z <- (0-2-2*y3-2*x3)/3
df2 <- data.frame(x3,y3,z)
plot3d(df2$x3,df2$y,df2$z)
#verschiebung um den Normalenvektor in positiver Richtung
df3 <- data.frame(x=rep(0,100),
                  y=rep(0,100),
                  z=rep(0,100))
for(i in 1:nrow(df2)){
  df3[i,]<- unlist(df2[i,])+(rnorm(1,3,1)*c(2,2,3))
}
#neue Punkte für sample auf der anderen seite der Ebene
x4 <- rnorm(100,0,3)
y4 <- rnorm(100,0,3)
z <- (0-2-2*y4-2*x4)/3
df2 <- data.frame(x4,y4,z)
df4 <- data.frame(x=rep(0,100),
                  y=rep(0,100),
                  z=rep(0,100))
#verschiebung in negative richtung
for(i in 1:nrow(df2)){
  df4[i,]<- unlist(df2[i,])-(rnorm(1,3,1)*c(2,2,3))
}
df4
df5 <- data.frame(x=c(df3$x,df4$x),y=c(df3$y,df4$y),z=c(df3$z,df4$z),group=c(rep(1,100),rep(2,100)))
plot3d(x=df5$x,y=df5$y,z=df5$z,col = df5$group)

#sampling in p dimensionen

generate_subdatasets <- function(obs,variables,distance,jitter,seed,coefficients,center=0,range=5){
  set.seed(seed)
  #sample punkte auf der Ebene für X_1 bis X_{p-1}
  points_on_plane <- matrix(rnorm(obs,center,range),nrow = obs,ncol=1)
  for(i in 2:(length(coefficients)-1)){
    set.seed(seed+i)
    points_on_plane <- cbind(points_on_plane,rnorm(obs,center,range))
  }
  #stelle nach X_p um
  helper1 <- apply(points_on_plane,1,function(x){x*coefficients[1:(variables-1)]})
  helper2 <- apply(helper1,2,sum)*(-1)/coefficients[variables]
  points_on_plane <- cbind(points_on_plane,helper2)
  #verschiebe punkte um skalierten normierten normalenvektor
  points_next_to_plane <- matrix(0,nrow=obs,ncol=variables)
  for(i in 1:nrow(points_next_to_plane)){
    points_next_to_plane[i,]<- unlist(points_on_plane[i,])-(rnorm(1,distance,jitter)*(coefficients/norm(coefficients,type="2")))
  }
  return(as.data.frame(points_next_to_plane)) 
}

#erstelle gesammtdatensatz für beide ausprägungen
generate_dataset <- function(obs,variables,distance,jitter,seed1,seed2,coefficients,center=0,range=3){
  #erste Gruppe mit positiven skalierungsfaktor
  group1 <- generate_subdatasets(obs = obs/2, variables = variables, distance = distance, jitter = jitter, seed = seed1, coefficients = coefficients, center=center,range=range)
  group1_y <- rep(1, times = obs/2)
  group1 <- cbind(group1, y = as.factor(group1_y))
  #zweite Gruppe mit negativem skalierungsfaktor
  group2 <- generate_subdatasets(obs = obs/2, variables = variables, distance = (-distance), jitter = jitter, seed = seed2, coefficients = coefficients, center=center,range=range)
  group2_y <- rep(2, times = obs/2)
  group2 <- cbind(group2, y = as.factor(group2_y))
  dataset <- rbind(group1, group2)
  dataset
}



# geringe Distance, da sonst alle Algorithmen perfekt klassifizieren
set.seed(100)
S1_coefficients <- runif(10,-100,100)
S1_data_train <- generate_dataset(obs = 1000, variables =  10, distance = 0.5, jitter = 0.3, seed1 = 1000, seed2 = 2000, coefficients = S1_coefficients)
S1_data_test <- generate_dataset(obs = 1000, variables =  10, distance = 0.5, jitter = 0.3, seed1 = 3000, seed2 = 4000, coefficients = S1_coefficients)

# weitere Distance, da sonst bei allen sehr schlechte Klassifizierung
set.seed(200)
S4_coefficients <- runif(50,-100,100)
S4_data_train <- generate_dataset(obs = 50, variables =  50, distance = 3, jitter = 1.5, seed1 = 1000, seed2 = 2000, coefficients = S4_coefficients)
S4_data_test <- generate_dataset(obs = 50, variables =  50, distance = 3, jitter = 1.5, seed1 = 3000, seed2 = 4000, coefficients = S4_coefficients)

set.seed(300)
S7_coefficients <- runif(200,-100,100)
S7_data_train <- generate_dataset(obs = 50, variables =  200, distance = 3, jitter = 1.5, seed1 = 1000, seed2 = 2000, coefficients = S7_coefficients)
S7_data_test <- generate_dataset(obs = 50, variables =  200, distance = 3, jitter = 1.5, seed1 = 3000, seed2 = 4000, coefficients = S7_coefficients)

save(S1_data_train, S1_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S1.RData")
save(S4_data_train, S4_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S4.RData")
save(S7_data_train, S7_data_test, file = "Abgabe/Ergebnisse/Daten/Data_S7.RData")
