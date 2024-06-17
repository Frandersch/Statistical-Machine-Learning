#einlesen
library(haven)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
class(allb)
str(allb)
length(unique(allb$respid))
