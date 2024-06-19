#einlesen
library(haven)
allb <- read_sav("Code/Daten/ZA5281_v1-0-1.sav") 
class(allb)
str(allb)
length(unique(allb$respid))
<<<<<<< HEAD
all.equal(allb$splt21)

#afjksadfkj
=======

allb$dg03
str(allb$dg03)

str(allb$splt21)

Split_A <- allb[allb$splt21 == 1, ]

equal <- sapply(Split_A, function(x) length(unique(x)) == 1)
nicht_beantwortet_A <- names(which(equal))



>>>>>>> 519ca5f74319abd782e67105988971ac8ce5332c
