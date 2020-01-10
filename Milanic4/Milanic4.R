library(readr)

#Glajenje casovnih vrst

#1. naloga
srebro <- read_csv("srebro19.csv")
#Podatki od maja do oktobra
srebro <- srebro[c(127:1),c(5)]

casovna_vrsta <- ts(data = srebro$Close)
casovna_vrsta <- as.numeric(gsub("\\$", "", casovna_vrsta))
graf_srebro <- ts.plot(casovna_vrsta, main = "Srebro", ylab = "EUR")
points(casovna_vrsta, y = NULL, col = "black", pch = 20)

#2. naloga
#funkcija G priredi Zglajene vrednosti
G <- function(vrsta, k){
  dolzina <- length(vrsta)
  zglajene <- c()
  for (i in (1+k):(dolzina+1)){
    zglajene[i] <- sum(vrsta[(i-1):(i-k)])/k
  }
  return(zglajene)
}

#Glajenje z drsecim povprecjem reda 5, napoved
glajena <- G(casovna_vrsta, 5)
napoved_prih <- function(vrsta,k){
  dolzina <- length(vrsta)
  return(sum(vrsta[(dolzina-k+1):dolzina])/k)
}
napovedane <- c(glajena, napoved_prih(casovna_vrsta, 5))

#Dodamo zglajeno vrsto na graf casovne vrste
ts.plot(ts(glajena), ts(casovna_vrsta), xlab='Time', ylab ='EUR', main = 'Drsece povprecje', col=c('green', 'black'), lwd = 2)

#Srednja kvadratna napaka
skn <- function(vrsta,k){
  dolzina <- length(vrsta)
  napaka <- 0
  for (i in (k):(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - G(vrsta,k)[i+1])^2
  }
  return(napaka/(dolzina-k))
}

skn5 <- skn(casovna_vrsta, 5)

#Red glajenja 15 in 30
glajenje15 <- G(casovna_vrsta, 15)
napoved15<- c(glajenje15, napoved_prih(casovna_vrsta, 15))
glajenje30 <- G(casovna_vrsta, 30)
napoved30<- c(glajenje30, napoved_prih(casovna_vrsta, 30))

#Graf 15
ts.plot(ts(glajenje15), ts(casovna_vrsta), xlab='Time', ylab ='EUR', main = 'Drsece povprecje reda 15', 
        col=c('green', 'black'), lwd = 2)

#Graf 30
ts.plot(ts(glajenje30), ts(casovna_vrsta), xlab='Time', ylab ='EUR', main = 'Drsece povprecje reda 30', 
        col=c('pink', 'black'), lwd = 2)

skn15 <- skn(casovna_vrsta, 15)
skn30 <- skn(casovna_vrsta, 30)

#3. naloga
#Eksponentno glajenje

eg <- function(vrsta,alpha){
  dolzina <- length(vrsta)
  zglajene_vred <- vrsta[1]
  for (i in 2:dolzina){
    zglajene_vred[i] <- alpha * vrsta[i] +(1-alpha)*zglajene_vred[i-1]
  }
  zglajena <- ts(zglajene_vred)
  return(zglajena)
}

#0.1 <= alpha <= 0.3
#izbrani alpha = 0.2

glajena_alpha <- eg(casovna_vrsta, 0.2)

ts.plot(casovna_vrsta, glajena_alpha, xlab = "Time", ylab = "EUR", main = "Eksponentno glajenje", lwd = 2:1, col=c("black", "purple"))
points(casovna_vrsta, y = NULL, col = "black", pch = 20)

