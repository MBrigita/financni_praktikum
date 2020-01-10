library(combinat)
library(Rlab)

#1. naloga
S0 = 50
u = 1.05
d = 0.95
T = 5
R = 0.03
W = c(1,2,3,4,5,6)
vrsta_1 <- c(50,52.5,49.88,47.38,45.01,47.26)
vrsta_2 <- c(50,52.5,55.12,57.88,60.78,63.81)
vrsta_3 <- c(50,47.5,49.88,47.38,45.01,42.76)
vrsta_4 <- c(50,47.5,45.12,47.38,45.01,47.26)
vrsta_5 <- c(50,52.5,49.83,52.37,54.99,52.24)

izplacilo <- function(vrsta, W, type){
  k = sum(vrsta*W)/sum(W)
  if(type == "call"){
    return(max(tail(vrsta,n=1)-k, 0))
  }
  if(type == "put"){
    return(max(k-tail(vrsta,n=1), 0))
  }
}

izplacila11 <- izplacilo(vrsta_1, W, "call")
izplacila12 <- izplacilo(vrsta_1, W, "put")
izplacila21 <- izplacilo(vrsta_2, W, "call")
izplacila22 <- izplacilo(vrsta_2, W, "put")
izplacila31 <- izplacilo(vrsta_3, W, "call")
izplacila32 <- izplacilo(vrsta_3, W, "put")
izplacila41 <- izplacilo(vrsta_4, W, "call")
izplacila42 <- izplacilo(vrsta_4, W, "put")
izplacila51 <- izplacilo(vrsta_5, W, "call")
izplacila52 <- izplacilo(vrsta_5, W, "put")

#2. naloga
binomski <- function(S0,u,d,R,T,W,type){
  mozni_izidi <- hcube(rep(2,T))-1
  trenutna_cena <- S0
  sestevki <- rowSums(mozni_izidi)
  for(i in c(1:2^T)){
    for(j in c(1:T)){
      if(mozni_izidi[i,j]==1){
        mozni_izidi[i,j] = u
      }
      else{
        mozni_izidi[i,j] = d
      }
    }
  }
  tabela_nova <- cbind(S0, mozni_izidi)
  tabela_zmnoz <- cbind(S0, mozni_izidi)
  for(i in c(1:2^T)){
    tabela_zmnoz[i,] <- cumprod(tabela_nova[i,])
  }
  izplacila <- c()
  for(i in c(1:2^T)){
    izplacila = cbind(izplacila, izplacilo(tabela_zmnoz[i,], W, type))
  }
  q <- (1+R-d)/(u-d)
  Q <- (q^sestevki)*((1-q)^(T-sestevki))
  premija <- sum(izplacila*Q)/(1+R)^T
  return(premija)
}

#3. naloga
S0 <- 60
R <- 0.01
T <- 15
W <- rep(1,16)
type <- "put"
N_1 <- 10
N_2 <- 100
N_3 <- 1000

monte <- function(S0,u,d,R,T,W,type,N){
  q <- (1+R-d)/(u-d)
  naklj_pot <- matrix(rbinom(N*T, 1, q), N, T)
  sestevki <- rowSums(naklj_pot)
  for(i in c(1:length(naklj_pot[,1]))){
    for(j in c(1:length(naklj_pot[1,]))){
      if(naklj_pot[i,j]==1){
        naklj_pot[i,j] = u
      }
      else{
        naklj_pot[i,j] = d
      }
    }
  }
  tabela_nova <- cbind(S0, naklj_pot)
  tabela_zmnoz <- cbind(S0, naklj_pot)
  for(i in c(1:length(naklj_pot[,1]))){
    tabela_zmnoz[i,] <- cumprod(tabela_nova[i,])
  }
  izplacila <- c()
  for(i in c(1:length(naklj_pot[,1]))){
    izplacila = cbind(izplacila, izplacilo(tabela_zmnoz[i,], W, type))
  }
  premija <- mean(izplacila)/(1+R)^T
  return(premija)
}

N1 <- c()
N2 <- c()
N3 <- c()
M <- 100
for (k in 1:M){
  N1 <- c(N1,monte(60,1.05,0.95,0.01,15,rep(1,16),"put",10))
  N2 <- c(N2,monte(60,1.05,0.95,0.01,15,rep(1,16),"put",100))
  N3 <- c(N3,monte(60,1.05,0.95,0.01,15,rep(1,16),"put",1000))
}

premija_binom <- binomski(60,1.05,0.95,0.01,15,rep(1,16),"put")

#N = 10
povprecje1 <- mean(N1)
odklon1 <- sqrt(var(N1))

h1 <- hist(N1, xlim = c(0,5), main = "Monte Carlo: N = 10", xlab = "Premija", ylab = "Frekvenca", col = "gold")
abline(v=povprecje1, col="green", lwd=2)
abline(v=premija_binom, col="red", lty=2)
arrows(x0=povprecje1, y0=0, x1=povprecje1 + odklon1, col="green", length=0.1, lwd = 2)
arrows(x0=povprecje1, y0=0, x1=povprecje1 - odklon1, col="green", length=0.1, lwd = 2)
legend("topright", legend=c("Monte Carlo", "analiza modela"), col=c("green","red"), lty=c("solid","dashed"), cex=0.9)

#N = 100
povprecje2 <- mean(N2)
odklon2 <- sqrt(var(N2))

h2 <- hist(N2, xlim=c(0,5), main="Monte Carlo: N = 100", xlab="Premija", ylab="Frekvenca", col="gold")
abline(v=povprecje2, col="green", lwd=2)
abline(v=premija_binom, col="red", lty=2)
arrows(x0=povprecje2, y0=0, x1 = povprecje2 + odklon2, col="green", length=0.1, lwd = 2)
arrows(x0=povprecje2, y0=0, x1=povprecje2 - odklon2, col="green", length=0.1, lwd = 2)

legend("topright", legend=c("Monte Carlo", "analiza modela"), col=c("green","red"), lty=c("solid","dashed"), cex=0.9)

#N = 1000
povprecje3 <- mean(N3)
odklon3 <- sqrt(var(N3))

h3 <- hist(N3, xlim=c(0,5), main="Monte Carlo: N = 1000", xlab="Premija", ylab="Frekvenca", col="gold")
abline(v=povprecje3, col="green", lwd=2)
abline(v=premija_binom, col="red", lty=2)
arrows(x0=povprecje3, y0=0, x1=povprecje3 + odklon3, col="green", length=0.1, lwd=2)
arrows(x0=povprecje3, y0=0, x1=povprecje3 - odklon3, col="green", length=0.1, lwd=2)

legend("topright", legend=c("Monte Carlo", "Analiza modela"), col=c("green","red"), lty=c("solid","dashed"), cex=0.9)

