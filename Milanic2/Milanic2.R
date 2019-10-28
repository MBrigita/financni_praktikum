
library(actuar)

#1.naloga
#1.a
vzorec1 <- read.table("vzorec1.txt", quote="\"", comment.char="")
historgam_1 <- hist(vzorec1$V1,main ="Histogram odskodnin", xlab = "Visina odskodnine", ylab ="Frequency", col ="yellow")

#1.b
parametri <- mde(vzorec1$V1,fun = ppareto1, start = list(shape = 1, min = 3),measure ='CvM')
alfa <-as.numeric(parametri$estimate[1])
x_min <-as.numeric(parametri$estimate[2])

#1.c
histogram_2 <-hist(vzorec1$V1,main ="Histogram odskodnin", xlab = "Visina odskodnine", ylab ="Frequency", 
                   col ="yellow", prob = TRUE, ylim= c(0,2), breaks =30) 
curve(dpareto1(x,alfa,x_min),add = TRUE,col = "red")
legend("topright",legend = c("Pareto porazdelitev"),col = c("red"),lty=1:2, cex=0.8)

#1.d waldove enakosti
N <- 20
p <- 1/2
mat_upanje_N <- N*p
mat_upanje_Y <- alfa*x_min/(alfa-1)
varianca_Y <- (x_min/(alfa-1))^2 * (alfa/(alfa -2))
varianca_N <- N*p*(1-p)
mat_upanje_S<- mat_upanje_N * mat_upanje_Y
varianca_S <- mat_upanje_N * varianca_Y + (mat_upanje_Y^2) * varianca_N

#2.naloga
#2.a
h <- 0.25
n <- 40
diskretiziranje <- discretize(ppareto1(x, alfa,x_min), from= 0, to = n*h, step=0.25,method = "rounding")

#2.b
plot(stepfun(seq(0,9.75,0.25),diffinv(diskretiziranje)),do.points=FALSE,xlim=c(0,20),lwd=2,col="orange",
     main = "Paretova porazdelitev",ylab = "Porazdelitvena funkcija")
curve(ppareto1(x,alfa,x_min), add = TRUE)

#2.c
diskretiziranje_2 <- discretize(ppareto1(x,alfa, x_min),from = 0, to = 100000, step = h, method = "rounding")
S <- aggregateDist(method = "recursive", model.freq = "binom",model.sev = diskretiziranje_2,size = N,
                   prob = p, maxit=1000000,tol = 0.002, convolve = 0, x.scale = h)

#2.d
S_mat_upanje<- sum( knots(S) * diff(S))
S_mat_upanje_kvadrat <- sum( knots(S)^2 * diff(S))
S_varianca <- S_mat_upanje_kvadrat - S_mat_upanje^2

#3.naloga

     
