library(readr)
library(tidyr)
library(dplyr)

#uvoz
EURIBOR_2010 <- read_csv("podatki/hist_EURIBOR_2010.csv")  %>% 
  select("X1","04/01/2010","01/02/2010","01/03/2010","06/04/2010",
         "03/05/2010","01/06/2010","01/07/2010","02/08/2010",
         "01/09/2010","01/10/2010","01/11/2010","01/12/2010")
EURIBOR_2011 <- read_csv("podatki/hist_EURIBOR_2011.csv") %>%
  select("X1","03/01/2011","01/02/2011",
         "01/03/2011","01/04/2011","02/05/2011",
         "01/06/2011","01/07/2011","01/08/2011",
         "01/09/2011","03/10/2011","01/11/2011","01/12/2011")
EURIBOR_2012 <- read_csv("podatki/hist_EURIBOR_2012.csv") %>% 
  select("X1","02/01/2012","01/02/2012","01/03/2012","02/04/2012",
         "02/05/2012","01/06/2012","02/07/2012","01/08/2012","03/09/2012",
         "01/10/2012","01/11/2012","03/12/2012")
#urejanje podatkov
EURIBOR_2010 <-EURIBOR_2010[1:15,]
EURIBOR_2012 <- t(EURIBOR_2012)
EURIBOR_2010<- t(EURIBOR_2010)
EURIBOR_2011 <- t(EURIBOR_2011)
colnames(EURIBOR_2010)<- EURIBOR_2010[1,]
colnames(EURIBOR_2011)<- EURIBOR_2011[1,]
colnames(EURIBOR_2012)<- EURIBOR_2012[1,]
EURIBOR_2010<- EURIBOR_2010[c(-1),c(-1,-2,-3)]
EURIBOR_2011<- EURIBOR_2011[c(-1),c(-1,-2,-3)]
EURIBOR_2012<- EURIBOR_2012[c(-1),c(-1,-2,-3)]

#1.NALOGA

#izbria  tipa terminske obrestne mere 3X9
izbira_tipa_2010 <- EURIBOR_2010[,c("3m","9m")]
izbira_tipa_2011 <- EURIBOR_2011[,c("3m","9m")]
izbira_tipa_2012 <- EURIBOR_2012[,c("3m","9m")]

#graf pri nalogi 1.c
euribor_39 <- rbind(izbira_tipa_2010,izbira_tipa_2011,izbira_tipa_2012, id = NULL)
cas_3 <-ts(data = euribor_39[,c("3m")], frequency = 6, start = c(04/01/2010))
cas_9 <-ts(data = euribor_39[,c("9m")], frequency = 6, start = c(04/01/2010))
ts.plot(cas_3,cas_9,col = c("green","pink"),main = "Euribor", ylab = "%")
legend("topright", c("3m", "9m"), col=c("green","pink"),  lty=1,lwd=1.5, bty = "n")

#2.NALOGA

#2.a izbira treh datumov
datum_1 <- EURIBOR_2010[8,]
datum_2 <- EURIBOR_2011[11,]
datum_3 <- EURIBOR_2012[11,]

#2.b izris grafa
plot(datum_1,type = "b", pch = 19,main = "Casovna struktura Euribor", ylab = "%", xlab = "Dospetje[mesec]", col = "purple",ylim = c(0, 2.5))
points(datum_2, y= NULL, col="green", pch=19)
lines(datum_2, y= NULL, col="green")
points(datum_3, y= NULL, col="orange", pch=19)
lines(datum_3, y= NULL, col="orange")
text(x= 11.5, y= 1.5, labels = '2.8.2010',col="purple")
text(x= 11.5, y= 0.7, labels = '1.11.2012',col="orange")
text(x= 11.5, y= 2.3, labels = '1.11.2011',col="green")

#3.NALOGA

#3.a
L_03 <- as.numeric(euribor_39[,1])
L_09 <- as.numeric(euribor_39[,2])
napoved3m <- ((1+0.75*L_09)/(1+0.25*L_03)-1)/0.5
napoved3m[c(1:6)] <- NA

#3.b
tabela_3b <- cbind(euribor_39,napoved3m, id = NULL)# %>% as.data.frame()
#names(tabela_3b) <- c("Euribor3m","Euribor9m","napoved3m")

#3.c
leto_1 <- tabela_3b[1:12,]
leto_2 <- tabela_3b[13:24,]
leto_3 <- tabela_3b[25:36,]
dejanski_3m <- as.numeric(tabela_3b[, c("3m")])
napovedan_3m <- as.numeric(tabela_3b[,c("napoved3m")])
model <- lm(napovedan_3m ~ dejanski_3m)

plot(leto_1, main = "3m Euribor 2010-2012", ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "purple", xlim=c(0,2), ylim=c(0,2))
points(leto_2, y = NULL, col = "green", pch = 16)
points(leto_3, y = NULL, col = "orange", pch = 16)
abline(model)
abline(0,1, lty = 'dashed')
legend("bottomright", c("2010", "2011", "2012"), col=c('purple','green', 'orange'), pch=c(16, 16, 16), bty = "n")

#3.d

#graf za leto 2010
dejanski_3m_2010 <- dejanski_3m[1:12]
napovedan_3m_2010 <- napovedan_3m[1:12]
model_2010 <- glm(napovedan_3m_2010 ~ dejanski_3m_2010)

plot(leto_1, main = "3m Euribor 2010", ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "purple", xlim=c(0,2), ylim=c(0,2))
abline(model_2010, col = "purple")
abline(0,1, lty = 'dashed')



#graf za leto 2011 
dejanski_3m_2011 <- dejanski_3m[13:24]
napovedan_3m_2011 <- napovedan_3m[13:24]
model_2011 <- lm(napovedan_3m_2011 ~ dejanski_3m_2011)

plot(leto_2, main = "3m Euribor 2011", ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "green", xlim=c(0,2), ylim=c(0,2))
abline(model_2011, col = "green")
abline(0,1, lty = 'dashed')


#graf za leto 2012 
dejanski_3m_2012 <- dejanski_3m[25:36]
napovedan_3m_2012 <- napovedan_3m[25:36]
model_2012 <- lm(napovedan_3m_2012 ~ dejanski_3m_2012)

plot(leto_3, main = "3m Euribor 2012", ylab = "Opazovano", xlab = "Napoved", pch = 16, col = "orange", xlim=c(0,2), ylim=c(0,2))
abline(model_2012, col = "orange")
abline(0,1, lty = 'dashed')

#3.e hipoteza pričakovanj trga
#Če bi hipoteza velja bi točke grafa ležale na simetrali lihih kvadrantov, vendar ležijo nad simetralo. To pomeni, da so bile
#napovedi preveč pesimistične. Torej hipoteze na podlagi empiričnih podatkov ne moremo potrditi.

