install.packages('mev')
install.packages('quantmod')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages("PerformanceAnalytics")
install.packages("evir")

library(PerformanceAnalytics)
library(quantmod)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(evir)

#adat letoltes yahoo-rol, es adj close kimentese
tesla_df <-
  getSymbols(
    "TSLA",
    env = globalenv(),
    source = "yahoo",
    auto.assign = FALSE,
    from = '2016-09-06',
    to = '2020-09-04'
  )[, 6]

#loghozamok és veszteseg kiszamitasa, oszlopok elnevezése
tesla_df$log_returns <- diff(log(tesla_df[,1]))
tesla_df <- na.omit(cbind(index(tesla_df), tesla_df %>% as_tibble))
tesla_df$loss<-tesla_df$log_returns*(-1)
names(tesla_df)[1:2] <- c("Date","Adjusted")

#új az Intro to R könyv szerint

summary(tesla_df$loss)
hist(tesla_df$loss[tesla_df$loss>0], xlim = c(0,0.2), main = "Histogram of Tesla losses", xlab = "Loss")
sum(tesla_df$loss>0.2)/length(tesla_df$loss[tesla_df$loss>0])
sum(tesla_df$loss[tesla_df$loss>0.2])/sum(tesla_df$loss[tesla_df$loss>0])
emplot(tesla_df$loss, main = "Empirical CCDF of Tesla losses")
emplot(tesla_df$loss[tesla_df$loss>0], alog="xy", main="Empirical CCDF of Tesla losses")
qplot(tesla_df$loss[tesla_df$loss>0], main = "Q-Q plot ",trim=100) #nem exponenciális eloszlást követ, fat tailed

meplot(tesla_df$loss[tesla_df$loss>0], omit=10, main="Mean excess function of Tesla losses")
meplot(tesla_df$loss[tesla_df$loss>0], omit=10, xlim=c(0,0.05),main="Mean excess function of Tesla losses") # 3-5% között lineáris, 3% lehetne a threshold

gpdfit<-gpd(tesla_df$loss[tesla_df$loss>0], threshold = 0.03) #ML-t használ, hogy megbecsülje a paramétereket a GPD eloszláshoz
gpdfit$converged # ha 0 a value akkor ML használva lett
gpdfit$par.ests #alakparaméter és skálaparaméter
gpdfit$par.ses #sztenderd hiba

plot(gpdfit)

tp<-tailplot(gpdfit)
gpd.q(tp, pp=0.999, ci.p =0.95)
quantile(tesla_df$loss[tesla_df$loss>0], probs = 0.999, type =1)

gpd.sfall(tp, 0.99)
