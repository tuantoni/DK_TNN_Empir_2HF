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
par_s<-gpdfit$par.ests #alakparaméter és skálaparaméter
gpdfit$par.ses #sztenderd hiba

plot(gpdfit) #gpd cdf

tp<-tailplot(gpdfit)
gpd.q(tp, pp=0.999, ci.p =0.95)
quantile(tesla_df$loss[tesla_df$loss>0], probs = 0.999, type =1)

gpd.sfall(tp, 0.99)


u<-0.03 #küszöb
n<-length(tesla_df$loss) #vesztesegek szama
# 
gpd_df<-data.frame(k=index(tesla_df$loss),Loss=sort(tesla_df$loss, decreasing=TRUE))
gpd_df<-cbind(gpd_df, y=gpd_df$Loss-u)
gpd_df_v2<-data.frame(k=index(gpd_df$y[gpd_df$y>0]),y=gpd_df$y[gpd_df$y>0], loss=gpd_df$Loss[gpd_df$y>0])
gpd_df_v3<-gpd_df_v2[,2]
Nu<-length(gpd_df_v3) #küszöböt meghaladó veszteségek száma

beta<-par_s[1]
xi<-par_s[2]

beta<-setNames(beta,NULL)
xi<-setNames(xi,NULL)
# 
#szignifikanciaszint(ek)
q<-0.99
q1<-0.95
# 
#EVT alapu becsles
VaR99<-u+(beta/xi)*(((n/Nu)*(1-q))^(-xi)-1)
CVaR99<-VaR99/(1-xi)+(beta-xi*u)/(1-xi)

VaR95<-u+(beta/xi)*(((n/Nu)*(1-q1))^(-xi)-1)
CVaR95<-VaR95/(1-xi)+(beta-xi*u)/(1-xi)


#historikus becsles
n_hist<-n*(1-q)
VaR_hist99<-nth(gpd_df$Loss,n_hist)
CVaR_hist99<-mean(gpd_df$Loss[1:n_hist])

Fx<-1-(Nu/n)*(1+xi*(gpd_df_v2$y/beta))^(-1/xi)

n_hist<-n*(1-q1)
VaR_hist95<-nth(gpd_df$Loss,n_hist)
CVaR_hist95<-mean(gpd_df$Loss[1:n_hist])

