install.packages('mev')
install.packages('quantmod')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)
library(quantmod)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(mev)
library(stats)

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

u<-0.0771 #küszöb
n<-length(tesla_df$loss) #vesztesegek szama

gpd_df<-data.frame(k=index(tesla_df$loss),Loss=sort(tesla_df$loss, decreasing=TRUE))
gpd_df<-cbind(gpd_df, y=gpd_df$Loss-u)
gpd_df_v2<-data.frame(k=index(gpd_df$y[gpd_df$y>0]),y=gpd_df$y[gpd_df$y>0], loss=gpd_df$Loss[gpd_df$y>0])
gpd_df_v3<-gpd_df_v2[,2]
Nu<-length(gpd_df_v3) #küszöböt meghaladó veszteségek száma

par_s<-gpd.mle(gpd_df_v3,args = c("scale", "shape"))
beta<-par_s[1]
xi<-par_s[2]

beta<-setNames(beta,NULL)
xi<-setNames(xi,NULL)

#szignifikanciaszint(ek)
q<-0.99
q1<-0.95

#EVT alapu becsles
VaR99<-u+(beta/xi)*(((n/Nu)*(1-q))^(-xi)-1)
CVaR99<-VaR99/(1-xi)+(beta-xi*u)/(1-xi)

VaR95<-u+(beta/xi)*(((n/Nu)*(1-q1))^(-xi)-1)
CVaR95<-VaR95/(1-xi)+(beta-xi*u)/(1-xi)


#historikus becsles
n_hist<-n*(1-q)
VaR_hist99<-nth(gpd_df$Loss,n_hist)
CVaR_hist99<-mean(gpd_df$Loss[1:n_hist])


n_hist<-n*(1-q1)
VaR_hist95<-nth(gpd_df$Loss,n_hist)
CVaR_hist95<-mean(gpd_df$Loss[1:n_hist])


#lnL
gpd_df_v2<-cbind(gpd_df_v2, lnL=-log(beta)-(1+1/xi)*log(1+xi/beta*gpd_df_v2$y))
sumlnL<-sum(gpd_df_v2$lnL)


ggplot(gpd_df_v2, aes(x=k, y=lnL))+
  geom_line()+
  labs(title="Tesla GPD distribution")

ggplot(mapping = aes(sample=gpd_df_v2$lnL))+
  geom_qq() + geom_qq_line(color=2)+labs(title="Normal Q-Q Plot")

#illeszkedés jósága
AIC(gpd_df_v2$lnL)


