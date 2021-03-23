# Figure 3-1
gdp <- read.csv("c:/work/data/gdpq.csv")
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1))/lag(gdp_sa, -1)*100, start=c(1970, 2), frequency=4)

library(tseries)

adf.test(log(gdp_sa))
adf.test(diff(log(gdp_sa)))

library(TSA)
set.seed(5)
y <- arima.sim(n=176, model=list(ar=0, ma=0.6))
lag.plot(y)

ty <- tar.sim(n=176, Phi1=c(0,0.5), Phi2=c(0,-0.6), p=1, d=1, sigma1=1, thd=-1, sigma2=2)$y
lag.plot(ty)
 
Keenan.test(ty)
Tsay.test(ty)

library(FinTS)
library(TSA)
library(quantmod)
# Yahoo! finance로부터 종합주가지수 종가 데이터 가져오기
kospi <- getSymbols("^KS11", auto.assign = FALSE)[,4]
kospi_r <- dailyReturn(kospi)

McLeod.Li.test(y=kospi_r)
ArchTest(kospi_r, lag=12)

##############################
par(mfrow=c(2,1))
plot(log(gdp_sa), ylab="log(GDP_SA)", xlab="", col="steelblue", main="")
plot(diff(log(gdp_sa)), ylab="diff(log(GDP_SA))", xlab="", col="steelblue", main="")

plot(log(gdp_o), ylab="log(GDP)", xlab="", col="steelblue", main="")
plot(diff(diff(log(gdp_o), 1), 4), ylab="diff(diff(log(GDP)), 4)", xlab="", col="steelblue", main="")

par(mfrow=c(2,1))
 acf(diff(diff(log(gdp_o)), 4), main="")
pacf(diff(diff(log(gdp_o)), 4), main="")

##############################
fin <- ts(read.csv("c:/work/data/kospi_n.csv", header=TRUE), start=1993, frequency=12)
kospi <- fin[,1]
er <- fin[,2]
plot(diff(log(kospi)), ylab="종합주가지수", xlab="", col="steelblue", main="")

par(mfrow=c(2,1))
 acf(diff(log(kospi)), main="")
pacf(diff(log(kospi)), main="")

kospi_fit = arima(log(kospi), order=c(1,1,0))
summary(kospi_fit)

kospi_fit = arima(log(kospi), order=c(0,1,1))
summary(kospi_fit)

library(forecast)
auto.arima(log(kospi))

##############################
#과대적합
kospi_fit = arima(log(kospi), order=c(1,1,1))
summary(kospi_fit)
kospi_fit = arima(log(kospi), order=c(0,1,2))
summary(kospi_fit)

#잔차
kospi_fit = arima(log(kospi), order=c(0,1,1))
tsdiag(kospi_fit)

##############################

plot(forecast(kospi_fit, h=12))

##############################
library(TSA)
library(tseries)
library(quantmod)

getSymbols("^KS11", from="1997-01-03", to="2014-8-31")
 plot(KS11)
 kospi = KS11$ KS11.Close
 r.kospi = diff(log(kospi))
 r.kospi1 = r.kospi[2:length(r.kospi)]
 
plot(kospi, main="KOSPI")
plot(r.kospi, main="dlog KOSPI")

hist(r.kospi1, breaks=100, freq=FALSE, main="", xlab="")
qqnorm(r.kospi1)
qqline(r.kospi1)
jarque.bera.test(na.omit(r.kospi1))
kurtosis(na.omit(r.kospi))
skewness(na.omit(r.kospi1))

par(mfrow=c(2,1))
  acf(r.kospi, na.action = na.pass, main = "")
 pacf(r.kospi, na.action = na.pass, main = "")

  acf(r.kospi^2, na.action = na.pass, main = "")
 pacf(r.kospi^2, na.action = na.pass, main = "")
 McLeod.Li.test(y=r.kospi)
 
library(fGarch) 
gg = garchFit(~arma(0,1) + garch(1,1), r.kospi1)
summary(gg)
plot(gg)

##############################
er <- getSymbols("KRW=X", auto.assign = FALSE)[,4]
r.er = diff(log(er))
r.er1 = r.er[2:length(r.er)]

hist(e.er1, breaks=100, freq=FALSE, main="", xlab="")
jarque.bera.test(na.omit(r.er1))

garch_er <- garchFit(~arma(0,1) + garch(1,1), r.er1)
summary(garch_er)

plot((fitted(garchFitoutput)[,1])^2, type='1', ylab='조건부 분산', xlab='')