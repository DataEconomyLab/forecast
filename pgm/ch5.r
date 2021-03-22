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
