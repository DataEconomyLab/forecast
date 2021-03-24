setwd("c:/work/data")

## 데이터읽기 ##

library(quantmod)
# Yahoo! Finance로부터 종합주가지수 종가 데이터 가져오기
kospi <- getSymbols("^KS11", auto.assign = FALSE)[,4]
er <- getSymbols("KRW=X", auto.assign = FALSE)[,4]

kospi_r <- dailyReturn(na.omit(kospi))
er_r <- dailyReturn(na.omit(er))

fin1 = merge(kospi, er)

# Fig 7-1
plot(fin1[,1], type="l", main="")
lines(fin1[,2], col=2)

# Fig 7-2
plot(as.matrix(fin1), xlab="종합주가지수", ylab="원/달러 환율")

cor(na.omit(as.matrix(fin1)))

# Figure 3-1
gdp <- read.csv("c:/work/data/gdpq.csv")
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1))/lag(gdp_sa, -1)*100, start=c(1970, 2), frequency=4)
ccf(gdp_gr, lag(gdp_gr, -1), lag=12, main="")

bc <- ts(read.csv("bc.csv", header=TRUE), start=1970, frequency=12)
ccf(bc[,1], bc[,2], lag=12, main="")

gdp_m <- ts(read.csv("C:/Users/ljyjj/Documents/GitHub/forecast/data/gdp_m.csv", header=TRUE), start=1980, frequency=12)
gdpm <- gdp_m[,2]
ipim <- gdp_m[,3]
gdp_m_gr <- ts((gdpm-lag(gdpm,-1))/lag(gdpm,-1)*100, start=c(1980, 2), frequency=4)
ipi_m_gr <- ts((ipim-lag(ipim,-1))/lag(ipim,-1)*100, start=c(1980, 2), frequency=4)

plot(as.numeric(ipi_m_gr), as.numeric(gdp_m_gr), xlab="제조업 산업생산지수", ylab="제조업 GDP")
abline(lm(gdp_m_gr~ipi_m_gr), col=2)

econ = ts(read.csv("gdpr.csv", header=TRUE), start=2000, frequency=4)
gdp_r = econ[,4]
ipi_r = econ[,5]
sbi_r = econ[,6]
dum1 = econli[,7]
dum2 = econ[,8]

gr1 = ts.union(gdp_r, ipi_r, sbi_r, dum1, dum2)
plot(gr1, main="", xlab="", col="steelblue")

gdp_r_lm = lm(gdp_r ~ ipi_r + sbi_r, data=gr1)
summary(gdp_r_lm)

plot(gdp_r, col="steelblue", ylim=c(-2,10), ylab="", xlab="")
lines(ts(predict(gdp_r_lm), start=c(2001,1), freq=4), col=2)

library(car)
durbinWatsonTest(gdp_r_lm)

gr2 = ts.union(gdp_r, ipi_r, sbi_r, gdp_r1 = lag(gdp_r,-1))

gdp_r_lmc = lm(gdp_r ~ ipi_r + sbi_r + gdp_r1, data=gr2)
durbinWatsonTest(gdp_r_lmc)
summary(gdp_r_lmc)

plot(gdp_r, col="steelblue", ylim=c(-2,10), ylab="", xlab="")
lines(ts(predict(gdp_r_lmc), start=c(2001,1), freq=4), col=2)

library(orcutt)

gdp_r_lm_co = cochrane.orcutt(gdp_r_lm)
plot(gdp_r, col="steelblue", ylim=c(-2,10), ylab="", xlab="")
lines(ts(predict(gdp_r_lm_co), start=c(2001,1), freq=4), col=2)

library(forecast)

fit <- auto.arima(gdp_r, xreg=cbind(ipi_r, sbi_r))
tsdisplay(arima.errors(fit))
Box.test(residuals(fit), type="Ljung", lag=8)

fcast <- forecast(fit, xreg=cbind(rep(mean(ipi_r[53:57], 4)), rep(mean(sbi_r[53:57]), 4)), h=4)
plot(fcast, main="")

fit1 <- Arima(gdp_r, xreg=cbind(ipi_r, sbi_r), order=c(1,0,0))
tsdisplay(arima.errors(fit1))
Box.test(residuals(fit1), type="Ljung")

##################################################################

library(tseries)
econ = read.ts("gdp_sa_r.csv", header=TRUE, start=2000, frequency=4, , sep=",")
ipi_sa_r = econ[,5]
sbi_sa_r = econ[,4]
gdp_sa_r = econ[,6]



