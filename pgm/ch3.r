# Figure 3-1
setwd("c:/work/data")
gdp <- read.csv("gdpq.csv", header=TRUE)
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1)) / lag(gdp_sa, -1)*100, start=c(1970, 2), frequency=4)
plot(gdp_gr, ylab="경제성장률(전기대비)", xlab="연도", col="steelblue", main="")
abline(h=0, lty=2, col="grey")

# Figure 3-2
hist(gdp_gr, breaks=12, col="lightblue", border="black", freq=FALSE, main="", xlab="", xlim=c(-10,10))
lines(density(gdp_gr))
shapiro.test(gdp_gr)

# Figure 3-3
library(quantmod)
# Yahoo! Finance로부터 종합주가지수 종가 데이터 가져오기
kospi <- getSymbols("^KS11", auto.assign = FALSE)
kospi_r <- dailyReturn(na.omit(kospi))
# 그래프
hist(kospi_r, breaks = 30, col="lightblue", border="black", freq=FALSE, main="", xlab="", xlim=c(-0.12, 0.12), ylim=c(0,50))
lines(density(kospi_r))
shapiro.test(kospi_r)

# Figure 3-4
set.seed(1)
nn = length(gdp_o)
wn = ts(rnorm(nn), start=1970, frequency=4)
par(mfrow=c(2,1))
plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
acf(wn, main="", col="steelblue")

# Figure 3-5
nn = length(gdp_sa)
sin = ts(sin(1:nn/nn*12*pi), start=1970, frequency=4)
par(mfrow=c(2,1))
plot(sin, main="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
acf(sin, main="", col="steelbook")

# Figure 3-6 - 8
plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
acf(gdp_o, main="", col="steelblue")

plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
acf(diff(log(gdp_o)), main="", col="steelblue")

plot(diff(log(gdp_o),4), main="", xlab="", ylab="", col="steelblue")
acf(diff(log(gdp_o),4), main="", col="steelblue")

# 표
Box.test(wn, lag=8, type="Ljung")
Box.test(sin, lag=8, type="Ljung")
Box.test(gdp_o, lag=8, type="Ljung")
Box.test(diff(log(gdp_o)), lag=8, type="Ljung")
Box.test(diff(log(gdp_o),4), lag=8, type="Ljung")

par(mfrow=c(2,1))
plot(wn, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 pacf(wn, main="", col="steelblue")

plot(sin, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 pacf(sin, main="", col="steelblue")

plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
 pacf(gdp_o, main="", col="steelblue")
 
plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
 pacf(diff(log(gdp_o)), main="", col="steelblue")
 
plot(diff(log(gdp_o),4), main="", xlab="", ylab="", col="steelblue")
 pacf(diff(log(gdp_o),4), main="", col="steelblue")
 
par(mfrow=c(2,1)) 
plot(wn, main="", xlab="", ylab="", col="steelblue")
  abline(h=0, lty=2, col="gray")

aa = spectrum(wn, spans = c(3,3), main="", col="steelblue")
plot(1:80/40, aa$spec, type="1", ylim=c(0,10))

plot(sin, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
pacf(sin, main="", col="steelblue")

plot(gdp_o, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")

plot(diff(log(gdp_o)), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o)), main="", col="steelblue")

plot(diff(log(gdp_o), 4), main="", xlab="", ylab="", col="steelblue")
pacf(diff(log(gdp_o), 4), main="", col="steelblue")

sin1 = ts(sin(1:nn/nn*12*pi), start=1970, frequency=4)
sin2 = ts(sin(1:nn/nn*36*pi), start=1970, frequency=4)
plot(cbind(sin1, sin2), main="", xlab="", ylab="", col="steelblue")
spectrum(cbind(sin1, sin2), spans=c(3,3), main="", col="steelblue")

spectrum(gdp_o, spans=c(3,3), main="", col="steelblue")
plot(sin1+sin2, main="", xlab="", ylab="", col="steelblue")
spectrum(sin1+sin2, spans=c(3,3), main="", col="steelblue")

plot(wn, main="", xlab="", ylab="", col="steelblue")
abline(h=0, lty=2, col="gray")
spectrum(wn, spans=c(3,3), main="", col=1:2)

plot(gdp_o, main="", xlab="", ylab="")
lines(gdp_sa, col=2)
spectrum(cbind(gdp_o, gdp_sa), spans=c(3,3), main="", col=1:2)

dlgdp1 = diff(log(gdp_o))
dlgdp2 = diff(log(gdp_o), 4)
dlgdp = cbind(dlgdp1, dlgdp2)
plot(dlgdp1, main="", xlab="", ylab="", col="steelblue")
lines(dlgdp2, col=2)
spectrum(na.omit(cbind(dlgdp1, dlgdp2)), spans=c(3,3), main="", col=c("steelblue"), "red")
