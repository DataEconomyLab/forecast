# Figure 2-3
setwd("c:/work/data")
gni_i <- ts(read.csv("gni.csv", header=TRUE), start=1970, frequency=1)
plot(gni_i[,2], xlab="연도", ylab="1인당 국민소득", col="steelblue")
points(gni_i[,2])

# Figure 2-5
gdp_d <- ts(read.csv("gdpq.csv", header=TRUE), start=1970, frequency=4)
plot(gdp_d[,1]/1000, ylab="GDP(조 원)", xlab="연도", col="steelblue")
lines(gdp_d[,2]/1000, col="red")

# Figure 2-6
cci <- ts(read.csv("bc.csv", header=TRUE), start=1970, frequency=12)
plot(cci[,1], ylab="동행지수순환변동치", xlab="연도", col="steelblue")
abline(h=100, lty=2, col="gray")

# Figure 2-7
climate <- ts(read.csv("climate.csv", header=TRUE), start=1980, frequency=12)
plot(climate, ylab="대전 평균기온", xlab="연도", col="steelblue")

# Figure 2-8
kospi1 <- read.csv("kospi_n.csv", header=TRUE)
log_kospi = ts(log(kospi1[,1]), start=1993, frequency=12)
kospi = ts(kospi1[,1], start=1993, frequency=12)
plot(cbind(kospi, log_kospi), xlab="연도", col="steelblue", main="")


# Figure 2-8
kospi1 <- read.csv("kospi_n.csv", header=TRUE)
log_kospi = ts(log(kospi1[,1]), start=1993, frequency=12)
kospi = ts(kospi1[,1], start=1993, frequency=12)
plot(cbind(kospi, log_kospi), xlab="연도", col="steelblue", main="")

# Figure 2-9
ipic1 <- read.csv("ipic.csv", header=TRUE)
ipi <- ts(ipic1[,1], start=2000, frequency=12)
ipi_c <- ts(ipic1[,2], start=2000, frequency=12)
plot(cbind(ipi, ipi_c), xlab="연도", col="steelblue", main="")

# Figure 2-10
kospi1 <- read.csv("kospi_n.csv", header=TRUE)
kospi = ts(kospi1[,1], start=1993, frequency=12)
plot(cbind(log(kospi),diff(log(kospi))), xlab="연도", ylab="log(KOSPI)", col="steelblue", main="")

# Figure 2-11
gdp <- read.csv("gdpq.csv", header=TRUE)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1)) / lag(gdp_sa,-1)*100, start=c(1970,2), frequency=4)
plot(cbind(gdp_sa, gdp_gr), ylab="GDP(조 원)", xlab="연도", col="steelblue", main="")

# Figure 2-12
library(quantmod)
# Yahoo! Finance로부터 종합주가지수 종가 데이터 가져오기
kospi<- getSymbols("^KS11", auto.assign = FALSE)
#그래프
chartSeries(kospi, subset='last 6 months', theme='white', TA=NULL, up.col="red", dn.col="blue")
addSMA(n=5, on=1, with.col=Cl, overlay = TRUE, col="brown")
addSMA(n=20, on=1, with.col=Cl, overlay = TRUE, col="red")
addSMA(n=60, on=1, with.col=Cl, overlay = TRUE, col="blue")
addSMA(n=120, on=1, with.col=Cl, overlay = TRUE, col="black")

# Figure 2-13, 2-14
gdp1 <- read.csv("gdpq.csv", header=TRUE)
gdp <- ts(gdp1[,1]/1000, start=1970, frequency=4)
ww=c(.5,1,1,1,.5)
gdpm5 = filter(gdp, sides=2, ww/sum(ww))
ww = c(1,1,1)
gdpm3 = filter(gdp, sides=2, ww/sum(ww))

dlgdp1 = diff(log(gdp))
dlgdp4 = diff(log(gdp), 4)

# 2-13
plot(gdp, ylab="GDP(조 원)", xlab="연도", col="steelblue")
lines(gdpm5, col="red")
# 2-14
plot(gdp, ylab="GDP(조 원)", xlab="연도", col="steelblue")
lines(gdpm3, col="red")

# 2-16
par(mfrow=c(2,2))
plot(gdp, main="GDP", ylab="", xlab="year", col="steelblue")
plot(gdpm5, main="GDPM5", ylab="", xlab="year", col="steelblue")
plot(dlgdp1, main="diff(log(GDP))", ylab="", xlab="year", col="steelblue")
plot(dlgdp4, main="diff(log(GDP),4)", ylab="", xlab="year", col="steelblue")
