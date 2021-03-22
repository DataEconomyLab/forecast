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
