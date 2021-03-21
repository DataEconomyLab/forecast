#2-1 GPD 시계열 읽기
setwd("c:/work/data")
gdp <- ts(read.csv("gdpq.csv", header=TRUE), start=1970, frequency=4)
gdp

#2-2 GDP 시계열도표 작성
plot(gdp[,1]/1000, ylab="GDP(조 원)", xlab="연도", col="black")
lines(gdp[,2]/1000, col="mediumpurple3")

#2-3 GDP의 차분과 이동평균
ww = c(.5,1,1,1,.5); gdpm5=filter(gdp, sides=2, ww/sum(ww))
dlgdp1=diff(log(gdp))
dlgdp4=diff(log(gdp),4)
par(mfrow=c(2,2))
plot(gdp, main="GDP", ylab="", xlab="YEAR")
plot(gdpm5, main="GDPM5", ylab="", xlab="YEAR")
plot(dlgdp1, main="diff(log(GDP))", ylab="", xlab="YEAR")
plot(dlgdp4, main="diff(log(GDP),4)", ylab="", xlab="YEAR")



