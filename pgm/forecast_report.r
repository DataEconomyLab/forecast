setwd("C:/Users/ljyjj/Documents/��۴�/4�г����")
gdp <- read.csv("GDP.csv", header=TRUE)
gdp$ts <- ts(gdp, start=1982, frequency=4)
plot(gdp[,1]/1000000, ylab="GDP(�� ��)", xlab="����", col="steelblue")
start(gdp$ts)
end(gdp$ts)
frequency(gdp$ts)
class(gdp$ts)
time(gdp$ts)
d<-decompose(gdp$ts)
gdp$adj <- gdp$ts - d$seasonal
gdp$ts
gdp$adj[,1]
plot(gdp[,1]/1000, ylab="GDP(�� ��)", xlab="����", col="steelblue")
lines(gdp[,1]/1000, col="black")
plot(gdp$ts[,1]/1000, col="black")
plot(gdp$adj[,1]/1000, col="black")

