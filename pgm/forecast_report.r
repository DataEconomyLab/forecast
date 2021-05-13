setwd("C:/Users/ljyjj/Documents/GitHub/forecast/data")
forecast_report <- ts(read.csv("forecast_report.csv", header=TRUE), start=1982, frequency=4)
head(forecast_report)

plot(forecast_report[,1]/1000, ylab="분기민간소비GDP(조 원)", xlab="연도", col="black")
lines(forecast_report[,2]/1000, col="mediumpurple3")

#################################################################################################################

par(mfrow=c(2,2))
spectrum(forecast_report[,1], spans=c(3,3), main="분기민간소비GDP 원계열", col="mediumpurple3")
spectrum(forecast_report[,2], spans=c(3,3), main="분기민간소비GDP 계절조정계열", col="mediumpurple3")
spectrum(na.omit(diff(log(forecast_report[,1]))), spans=c(3,3), main="diff(log(GDP))", col="mediumpurple3")
spectrum(na.omit(diff(log(forecast_report[,1]), 4)), spans=c(3,3), main="diff(log(GDP), 4)", col="mediumpurple3")

#################################################################################################################

par(mfrow=c(2,2))
acf(diff(log(forecast_report[,1])), main="로그 계열 상관도표")
pacf(diff(log(forecast_report[,1])), main="로그 계열 부분상관도표")
acf(diff(log(forecast_report[,1]), 4), main="로그차분 계열 상관도표")
pacf(diff(log(forecast_report[,1]), 4), main="로그차분 계열 부분상관도표")

#################################################################################################################

library(forecast)
plot(diff(log(forecast_report[,1])), ylab="분기민간소비GDP(조 원)", xlab="연도", col="black")
acf(diff(log(forecast_report[,1])), main="로그 계열 상관도표")
pacf(diff(log(forecast_report[,1], 4)), main="로그 계열 부분상관도표")
gdp_fit = arima(log(forecast_report[,1]))

# ARIMA 모형의 추정
report_fit = arima(log(forecast_report[,1]), order=c(0,1,0))
summary(report_fit)

# ARIMA 모형의 과대적합 검토
report_fit1 = arima(log(forecast_report[,1]), order=c(1,1,1))
summary(report_fit1)
report_fit2 = arima(log(forecast_report[,1]), order=c(0,1,2))
summary(report_fit1)

# ARIMA 모형의 진단
tsdiag(report_fit)
plot(forecast(report_fit, h=12))

