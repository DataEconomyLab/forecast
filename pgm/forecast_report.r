setwd("C:/Users/ljyjj/Documents/GitHub/forecast/data")
forecast_report <- ts(read.csv("forecast_report.csv", header=TRUE), start=1982, frequency=4)
head(forecast_report)

plot(forecast_report[,1]/1000, ylab="�б�ΰ��Һ�GDP(�� ��)", xlab="����", col="black")
lines(forecast_report[,2]/1000, col="mediumpurple3")

#################################################################################################################

par(mfrow=c(2,2))
spectrum(forecast_report[,1], spans=c(3,3), main="�б�ΰ��Һ�GDP ���迭", col="mediumpurple3")
spectrum(forecast_report[,2], spans=c(3,3), main="�б�ΰ��Һ�GDP ���������迭", col="mediumpurple3")
spectrum(na.omit(diff(log(forecast_report[,1]))), spans=c(3,3), main="diff(log(GDP))", col="mediumpurple3")
spectrum(na.omit(diff(log(forecast_report[,1]), 4)), spans=c(3,3), main="diff(log(GDP), 4)", col="mediumpurple3")

#################################################################################################################

par(mfrow=c(2,2))
acf(diff(log(forecast_report[,1])), main="�α� �迭 �����ǥ")
pacf(diff(log(forecast_report[,1])), main="�α� �迭 �κл����ǥ")
acf(diff(log(forecast_report[,1]), 4), main="�α����� �迭 �����ǥ")
pacf(diff(log(forecast_report[,1]), 4), main="�α����� �迭 �κл����ǥ")

############################################################################################
setwd("C:/Users/ljyjj/Documents/GitHub/forecast/data")
forecast_report <- ts(read.csv("forecast_report.csv", header=TRUE), start=1982, frequency=4)

library(tseries)
gdp <- forecast_report[,1]
adf.test(log(gdp))

library(forecast)
par(mfrow=c(1,1))
acf(diff(log(gdp),4))
pacf(diff(log(gdp),4))

# ARIMA ������ ����
auto.arima(log(gdp))
report_fit = arima(log(gdp), order=c(0,1,1), seasonal=c(0,1,1))
summary(report_fit)

# ARIMA ������ �������� ����
report_fit1 = arima(log(gdp), order=c(1,1,1), seasonal=c(0,1,1))
summary(report_fit1)
report_fit2 = arima(log(gdp), order=c(0,1,2), seasonal=c(0,1,1))
summary(report_fit2)

# ARIMA ������ ����
tsdiag(report_fit)
plot(forecast(report_fit, h=12))

