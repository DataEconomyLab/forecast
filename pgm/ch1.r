library(forecast)
fo1 <- "https://github.com/data-better/Forecasting/tree/gh-pages/data/"
gdp <- read.csv("gdpq.csv", header=TRUE)
gdp_o  <- ts(gdp[,1] / 1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2] / 1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1)) / lag(gdp_sa, -1)*100, start=c(1970,2), frequency=4)

# Fit model to first few years of AirPassengers data
gdp.model <- Arima(window(gdp_o, end=2008+11/12), order=c(2,1,3), seasonal=list(order=c(0,1,1), period=4), lambda=0)
plot(forecast(gdp.model, h=20))
lines(gdp_o)

# Apply fitted model to later data
gdp.model2 <- Arima(window(gdp_o, start=2009), model=gdp.model)

# Forecast accuracy measures on the log scale.
# in-sample one-step forecasts.
accuracy(gdp.model)
# out-of-sample one-step forecasts.
accuracy(gdp.model2)
# out-of-sample multi-step forecasts
accuracy(forecast(gdp.model, h=20, lamdba=NULL), window(gdp_o, start=2009))

