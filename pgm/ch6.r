library(tseries)
library(mFilter)

# Figure 3-1
gdp <- read.csv("c:/work/data/gdpq.csv")
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1))/lag(gdp_sa, -1)*100, start=c(1970, 2), frequency=4)

## Figure 5-1 ##
plot(gdp, ylab="GDP", xlab="", col="steelblue")
#lines(gdpsa, col=2)

# GDP 변동요인 분해
lgdp.hp = mFilter(log(gdp_sa), filter="HP")    # Hodric-prescott filter
gdp_t = exp(lgdp.hp$trend)
gdpsam = exp((log(gdp_sa) + lag(log(gdp_sa), -1) + lag(log(gdp_sa), 1)) / 3)
gdp_s = gdp_o / gdp_sa * 100
gdp_i = gdp_sa / gdpsam * 100
gdp_c = gdpsam/gdp_t * 100

## Fig 5-2 ##
par(mfrow=c(2,2))
plot(gdp_t, main="추세변동요인", col="steelblue")
plot(gdp_c, main="순환변동요인", col="steelblue")
plot(gdp_s, main="계절변동요인", col="steelblue")
plot(gdp_i, main="불규칙변동요인", col="steelblue")

## Fig 5-4 ##
par(mfrow=c(1,1))
plot(gdp, ylab="GDP", xlab="", col="steelblue")
lines(gdpsa, col="red")

