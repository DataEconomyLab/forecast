# Figure 3-1
gdp <- read.csv("c:/work/data/gdpq.csv")
gdp_o  <- ts(gdp[,1]/1000, start=1970, frequency=4)
gdp_sa <- ts(gdp[,2]/1000, start=1970, frequency=4)
gdp_gr <- ts((gdp_sa - lag(gdp_sa, -1))/lag(gdp_sa, -1)*100, start=c(1970, 2), frequency=4)

set.seed(1)
nn = length(gdp_sa)
nn = 176
wn = ts(rnorm(nn), start=1970, frequency=4)

plot(wn, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")

par(mfrow=c(2,1))
  acf(wn, main="", col="steelblue")
 pacf(wn, main="", col="steelblue")

par(mfrow=c(1,1))
 spectrum(wn, spans=c(3,3), main="")
 
set.seed(5)
arsim1 <- ts(arima.sim(list(order=c(1,0,0), ar= 0.6), n=nn), start=1970, freq=4)
arsim2 <- ts(arima.sim(list(order=c(1,0,0), ar=-0.6), n=nn), start=1970, freq=4)

plot(arsim1, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")

plot(arsim2, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")

par(mfrow=c(2,1))
 acf(arsim1, main="")
 pacf(arsim1, main="")
 
 acf(arsim2, main="")
 pacf(arsim2, main="") 
 
par(mfrow=c(1,1)) 
 spectrum(arsim1, spans=c(3,3), main="")
 spectrum(arsim2, spans=c(3,3), main="")

masim1 <- ts(arima.sim(list(order=c(0,0,1), ma= 0.6), n=nn), start=1970, freq=4)
masim2 <- ts(arima.sim(list(order=c(0,0,1), ma=-0.6), n=nn), start=1970, freq=4)
 
 plot(masim1, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 
 plot(masim2, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 
par(mfrow=c(2,1))
  acf(masim1, main="")
 pacf(masim1, main="")
 
  acf(masim2, main="")
 pacf(masim2, main="") 
 
par(mfrow=c(1,1)) 
 spectrum(masim1, spans=c(3,3), main="")
 spectrum(masim2, spans=c(3,3), main="")
 
armasim <- ts(arima.sim(list(order=c(1,0,1), ar= 0.6), n=nn), start=1970, freq=4)

plot(armasim, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 
par(mfrow=c(2,1))
   acf(armasim, main="")
  pacf(armasim, main="")
  
par(mfrow=c(1,1))  
 spectrum(armasim, spans=c(3,3), main="")
 
rwsim <- ts(arima.sim(list(order=c(0,1,0)), n=nn), start=1970, freq=4)
 plot(rwsim, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 
par(mfrow=c(2,1))
  acf(rwsim, main="")
 pacf(rwsim, main="")
 
par(mfrow=c(2,1))
  acf(rwsim, main="")
 pacf(rwsim, main="")

par(mfrow=c(2,1))  
  acf(rwsim, main="")
 pacf(rwsim, main="")
 
par(mfrow=c(1,1))  
 spectrum(rwsim, spans=c(3,3), main="")
 
#####################################################
set.seed(5)

v1 = rnorm(154,0,1)
rwsim1 = ts(cumsum(v1), start=1970, freq=4) 
v2 = rnorm(154,0.2,1)
rwsim2 = ts(cumsum(v2), start=1970, freq=4) 
 
plot(rwsim2, main="", xlab="", ylab="", col="steelblue", ylim=c(-5,40))
 lines(rwsim1, col="red")
 abline(h=0, lty=2, col="gray")
 
par(mfrow=c(2,1))
  acf(rwsim1, main="")
 pacf(rwsim1, main="")
  acf(rwsim2, main="")
 pacf(rwsim2, main="")

par(mfrow=c(1,1))
 spectrum(cbind(rwsim1, rwsim2), spans=c(3,3), main="")

#####################################################
set.seed(1)
arimasim <- ts(arima.sim(list(order = c(1,1,1), ar=0.6, ma=0.6), n=nn), start=1970, freq=4)
plot(arimasim, main="", xlab="", ylab="", col="steelblue")
 abline(h=0, lty=2, col="gray")
 
par(mfrow=c(2,1))
  acf(arimasim, main="")
 pacf(arimasim, main="")

par(mfrow=c(1,1))
 spectrum(arimasim, spans=c(3,3), main="")
 
#####################################################
 library(forecast)
 set.seed(1)
 m <- list(
   arma=c(1,1,0,1,4,1,1),
   model=list(
     phi=0.5,
     theta=c(-0.5,  0.0,  0.0,  -0.5, 0.25)
   ),
   sigma2=1,
   x=NA
 )
 sarimasim <- ts(simulate.Arima(m, nsim=nn+21, future=FALSE),start=1970, frequency=4)
 
 plot(sarimasim, main="", xlab="", ylab="", col="steelblue")
 
 par(mfrow=c(2,1))
 acf(sarimasim, main="")
 pacf(sarimasim, main="")
 
 par(mfrow=c(1,1))
 spectrum(sarimasim, spans=c(3,3), main="")
 
 plot(diff(sarimasim), main="", xlab="", ylab="", col="steelblue")
 
 par(mfrow=c(2,1))
 acf(diff(sarimasim), main="")
 pacf(diff(sarimasim), main="")
 
 par(mfrow=c(1,1))
 spectrum(diff(sarimasim), spans=c(3,3), main="")

#####################################################
library(tsDyn)
library(sm)
 
TvarMat <- c(-1, -0.5, 0.2, 1, 0.7, -0.2)
sim <- setar.sim(B=TvarMat, lag=2, type="simul", n=154, nthresh=1, Thresh=0, starting=c(2.8, 2.2))$serie

plot(ts(sim, start=1970, frequency=4), xlab=)
 
par(mfrow=c(1,2))
autopairs(sim, lag=1, type="regression")
autopairs(sim, lag=2, type="regression")

########################################
library(TSA)
set.seed(5)
tarsim <- tar.sim(n=176, Phi1=c(0, 0.7), Phi2=c(0,-0.9), p=1, d=1, sigma1=1, thd=0.5, sigma2=2)$y

plot(ts(tarsim, start=1970, frequency=4), xlab="", ylab="", col="steelblue")
lagplot(tarsim, method="gam")

library(tsDyn)
library(sm)
par(mfrow=c(1,2))
autopairs(tarsim, lag=1, type="regression")
autopairs(tarsim, lag=2, type="regression")

########################################
# Figure 4-20
library(quantmod)
# Yahoo! Finance????????? ?????????????????? ?????? ????????? ????????????
kospi <- getSymbols("^KS11", auto.assign= FALSE)[,4]
kospi_r <- dailyReturn(na.omit(kospi))
plot(kospi_r, main="")
plot(kospi_r^2, main="")

library("fGarch")
set.seed(5)
spec = garchSpec(model = list(alpha = c(0.5, 0.4), beta=0))
garchsim = garchSim(spec, n=176)

plot(ts(garchsim, start=1970, frequency=4), xlab="", ylab="", col="steelblue")

par(mfrow=c(2,1))
  acf(garchsim, main="")
 pacf(garchsim, main="")

  acf(garchsim^2, main="")
 pacf(garchsim^2, main="")

