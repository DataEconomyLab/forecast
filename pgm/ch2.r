# Figure 2-3
setwd("c:/work/data")
gni_i <- ts(read.csv("gni.csv", header=TRUE), start=1970, frequency=1)
plot(gni_i[,2], xlab="연도", ylab="1인당 국민소득", col="steelblue")
points(gni_i[,2])
