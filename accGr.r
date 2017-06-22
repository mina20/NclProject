# Graphical persentation of accuracy table

#################
# RMSE value
library(ggplot2)
x <- c("Argo","GFT","Santillan","GFT+AR-3","AR-3","Naive")
y <- c(0.642,2.217,0.933,0.912,0.957,0.345)
myd <- data.frame(x,y)
png("rmse.png")
qplot(x, y, colour = x,xlab="Model",ylab="RMSE",main="Root Mean Square Error",data=myd)
dev.off()
z <- c(0.665,1.836,1.050,0.889,0.926,0.198)
png("mae.png")
qplot(x, z, colour = x,xlab="Model",ylab="MAE",main="Mean Absolute Error",data=myd)
dev.off()
m <- c(0.791,1.940,1.374,1.038,1.008,0.089)
png("mape.png")
qplot(x, m, colour = x,xlab="Model",ylab="MAPE",main="Mean Absolute Percentage Error",data=myd)
dev.off()
png("cor.png")
co <- c(0.985,0.877,0.969,0.968,0.964,0.961)
qplot(x, co, colour = x,xlab="Model",ylab="Correlation",main="Correlation",data=myd)
dev.off()
co1 <- c(0.725,0.706,0.652,0.512,0.385,0.437)
png("coI.png")
qplot(x, co1, colour = x,xlab="Model",ylab="corr.of increament",main="Correlation of increament",data=myd)
dev.off()





