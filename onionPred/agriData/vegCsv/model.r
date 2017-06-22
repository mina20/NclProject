#
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(imputeTS)
library(argo)
data <- read.csv("test.csv")
df_xt <- xts(read.zoo(data))
#df_xt <- apply.monthly(df_xt,mean)
x <- df_xt[,-ncol(df_xt)]
y <- df_xt$Modal.PriceOnion

mx <- max(y,na.rm=T)
mn <- min(y,na.rm=T)
newMax=100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }
sc_exo <- range02(x)
##################################
range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}
rev_01 <- function(x,mx=max(x,na.rm=T),mn=min(x,na.rm=T)){x*(mx-mn)+mn}
y_sc <- range01(y)
max_lag <- 24
our_argo <- list()
for(i in 1:max_lag){
our_argo[i] <- argo(log(y/mx), exogen = log((sc_exo + 5)),alpha = 1, N_lag=1:i, use_all_previous = FALSE, N_training = 40)
}
temp1 <- merge(our_argo[[1]],our_argo[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,our_argo[[i]])
}
temp1 <- merge(our_argo[[1]],our_argo[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,our_argo[[i]])
}
n <- ncol(temp3)
for(i in 1:n){
rme <- sqrt (mean(  (temp3[,i]-temp3[,1])^2)  )
argo_rmse <- c(argo_rmse,rme)
}
argo_rmse <- argo_rmse[-1]
Lag_argo <- colnames(temp3)[-1]
Argo <- data.frame(Lag_argo,argo_rmse)
qplot(Lag_argo,argo_rmse , colour = Lag_argo,data=Argo,size=I(4.0))
dev.off()
#########################################################################################
#Models on weekly data lag is 2
#lag for month in 4
my_argo <- argo(log(y / mx), exogen = log((sc_exo + 5) / mx),alpha = 1, N_lag=1:2, use_all_previous = FALSE, N_training = 160)
#our_argo <- argo(log(y / mx), exogen = log((sc_exo + 5) / mx),alpha = 1, N_lag=1:16, use_all_previous = FALSE, N_training = 160)
sc_argo <- argo(y_sc, exogen = log((sc_exo + 5) / mx),alpha = 1, N_lag=1:2, use_all_previous = FALSE, N_training = 160)
#simple_argo <- argo(log(y) , exogen = log(exogen),alpha = 1, N_lag=1:52,use_all_previous = FALSE, N_training = 104)
santillana <- argo(y, exogen = sc_exo, alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 160)
#ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:2, N_training = 32)
ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 160)
naive <- lag(y,k=1)
################################
my_argo$pred <- exp(my_argo$pred)*mx
#our_argo$pred <- exp(our_argo$pred)*mx
sc_argo$pred <-rev_01(sc_argo$pred,mx,mn) 
pred <- merge(y,my_argo$pred,sc_argo$pred,santillana$pred,ar1$pred,naive,all=FALSE)
colnames(pred)<- c('Real','my_argo','sc_argo','santillana','AR1','Naive')
st <- which(index(pred)=='2016-01-31')
pr <- pred[st:nrow(pred),]
pred <- na.omit(pred)
#pred <- pred[st:nrow(pred)]
rmse <- c()
mae <-c()
mape <- c()
corr <- c()
n <- ncol(pred)
for(i in 1:n){
        rme <- sqrt (mean(  (pred[,i]-pred[,1])^2)  )
        ma <- mean(abs(pred[,i]-pred[,1]))
       map <- mean((1/pred[,1])*(abs(pred[,i]-pred[,1])))
	co <- cor(pred[,1],pred[,i],method='pearson') 
        rmse <- c(rmse,rme)
        mae <- c(mae,ma)
	mape <-c(mape,map)
	corr <- c(corr,co)
        }
#Plot rmse and mae
#Dr <- max(pred[,1]-min(pred[,1]))
mp <- mape[length(mape)]
rm <- rmse[length(rmse)]
ma <- mae[length(mae)]
rmse <- rmse/rm
mae <- mae/ma
mape <- mape/mp
rmse <- rmse[-1]
mae <- mae[-1]
corr <- corr[-1]
mape <- mape[-1]
Model <- colnames(pred)[-1]
md <- data.frame(Model,rmse)
mdd <- data.frame(Model,mae)
myd <- data.frame(Model,mape)
cod <- data.frame(Model,corr)




























