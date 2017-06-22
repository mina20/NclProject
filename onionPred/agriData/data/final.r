library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
df <- read.csv('wdata.csv')
df_xt <- xts(read.zoo(df))
df_imp <- df_xt   #na.interpolation(df_xt,option="spline")
df_imp$MAX <- df_imp$MAX+273
df_imp$MIN <- df_imp$MIN+273
df_imp$R.F <- df_imp$R.F*1000
df_imp$EVP <- df_imp$EVP*1000
df_imp$DR <- df_imp$DR*3600
df_imp$SSH <- df_imp$SSH*3600
df_imp$RH <- df_imp$RH*100
df_imp$VP <- df_imp$VP*100
x <- df_xt[,-ncol(df_xt)]
y <- df_xt$Modal.Price.onion
exogen <- na.interpolation(x, option="linear")
mx <- max(y,na.rm=T)
mn <- min(y,na.rm=T)
###################################################
#sc_exo <- scale(exogen, center = TRUE, scale = TRUE)
#sc_y <- scale(y,center = TRUE, scale=TRUE) 
#Scale between 0 to 1
#range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}
#rev_01 <- function(x,mx,mn){x*(mx-mn)+mn}
#range02 <- function(x){ (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin }
#scale 0-100
newMax=100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }
sc_exo <- range02(exogen)
#sc_y <- range01(y)
max_lag <- 52
our_argo <- list()
for(i in 1:max_lag){
our_argo[i] <- argo(log(y/mx), exogen = log((sc_exo + 5)),alpha = 1, N_lag=1:i, use_all_previous = FALSE, N_training = 160)
}
temp1 <- merge(our_argo[[1]],our_argo[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,our_argo[[i]])
}
colnames(temp1)<-c(1:max_lag)
temp1 <- exp(temp1)*mx
temp2 <- cbind(y,temp1)
temp3 <- na.omit(temp2)
argo_rmse <- c()
n <- ncol(temp3)
for(i in 1:n){
rme <- sqrt (mean(  (temp3[,i]-temp3[,1])^2)  )
argo_rmse <- c(argo_rmse,rme)
}
#colnames(temp3) <- c(1:25)
argo_rmse <- argo_rmse[-1]
Lag_argo <- colnames(temp3)[-1]
Argo <- data.frame(Lag_argo,argo_rmse)
pdf('nag_argo_orange.pdf')
qplot(Lag_argo,argo_rmse , colour = Lag_argo,data=Argo,size=I(4.0))
dev.off()

#####################################################
#our_argo <- argo(log(y/mx), exogen = log(sc_exo+5),alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
my_argo <- argo(log(y/mx), exogen = log(sc_exo+5),alpha = 1, N_lag=1:3, use_all_previous = FALSE, N_training = 160)
santillana <- argo(y, exogen = sc_exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 160)
ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:2, N_training = 160)
ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 160)
naive <- lag(y,k=1)
########################################################
#our_argo_sc <- argo(sc_y, exogen = log(sc_exo+5),alpha = 1, N_lag=1:24, use_all_previous = FALSE, N_training = 48)
#santillana_sc<- argo(sc_y, exogen = sc_exo , alpha = 1, use_all_previous = FALSE, N_training = 48)
#ar2_sc <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:3, N_training = 48)
#ar1_sc <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 48)
#naive1 <- lag(y,k=1)
#our_argo_sc$pred <- rev_01(our_argo1$pred ,mx ,mn)
#santillana_sc$pred <- rev_01(santillana$pred,mx,mn)
#############################################################
#pred <- merge(y,exp(our_argo$pred)*mx,exp(my_argo$pred)*mx,santillana$pred,ar2$pred,ar1$pred,naive,all=FALSE)
pred <- merge(y,exp(my_argo$pred)*mx,santillana$pred,ar2$pred,ar1$pred,naive,all=FALSE)
colnames(pred)<- c('Real',"my_argo",'santillana','AR2','AR1','Naive')
pred <- na.omit(pred)
#st <- which(index(pred)=='2010-03-21')
#pred <- pred[st:nrow(pred)-1,]
#pre <- merge(y,our_argo_sc$pred,santillana_sc$pred,ar2_sc$pred,ar1_sc$pred,naive,all=FALSE)
#colnames(pred)<- c('Real','Argo_sc','santillana_sc','AR2_sc','AR1_sc','Naive')
#pre <- pre[st:nrow(pre)-1,]
#error <- function(pred[-nrow(pred),]){
#pred <- pred[st:nrow(pred)-1,]
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
#}




















