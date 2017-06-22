#onion price on monthly data
library(zoo)
library(xts)
library(reshape2)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
df <- read.csv('wdata.csv')
df_xt <- xts(read.zoo(df))
df_imp <- df_xt
#df_imp <- na.interpolation(df_xt,option="linear")
df_imp$MAX <- df_imp$MAX+273
df_imp$MIN <- df_imp$MIN+273
df_imp$R.F <- df_imp$R.F*1000
df_imp$EVP <- df_imp$EVP*1000
df_imp$DR <- df_imp$DR*3600
df_imp$SSH <- df_imp$SSH*3600
df_imp$RH <- df_imp$RH*100
df_imp$VP <- df_imp$VP*100
mdf <- apply.monthly(df_imp,mean)
#mdf <- df_imp
###############################################
data <- data.frame(date=index(df_w), coredata(df_w))
new_df<- data.frame()
var=12
#v <- c(31,32,33,34,35,40,47)
#for(i in 8:nrow(mdf))
for(i in 17:nrow(data)){
new_data <- cbind(data[i,],data[var,13:19])
for(j in (var-1):(var-11)){
new_data <- cbind(new_data,data[j,13:19])
}
 var=var+1
new_df <- rbind(new_df,new_data)

}
mdf<- xts(read.zoo(new_df))
##################################################
xx <- mdf[,-ncol(mdf)]
xx <- na.interpolation(xx,option="linear")
exogen <- xx
y <- mdf[,ncol(mdf)]
y[nrow(y),] <- NA
mn <- min(y,na.rm=T)
mx <- max(y,na.rm=T)
newMax=100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }
sc_exo <- range02(exogen)
##################################
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
qplot(Lag_argo,argo_rmse , colour = Lag_argo,data=Argo,size=I(4.0))
dev.off()
########################################
#our_argo <- argo(log(y/mx), exogen = log((exogen+5)),alpha = 1, N_lag=1:12, use_all_previous = FALSE, N_training = 24)
#my_argo <- argo(log(y),exogen=log(exogen+o.5),alpha=1,N_lag=1:2,use_all_previous=FALSE,N_training=24)
#santillana <- argo(y, exogen = exogen, alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 24)
#ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:3, N_training = 24)
#ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 24)
#naive <- lag(y,k=1)
#############################################
range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}
rev_01 <- function(x,mx=max(x,na.rm=T),mn=min(x,na.rm=T)){x*(mx-mn)+mn}
y_sc <- range01(y)
#optimal lag for week=1,month=3,4
#argo_sc <- argo(y_sc, exogen = log(sc_exo+5),alpha = 1, N_lag=1, use_all_previous = FALSE, N_training = 160)
my_argo <- argo(log(y/mx), exogen = log((sc_exo+5)/100),alpha = 1, N_lag=1:4, use_all_previous = FALSE, N_training = 48)
santillana <- argo(y, exogen = sc_exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 48)
ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:2, N_training = 48)
#ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 48)
naive <- lag(y,k=1)
###############################################
#heatmap

my_argo$pred <- exp(my_argo$pred)*mx
#argo_sc$pred <-rev_01(argo_sc$pred,mx,mn) 
pred <- merge(y,my_argo$pred,santillana$pred,ar2$pred,naive,all=FALSE)
colnames(pred)<- c('Real','my_argo','santillana','AR2','Naive')
st <- which(index(pred)=='2015-12-20')
end <- which(index(pred)=='2015-02-02')
pr <- pred[st:nrow(pred),]
pred <- pred[st:end]
#pred <- na.omit(pred)
pred <- pred[st:nrow(pred),]
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
        #resi[,i] <- pred[,1]-pred[,i]
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
dat <- cbind(md,myd,mdd,cod)
dat <- dat[,-c(3,5,7)]
mdat <- melt(dat, id.vars="Model")
p <- ggplot(mdat, aes(variable, value, fill=Model)) + geom_bar(stat="identity", position="dodge")
p1<-autoplot(pred,facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
#dev.off()
#pdf('real_argo.pdf')
p2 <-autoplot(pred[,1:2],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
#dev.off()
#pdf('real_sim.pdf')
p3<-autoplot(pred[,c(1,3)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
#dev.off()
#pdf('real_my.pdf')
p4<-autoplot(pred[,c(1,4)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
#dev.off()
#pdf('real_sant.pdf')
p5<-autoplot(pred[,c(1,5)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="predicted Values")
#dev.off()
#pdf('real_ar2')
p6<-autoplot(pred[,c(1,6)],facets=FALSE, geom = "line",size=3,xlab='Time',ylab='predicted Values')

argo_rmse <- c(355.5433,353.7057,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188,355.7188)




















