#onion price on monthly data
library(zoo)
library(xts)
library(ggplot2)
library(reshape2)
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
#mdf <- apply.monthly(df_imp,mean)
df_w <- df_imp
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
#xx <- mdf[,-ncol(new_df)]
xx <- mdf[,-21]
xx <- na.interpolation(xx,option="linear")
exogen <- xx
#y <- mdf[,ncol(mdf)]
y <- mdf$Modal.Price.onion
xx1 <- df_w[-(1:16),-ncol(df_w)]
xx1 <- na.interpolation(xx1,option="linear")
y[nrow(y),] <- NA
mn <- min(y,na.rm=T)
mx <- max(y,na.rm=T)
newMax=100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }
sc_exo <- range02(exogen)
exo <- range02(xx1)
##################################

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
#argo_sc <- argo(y_sc, exogen = log(sc_exo+5),alpha = 1, N_lag=1:12, use_all_previous = FALSE, N_training = 160)
my_argo <- argo(log(y/1000), exogen = log((sc_exo+5)/100),alpha = 1, N_lag=5:52, use_all_previous = FALSE, N_training = 160)
#my_argo1 <- argo(log(y/1000), exogen = log((sc_exo+5)/100),alpha = 1, N_lag=5:6, use_all_previous = FALSE, N_training = 160)

santillana <- argo(y, exogen = exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
my_santillana <- argo(y, exogen = sc_exo , alpha = 1, N_lag = 5:52, use_all_previous = TRUE,  N_training = 160)
#my_sani<-argo(y, exogen = sc_exo , alpha = 1, N_lag = 5:52, use_all_previous = TRUE,  N_training = 160)
#my_santillana <- argo(log(y/1000), exogen = log((sc_exo+5)/100) , alpha = 1, N_lag = 5:21, use_all_previous = TRUE,  N_training = 160)
#my_santillana1 <- argo(log(y/1000), exogen = log((sc_exo+5)/100) , alpha = 1, N_lag = 5:6, use_all_previous = TRUE,  N_training = 104)
#ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=5:6, N_training = 104)
mod_ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=5, N_training = 104)
mod_naive <-lag(y, 4, na.pad = TRUE)
###############################################
#heatmap

my_argo$pred <- exp(my_argo$pred)*1000
#my_santillana$pred <- exp(my_santillana$pred)*1000
#my_santillana1$pred <- exp(my_santillana1$pred)*1000
#argo_sc$pred <-rev_01(argo_sc$pred,mx,mn) 
pred <- merge(y,my_argo$pred,my_santillana$pred,santillana$pred,mod_ar1$pred,mod_naive,all=FALSE)
colnames(pred)<- c('Real','my_argo','my_santillana','santillana','Mod_ar1','Mod_Naive')
st <- which(index(pred)=='2013-07-07')
end <- which(index(pred)=='2014-02-02')
pr <- pred[st:nrow(pred),]
pred <- na.omit(pred)
#pred <- na.omit(pred)
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
#pdf(paste0("acc.pdf"),width=15, height=8)
p <- ggplot(mdat, aes(variable, value, fill=Model)) + geom_bar(stat="identity", position="dodge")
blue.bold.italic.16.text <- element_text(face = "bold.italic", color = "black", size = 18)
p+ theme(axis.text= blue.bold.italic.16.text)+theme(strip.text=element_text(face='bold.italic',size = 18, colour = "black")
#theme(strip.text=element_text(face='bold.italic',size = 10, colour = "black"))
#p+theme(blue.bold.italic.16.text)
#p+ theme(axis.text = blue.bold.italic.16.text)+theme(strip.text=element_text(size = 10, colour = "black"))
#dev.off()
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






















