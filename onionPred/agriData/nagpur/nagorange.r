#Pridection of onion price date range jan-2005-dec-2015
library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
df <- read.csv('nag.csv')
df_xt <- xts(read.zoo(df))
mdf <- apply.monthly(df_xt,mean)
#xx <- mdf[,-2]
v <- c(1,2,3)
xx <- mdf[,-v]
y <- mdf$Modal.Price.orange
imp_xx <-  na.interpolation(xx, option="linear")
newMax=100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }
exo <- range02(imp_xx)
max_lag <- 24
our_argo <- list()
for(i in 1:max_lag){
our_argo[i] <- argo(log(y/1000), exogen = log((exo + 1.5)),alpha = 1, N_lag=1:i, use_all_previous = FALSE, N_training = 48)
}
temp1 <- merge(our_argo[[1]],our_argo[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,our_argo[[i]])
}
colnames(temp1)<-c(1:max_lag)
temp1 <- exp(temp1)*1000
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
#####################################

#our_argo <- argo(log(y/1000), exogen = log((exo + 1.5)),alpha = 1, N_lag=1:24, use_all_previous = FALSE, N_training = 48)
#try_argo <-  argo(log(y/100), exogen = log((exo + 1.5)),alpha = 1, N_lag=1:12, use_all_previous=FALSE,N_training = 36)
my_argo <- argo(log(y/1000),exogen=log(exo+1.5),alpha=1,N_lag=4,use_all_previous=FALSE,N_training=48)
santillana <- argo(y, exogen = exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 48)
ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1, N_training = 48)
ar2 <- argo(y,alph=NA, use_all_previous = FALSE, N_lag=1:2, N_training = 48)
naive <- lag(y,k=1)
#######################################
#our_argo$pred <- exp(our_argo$pred)*1000
my_argo$pred <- exp(my_argo$pred)*1000
#try_argo <- exp(try_argo$pred)*100
pred <- merge(y,my_argo$pred,santillana$pred,ar2$pred,ar1$pred,naive,all=FALSE)
colnames(pred) <- c('Real','my_Argo','santillana',"AR2",'AR1','Naive')
#pred <- pred[which(index(pred)=='2012-02-29'):nrow(pred)-1,]
#pred <- pred[which(index(pred)=='2012-02-29'):nrow(pred)-1,]
pred <- na.omit(pred)
#########################################
#pdf(paste0("h_orange_nag.pdf"),width=4, height=10)
#heatmap_argo(as.matrix(our_argo$coef),0.1)
#dev.off()
#pdf("nagMyargo.pdf")
#heatmap_argo(as.matrix(my_argo$coef),0.1)
#dev.off()
#################################
#pred <- pred[which(index(pred)=='2011-11-30'):nrow(pred)-1,]
p1<-autoplot(pred,facets=FALSE, geom = "line",size=1.5,xlab="Time",ylab="Predicted Values")

p2<-autoplot(pred[,c(1,2)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Predicted Values")
p3<-autoplot(pred[,c(1,3)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Predicted Values")
p4<-autoplot(pred[,c(1,4)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Predicted Values")
p5<-autoplot(pred[,c(1,5)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="predicted Values")
p6<-autoplot(pred[,c(1,6)],facets=FALSE, geom = "line",size=1,xlab='Time',ylab='predicted Values')
p6<-autoplot(pred[,c(1,7)],facets=FALSE, geom = "line",size=1,xlab='Time',ylab='predicted Values')
################################################
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
#########################################
#h1<-heatmap_argo(as.matrix(our_argo$coef),0.1)
#h2 <- heatmap_argo(as.matrix(my_argo$coef),0.1)
q1<- qplot(Model,rmse , colour = Model,data=md,size=I(4.0))
q2<- qplot(Model,mae , colour = Model,data=mdd,size=I(4.0))
q3<- qplot(Model,mape , colour = Model,data=myd,size=I(4.0))
q4<- qplot(Model,corr , colour = Model,data=cod,size=I(4.0))











































