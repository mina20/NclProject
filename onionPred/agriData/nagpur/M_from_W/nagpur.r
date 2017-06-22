#Pridection of onion price date range jan-2005-dec-2015
library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
library(dplyr)
df <- read.csv('nag.csv')

##############################################
#Date1=seq.Date(as.Date("2015-12-01"),as.Date("2015-12-31"),1)
#date1 <- data.frame(Date1)
#colnames(date1)<-"Date"
#date1$Date <- as.Date(date1$Date)
#df$Date <- as.Date(df$Date)
#df1<- full_join(df,date1)
#zz <- read.zoo(df)
df_xt <- xts(read.zoo(df))
#df_xt <- xts(zz,order.by=as.Date(df$Date))

#mdf <- df_xt
df_w <- apply.weekly(df_xt,mean)
#################################################
data <- data.frame(date=index(df_w), coredata(df_w))
new_df<- data.frame()
var=12
#v <- c(31,32,33,34,35,40,47)
#for(i in 8:nrow(mdf))
for(i in 17:nrow(data)){
new_data <- cbind(data[i,],data[var,32:38])
for(j in (var-1):(var-11)){
new_data <- cbind(new_data,data[j,32:38])
}
 var=var+1
new_df <- rbind(new_df,new_data)

}
mdf1<- xts(read.zoo(new_df))
########################################################3
#mdf <- apply.monthly(mdf,mean)
#mdf <- apply.monthly(df_xt,mean)
#xx <- mdf[,-2]
#mdf <- na.fill(mdf,0)
#Date1=seq.Date(as.Date("2015-12-01"),as.Date("2015-12-31"),1)
#date1 <- data.frame(Date1)
#colnames(date1)<-"Date"
#date1$Date <- as.Date(date1$Date)
#df$Date <- as.Date(df$Date)
#df1<- full_join(df,date1)
#zz <- read.zoo(df)
xx <- mdf1[,-c(1,2,3)]
#xx <- mdf1[,-c(4,5,6)]
xx1 <- df_w[,-c(4,5,6)]
xx1 <- na.interpolation(xx1, option="linear")
xx <- na.interpolation(xx, option="linear")
y <- mdf1$Modal.Price.orange
#y <- mdf1$Modal.Price.onion
newMax = 100
newMin=0
range02 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (newMax - newMin) + newMin }

#####################################
#Choosing best argo lag
exo <- range02(xx)
exo1 <- range02(xx1)
exo1 <- exo1[-(1:16),]
#mod_df <- range02(mdf)
#exo <- range02(imp_xx)
#max_lag <- 32

#####################################################################
#x <- xts(order.by=index(mdf))
x <- data.frame()
var=4
#v <- c(31,32,33,34,35,40,47)
#for(i in 8:nrow(mdf))
#for(i in 8:10){
#new_data <- cbind(mdf[i,],coredata(mdf[var,31:37]))
#for(j in (var-1):(var-3)){
#new_data <- cbind(new_data,coredata(mdf[j,31:37]))
#}
# var=var+1
#x<- bind_cols(x,new_data)

#}
#exo <- data2[3:nrow(data2),-c(4,5,6)]
#y <- data2$Model.Price.Onion
 
#my_santillana <- argo(y, exogen = exo , alpha = 1, N_lag = 5:20, use_all_previous = TRUE,  N_training = 100)



######################################################################
#our_argo <- argo(log(y/1000),exogen=log(exo+1.5),alpha=1,N_lag=1:24,use_all_previous=FALSE,N_training=48)
#mod_argo <- argo(log(y/1000),exogen=log(exo+1.5),alpha=1,N_lag=5:8,use_all_previous=FALSE,N_training=160)
my_argo <- argo(log(y/1000),exogen=log((exo+1.5)/100),alpha=1,N_lag=5:52,use_all_previous=FALSE,N_training=180)
#mod_argo <- argo(log(y/1000),exogen=log(exo+1.5)/100),alpha=1,N_lag=5,use_all_previous=FALSE,N_training=104)
santillana <- argo(y, exogen = exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
#mod_santillana  <- argo(y, exogen = exo , alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
my_santillana <- argo(y, exogen = exo , alpha = 1, N_lag = 5:12, use_all_previous = TRUE,  N_training = 180)
#my_sani1 <-argo(y, exogen = exo , alpha = 1, N_lag = 5:8, use_all_previous = TRUE,  N_training = 160)
#my_sani <-argo(y, exogen = exo , alpha = 1, N_lag = 5:21, use_all_previous = TRUE,  N_training = 160)
ar1 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=5, N_training = 104)
#ar2 <- argo(y,alph=1, use_all_previous = FALSE, N_lag=5:6, N_training = 160)
naive <-lag(y, 4, na.pad = TRUE) 
#######################################
#mod_argo$pred <- exp(mod_argo$pred)*1000
my_argo$pred <- exp(my_argo$pred)*1000
#lag2_argo$pred <- exp(lag2_argo$pred)*1000
#try_argo <- exp(try_argo$pred)*100
pre <- merge(y,my_argo$pred,santillana$pred,my_santillana$pred,ar1$pred,naive,all=FALSE)
#pre <- merge(y,santillana$pred,my_santillana$pred,ar2$pred,ar1$pred,all=FALSE)
#colnames(pre) <- c('Real','santillana','my_santillana',"mod_AR2",'mod_AR1')
colnames(pre) <- c('Real','my_Argo','santillana','my_santillana','mod_AR1','Naive')
#pre1 <- pre[which(1:nrow(pre),]
#pred <- pred[which(index(pred)=='2012-07-31'):nrow(pred)-1,]
pred <- na.omit(pre)
#########################################
#pdf(paste0("nag_argo.pdf"),width=4, height=10)
#heatmap_argo(as.matrix(our_argo$coef),0.1)
#dev.off()
#pdf("nagMyargo.pdf")
#heatmap_argo(as.matrix(my_argo$coef),0.1)
#dev.off()
#################################
#pred <- pred[which(index(pred)=='2011-11-30'):nrow(pred)-1,]
p1<-autoplot(pred,facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")

p2<-autoplot(pred[,c(1:2)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
p3<-autoplot(pred[,c(1,3)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
p4<-autoplot(pred[,c(1,4)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
p5<-autoplot(pred[,c(1,5)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="predicted Values")
p6<-autoplot(pred[,c(1,6)],facets=FALSE, geom = "line",size=3,xlab='Time',ylab='predicted Values')
#p7<-autoplot(pred[,c(1,7)],facets=FALSE, geom = "line",size=1.5,xlab='Time',ylab='predicted Values')
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
acc <- cbind(md,mdd,myd,cod)
#write.csv('md.csv',file="md")
#########################################
#h1<-heatmap_argo(as.matrix(our_argo$coef),0.1)
#h2 <- heatmap_argo(as.matrix(my_argo$coef),0.1)
q1<- qplot(Model,rmse , colour = Model,data=md,size=I(10))
q2<- qplot(Model,mae , colour = Model,data=mdd,size=I(10))
q3<- qplot(Model,mape , colour = Model,data=myd,size=I(10))
q4<- qplot(Model,corr , colour = Model,data=cod,size=I(10))
#bar plot
#ggplot(data = myd, aes(x = Model, y = mape, fill = Model)) +  geom_bar(stat="identity",width=0.2)









































