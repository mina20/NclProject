#onion prediction from argo using weather and other prices
library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
data_ca <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/cabbage.csv")
data_cu<- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/cucumber.csv")
data_car<- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/carrot.csv")
data_l<- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/ladiesfing.csv")
data_b <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/bitter.csv")
data_o <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/onion.csv")
data_pu <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/punetem.csv")
data_po <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/potato.csv")
data_j <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/jawar.csv")
data_to <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/tomato.csv")
data_ter <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/termaric.csv")
data_gin <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/ginger.csv")
data_bee <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/beetroot.csv")
data_pp <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/pp.csv")
data_pu1 <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/pu1.csv")
data_ca$Date <- as.Date(data_ca$Date)
data_po$Date <- as.Date(data_po$Date)
data_car$Date <- as.Date(data_car$Date)
data_cu$Date <- as.Date(data_cu$Date)
data_l$Date <- as.Date(data_l$Date)
data_b$Date <- as.Date(data_b$Date)
data_o$Date <- as.Date(data_o$Date)
data_j$Date <- as.Date(data_j$Date)
data_to$Date <- as.Date(data_to$Date)
data_ter$Date <- as.Date(data_ter$Date)
data_gin$Date <- as.Date(data_gin$Date)
data_bee$Date <- as.Date(data_bee$Date)
data_pp$Date <- as.Date(data_pp$Date)
data_pu$Date <- as.Date(data_pu$Date)
Date=seq.Date(as.Date("2006-03-01"),as.Date("2015-11-30"),1)
date_data <- data.frame(Date)
onion_date=seq.Date(as.Date("2006-03-01"),as.Date("2015-12-06"),1)
onion_date <- data.frame(onion_date)
colnames(onion_date)<-"Date"
pp=seq.Date(as.Date("2002-06-04"),as.Date("2017-04-16"),1)
pp <- data.frame(pp)
 colnames(pp)<-"Date"
df_pp <- merge(data_pp,pp,by="Date",all=TRUE)
df_b <- merge(data_b,date_data,by="Date",all=TRUE)
df_po <- merge(data_po,date_data,by="Date",all=TRUE)
df_car <- merge(data_car,date_data,by="Date",all=TRUE)
df_ca <- merge(data_ca,date_data,by="Date",all=TRUE)
df_cu <- merge(data_cu,date_data,by="Date",all=TRUE)
df_l <- merge(data_l,date_data,by="Date",all=TRUE)
df_o <- merge(data_o,onion_date,by="Date",all=TRUE)
df_pu <- merge(data_pu,date_data,by="Date",all=TRUE)
df_j <- merge(data_j,date_data,by="Date",all=TRUE)
df_to <- merge(data_to,date_data,by="Date",all=TRUE)
df_ter <- merge(data_ter,date_data,by="Date",all=TRUE)
df_gin <- merge(data_gin,date_data,by="Date",all=TRUE)
df_bee <- merge(data_bee,date_data,by="Date",all=TRUE)
ind_cu <- which(df_cu$Date=='2006-03-01')
ind_o <- which(df_o$Date=='2006-03-01')
ind_ca <- which(df_ca$Date=='2006-03-01')
ind_pu <- which(df_pu$Date=='2006-03-01')
df_ca <- df_ca[ind_ca:nrow(df_ca),]
df_cu <- df_cu[ind_cu:nrow(df_cu),]
df_pu <- df_pu[ind_pu:nrow(df_pu),]
df_o <- df_o[ind_o:nrow(df_o),]
df_pp <- na.locf(df_pp,option="locf",na.remaining="rev")

#######################################
xts_b <- xts(df_b,order.by=df_b$Date)
xts_ca <- xts(df_ca,order.by=df_ca$Date)
xts_cu <- xts(df_cu,order.by=df_cu$Date)
xts_pu <- xts(df_pu,order.by=df_pu$Date)
xts_o <- xts(df_o,order.by=df_o$Date)
xts_l <- xts(df_l,order.by=df_l$Date)
xts_po <- xts(df_po,order.by=df_po$Date)
xts_car <- xts(df_car,order.by=df_car$Date)
xts_j <- xts(df_j,order.by=df_j$Date)
xts_to <- xts(df_to,order.by=df_to$Date)
xts_ter <- xts(df_ter,order.by=df_ter$Date)
xts_gin <- xts(df_gin,order.by=df_gin$Date)
xts_bee <- xts(df_bee,order.by=df_bee$Date)
xts_pp <- xts(df_pp,order.by = df_pp$Date)
##################################
dff_b <- apply.daily(xts_b, mean)
dff_ca <- apply.daily(xts_ca, mean)
dff_cu <- apply.daily(xts_cu, mean)
dff_pu <- apply.daily(xts_pu, mean)
dff_o <- apply.daily(xts_o, mean)
dff_l <- apply.daily(xts_l, mean)
dff_po <- apply.daily(xts_po, mean)
dff_car <- apply.daily(xts_car, mean)
dff_j <- apply.daily(xts_j, mean)
dff_to <- apply.daily(xts_to, mean)
dff_ter <- apply.daily(xts_ter, mean)
dff_gin <- apply.daily(xts_gin, mean)
dff_bee <- apply.daily(xts_bee, mean)
dff_pp <- apply.daily(xts_pp, mean)
############################
ind1 <- which(index(dff_o)=="2015-12-01")
ind2 <- which(index(dff_o)=="2015-12-06")
temp <- dff_o[ind1:ind2,]
dff_o <- dff_o[-(ind1:ind2),]
st <- which(index(dff_pp)=="2006-03-01")
end <- which(index(dff_pp)=="2015-11-30")
imp_pp <- dff_pp[st:end,-1]
#
imp_b <- na.ma(dff_b[,-1], k = 2, weighting = "exponential")
imp_ca <- na.interpolation(dff_ca[,-1], option="linear")
imp_cu <- na.ma(dff_cu[,-1], k = 2, weighting = "exponential")
imp_car <- na.ma(dff_car[,-1], k = 2, weighting = "exponential")
imp_po <- na.ma(dff_po[,-1], k = 2, weighting = "exponential")
imp_pu <- na.ma(dff_pu[,-1], k = 2, weighting = "exponential")
imp_o <- na.ma(dff_o[,-1], k = 2, weighting = "exponential")
imp_l <- na.ma(dff_l[,-1], k = 2, weighting = "exponential")
imp_j <- na.locf(dff_j[,-1],option="locf",na.remaining="rev")
imp_to <- na.ma(dff_to[,-1], k = 2, weighting = "exponential")
imp_bee <- na.ma(dff_bee[,-1], k = 2, weighting = "exponential")
imp_ter <- na.locf(dff_ter[,-1],option="locf",na.remaining="rev")
imp_gin <- na.locf(dff_gin[,-1],option="locf",na.remaining="rev")
imp_pp <- na.locf(imp_pp[,-1],option="locf",na.remaining="rev")

data <- cbind(imp_b,imp_cu,imp_ca,imp_l,imp_po,imp_car,imp_j,imp_to,imp_bee,imp_ter,imp_gin,imp_pu)
data <- merge(data,imp_pp)
######################################
imp_o <- rbind(imp_o,temp[,-1])
data_w <- apply.weekly(data,mean)
ydata_merged <- merge(data,imp_o,join="right")
ywdata_merged <- apply.weekly(ydata_merged,mean)
#yfill <- na.fill(ywdata_merged,fill=0)
x <- ywdata_merged[,-ncol(ywdata_merged)]
y <- ywdata_merged$Modal.Price.onion
exogen <- na.interpolation(x, option="linear")
#scale_data <- scale(exogen)
#scale_y <- scale(y)
#new_data <- scale(exogen, center = TRUE, scale = TRUE)
#new_y <- scale(y,center = TRUE, scale=TRUE)
#############################

our_argo <- argo(log(y), exogen = log(exogen+0.5),alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
santillana <- argo(y, exogen = exogen, alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
ar3 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:3, N_training = 104)
naive <- lag(y,k=1)
###################################
#our_argo$pred <- logit_inv(our_argo$pred)*max(y)
pred <- merge(y,naive,our_argo$pred,santillana$pred,ar3$pred,all=FALSE)
st <- which(index(pred)=='2010-08-15')
pre <- pred[st:nrow(pred)-1,]
pred <- pred[st:nrow(pred)-1,]
########################################33
#Heat maps
#pdf(paste0("heatmapargo.pdf"),width=4, height=10)
#heatmap_argo(as.matrix(our_argo$coef),0.1)
#dev.off()
#pdf(paste0("heatmapsanti.pdf"),width=4, height=10)
#heatmap_argo(as.matrix(santillana$coef),0.1)
#dev.off()
#pdf(paste0("heatmapar3.pdf"),width=4, height=10)
#heatmap_argo(as.matrix(ar3$coef),0.1)
#dev.off()

#######################################################
#col <- c('Onion-Price','Naive','Argo','Santillana','Ar3')
#colnames(pred) <- col
#pdf("all_pred.pdf")
#autoplot(pre,facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
#dev.off()
#pdf("onion_argo.pdf")
#autoplot(pre[,c(1,3)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
#dev.off()
#pdf("onio-santi.pdf")
#autoplot(pre[,c(1,4)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
#dev.off()

#pdf("onio-ar3.pdf")
#autoplot(pre[,c(1,5)],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
#dev.off()

####################################################
rmse <- c()
mae <-c()
n <- ncol(pred)
for(i in 1:n){
        rme <- sqrt(mean((pred[,i]-pred[,1])^2))
        ma <- mean(abs(pred[,i]-pred[,1]))
        rmse <- c(rmse,rme)
        mae <- c(mae,ma)
        }
#Plot rmse and mae
Dr <- max(pred[,1]-min(pred[,1]))
rmse <- rmse[-1]
mae <- mae[-1]
Model <- colnames(pred)[-1]
md <- data.frame(Model,rmse)
mdd <- data.frame(Model,mae)
pdf("rmse.pdf")
qplot(Model,rmse , colour = Model,data=md,size=I(2.5))
dev.off()
pdf("mae.pdf")
qplot(Model,mae, colour = Model,data=mdd,size=I(2.5))
dev.off()



























































