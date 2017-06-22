##Data cleaning

#Load data and fill missing valus
library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
#files = list.files(pattern="*.csv")
#myfiles =lapply(files,read.csv, stringsAsFactors = FALSE)
#names(files) <- gsub(".csv","",list.files("agriData",full.names = FALSE),fixed = TRUE)
data_b <- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/brinjal.csv")
data_p <- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/potato.csv")
data_t<- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/tomato.csv")
data_c<- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/cucumber.csv")
data_l<- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/ladiesfing.csv")
#data_ch<- read.csv("redchilli.csv")
data_ca<- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/carrot.csv")
data_o<- read.csv("/home/gadgil/samina/onionPred/agriData/vegCsv/inst/extdata/onion.csv")
#convert price.Date into Date class
data_b$Price.Date <- as.Date(data_b$Price.Date)
data_p$Price.Date <- as.Date(data_p$Price.Date)
data_t$Price.Date <- as.Date(data_t$Price.Date)
data_c$Price.Date <- as.Date(data_c$Price.Date)
data_l$Price.Date <- as.Date(data_l$Price.Date)
#data_ch$Price.Date <- as.Date(data_ch$Price.Date)
data_ca$Price.Date <- as.Date(data_ca$Price.Date)
data_o$Price.Date <- as.Date(data_o$Price.Date)
#generate date file having all dates
Price.Date=seq.Date(as.Date("2012/01/08"),as.Date("2017/04/09"),1)
date_data <- data.frame(Price.Date)
onion_date=seq.Date(as.Date("2012/01/08"),as.Date("2017/04/16"),1)
onion_date <- data.frame(onion_date)
colnames(onion_date)<-"Price.Date"
#merge all files with date to missing dates with na
df_b <- merge(data_b,date_data,by="Price.Date",all=TRUE)
df_p <- merge(data_p,date_data,by="Price.Date",all=TRUE)
df_c <- merge(data_c,date_data,by="Price.Date",all=TRUE)
df_l <- merge(data_l,date_data,by="Price.Date",all=TRUE)
#df_ch <- merge(data_ch,date_data,by="Price.Date",all=TRUE)
df_ca <- merge(data_ca,date_data,by="Price.Date",all=TRUE)
df_t <- merge(data_t,date_data,by="Price.Date",all=TRUE)
df_o <- merge(data_o,onion_date,by="Price.Date",all=TRUE)
#Convert into xts object
xts_b <- xts(df_b,order.by=df_b$Price.Date)
xts_p <- xts(df_p,order.by=df_p$Price.Date)
xts_c <- xts(df_c,order.by=df_c$Price.Date)
xts_l<- xts(df_l,order.by=df_l$Price.Date)
#xts_ch<- xts(df_ch,order.by=df_ch$Price.Date)
xts_ca<- xts(df_ca,order.by=df_ca$Price.Date)
xts_t<- xts(df_t,order.by=df_t$Price.Date)
xts_o<- xts(df_o,order.by=df_o$Price.Date)
#dfX <- xts(df,order.by=as.Date(df$Price.Date))
dff_b <- apply.daily(xts_b, mean)
dff_p <- apply.daily(xts_p, mean)
dff_c <- apply.daily(xts_c, mean)
dff_l <- apply.daily(xts_l, mean)
#dff_ch <- apply.daily(xts_ch, mean)
dff_ca <- apply.daily(xts_ca, mean)
dff_t <- apply.daily(xts_t, mean)
dff_o <- apply.daily(xts_o, mean)
ind1 <- which(index(dff_o)=="2017-04-11")
ind2 <- which(index(dff_o)=="2017-04-16")
temp <- dff_o[ind1:ind2,]
dff_o <- dff_o[-(ind1:ind2),]
#data <- cbind(imp_o,imp_b,imp_p,imp_c,imp_l,imp_ca,imp_ch,imp_t)
#impute missing values with moving weighted avarage 
imp_b <- na.ma(dff_b[,-1], k = 3, weighting = "exponential")
imp_p <- na.ma(dff_p[,-1], k = 1, weighting = "exponential")
imp_c <- na.ma(dff_c[,-1], k = 3, weighting = "exponential")
imp_l <- na.ma(dff_l[,-1], k = 3, weighting = "exponential")
#imp_ch <- na.interpolation(dff_ch[,-1], option="spline")
imp_ca <- na.ma(dff_ca[,-1], k = 1, weighting = "exponential")
imp_t <- na.ma(dff_t[,-1], k = 3, weighting = "exponential")
imp_o <- na.ma(dff_o[,-1], k = 3, weighting = "exponential")
imp_cu <- na.interpolation(imp_c, option="spline")
#check if there any na valus
#apply(imp_l, 2, function(x) any(is.na(x)))
#combine all data to one data set
data <- cbind(imp_b,imp_p,imp_cu,imp_l,imp_ca,imp_t)
data <- na.interpolation(data,option="spline")
imp_o <- rbind(imp_o,temp[,-1])
#:data1 <- rbind(data,temp[,-1])
data_w <- apply.weekly(data,mean)
ydata_merged <- merge(data,imp_o,join="right")
#yydata_merged <- na.interpolation(ydata_merged, option="spline")
ywdata_merged <- apply.weekly(ydata_merged,mean)
#ind <- ywdata_merged[276,]
yfill <- na.fill(ywdata_merged,fill=0)
x <- yfill[,-(19:21)]
y <- yfill$Modal.PriceOnion
#############################################################################################
#write.csv(data.frame(data),file="data_d")
#write.csv(data.frame(data_w),file="data_w")
#plotting daily data
#pdf("Lf.pdf")
#ts.plot(imp_l,gpars= list(col=rainbow(10)))
#dev.off()
#pdf("Onio.pdf")
#ts.plot(imp_o,gpars= list(col=rainbow(15)))
#dev.off()
#plotting weekly data 
#w_l <- apply.weekly(imp_l,mean)
#pdf("wl.pdf")
#ts.plot(w_l,gpars= list(col=rainbow(20)))
#dev.off()
############################################################################################
#fitting models to data
exogen <- x
santillana <- argo(y, exogen = exogen, alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
our_argo <- argo(log(y / 100), exogen = log((exogen + 0.5) / 100),alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
simple_argo <- argo(y , exogen = exogen,alpha = 1, N_lag=1:52,use_all_previous = FALSE, N_training = 104)
my_argo <- argo(y,exogen=exogen,alpha=1,N_lag=1:3,use_all_previous = FALSE,N_training = 104)
ar3 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:3, N_training = 104)
########################################################################################
#plotting prediction and result
our_argo$pred <- exp(our_argo$pred)*100
naive <- lag(y,k=1)
pre <- merge(y,naive,our_argo$pred,simple_argo$pred,my_argo$pred,santillana$pred,ar3$pred,all=FALSE)
names(pre) <- c("Real","Naive","ARGO","SiARGO","MyARGO","Santillana", "AR3")
start <- which(index(pre)=="2015-01-11")
end <- which(index(pre)=="2017-04-16")
pred <- pre[start:end]
#is.na(pred$Real) <- pred$Real == 0
#pred$Real <- na.locf(pred$Real)
pdf("new.pdf")
autoplot(pred,facet=FALSE, geom = "line",size=1,xlab="Time",ylab="Predicted Price")
dev.off()
pdf("ra.pdf")
autoplot(pred[,1:2],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
var <- c(1,3)
var1 <- c(1,4)
var2 <- c(1,5)
var3 <- c(1,6)
var4 <- c(1,7)
pdf("rs.pdf")
autoplot(pred[,var],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
pdf("rm.pdf")
autoplot(pred[,var1],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
pdf("rsa.pdf")
autoplot(pred[,var2],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
pdf("rar3.pdf")
autoplot(pred[,var3],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
pdf("aa.pdf")
autoplot(pred[,var4],facets=FALSE, geom = "line",size=1,xlab="Time",ylab="Rs/Quintal")
dev.off()
####################################################################
#heat maps
pdf(paste0("heatmapSiargo.pdf"),width=4, height=10)
heatmap_argo(as.matrix(simple_argo$coef),0.1)
dev.off()
pdf(paste0("heatmapargo.pdf"),width=4, height=10)
heatmap_argo(as.matrix(our_argo$coef),0.1)
dev.off()
pdf(paste0("heatmapmyargo.pdf"),width=4, height=10)
heatmap_argo(as.matrix(my_argo$coef),0.1)
dev.off()
pdf(paste0("heatmapSnti.pdf"),width=4, height=10)
heatmap_argo(as.matrix(santillana$coef),0.1)
dev.off()
#####################################################################
#Model complexity
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






















