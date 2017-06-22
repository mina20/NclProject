###################
#onion price for nagpur region using weather data
#####################
library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
###########################
df1 <- read.csv('nagpur2.csv')
df2<- read.csv("onion.csv")
df3 <- read.csv('beetroot.csv')
df0 <- read.csv('orange.csv')
df4 <- read.csv('banana.csv')
df5 <- read.csv('apple.csv')
df6 <- read.csv('potato.csv')
df7 <- read.csv('redchili.csv')
df8 <- read.csv('soyabean.csv')
#################################
df1$MAX <- df1$MAX+273
df1$MI  <- df1$MIN+273
df1$..DBT <- df1$..DBT+273
df1$..WBT <- df1$..WBT+273
df1$DPT <- df1$..DPT+273
df1$DR. <- df1$DR.*3600
df1$SSH <- df1$SSH*3600
df1$EVP <- df1$EVP*1000
df1$R.F <- df1$R.F*1000
df1$.RH <- df1$.RH*100
df1$..VP <- df1$..VP*100
df1$FFF <- df1$FFF*1000
##############################
df0$Date <- as.Date(df0$Price.Date)
df2$Date <- as.Date(df2$Price.Date)
df3$Date <- as.Date(df3$Price.Date)
df4$Date <- as.Date(df4$Price.Date)
df5$Date <- as.Date(df5$Price.Date)
df6$Date <- as.Date(df6$Price.Date)
df7$Date <- as.Date(df7$Price.Date)
df8$Date <- as.Date(df8$Price.Date)
df1$Date<- as.Date(df1$date)
Date=seq.Date(as.Date("2005-01-01"),as.Date("2015-11-30"),1)
Date<- data.frame(Date)
colnames(Date) <- "Date"
dff0 <- merge(Date,df0,by='Date',all=TRUE)
dff1 <- merge(Date,df1,by="Date",all=TRUE)
dff2 <- merge(Date,df2,by="Date",all=TRUE)
dff3 <- merge(Date,df3,by="Date",all=TRUE)
dff4 <- merge(Date,df4,by="Date",all=TRUE)
dff5 <- merge(Date,df5,by="Date",all=TRUE)
dff6 <- merge(Date,df6,by="Date",all=TRUE)
dff7 <- merge(Date,df7,by="Date",all=TRUE)
dff8 <- merge(Date,df8,by="Date",all=TRUE)
###########################################
xts0<- xts(dff0,order.by=dff0$Date)
xts1<- xts(dff1,order.by=dff1$Date)
xts2<- xts(dff2,order.by=dff2$Date)
xts3<- xts(dff3,order.by=dff3$Date)
xts4<- xts(dff4,order.by=dff4$Date)
xts5<- xts(dff5,order.by=dff5$Date)
xts6<- xts(dff6,order.by=dff6$Date)
xts7<- xts(dff7,order.by=dff7$Date)
xts8<- xts(dff8,order.by=dff8$Date)
#########################################
d0 <- apply.daily(xts0, mean)
d1 <- apply.daily(xts1, mean)
d2 <- apply.daily(xts2, mean)
d3 <- apply.daily(xts3, mean)
d4 <- apply.daily(xts4, mean)
d5 <- apply.daily(xts5, mean)
d6 <- apply.daily(xts6, mean)
d7 <- apply.daily(xts7, mean)
d8 <- apply.daily(xts8, mean)
var <- c(1,5)
d1 <- d1[,-(1:2)]
d0 <- d0[,-var]
d2 <- d2[,-var]
d3 <- d3[,-var]
d4 <- d4[,-var]
d5 <- d5[,-var]
d6 <- d6[,-var]
d7 <- d7[,-var]
d8 <- d8[,-var]
#########################################
i0 <- na.ma(d0, k=1,weighting = "exponential")
i1 <- na.ma(d1, k=3,weighting = "exponential")
i2 <- na.ma(d2, k = 3, weighting = "exponential")
i3 <- na.ma(d3, k = 3, weighting = "exponential")
i4 <- na.ma(d4, k = 1, weighting = "exponential")
i5 <- na.ma(d5, k = 1, weighting = "exponential")
i6 <- na.ma(d6, k = 3, weighting = "exponential")
i7 <- na.ma(d7, k = 1, weighting = "exponential")
i8 <- na.ma(d8, k = 1, weighting = "exponential")
ind <- which(index(i1)=='2015-11-30')
i11 <- i1[1:ind,]
####################################
dat <- cbind(i0,i2,i3,i4,i5,i6,i7,i8,i11)
pp <- read.csv('ppn.csv')
pp1=seq.Date(as.Date("2002-06-04"),as.Date("2017-04-16"),1)
pp1 <- data.frame(pp1)
pp$Date <- as.Date(pp$Month)
pp <- pp[,-1]
colnames(pp1)<-c('Date')
df_pp <- merge(pp,pp1,by="Date",all=TRUE)
ind1 <- which(df_pp$Date=='2005-01-01')
ind2 <- which(df_pp$Date=='2015-11-30')
df_pp <- df_pp[ind1:ind2,]
df_pp <- na.locf(df_pp,option="locf",na.remaining="rev")
df_pp$Date <- as.Date(df_pp$Date)

df_z <- read.zoo(df_pp)
df_xt <- xts(df_z)
df_xt <- apply.daily(df_xt,mean)
data <- merge(dat,df_xt)
write.zoo(data,file='nag_data.csv',index=TRUE)
write.csv(data.frame(data),file="nag_d.csv")



























































