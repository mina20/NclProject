library(zoo)
library(xts)
library(ggplot2)
library(ggfortify)
library(imputeTS)
library(argo)
data_pu <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/punetem.csv")
data_o <-read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/onion.csv")
data_pp <- read.csv("/home/gadgil/samina/onionPred/agriData/data/inst/extdata/pp.csv")
data_o$Date <- as.Date(data_o$Date)
data_pp$Date <- as.Date(data_pp$Date)
data_pu$Date <- as.Date(data_pu$Date)
onion_date=seq.Date(as.Date("2005-01-01"),as.Date("2015-12-06"),1)
onion_date <- data.frame(onion_date)
colnames(onion_date)<-"Date"
pp=seq.Date(as.Date("2002-06-04"),as.Date("2017-04-16"),1)
pp <- data.frame(pp)
colnames(pp)<-"Date"
df_o <- merge(data_o,onion_date,by="Date",all=TRUE)
temp_o <- na.locf(df_o[1:4,],option="locf",na.remaining="rev")
dff_o <- rbind(temp_o,df_o[-(1:4),])
df_pu <- merge(data_pu,onion_date,by="Date",all=TRUE)
df_pp <- merge(data_pp,pp,by="Date",all=TRUE)
xts_pu <- xts(df_pu,order.by=df_pu$Date)
xts_o <- xts(df_o,order.by=df_o$Date)
xts_pp <- xts(df_pp,order.by = df_pp$Date)
dff_pu <- apply.daily(xts_pu, mean)
dff_o <- apply.daily(xts_o, mean)
dff_pp <- apply.daily(xts_pp, mean)
ind1 <- which(index(dff_o)=="2015-12-01")
ind2 <- which(index(dff_o)=="2015-12-06")
temp <- dff_o[ind1:ind2,]
dff_o <- dff_o[-(ind1:ind2),]
#imp_pp <- dff_pp[st:end,-1]
st <- which(index(dff_pp)=="2005-01-01")
end <- which(index(dff_pp)=="2015-11-30")
imp_pp <- dff_pp[st:end,-1]
imp_pp <- na.locf(imp_pp,option="locf",na.remaining="rev")
imp_pu <- na.interpolation(dff_pu[,-1], option="linear")
data <- cbind(imp_pu,imp_pp)
temp <- na.locf(temp[,-1],option="locf",na.remaining="rev")
imp_o <- rbind(dff_o[,-1],temp[,-1])
ydata_merged <- merge(data,imp_o,join="right")
ywdata_merged <- apply.weekly(ydata_merged,mean)
exogen <- ywdata_merged[,-ncol(ywdata_merged)]
y <- ywdata_merged$Modal.Price.onion
exogen <- na.locf(exogen,option="locf",na.remaining="rev")


























