# this is the final script for dengue fever using google trend as data#
#next step is to add correlated data and some irrelevent terms into it#
                                                                      #
#######################################################################


library("zoo")
library("xts")
library("argo")
library("ggplot2")
#loading the data
dataCD <- read.csv("/home/gadgil/samina/argoDengo/inst/extdata/CD.csv")
dataGT <- read.csv("/home/gadgil/samina/argoDengo/inst/extdata/GT.csv")
#convert data into xts:zoo class
CD <- read.zoo(dataCD)
GT <- read.zoo(dataGT)
CD <- as.xts(CD)
GT <- as.xts(GT)
#seperate y
#y <- GT$dengue
y <- GT$dengue
exogen <- GT[,-1]
#fitting models

senetella <- argo(y, exogen = exogen,alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104
)
ar3 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:3,N_training = 104)
#use of logit is making problem here
our_argo <- argo(logit(y/100),exogen = log((exogen + 0.5) / 100),alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
#plotting prediction and heat map

pred_xts_blend <- merge(our_argo$pred,senetella$pred,ar3$pred,all=FALSE)
names(pred_xts_blend) <- c("ARGO","Santillana", "AR3")
model_names <- c("ARGO","Santillana", "AR3")
model_names <- c("ARGO","Santillana", "AR3")
legend_names <- c("ARGO","Santillana et al. (2014)","AR(3)")
pred_xts_blend <- pred_xts_blend["2015-03-23/"]
zoom_periods <- c("2015-03-23/2015-05-04","2015-05-25/2015-07-06","2016-10-03/2017-02-19")
GC_GT_cut_date <- tail(zoo::index(our_argo$pred),1)+1
#Plot of prediction
start_period <- which(dataGT$Week=='2015-03-29')
end_period <- which(data$Week=='2017-02-12')
actual <- data[start_period:end_period,]
actual <- data[,2]
write.csv(pred$dengue,file="actual.csv")
actual <- read.csv("actual.csv")
write.csv(pred_xts_blend,file="pred.csv")
pred <- read.csv("pred.csv")
pred <- pred[-100,]
zoo.data <- read.zoo(pred)
errorData <- cbind(actual,pred$ARGO,pred$Santillana,pred$AR3)
myColors <- c("red", "darkgreen", "blue", "violet")
png("reDe.png")
plot(x = zoo.data, ylab = "prediction", main = "Prediction",col = myColors, screens = 1)
legend(x = "topleft", legend = c("GT","ARGO", "Santillana", "AR3"),lty = 1,col = myColors)
 dev.off()
maeArgo <- (mean(abs(errorData[,1]-errorData[,2])))
maeSan <- (mean(abs(errorData[,1]-errorData[,3])))
maeAR3 <- (mean(abs(errorData[,1]-errorData[,4])))




