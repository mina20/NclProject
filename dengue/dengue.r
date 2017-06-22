###

library("zoo")
library("xts")
library("argo")
library("ggplot2")
library(ggfortify)
data <- read.csv('/home/gadgil/samina/dengue/inst/extdata/dengue.csv')
data.zoo <- read.zoo(data)
df <- xts(data.zoo)
exogen <- df[,-4]
#y <- df$dengue
y <- df$dengue.fever
y <- y/100
y[nrow(y),]<- NA
#########################3

santillana <- argo(y, exogen = exogen,alpha = 1, N_lag = NULL, use_all_previous = TRUE,  N_training = 104)
our_argo <- argo(log(y/100),exogen = log((exogen + 0.5) / 100),alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
ar2 <- argo(y, alpha = NA, use_all_previous = FALSE, N_lag=1:2,N_training = 104)
naive <- lag(y,k=1)
#sim_argo <- argo(y,exogen = exogen ,alpha = 1, N_lag=1:52, use_all_previous = FALSE, N_training = 104)
my_argo <- argo(log(y/100),exogen = log((exogen + 0.5) / 100),alpha = 1, N_lag=1:2, use_all_previous = FALSE, N_training = 104)
our_argo$pred <- exp(our_argo$pred)*100
my_argo$pred <- exp(my_argo$pred)*100


#########################3
pred <- merge(y,our_argo$pred,my_argo$pred,santillana$pred,ar2$pred,naive,all=FALSE)
colnames(pred)<- c('Real','Argo','my_argo','santillana','AR2','Naive')
st <- which(index(pred)=='2015-11-22')
pred <- pred[st:nrow(pred)-1,]
####################################
#Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#######################################
#
#multiplot(h1,h2, cols=2)

#dev.off()
####################################3
#plots
#pdf('all.pdf')
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
p5<-autoplot(pred[,c(1,5)],facets=FALSE, geom = "line",size=3,xlab="Time",ylab="Predicted Values")
#dev.off()
#pdf('real_ar2')
p6<-autoplot(pred[,c(1,6)],facets=FALSE, geom = "line",size=3,xlab='Time',ylab='Predicted Values')
#p7 <- autoplot(pred[,c(1,7)],facets=FALSE,geom='line',size=3,xlab='Time',ylab='Predicted Values')
#dev.off()
pdf(paste0("deng3.2.pdf"),width=20, height=25)
multiplot(p1, p2, p3, p4,p5,p6,cols=1)
dev.off()
#pdf('report_pl2.pdf')
#multiplot(p1, p2, p3, p4,p5,p6, cols=2)
#dev.off()
##############################



#######################################33
#error <- function(pred){
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
#pdf("rmse.pdf")
q1<-qplot(Model,rmse , colour = Model,data=md ,size=I(10))
#dev.off()
#pdf("mae.pdf")
q2<-qplot(Model,mae, colour = Model,data=mdd,size=I(10))
#dev.off()
#pdf('mape.pdf')
q3<-qplot(Model,mape, colour = Model,data=myd,size=I(4.0))
#dev.off()
#pdf('corr')
q4 <- qplot(Model,corr, colour = Model,data=cod,size=I(4.0))
#dev.off()
#pdf('Acc.pdf')
#multiplot(q1,q2,q3, cols=3)
#dev.off()
##############################



























