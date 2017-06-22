max_lag <- 52
my_santillana <- list()
for(i in 6:(max_lag-1)){
my_santillana[i] <- argo(y, exogen = sc_exo ,alpha = 1, N_lag=5:i, use_all_previous = FALSE, N_training = 160)
#i=i+1
}
temp1 <- merge(my_santillana[[1]],my_santillana[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,my_santillana[[i]])
}
colnames(temp1)<-c(1:max_lag)
temp1 <- exp(temp1)*mx
temp2 <- cbind(y,temp1)
temp3 <- na.omit(temp2)
santi_rmse <- c()
n <- ncol(temp3)
for(i in 1:n){
rme <- sqrt (mean(  (temp3[,i]-temp3[,1])^2)  )
santi_rmse <- c(santi_rmse,rme)
}
#colnames(temp3) <- c(1:25)
santi_rmse <- santi_rmse[-1]
Lag_santi <- colnames(temp3)[-1]
santi <- data.frame(Lag_santi,santi_rmse)
qplot(Lag_santi,santi_rmse , colour = Lag_santi,data=santi,size=I(6.0))
dev.off()
####################################################################################
lag for nagpure data.
our_argo <- list()
max_lag <- 24
our_argo <- list()
my_santillana <- list()
for(i in 6:max_lag){
our_argo[i] <- argo(log(y/1000), exogen = log((exo + 1.5)),alpha = 1, N_lag=5:i, use_all_previous = FALSE, N_training = 160)
my_santillana[i] <- argo(y, exogen = exo , alpha = 1, N_lag = 5:i, use_all_previous = TRUE,  N_training = 160)
}
temp_1<- merge(my_santillana[[1]],my_santillana[[2]])
temp1 <- merge(our_argo[[1]],our_argo[[2]])
for(j in 3:max_lag){
	 temp1 <- cbind(temp1,our_argo[[i]])
	temp_1 <- cbind(temp_1,my_santillana[[i]])	
}
colnames(temp1)<-c(1:max_lag)
colnames(temp_1)<- c(1:max_lag)
temp1 <- exp(temp1)*1000
temp2 <- cbind(y,temp1)
temp_2 <- cbind(y,temp_1)
temp3 <- na.omit(temp2)
temp_3 <- na.omit(temp_2)
argo_rmse <- c()
santi_rmse <- c()
n <- ncol(temp3)
for(i in 1:n){
rme <- sqrt (mean(  (temp3[,i]-temp3[,1])^2)  )
rme1 <- sqrt(mean((temp_3[,i]-temp_3[,1])^2) )
argo_rmse <- c(argo_rmse,rme)
santi_rsme <- c(santi_rmse,rme1)
}
argo_rmse <- argo_rmse[-1]
santi_rmse <- santi_rmse[,-1]
Lag_argo <- colnames(temp3)[-1]
Argo <- data.frame(Lag_argo,argo_rmse)
Lag_santillana <- colnames(temp_3)[-1]
santi <- data.frame(Lag_santillana,santi_rmse)
pdf('nag_argo.pdf')
qplot(Argo,argo_rmse , colour = Lag_argo,data=Argo,size=I(4.0))
dev.off()
