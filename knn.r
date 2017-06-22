#KNN algorithm
library(caret)
 data = iris
 k = 5
 tr4 = c()
 cl = c()
 s = c()
 #label = data[1:nrow(data),ncol(data)]
 ind = iris[sample(1:nrow(data)),]
 tr = ind[1:100,]
 ts = ind[101:150,-ncol(data)]
 disMat =as.matrix( dist(tr, method='euclidean', diag=F,upper=F))

 label = ind[1:nrow(data),ncol(data)]  
for(i in 1:nrow(tr)){
 	
 d = as.matrix(disMat)

 s = sort(d[i,])

 s = s[-1][1:k]

 no = as.numeric(names(s))

 labelTr = label[no]

 w1 = length(which(labelTr=="setosa"))

 w2 = length(which(labelTr=="versicolor"))

 w3 = length(which(labelTr=="virginica"))
 w = c(w1,w2,w3)

 maxw= which.max(w)
	if(maxw == 1){
	
		v = "setosa"
	}
 	else if(maxw==2){
	
		v = "versicolor"
	}
	else{
		v = "virginica"
	}
cl = c(cl,v)
		
}
t=as.matrix(table(cl))

tr1=rbind(length(which(tr=="setosa")),length(which(tr=="versicolor")),length(which(tr=="virginica")))
#colnames(tr1)=c("setosa","versicolor","virginica")
cm=cbind(t,tr1)
