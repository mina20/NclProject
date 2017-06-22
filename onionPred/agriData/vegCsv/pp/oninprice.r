#Another effecient ways

Price.Date=seq.Date(as.Date("2012/01/08"),as.Date("2017/04/09"),1)
date_data <- data.frame(Price.Date)
data <- data.frame(data)
change_class <- function(data){
	data$Price.Date <- as.Date(data$Price.Date)
	}
files <- list.files(path=""/home/gadgil/samina/onionPred/agriData/vegCsv",pattern="*.csv",full.names=F)
for(i in 1:length(files)){ 
	oname = paste("file_", i, sep="")
	assign(oname, read.csv(paste(oname, sep="")))
	#print(temp[i])
	#change_class(temp[i][1])
	temp_1 <- read.csv(temp[i][1])
	temp_1$Price.Date <- as.Date(temp_1$Price.Date)
	df_temp_past(str(i)) <-  merge(temp_1,date_data,by="Price.Date",all=TRUE)
	#xts_temp[i][1] <- xts(df_temp[i][1],order.by=df$Price.Date)
	}

for(file in temp){
	sample <- read.csv(file,header=T,sep=' ')
}


