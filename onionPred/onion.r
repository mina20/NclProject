:w##application of my_project to agricultural product

#loading data

dataOnion <- read.csv("/home/gadgil/samina/onionPred/onion.csv",sep=" ",header=T)
data1 <- which(dataOnion$centre=='MUMBAI')
data <- dataOnion[data1,]

#write.csv(data2,file="muprice.csv")
#data conversion to xts
data <- read.csv("muprice.csv")
data$week <- as.Date(as.character(data$week),format="%d-%m-%Y")
zoo.data <- read.zoo(data)
xts.data <- xts(zoo.data)
pdf("price.pdf")
plot(x = zoo.data, ylab = "Rs./qtl", main = "Onion Price",col = "darkgreen", screens = 1)
dev.off()
