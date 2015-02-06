# import data from file, before that, sava as .csv
sourceData <- read.csv(file.choose(),header=F)
# want to delete the comma in data and put it to numeric type
attach(sourceData)
for(i in 4:15) {
  sourceData[,i] <- as.numeric(gsub(",","",sourceData[,i]))
}
