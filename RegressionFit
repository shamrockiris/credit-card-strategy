# having 2 for loops because there's no data for December 2014, discuss it differently.
s <- read.csv(file.choose(),header=T)
fo <- rep(0,12)
for(i in 3:12){
  pa <- lm(s[-1,i]~s[-1,2])
  fo[i-2] <- coef(pa)[1]+coef(pa)[2]*2015
}
for(i in 13:14){
  pa <- lm(s[-1:-2,i]~s[-1:-2,2])
  fo[i-2] <- coef(pa)[1]+coef(pa)[2]*2015
}
temp <- read.csv("/Users/xiaowenwang/Documents/credit_card_strategy/Forcast2015.csv",header=T)
temp <- cbind(temp,fo)
names(temp)[length(temp)] <- readline(prompt = "Enter a row name:") 
write.csv(temp, file="/Users/xiaowenwang/Documents/credit_card_strategy/Forcast2015.csv",row.names=FALSE)
