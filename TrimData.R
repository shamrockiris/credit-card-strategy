n <- readline(prompt="Enter an category name: ")
m.name <- readline(prompt="Enter a new name for the table:")
m.path <- "/Users/xiaowenwang/Documents/credit_card_strategy/"
path <- paste(m.path, m.name, sep = "")
r <- sourceData[V2 == "Sales" & V3 == n,4:15]
mo <- 1:12
year <- c(0,2014:1992)
m <- rbind(mo,r)
m <- cbind(year,m)
write.csv(m, file = path)
