data <- read.csv("Joined.csv")
data
is.na(data$Migrants.rounded)
data[which(is.na(data$Migrants.rounded)), "Migrants.rounded"] <- 0	
data
data$colony <- 0
data[which(data$Country=="France"), "colony"] <- 1
data
write.table(data, file = "Joined.csv")
read.csv("Joined.csv")
