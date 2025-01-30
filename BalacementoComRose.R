if (!require(ROSE)) install.packages("ROSE", dependencies=TRUE)
library(ROSE)

data <- read.csv("dataset/online_shoppers_intention.csv")

data$Revenue <- as.factor(data$Revenue)
data$Month <- as.factor(data$Month)
data$VisitorType <- as.factor(data$VisitorType)
data$Weekend <- as.factor(data$Weekend)

data_balanced <- ROSE(Revenue ~ ., data = data, seed = 123)$data

table(data_balanced$Revenue)
