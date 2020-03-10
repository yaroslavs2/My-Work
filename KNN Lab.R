setwd("C:\\Users\\user\\Documents\\BIS 348")
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5,6)]

bank.df$education1 <- ifelse(bank.df$Education == "1", 1, 0)
bank.df$education2 <- ifelse(bank.df$Education == "2", 1, 0)
bank.df$education3 <- ifelse(bank.df$Education == "3", 1, 0)

#Partition and normalize
set.seed(12345)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df

norm.values <- preProcess(bank.df[ ,1:6], method=c("center", "scale"))
train.norm.df[,1:6] <- predict(norm.values, train.df[, 1:6])
valid.norm.df[,1:6] <- predict(norm.values, valid.df[, 1:6])
bank.norm.df[,1:6] <- predict(norm.values, bank.df[, 1:6])

library(FNN)
bank.knn <- knn(train = train.norm.df[ , -c(7)], test = data.frame(Age = 40, Experience = 10, 
                                                                   Income = 84, Family = 2, 
                                                                   CCAVG = 2, Education_1 = 0,
                                                                   Education_2 = 1, Education_3 = 0,
                                                                   Mortgage = 0, Securities.Account = 0,
                                                                   CD.Account = 0, Online = 1, CreditCard = 1), 
                cl = train.norm.df[, 7], k = 1)

row.names(train.norm.df)[attr(bank.knn, "nn.index")]
bank.norm.df[915,]

#Part 1: Customer would be classified as 1, or loan
#accepted group, as nearest neighbor also classified as 1

accuracy.df <- data.frame(k =  seq(1, 14, 1), accuracy = rep(0, 14))

for(i in 1:14) {
  bank.knn.pred <- knn(train.norm.df[, -c(7)], valid.norm.df[, -c(7)], cl = train.norm.df[, 7], k=i)
  accuracy.df[i, 2] <- confusionMatrix(as.factor(bank.knn.pred), as.factor(valid.norm.df[, 7]))$overall[1]
}

accuracy.df 
#Part B: Best K is 3 as it has highest accuracy

bank.knn3 <- knn(train = train.norm.df[ , -c(7)], test = data.frame(Age = 40, Experience = 10, 
                                                                    Income = 84, Family = 2, 
                                                                    CCAVG = 2, Education_1 = 0,
                                                                    Education_2 = 1, Education_3 = 0,
                                                                    Mortgage = 0, Securities.Account = 0,
                                                                    CD.Account = 0, Online = 1, CreditCard = 1), 
                 cl = train.norm.df[, 7], k = 3)

bank.knn3
row.names(train.norm.df)[attr(bank.knn3, "nn.index")]
bank.norm.df[c(915, 4293, 663), ]

#Part D: Classify as 1 