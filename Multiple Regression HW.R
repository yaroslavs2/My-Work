setwd("C:\\Users\\user\\Documents\\BIS 348")

boston.df <- read.csv("BostonHousing.csv", header = TRUE)

#Q1: To obtain an honest estimate of future classification error.
#The training data is used to construct a classifier.
#Validation data computes the confusion matrix.

selected.df <- boston.df[c(1, 4, 6, 13)]

set.seed(12345) #set seed for reproducing the partition
train.index <- sample(c(1:506), 304)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

boston.lm <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)

options(scipen = 999)
summary(boston.lm) 
#Equation for House with 6 rooms, not bound by river, and crime rate of 0.1:
#MEDV = -33.1192 +(-.2535 * .1) + (2.7932 * 0) +  (8.9604 * 6)
#MEDV = 20.66855
#Median house price = $20,668.55

#Question D part 1: INDUS, NOX, and TAX variables likely positively
#correlated. Industrialized areas are usually more polluted and have
#higher property taxes.

##correlation table
heatmap(cor(boston.df), Rowv = NA, Colv = NA)
#Drop INDUS, AGE, and NOX

boston.lm.1 <- lm(MEDV~ CRIM, data = train.df)
boston.lm.2 <- lm(MEDV ~ CRIM + CHAS, data = train.df)
boston.lm.3 <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)


boston.lm.1.valid.pred <- predict(boston.lm.1, valid.df)
boston.lm.2.valid.pred <- predict(boston.lm.2, valid.df)
boston.lm.3.valid.pred <- predict(boston.lm.3, valid.df)


boston.lm.1.train.pred <- predict(boston.lm.1, train.df)
boston.lm.2.train.pred <- predict(boston.lm.2, train.df)
boston.lm.3.train.pred <- predict(boston.lm.3, train.df)



boston.lm.1.train.accuracy <- accuracy(boston.lm.1.train.pred, train.df$MEDV)
boston.lm.2.train.accuracy <- accuracy(boston.lm.2.train.pred, train.df$MEDV)
boston.lm.3.train.accuracy <- accuracy(boston.lm.3.train.pred, train.df$MEDV)



boston.lm.1.valid.accuracy <- accuracy(boston.lm.1.valid.pred, valid.df$MEDV)
boston.lm.2.valid.accuracy <- accuracy(boston.lm.2.valid.pred, valid.df$MEDV)
boston.lm.3.valid.accuracy <- accuracy(boston.lm.3.valid.pred, valid.df$MEDV)



train.valid.rmse.df <- data.frame("Model#" = c(1,2,3), "Valid_RMSE" = c(boston.lm.1.valid.accuracy[1,2],
                                                                                boston.lm.2.valid.accuracy[1,2],
                                                                                boston.lm.3.valid.accuracy[1,2]),
                                                                                
                                  "Train_RMSE" = c(boston.lm.1.train.accuracy[1,2],
                                                   boston.lm.2.train.accuracy[1,2],
                                                   boston.lm.3.train.accuracy[1,2]))
                                                   


plot(train.valid.rmse.df$Train_RMSE, type = "o",col = "red", xlab = "Model#", ylab = "RMSE", 
     main = "Train vs. Valid RSME")
lines(train.valid.rmse.df$Valid_RMSE, type = "o", col = "blue")



















