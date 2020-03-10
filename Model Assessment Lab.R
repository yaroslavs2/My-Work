setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

housing.df <- read.csv("BostonHousing.csv")

set.seed(12345) #set seed for reproducing the partition
train.index <- sample(c(1:506), 304)
train.df <- housing.df[train.index, ]
valid.df <- housing.df[-train.index, ]


#CRIM, CHAS, RM, AGE, LSTAT
boston.lm.crime <- lm(MEDV ~ CRIM, data = train.df)
boston.lm.charles <- lm(MEDV ~ CHAS, data = train.df)
boston.lm.rooms <- lm(MEDV ~ RM, data = train.df)
boston.lm.age <- lm(MEDV ~ AGE, data = train.df)
boston.lm.lowerstat <- lm(MEDV ~ LSTAT, data = train.df)

#combo of 2
boston.lm.1 <- lm(MEDV ~ CRIM + CHAS, data = train.df)
boston.lm.2 <- lm(MEDV ~ CRIM + RM, data = train.df)
boston.lm.3 <- lm(MEDV ~ CRIM + AGE, data = train.df)
boston.lm.4 <- lm(MEDV ~ CRIM + LSTAT, data = train.df)
boston.lm.5 <- lm(MEDV ~ CHAS + RM, data = train.df)
boston.lm.6 <- lm(MEDV ~ CHAS + AGE, data = train.df)
boston.lm.7 <- lm(MEDV ~ CHAS + LSTAT, data = train.df)
boston.lm.8 <- lm(MEDV ~ RM + AGE, data = train.df)
boston.lm.9 <- lm(MEDV ~ RM + LSTAT, data = train.df)
boston.lm.10 <- lm(MEDV ~ AGE + LSTAT, data = train.df)

#combo of 3
boston.lm.11 <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)
boston.lm.12 <- lm(MEDV ~ CRIM + CHAS + AGE, data = train.df)
boston.lm.13 <- lm(MEDV ~ CRIM + CHAS + LSTAT, data = train.df)
boston.lm.14 <- lm(MEDV ~ CRIM + RM + AGE, data = train.df)
boston.lm.15 <- lm(MEDV ~ CRIM + RM + LSTAT, data = train.df)
boston.lm.16 <- lm(MEDV ~ CRIM + AGE + LSTAT, data = train.df)
boston.lm.17 <- lm(MEDV ~ CHAS + RM + AGE, data = train.df)
boston.lm.18 <- lm(MEDV ~ CHAS + RM + LSTAT, data = train.df)
boston.lm.19 <- lm(MEDV ~ CHAS + AGE + LSTAT, data = train.df)
boston.lm.20 <- lm(MEDV ~ RM + AGE + LSTAT, data = train.df)

#combo of 4
boston.lm.21 <- lm(MEDV ~ CRIM + CHAS + RM + AGE, data = train.df)
boston.lm.22 <- lm(MEDV ~ CRIM + CHAS + RM + LSTAT, data = train.df)
boston.lm.23 <- lm(MEDV ~ CRIM + CHAS + AGE + LSTAT, data = train.df)
boston.lm.24 <- lm(MEDV ~ CRIM + RM + AGE + LSTAT, data = train.df)
boston.lm.25 <- lm(MEDV ~ CHAS + RM + AGE + LSTAT, data = train.df)

#combo of 5
boston.lm.26 <- lm(MEDV ~ CRIM + CHAS + RM + AGE + LSTAT, data = train.df)

#validation predictions
boston.lm.crime.valid.pred <- predict(boston.lm.crime, valid.df)
boston.lm.charles.valid.pred <- predict(boston.lm.charles, valid.df)
boston.lm.rooms.valid.pred <- predict(boston.lm.rooms, valid.df)
boston.lm.age.valid.pred <- predict(boston.lm.age, valid.df)
boston.lm.lowerstat.valid.pred <- predict(boston.lm.lowerstat, valid.df)

boston.lm.1.valid.pred <- predict(boston.lm.1, valid.df)
boston.lm.2.valid.pred <- predict(boston.lm.2, valid.df)
boston.lm.3.valid.pred <- predict(boston.lm.3, valid.df)
boston.lm.4.valid.pred <- predict(boston.lm.4, valid.df)
boston.lm.5.valid.pred <- predict(boston.lm.5, valid.df)
boston.lm.6.valid.pred <- predict(boston.lm.6, valid.df)
boston.lm.7.valid.pred <- predict(boston.lm.7, valid.df)
boston.lm.8.valid.pred <- predict(boston.lm.8, valid.df)
boston.lm.9.valid.pred <- predict(boston.lm.9, valid.df)
boston.lm.10.valid.pred <- predict(boston.lm.10, valid.df)
boston.lm.11.valid.pred <- predict(boston.lm.11, valid.df)
boston.lm.12.valid.pred <- predict(boston.lm.12, valid.df)
boston.lm.13.valid.pred <- predict(boston.lm.13, valid.df)
boston.lm.14.valid.pred <- predict(boston.lm.14, valid.df)
boston.lm.15.valid.pred <- predict(boston.lm.15, valid.df)
boston.lm.16.valid.pred <- predict(boston.lm.16, valid.df)
boston.lm.17.valid.pred <- predict(boston.lm.17, valid.df)
boston.lm.18.valid.pred <- predict(boston.lm.18, valid.df)
boston.lm.19.valid.pred <- predict(boston.lm.19, valid.df)
boston.lm.20.valid.pred <- predict(boston.lm.20, valid.df)
boston.lm.21.valid.pred <- predict(boston.lm.21, valid.df)
boston.lm.22.valid.pred <- predict(boston.lm.22, valid.df)
boston.lm.23.valid.pred <- predict(boston.lm.23, valid.df)
boston.lm.24.valid.pred <- predict(boston.lm.24, valid.df)
boston.lm.25.valid.pred <- predict(boston.lm.25, valid.df)
boston.lm.26.valid.pred <- predict(boston.lm.26, valid.df)

#training predictions

boston.lm.crime.train.pred <- predict(boston.lm.crime, train.df)
boston.lm.charles.train.pred <- predict(boston.lm.charles, train.df)
boston.lm.rooms.train.pred <- predict(boston.lm.rooms, train.df)
boston.lm.age.train.pred <- predict(boston.lm.age, train.df)
boston.lm.lowerstat.train.pred <- predict(boston.lm.lowerstat, train.df)

boston.lm.1.train.pred <- predict(boston.lm.1, train.df)
boston.lm.2.train.pred <- predict(boston.lm.2, train.df)
boston.lm.3.train.pred <- predict(boston.lm.3, train.df)
boston.lm.4.train.pred <- predict(boston.lm.4, train.df)
boston.lm.5.train.pred <- predict(boston.lm.5, train.df)
boston.lm.6.train.pred <- predict(boston.lm.6, train.df)
boston.lm.7.train.pred <- predict(boston.lm.7, train.df)
boston.lm.8.train.pred <- predict(boston.lm.8, train.df)
boston.lm.9.train.pred <- predict(boston.lm.9, train.df)
boston.lm.10.train.pred <- predict(boston.lm.10, train.df)
boston.lm.11.train.pred <- predict(boston.lm.11, train.df)
boston.lm.12.train.pred <- predict(boston.lm.12, train.df)
boston.lm.13.train.pred <- predict(boston.lm.13, train.df)
boston.lm.14.train.pred <- predict(boston.lm.14, train.df)
boston.lm.15.train.pred <- predict(boston.lm.15, train.df)
boston.lm.16.train.pred <- predict(boston.lm.16, train.df)
boston.lm.17.train.pred <- predict(boston.lm.17, train.df)
boston.lm.18.train.pred <- predict(boston.lm.18, train.df)
boston.lm.19.train.pred <- predict(boston.lm.19, train.df)
boston.lm.20.train.pred <- predict(boston.lm.20, train.df)
boston.lm.21.train.pred <- predict(boston.lm.21, train.df)
boston.lm.22.train.pred <- predict(boston.lm.22, train.df)
boston.lm.23.train.pred <- predict(boston.lm.23, train.df)
boston.lm.24.train.pred <- predict(boston.lm.24, train.df)
boston.lm.25.train.pred <- predict(boston.lm.25, train.df)
boston.lm.26.train.pred <- predict(boston.lm.26, train.df)

library(forecast)
boston.lm.crime.train.accuracy <- accuracy(boston.lm.crime.train.pred, train.df$MEDV)
boston.lm.charles.train.accuracy <- accuracy(boston.lm.charles.train.pred, train.df$MEDV)
boston.lm.rooms.train.accuracy <- accuracy(boston.lm.rooms.train.pred, train.df$MEDV)
boston.lm.age.train.accuracy <- accuracy(boston.lm.age.train.pred, train.df$MEDV)
boston.lm.lowerstat.train.accuracy <- accuracy(boston.lm.lowerstat.train.pred, train.df$MEDV)

boston.lm.1.train.accuracy <- accuracy(boston.lm.1.train.pred, train.df$MEDV)
boston.lm.2.train.accuracy <- accuracy(boston.lm.2.train.pred, train.df$MEDV)
boston.lm.3.train.accuracy <- accuracy(boston.lm.3.train.pred, train.df$MEDV)
boston.lm.4.train.accuracy <- accuracy(boston.lm.4.train.pred, train.df$MEDV)
boston.lm.5.train.accuracy <- accuracy(boston.lm.5.train.pred, train.df$MEDV)
boston.lm.6.train.accuracy <- accuracy(boston.lm.6.train.pred, train.df$MEDV)
boston.lm.7.train.accuracy <- accuracy(boston.lm.7.train.pred, train.df$MEDV)
boston.lm.8.train.accuracy <- accuracy(boston.lm.8.train.pred, train.df$MEDV)
boston.lm.9.train.accuracy <- accuracy(boston.lm.9.train.pred, train.df$MEDV)
boston.lm.10.train.accuracy <- accuracy(boston.lm.10.train.pred, train.df$MEDV)
boston.lm.11.train.accuracy <- accuracy(boston.lm.11.train.pred, train.df$MEDV)
boston.lm.12.train.accuracy <- accuracy(boston.lm.12.train.pred, train.df$MEDV)
boston.lm.13.train.accuracy <- accuracy(boston.lm.13.train.pred, train.df$MEDV)
boston.lm.14.train.accuracy <- accuracy(boston.lm.14.train.pred, train.df$MEDV)
boston.lm.15.train.accuracy <- accuracy(boston.lm.15.train.pred, train.df$MEDV)
boston.lm.16.train.accuracy <- accuracy(boston.lm.16.train.pred, train.df$MEDV)
boston.lm.17.train.accuracy <- accuracy(boston.lm.17.train.pred, train.df$MEDV)
boston.lm.18.train.accuracy <- accuracy(boston.lm.18.train.pred, train.df$MEDV)
boston.lm.19.train.accuracy <- accuracy(boston.lm.19.train.pred, train.df$MEDV)
boston.lm.20.train.accuracy <- accuracy(boston.lm.20.train.pred, train.df$MEDV)
boston.lm.21.train.accuracy <- accuracy(boston.lm.21.train.pred, train.df$MEDV)
boston.lm.22.train.accuracy <- accuracy(boston.lm.22.train.pred, train.df$MEDV)
boston.lm.23.train.accuracy <- accuracy(boston.lm.23.train.pred, train.df$MEDV)
boston.lm.24.train.accuracy <- accuracy(boston.lm.24.train.pred, train.df$MEDV)
boston.lm.25.train.accuracy <- accuracy(boston.lm.25.train.pred, train.df$MEDV)
boston.lm.26.train.accuracy <- accuracy(boston.lm.26.train.pred, train.df$MEDV)

boston.lm.crime.valid.accuracy <- accuracy(boston.lm.crime.valid.pred, valid.df$MEDV)
boston.lm.charles.valid.accuracy <- accuracy(boston.lm.charles.valid.pred, valid.df$MEDV)
boston.lm.rooms.valid.accuracy <- accuracy(boston.lm.rooms.valid.pred, valid.df$MEDV)
boston.lm.age.valid.accuracy <- accuracy(boston.lm.age.valid.pred, valid.df$MEDV)
boston.lm.lowerstat.valid.accuracy <- accuracy(boston.lm.lowerstat.valid.pred, valid.df$MEDV)

boston.lm.1.valid.accuracy <- accuracy(boston.lm.1.valid.pred, valid.df$MEDV)
boston.lm.2.valid.accuracy <- accuracy(boston.lm.2.valid.pred, valid.df$MEDV)
boston.lm.3.valid.accuracy <- accuracy(boston.lm.3.valid.pred, valid.df$MEDV)
boston.lm.4.valid.accuracy <- accuracy(boston.lm.4.valid.pred, valid.df$MEDV)
boston.lm.5.valid.accuracy <- accuracy(boston.lm.5.valid.pred, valid.df$MEDV)
boston.lm.6.valid.accuracy <- accuracy(boston.lm.6.valid.pred, valid.df$MEDV)
boston.lm.7.valid.accuracy <- accuracy(boston.lm.7.valid.pred, valid.df$MEDV)
boston.lm.8.valid.accuracy <- accuracy(boston.lm.8.valid.pred, valid.df$MEDV)
boston.lm.9.valid.accuracy <- accuracy(boston.lm.9.valid.pred, valid.df$MEDV)
boston.lm.10.valid.accuracy <- accuracy(boston.lm.10.valid.pred, valid.df$MEDV)
boston.lm.11.valid.accuracy <- accuracy(boston.lm.11.valid.pred, valid.df$MEDV)
boston.lm.12.valid.accuracy <- accuracy(boston.lm.12.valid.pred, valid.df$MEDV)
boston.lm.13.valid.accuracy <- accuracy(boston.lm.13.valid.pred, valid.df$MEDV)
boston.lm.14.valid.accuracy <- accuracy(boston.lm.14.valid.pred, valid.df$MEDV)
boston.lm.15.valid.accuracy <- accuracy(boston.lm.15.valid.pred, valid.df$MEDV)
boston.lm.16.valid.accuracy <- accuracy(boston.lm.16.valid.pred, valid.df$MEDV)
boston.lm.17.valid.accuracy <- accuracy(boston.lm.17.valid.pred, valid.df$MEDV)
boston.lm.18.valid.accuracy <- accuracy(boston.lm.18.valid.pred, valid.df$MEDV)
boston.lm.19.valid.accuracy <- accuracy(boston.lm.19.valid.pred, valid.df$MEDV)
boston.lm.20.valid.accuracy <- accuracy(boston.lm.20.valid.pred, valid.df$MEDV)
boston.lm.21.valid.accuracy <- accuracy(boston.lm.21.valid.pred, valid.df$MEDV)
boston.lm.22.valid.accuracy <- accuracy(boston.lm.22.valid.pred, valid.df$MEDV)
boston.lm.23.valid.accuracy <- accuracy(boston.lm.23.valid.pred, valid.df$MEDV)
boston.lm.24.valid.accuracy <- accuracy(boston.lm.24.valid.pred, valid.df$MEDV)
boston.lm.25.valid.accuracy <- accuracy(boston.lm.25.valid.pred, valid.df$MEDV)
boston.lm.26.valid.accuracy <- accuracy(boston.lm.26.valid.pred, valid.df$MEDV)

train.valid.rmse.df <- data.frame("Model#" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                                               16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31), 
                                                               "Valid_RMSE" = c(boston.lm.1.valid.accuracy[1,2],
                                                                                boston.lm.2.valid.accuracy[1,2],
                                                                                boston.lm.3.valid.accuracy[1,2],
                                                                                boston.lm.4.valid.accuracy[1,2],
                                                                                boston.lm.5.valid.accuracy[1,2],
                                                                                boston.lm.6.valid.accuracy[1,2],
                                                                                boston.lm.7.valid.accuracy[1,2],
                                                                                boston.lm.8.valid.accuracy[1,2],
                                                                                boston.lm.9.valid.accuracy[1,2],
                                                                                boston.lm.10.valid.accuracy[1,2],
                                                                                boston.lm.11.valid.accuracy[1,2], 
                                                                                boston.lm.12.valid.accuracy[1,2],
                                                                                boston.lm.13.valid.accuracy[1,2],
                                                                                boston.lm.14.valid.accuracy[1,2],
                                                                                boston.lm.15.valid.accuracy[1,2],
                                                                                boston.lm.16.valid.accuracy[1,2],
                                                                                boston.lm.17.valid.accuracy[1,2],
                                                                                boston.lm.18.valid.accuracy[1,2],
                                                                                boston.lm.19.valid.accuracy[1,2],
                                                                                boston.lm.20.valid.accuracy[1,2],
                                                                                boston.lm.21.valid.accuracy[1,2],
                                                                                boston.lm.22.valid.accuracy[1,2],
                                                                                boston.lm.23.valid.accuracy[1,2],
                                                                                boston.lm.24.valid.accuracy[1,2],
                                                                                boston.lm.25.valid.accuracy[1,2],
                                                                                boston.lm.26.valid.accuracy[1,2],
                                                                                boston.lm.crime.valid.accuracy[1,2],
                                                                                boston.lm.charles.valid.accuracy[1,2],
                                                                                boston.lm.rooms.valid.accuracy[1,2],
                                                                                boston.lm.age.valid.accuracy[1,2],
                                                                                boston.lm.lowerstat.valid.accuracy[1,2]),
"Train_RMSE" = c(boston.lm.1.train.accuracy[1,2],
                 boston.lm.2.train.accuracy[1,2],
                 boston.lm.3.train.accuracy[1,2],
                 boston.lm.4.train.accuracy[1,2],
                 boston.lm.5.train.accuracy[1,2],
                 boston.lm.6.train.accuracy[1,2],
                 boston.lm.7.train.accuracy[1,2],
                 boston.lm.8.train.accuracy[1,2],
                 boston.lm.9.train.accuracy[1,2],
                 boston.lm.10.train.accuracy[1,2],
                 boston.lm.11.train.accuracy[1,2], 
                 boston.lm.12.train.accuracy[1,2],
                 boston.lm.13.train.accuracy[1,2],
                 boston.lm.14.train.accuracy[1,2],
                 boston.lm.15.train.accuracy[1,2],
                 boston.lm.16.train.accuracy[1,2],
                 boston.lm.17.train.accuracy[1,2],
                 boston.lm.18.train.accuracy[1,2],
                 boston.lm.19.train.accuracy[1,2],
                 boston.lm.20.train.accuracy[1,2],
                 boston.lm.21.train.accuracy[1,2],
                 boston.lm.22.train.accuracy[1,2],
                 boston.lm.23.train.accuracy[1,2],
                 boston.lm.24.train.accuracy[1,2],
                 boston.lm.25.train.accuracy[1,2],
                 boston.lm.26.train.accuracy[1,2],
                 boston.lm.crime.train.accuracy[1,2],
                 boston.lm.charles.train.accuracy[1,2],
                 boston.lm.rooms.train.accuracy[1,2],
                 boston.lm.age.train.accuracy[1,2],
                 boston.lm.lowerstat.train.accuracy[1,2]))


plot(train.valid.rmse.df$Train_RMSE, type = "o",col = "red", xlab = "Model#", ylab = "RMSE", 
     main = "Train vs. Valid RSME")
lines(train.valid.rmse.df$Valid_RMSE, type = "o", col = "blue")

#Model 19 is the best model to choose since it has the
#lowest validation RMSE