setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

housing.df <- read.csv("BostonHousing.csv")

selected.df <- housing.df[-c(14)]
set.seed(12345) 
train.index <- sample(c(1:dim(selected.df)[1]), dim(selected.df)[1]*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

housing.lm <- lm(MEDV ~ ., data = train.df)
options(scipen = 999)
summary(housing.lm)

housing.lm.back <- step(housing.lm, direction="backward")
summary(housing.lm.back) 
housing.lm.back.pred <- predict(housing.lm.back, valid.df)
accuracy(housing.lm.back.pred, valid.df$MEDV)



null = lm(MEDV ~ 1, data = train.df)
housing.lm.forw <- step(null, scope=list(lower=null, upper=housing.lm), direction="forward")
summary(housing.lm.forw)
housing.lm.forw.pred <- predict(housing.lm.forw, valid.df)
accuracy(housing.lm.forw.pred, valid.df$MEDV)

housing.lm.step <- step(null, scope=list(upper=housing.lm), data = train.df, direction="both")
summary(housing.lm.step) 
housing.lm.step.pred <- predict(housing.lm.step, valid.df)
accuracy(housing.lm.step.pred, valid.df$MEDV)

#scorehousing
scorehousing.df <- read.csv("ScoreBostonHousing.csv")

selected.df <- scorehousing.df[-c(14)]
set.seed(12345) 
train.index <- sample(c(1:dim(selected.df)[1]), dim(selected.df)[1]*0.6)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

#Not sure how to code to predict MEDV
scorehousing.lm <- lm(MEDV ~ ., data = train.df)

scorehousing.lm.back <- step(scorehousing.lm, direction="backward")
summary(scorehousing.lm.back) 
scorehousing.lm.back.pred <- predict(scorehousing.lm.back, valid.df)
accuracy(scorehousing.lm.back.pred, valid.df$MEDV)