setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())
Auction.df <- read.csv("eBayAuctions.csv")
str(Auction.df)

#create dummy variables
for(level in unique(Auction.df$Category)){
  Auction.df[level] <- ifelse(Auction.df$Category == level, 1, 0)
}

for(level in unique(Auction.df$currency)){
  Auction.df[level] <- ifelse(Auction.df$currency == level, 1, 0)
}

for(level in unique(Auction.df$endDay)){
  Auction.df[level] <- ifelse(Auction.df$endDay == level, 1, 0)
}



## part a
pivot.Category <- aggregate(Auction.df$Competitive., by = list(Auction.df$Category), FUN = mean)
names(pivot.Category) <- c("Category", "MeanCompetitive")
pivot.currency <- aggregate(Auction.df$Competitive., by = list(Auction.df$currency), FUN = mean)
names(pivot.currency) <- c("currency", "MeanCompetitive")
pivot.endDay <- aggregate(Auction.df$Competitive., by = list(Auction.df$endDay), FUN = mean)
names(pivot.endDay) <- c("endDay", "MeanCompetitive")
pivot.Duration <- aggregate(Auction.df$Competitive., by = list(Auction.df$Duration), FUN = mean)
names(pivot.Duration) <- c("Duration", "MeanCompetitive")

# from the pivot.currency Table, 'EUR' and 'US' are similar.
# Create dummy variables for Currency;
Auction.df$USEUR <- Auction.df$US + Auction.df$EUR



## part b

set.seed(12345)
train.index <- sample(c(1:dim(Auction.df)[1]), dim(Auction.df)[1]*0.6)
train.df <- Auction.df[train.index, ]
valid.df <- Auction.df[-train.index, ]

# run a logistic model with all predictors, cutoff = 0.5
logit.reg <- glm(Competitive. ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df[ , -8], type = "response")
data.frame(actual = valid.df$Competitive., predicted = round(logit.reg.pred,2))

# confusion matrix 
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$Competitive.))
cmatrix

scoreauction.df <- read.csv("ScoreeBayAuctions.csv")

set.seed(12345)
score.train.index <- sample(c(1:dim(Auction.df)[1]), dim(Auction.df)[1]*0.6)
score.train.df <- Auction.df[train.index, ]
score.valid.df <- Auction.df[-train.index, ]


for(level in unique(scoreauction.df$Category)){
  scoreauction.df[level] <- ifelse(scoreauction.df$Category == level, 1, 0)
}

for(level in unique(scoreauction.df$currency)){
  scoreauction.df[level] <- ifelse(scoreauction.df$currency == level, 1, 0)
}

for(level in unique(Auction.df$endDay)){
  scoreauction.df[level] <- ifelse(scoreauction.df$endDay == level, 1, 0)
}


logit.reg.pred.score <- predict(logit.reg, score.valid.df, type = "response")
data.frame(actual = valid.df$Competitive., predicted = round(logit.reg.pred.score,2))

scorecmatrix <- confusionMatrix(as.factor(ifelse(logit.reg.pred.score > 0.5, 1, 0)), as.factor(valid.df$Competitive.))
scorecmatrix
