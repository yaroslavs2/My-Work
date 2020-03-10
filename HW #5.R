setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

ebay.df <- read.csv("eBayAuctions.csv")

#create dummy variables
for(level in unique(ebay.df$Category)){
  ebay.df[level] <- ifelse(ebay.df$Category == level, 1, 0)
}

for(level in unique(ebay.df$currency)){
  ebay.df[level] <- ifelse(ebay.df$currency == level, 1, 0)
}

for(level in unique(ebay.df$endDay)){
  ebay.df[level] <- ifelse(ebay.df$endDay == level, 1, 0)
}



## part a
pivot.Category <- aggregate(ebay.df$Competitive., by = list(ebay.df$Category), FUN = mean)
names(pivot.Category) <- c("Category", "MeanCompetitive")
pivot.currency <- aggregate(ebay.df$Competitive., by = list(ebay.df$currency), FUN = mean)
names(pivot.currency) <- c("currency", "MeanCompetitive")
pivot.endDay <- aggregate(ebay.df$Competitive., by = list(ebay.df$endDay), FUN = mean)
names(pivot.endDay) <- c("endDay", "MeanCompetitive")
pivot.Duration <- aggregate(ebay.df$Competitive., by = list(ebay.df$Duration), FUN = mean)
names(pivot.Duration) <- c("Duration", "MeanCompetitive")

# from the pivot.currency Table, 'EUR' and 'US' are similar.
# Create dummy variables for Currency;
ebay.df$USEUR <- ebay.df$US + ebay.df$EUR



## part b

set.seed(12345)
train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

# run a logistic model with all predictors, cutoff = 0.5
logit.reg <- glm(Competitive. ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df[ , -8], type = "response")
data.frame(actual = valid.df$Competitive., predicted = round(logit.reg.pred,2))

# confusion matrix 
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.05 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.05, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.1 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.1, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.15 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.15, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.2 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.2, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.25 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.25, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.3 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.3, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.35 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.35, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.4 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.4, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.45 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.45, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.5 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.55 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.55, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.6 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.6, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.65 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.65, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.7 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.7, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.75 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.75, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.8 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.8, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.85 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.85, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.9 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.9, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.95 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.95, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]
cmatrix.10 <- confusionMatrix(as.factor(ifelse(logit.reg.pred > 1, 1, 0)), as.factor(valid.df$Competitive.))$overall[1]

library(forecast)
cutoff <- c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55,
            0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
cmatrix.all <- c(cmatrix, cmatrix.05, cmatrix.1, cmatrix.15, cmatrix.2, cmatrix.25,
                 cmatrix.3, cmatrix.35, cmatrix.4, cmatrix.45, cmatrix.5, cmatrix.55,
                 cmatrix.6, cmatrix.65, cmatrix.7, cmatrix.75, cmatrix.8, cmatrix.85,
                 cmatrix.9, cmatrix.95, cmatrix.10)
plot(cmatrix.all ~ cutoff,
     xlab="Cutoff", ylab="Accuracy", main="", type="l")

#plot lift chart
library(gains)
gain.full <- gains(valid.df$Competitive., logit.reg.pred, groups=10)

data.frame("depth" =  gain.full["depth"], "obs" = gain.full["obs"], "cume.obs" = gain.full["cume.obs"], 
           "mean.resp" = gain.full["mean.resp"], "cume.mean.resp" = gain.full["cume.mean.resp"], "cume.pct.of.total" = gain.full["cume.pct.of.total"], 
           "lift" = gain.full["lift"], "cume.lift" = gain.full["cume.lift"], "mean.prediction" = gain.full["mean.prediction"])


# plot lift chart
plot(c(0, gain.full$cume.pct.of.total*sum(valid.df$Competitive.))~c(0,gain.full$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Competitive.))~c(0, dim(valid.df)[1]), lty=2)


install.packages("pROC")
library(pROC)
r <- roc(ebay.df$Competitive., logit.reg.pred)
plot.roc(r)

auc(r)
