setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

mower.df <- read.csv("RidingMowers.csv")

dim(mower.df)
length(mower.df)
head(mower.df)
str(mower.df)
summary(mower.df)
#a: 50% of households were owners of a riding mower

plot(mower.df$Income ~ mower.df$Lot_Size, ylab = "Income", xlab = "Lot Size",
     col = ifelse(mower.df$Ownership == "Owner", "black", "gray"))
#b: Owners appear to heave a higher income

#dummy variable code below
mower.df$Owner <- ifelse(mower.df$Ownership == "Owner", 1, 0)

train.index <- sample(c(1:dim(mower.df)[1]), dim(mower.df)[1]*0.6)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[-train.index, ]

logit.reg1 <- glm(Owner ~ Income + Lot_Size, data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg1)

logit.reg1.pred <- predict(logit.reg1, valid.df, type = "response")


install.packages("caret")
library(caret)
cmatrix <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.5, 1, 0)), as.factor(valid.df$Owner))

#c:80% correctly predicted/classified as owners

#d: probability should be increased

# y = .12383(60) + .34653(20) - 14.87322 
# y = -.512822
#odds = e^(-.512822) = 0.5988
#Odds that a household with $60k income and 20k lot size
#is an owner are 59.88%

#$60k income and 20k lot size household classified as owner

# 0 = 0.12383(Income) + 16(.35653) - 14.87322
# 0 = 0.12383(Income) - 9.16874
# Income = 74.043

#g: Income should be at least $74,043 to be classified as owner

#plot lift chart
library(gains)
gain <- gains(valid.df$Owner, logit.reg1.pred, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$Owner, "response" = round(logit.reg1.pred, 4))[order(logit.reg1.pred, decreasing=TRUE), ]
sort[1:200, ]

# plot lift chart
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Owner)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Owner))~c(0, dim(valid.df)[1]), lty=2)



