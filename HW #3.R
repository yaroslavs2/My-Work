setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

ebay.df <- read.csv("eBayAuctions.csv")

#Dummy variables for Category
#Commented ones are the ones that were dropped after running Pivot Table
ebay.df$MusicMovieGame <- ifelse(ebay.df$Category == "Music/Movie/Game", 1, 0)
ebay.df$Automotive <- ifelse(ebay.df$Category == "Automotive", 1, 0)
ebay.df$Books <- ifelse(ebay.df$Category == "Books", 1, 0)
#ebay.df$HomeGarden <- ifelse(ebay.df$Category == "Home/Garden", 1, 0)
ebay.df$Collectibles <- ifelse(ebay.df$Category == "Collectibles", 1, 0)
#ebay.df$BusinessIndustral <- ifelse(ebay.df$Category == "Business/Industrial", 1, 0)
ebay.df$ToysHobbies <- ifelse(ebay.df$Category == "Toys/Hobbies", 1, 0)
ebay.df$Electronics <- ifelse(ebay.df$Category == "Electronics", 1, 0)
#ebay.df$Computer <- ifelse(ebay.df$Category == "Computer", 1, 0)
ebay.df$EverythingElse <- ifelse(ebay.df$Category == "EverythingElse", 1, 0)
ebay.df$ClothingAccessories <- ifelse(ebay.df$Category == "Clothing/Accessories", 1, 0)
ebay.df$CoinsStamps <- ifelse(ebay.df$Category == "Coins/Stamps", 1, 0)
ebay.df$AntiqueArtCraft <- ifelse(ebay.df$Category == "Antique/Art/Craft", 1, 0)
ebay.df$Jewelry <- ifelse(ebay.df$Category == "Jewelry", 1, 0)
ebay.df$PotteryGlass <- ifelse(ebay.df$Category == "Pottery/Glass", 1, 0)
ebay.df$SportingGoods <- ifelse(ebay.df$Category == "SportingGoods", 1, 0)
ebay.df$HealthBeauty <- ifelse(ebay.df$Category == "Health/Beauty", 1, 0)
ebay.df$Photography <- ifelse(ebay.df$Category == "Photography", 1, 0)

#Dummy variables for currency
ebay.df$USCurrency <- ifelse(ebay.df$currency == "US", 1, 0)
ebay.df$UKCurrency <- ifelse(ebay.df$currency == "GBP", 1, 0)
ebay.df$EuroCurrency <- ifelse(ebay.df$currency == "EUR", 1, 0)

#Dummy variables for endDay
ebay.df$Monday <- ifelse(ebay.df$endDay== "Mon", 1, 0)
ebay.df$Tuesday <- ifelse(ebay.df$endDay== "Tue", 1, 0)
ebay.df$Wednesday <- ifelse(ebay.df$endDay== "Wed", 1, 0)
ebay.df$Thursday <- ifelse(ebay.df$endDay== "Thu", 1, 0)
ebay.df$Friday <- ifelse(ebay.df$endDay== "Fri", 1, 0)
ebay.df$Saturday <- ifelse(ebay.df$endDay== "Sat", 1, 0)
ebay.df$Sunday <- ifelse(ebay.df$endDay== "Sun", 1, 0)

#Dummy variables for Duration
ebay.df$Duration1 <- ifelse(ebay.df$Duration== "1", 1, 0)
ebay.df$Duration3 <- ifelse(ebay.df$Duration== "3", 1, 0)
ebay.df$Duration5 <- ifelse(ebay.df$Duration== "5", 1, 0)
ebay.df$Duration7 <- ifelse(ebay.df$Duration== "7", 1, 0)
ebay.df$Duration10 <- ifelse(ebay.df$Duration== "10", 1, 0)

data.for.plot <- aggregate(ebay.df$Competitive., by = list(ebay.df$Category), FUN = mean) #Pivot Table
#Drop Business/Industrial, Computer, and Home/Garden variables

train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

logit.reg1 <- glm(Competitive. ~ ., data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg1)

logit.reg2 <- glm(Competitive. ~ . -ClosePrice -OpenPrice , data = train.df, family = "binomial")
options(scipen=999)
summary(logit.reg2)

library(forecast)
accuracy(logit.reg1)
accuracy(logit.reg2)

#The first model that includes price is more accurate as it has a lower RMSE

logit.reg1.pred <- predict(logit.reg1, valid.df, type = "response")
logit.reg2.pred <- predict(logit.reg2, valid.df, type = "response")

library(caret)
cmatrix1 <- confusionMatrix(as.factor(ifelse(logit.reg1.pred > 0.5, 1, 0)), as.factor(valid.df$Competitive.))
cmatrix2 <- confusionMatrix(as.factor(ifelse(logit.reg2.pred > 0.5, 1, 0)), as.factor(valid.df$Competitive.))

#plot lift chart for reg1
library(gains)
gain <- gains(valid.df$Competitive., logit.reg1.pred, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$Competitive., "response" = round(logit.reg1.pred, 4))[order(logit.reg1.pred, decreasing=TRUE), ]
sort[1:200, ]

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Competitive.)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Competitive.))~c(0, dim(valid.df)[1]), lty=2)

#plot lift chart for reg2
gain <- gains(valid.df$Competitive., logit.reg2.pred, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$Competitive., "response" = round(logit.reg2.pred, 4))[order(logit.reg2.pred, decreasing=TRUE), ]
sort[1:200, ]

plot(c(0, gain$cume.pct.of.total*sum(valid.df$Competitive.)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Competitive.))~c(0, dim(valid.df)[1]), lty=2)
