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

data.for.plot <- aggregate(ebay.df$Competitive., by = list(ebay.df$Category), FUN = mean) #Pivot Table
#Drop Business/Industrial, Computer, and Home/Garden variables

nor <- preProcess(ebay.df[, 2:3], method=c("range"))
ebay.df[, c(3:4, 6:7)] <- predict(nor, ebay.df[, c(3:4, 6:7)]) 

train.index <- sample(c(1:dim(ebay.df)[1]), dim(ebay.df)[1]*0.6)
train.df <- ebay.df[train.index, ]
valid.df <- ebay.df[-train.index, ]

install.packages("neuralnet")
library(neuralnet)
library(caret)



ebay.nn <- neuralnet(Competitive. ~ sellerRating + Duration + ClosePrice + OpenPrice +
                     MusicMovieGame + Automotive + Books + Collectibles + ToysHobbies +
                     Electronics + EverythingElse + ClothingAccessories + CoinsStamps +
                     AntiqueArtCraft + Jewelry + PotteryGlass + SportingGoods + 
                     HealthBeauty + Photography + USCurrency + UKCurrency + EuroCurrency +
                     Monday + Tuesday + Wednesday + Thursday + Friday + Saturday + Sunday, 
                     data = ebay.df, linear.output = F, hidden = 3, threshold = 0.5)

ebay.nn$weights
prediction(ebay.nn)
plot(ebay.nn, rep="best")

ebay.nn.pred <- compute(ebay.nn, data.frame(valid.df$sellerRating, valid.df$Duration, valid.df$ClosePrice, valid.df$OpenPrice,
                                            valid.df$MusicMovieGame, valid.df$Automotive, valid.df$Books, valid.df$Collectibles, valid.df$ToysHobbies,
                                            valid.df$Electronics, valid.df$EverythingElse, valid.df$ClothingAccessories, valid.df$CoinsStamps,
                                            valid.df$AntiqueArtCraft, valid.df$Jewelry, valid.df$PotteryGlass, valid.df$SportingGoods, 
                                            valid.df$HealthBeauty, valid.df$Photography, valid.df$USCurrency, valid.df$UKCurrency, valid.df$EuroCurrency,
                                            valid.df$Monday, valid.df$Tuesday, valid.df$Wednesday, valid.df$Thursday, valid.df$Friday, valid.df$Saturday, valid.df$Sunday))

cmatrix <- confusionMatrix(as.factor(ifelse(ebay.nn.pred$net.result > 0.25, 1, 0)), as.factor(valid.df$Competitive.))
cmatrix
data.frame(ebay.nn.pred, valid.df$Competitive.)

#lift chart
library(gains)
gain <- gains(valid.df$Competitive., ebay.nn.pred$net.result, groups=10)
gain

data.frame("depth" =  gain["depth"], "obs" = gain["obs"], "cume.obs" = gain["cume.obs"], 
           "mean.resp" = gain["mean.resp"], "cume.mean.resp" = gain["cume.mean.resp"], "cume.pct.of.total" = gain["cume.pct.of.total"], 
           "lift" = gain["lift"], "cume.lift" = gain["cume.lift"], "mean.prediction" = gain["mean.prediction"])

sort <- data.frame("target" = valid.df$Competitive., "response" = round(ebay.nn.pred$net.result, 4))[order(ebay.nn.pred$net.result, decreasing=TRUE), ]
sort[1:200, ]

# plot lift chart
plot(c(0, gain$cume.pct.of.total*sum(valid.df$Competitive.)) ~ c(0, gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Competitive.))~c(0, dim(valid.df)[1]), lty=2)

#plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Competitive.)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#NN Accuracy: 96.32%
#Regression Accuracy:63.75%

#Neural Network Model is better

