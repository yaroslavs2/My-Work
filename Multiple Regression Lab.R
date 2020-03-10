setwd("C:\\Users\\user\\Documents\\BIS 348")

tayko.df <- read.csv("Tayko.csv", header = TRUE)

data.for.plot <- aggregate(tayko.df$Spending, by = list(tayko.df$Web.order), FUN = mean) #Pivot Table
mean(tayko.df$Web.order)
sd(tayko.df$Web.order)

genderplot <- aggregate(tayko.df$Spending, by = list(tayko.df$Gender.male), FUN = mean)
mean(tayko.df$Gender.male)
sd(tayko.df$Gender.male)

resAddressPlot <- aggregate(tayko.df$Spending, by = list(tayko.df$Address_is_res), FUN = mean)
mean(tayko.df$Address_is_res)
sd(tayko.df$Address_is_res)

usAddressplot <- aggregate(tayko.df$Spending, by = list(tayko.df$US), FUN = mean)
mean(tayko.df$US)
sd(tayko.df$US)

plot(tayko.df$Spending ~ tayko.df$Freq, xlab = "Spending", ylab = "Freq") #Yes, positive linear relationship
plot(tayko.df$Spending ~ tayko.df$last_update_days_ago, xlab = "Spending", ylab = "Last Update") #No clear linear relationship

selected.df <- tayko.df[c(2, 18:19, 21:23, 25)] #selected variables for regression


set.seed(12345)
train.index <- sample(c(1:2000), 1200)
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]

tayko.lm <- lm(Spending ~ Freq + Web.order + Gender.male + Address_is_res + US + last_update_days_ago, data = train.df)
#use summarize to find equation
options(scipen = 999)
summary(tayko.lm) #Spending = 4.28762 + (94.56287 * Freq) + (15.05527 * Web_Order) + (-1.70251 * Gender.male) + 
#(-84.96925 * Address_is_res) + (-7.11696 * US) + (-.00767 * last_update_days_ago)

#Question C part 3: Someone who had a large number of transactions in the preceding

tayko.lm.back <- step(tayko.lm, direction="backward")
summary(tayko.lm.back) #it dropped the gender variable first

library(forecast)
tayko.lm.pred <- predict(tayko.lm, valid.df)
options(scipen = 999,digits = 0)
some.residuales <- valid.df$Spending[1:20] - tayko.lm.pred[1:20]
data.frame("Predicted" = tayko.lm.pred[1:20], "Actual" = valid.df$Spending[1:20], "residual" = some.residuales)
options(scipen = 999,digits = 3)
#use accuracy() to compute common accuracy measures
performance <- accuracy(tayko.lm.pred, valid.df$Spending)

tayko.lm.pred <- predict(tayko.lm, valid.df)
all.residuals <- valid.df$Spending - tayko.lm.pred
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")
#They do appear to follow a normal distribution. 
#A skewed distribution could make the predictive performance
#less consistent.