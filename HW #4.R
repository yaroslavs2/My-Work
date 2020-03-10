setwd("C:\\Users\\user\\Documents\\BIS 348")
rm(list=ls())

car.df <- read.csv("ToyotaCorolla.csv")

#partition
train.index <- sample(c(1:dim(car.df)[1]), dim(car.df)[1]*0.6)
train.df <- car.df[train.index, ]
valid.df <- car.df[-train.index, ]

# normalize
library(caret)
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)

car.df$Diesel <- ifelse(car.df$Fuel_Type == "Diesel", 1, 0)
car.df$CNG <- ifelse(car.df$Fuel_Type == "CNG", 1, 0)

library(neuralnet)

#2 nodes, 1 hidden layer
car.nn <- neuralnet(Price ~ Age_08_04 + KM + Diesel + CNG + HP + Automatic +
                    Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period +
                    Airco + Automatic_airco + CD_Player + Powered_Windows +
                    Sport_Model + Tow_Bar, data = car.df, linear.output = F, hidden = 2, threshold = 0.5)

plot(car.nn, rep = "best")

car.nn.pred <- compute(car.nn, data.frame(car.df$Age_08_04, car.df$KM, car.df$Diesel, car.df$CNG, car.df$HP, car.df$Automatic,
                                          car.df$Doors, car.df$Quarterly_Tax, car.df$Mfr_Guarantee, car.df$Guarantee_Period,
                                          car.df$Airco, car.df$Automatic_airco, car.df$CD_Player, car.df$Powered_Windows,
                                          car.df$Sport_Model, car.df$Tow_Bar))

library(forecast)
accuracy(as.vector(car.nn.pred$net.result), valid.norm.df$Price)
#validation RMSE = 0.7866
accuracy(as.vector(car.nn.pred$net.result), train.norm.df$Price)
#training RMSE = 0.7822

#5 nodes, 1 hidden layer
car.nn.2 <- neuralnet(Price ~ Age_08_04 + KM + Diesel + CNG + HP + Automatic +
                        Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period +
                        Airco + Automatic_airco + CD_Player + Powered_Windows +
                        Sport_Model + Tow_Bar, data = car.df, linear.output = F, hidden = 5, threshold = 0.5)

plot(car.nn.2, rep = "best")

car.nn.pred.2 <- compute(car.nn.2, data.frame(car.df$Age_08_04, car.df$KM, car.df$Diesel, car.df$CNG, car.df$HP, car.df$Automatic,
                                          car.df$Doors, car.df$Quarterly_Tax, car.df$Mfr_Guarantee, car.df$Guarantee_Period,
                                          car.df$Airco, car.df$Automatic_airco, car.df$CD_Player, car.df$Powered_Windows,
                                          car.df$Sport_Model, car.df$Tow_Bar))

accuracy(as.vector(car.nn.pred.2$net.result), valid.norm.df$Price)
#validation RMSE: .7836
accuracy(as.vector(car.nn.pred.2$net.result), train.norm.df$Price)
#training RMSE: .7865

#5 nodes in each of the 2 hidden layers
car.nn.3 <- neuralnet(Price ~ Age_08_04 + KM + Diesel + CNG + HP + Automatic +
                      Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period +
                      Airco + Automatic_airco + CD_Player + Powered_Windows +
                      Sport_Model + Tow_Bar, data = car.df, linear.output = F, hidden = c(5,5), threshold = 0.5)

plot(car.nn.3, rep = "best")

car.nn.pred.3 <- compute(car.nn.3, data.frame(car.df$Age_08_04, car.df$KM, car.df$Diesel, car.df$CNG, car.df$HP, car.df$Automatic,
                                              car.df$Doors, car.df$Quarterly_Tax, car.df$Mfr_Guarantee, car.df$Guarantee_Period,
                                              car.df$Airco, car.df$Automatic_airco, car.df$CD_Player, car.df$Powered_Windows,
                                              car.df$Sport_Model, car.df$Tow_Bar))

accuracy(as.vector(car.nn.pred.3$net.result), valid.norm.df$Price)
#validation RMSE: .7836
accuracy(as.vector(car.nn.pred.3$net.result), train.norm.df$Price)
#training RMSE: .7865

#validation RMSE decreases as nodes increase, then flattens as 
#hidden layers increase

#training RMSE increases as nodes increase, then flattens as
#hidden layers increase

#ideal number of nodes/layers depends on which data (training or validation)
#you're using. Increase nodes if using validation data, decrease nodes if
#using training data. Number of hidden layers doesn't seem to matter.
