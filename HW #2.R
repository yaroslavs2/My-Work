setwd("C:\\Users\\user\\Documents\\BIS 348")

car.df <- read.csv("ToyotaCorolla.csv", header = TRUE)

xtotal <- model.matrix(~ 0 + Fuel_Type, data = car.df) #dummy variables for fuel_type
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)

summary(car.df$Price)
summary(car.df$Age_08_04)
summary(car.df$KM)
summary(car.df$HP)
summary(car.df$Fuel_Type)

hist(car.df$Price, xlab = "Price", main = "Price of Car", freq = FALSE)
hist(car.df$Age_08_04, xlab = "Age", main = "Age of Car", freq = FALSE)
hist(car.df$KM, xlab = "Kilometers", main = "Mileage", freq = FALSE)
hist(car.df$HP, xlab = "HP", main = "Horsepower", freq = FALSE)

data.for.plot <- aggregate(car.df$Price, by = list(car.df$Met_Color), FUN = mean)
names(data.for.plot) <- c("Met_Color=1", "Met_Color=0")
barplot(data.for.plot$`Met_Color=0`,  names.arg = data.for.plot$`Met_Color=1`,
        xlab = "Color", ylab = "Price")

boxplot(car.df$Price, ylab = "Price")
boxplot(car.df$Age_08_04, ylab = "Age")
boxplot(car.df$KM, ylab = "Mileage")
boxplot(car.df$HP, ylab = "Horsepower")

boxplot(car.df$Price ~ car.df$Met_Color, xlab = "Color", ylab = "Price")
