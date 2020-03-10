setwd("C:\\Users\\user\\Documents\\BIS 348")

supermarket.df <- read.csv("organics.csv", header = TRUE)

#Question 2

dim(supermarket.df) 
length(supermarket.df)
head(supermarket.df)
str(supermarket.df)
summary(supermarket.df)
cor(supermarket.df[, c(10, 12:13)])

#Question 3/Data Partition

selected.df <- supermarket.df 

train.index <- sample(c(1:22223), 22223 * 0.6 )
train.df <- selected.df[train.index, ]
valid.df <- selected.df[-train.index, ]
