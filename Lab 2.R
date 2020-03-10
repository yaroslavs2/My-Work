setwd("C:\\Users\\user\\Documents\\BIS 348")

##data summary and visualization
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab
# Practice showing different subsets of the data
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column

# compute mean, standard dev., min, max, median, length, and missing values of CRIM
mean(housing.df$TOTAL.VALUE)
sd(housing.df$TOTAL.VALUE)
min(housing.df$TOTAL.VALUE)
max(housing.df$TOTAL.VALUE)
median(housing.df$TOTAL.VALUE)
length(housing.df$TOTAL.VALUE)
# find the number of missing values of variable CRIM
sum(is.na(housing.df$TOTAL.VALUE))

summary(housing.df)  # find summary statistics for each column
round(cor(housing.df[ , -14]),2)

##=============================================
## Boston housing data
housing.df <- read.csv("BostonHousing.csv", header = TRUE)
## scatter plot with axes names
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab = "MDEV", ylab = "LSTAT")


##bar chart of CHAS vs. mean MEDV
# compute mean MEDV per CHAS = (0, 1)
data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN = mean) #Pivot Table
names(data.for.plot) <- c("CHAS", "MeanMEDV")
barplot(data.for.plot$MeanMEDV,  names.arg = data.for.plot$CHAS,
        xlab = "CHAS", ylab = "Avg. MEDV")

## barchart of CHAS vs. % CAT.MEDV
data.for.plot <- aggregate(housing.df$CAT..MEDV, by = list(housing.df$CHAS), FUN = mean)
names(data.for.plot) <- c("CHAS", "MeanCATMEDV")
barplot(data.for.plot$MeanCATMEDV * 100,  names.arg = data.for.plot$CHAS,
        xlab = "CHAS", ylab = "% of CAT.MEDV")

## histogram of MEDV
hist(housing.df$MEDV, xlab= "MEDV")
hist(housing.df$MEDV, xlab= "MEDV", main="Density of MEDV", freq = FALSE)

##boxplot
boxplot(housing.df$MEDV, ylab = "MEDV")

## boxplot of MEDV for different values of CHAS
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab = "MEDV")


## side-by-side boxplots
# use par() to split the plots into panels.
par(mfcol = c(1, 4))
boxplot(housing.df$NOX ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
boxplot(housing.df$LSTAT ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
boxplot(housing.df$PTRATIO ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
boxplot(housing.df$INDUS ~ housing.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")


## simple heatmap of correlations (without values)
heatmap(cor(housing.df), Rowv = NA, Colv = NA)

par(mfcol = c(1, 1))
## color plot
par(xpd=TRUE) # allow legend to be displayed outside of plot area
plot(housing.df$NOX ~ housing.df$LSTAT, ylab = "NOX", xlab = "LSTAT",
     col = ifelse(housing.df$CAT..MEDV == 1, "red", "blue"))
# add legend outside of plotting area
# In legend() use argument inset =  to control the location of the legend relative to the plot.
legend("topleft", inset=c(0, -0.1),
       legend = c("CAT.MEDV = 1", "CAT.MEDV = 0"), col = c("black", "gray"),
       pch = 1, cex = 0.5) #pch decides shape


## simple plot
# use plot() to generate a matrix of 4X4 panels with variable name on the diagonal,
# and scatter plots in the remaining panels.
plot(housing.df[, c(1, 3, 12, 13)])

##===================================
#lab assignment:
# using WestRoxbury.csv, generate:
#1. scatterplot between TOTAL.VALUE and LIVING.AREA
#2. Bar chart of TOTAL.VALUE by FIREPLACE
#3. Histogram of TOTAL.VALUE
#4. Heatmap of correlations of all numeric variables
#5. Matrix plots of TOTAL.VALUE, FLOORS, ROOMS, LIVING.AREA, and FIREPLACE 

housing.df <- read.csv("WestRoxbury.csv", header = TRUE)
plot(housing.df$TOTAL.VALUE ~ housing.df$LIVING.AREA, xlab = "Total value", ylab = "Living Area")

data.for.plot <- aggregate(housing.df$TOTAL.VALUE, by = list(housing.df$FIREPLACE), FUN = mean)
names(data.for.plot) <- c("FirePlace", "Total Value")
barplot(data.for.plot$`Total Value`, names.arg = data.for.plot$FIREPLACE,
        xlab = "FirePlace", ylab = "Total Value")

hist(housing.df$TOTAL.VALUE, xlab= "Total Value")
hist(housing.df$TOTAL.VALUE, xlab= "Total Value", main="Total Value", freq = FALSE)

heatmap(cor(housing.df[, c(1:3, 5:13)]), Rowv = NA, Colv = NA)

plot(housing.df[, c(1, 6:8, 13)])

