## Hi Team! This is a test to see if we can use github to collaborate.
library(pacman)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)


data <- read.csv("house_sales.csv")
attach(data)
#Convert date column using lubridate package
data$date <- parse_date_time(data[,2], orders = "%Y%m%%d%H%M%S")
head(data)

## checking where empty values are located.
colSums(is.na(data))
#fill in sqft_living with basic addition of sqft_above and sqft_basement
data$sqft_living <- data$sqft_above + data$sqft_basement
head(data)
#fill in bedrooms and bathrooms
data$bedrooms[which(is.na(data$bedrooms))] <- 2.5
data$bathrooms[which(is.na(data$bathrooms))] <- 2
data$sqft_lot[which(is.na(data$sqft_lot))] <- 6500
##Use data2 from her because it has less columns. I left data alone to retain its columns

#Removing date column to check correlation table
data.cor <- subset(data, select = -c(date))
head(data.cor)
cor(data.cor)
#Columns with high correlation values with grade as dependent variable:
#price, bathrooms, sqft_living, floors,sqft_above, yr_built, sqft_living15
data2 <- subset(data, select = c(grade, price, bathrooms, sqft_living, floors, yr_built, sqft_living15))
head(data2)
#missing data is filled in.
##Moving forward we will look at the outliers of the main categories that we will be inspecting.
boxplot(data2$bathrooms)
##bathroom outlier > 4
boxplot(data2$floors)
##no outliers
boxplot(data2$yr_built)
##no outliers
boxplot(data2$sqft_living)
##sqft_living outliers > 4000
boxplot(data2$sqft_living15)
##sqft_living outliers > 3700
boxplot(data2$price)
##outliers > 1000000
boxplot(data2$sqft_lot)


#checking and removing outliers and putting back into data2
data2 <- subset(data2, sqft_living15 < 3750
                       & sqft_living < 3500 
                       & bathrooms < 3.5 
                       & price <= 900000)
##Making a scaled version of data2 to use in linear regression models
data2.scaled <- scale(data2)
cor(data2.scaled)
data2.df <- as.data.frame(data2.scaled)
##making a multiple linear regression model 
m1 <- lm(grade ~ price + sqft_living + yr_built + sqft_living15, data = data2)
m2 <- lm(price ~ grade + sqft_living + yr_built + sqft_living15, data = data2)
summary(m1)
summary(m2)


##3dscatterplot comparing yr_built,price and sqft_living colored by grade
require(scatterplot3d)
library(rgl)
head(data2.df)
##plot3d(data2$price, data2$sqft_living, data2$yr_built, col = data2$grade)
scatterplot3d(data2$price, data2$sqft_living, data2$yr_built, 
              color = data2$grade,
              type="p",
              zlab = "Year Built",
              ylab = "Square Feet",
              xlab = "Price",
              legend3d(color)
              )

?scatterplot3d
