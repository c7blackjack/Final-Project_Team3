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
data2 <- subset(data, select = c(price, bedrooms, bathrooms, sqft_living, sqft_lot, waterfront, view))
head(data2)
#missing data is filled in.
##Moving forward we will look at the outliers of the main categories that we will be inspecting.
boxplot(data2$bathrooms)
##bathroom outlier > 4
boxplot(data2$bedrooms)
##bedroom outliers <2 and >5
boxplot(data2$sqft_living)
##sqft_living outliers > 4000
boxplot(data2$price)
##outliers > 1000000
boxplot(data2$sqft_lot)


#checking and removing outliers and putting back into data2
data2 <- subset(data2, bedrooms <=5 
                       & bedrooms >= 2 
                       & sqft_living < 3500 
                       & bathrooms < 3.5 
                       & price <= 900000
                       & sqft_lot <=13000)
##Making a scaled version of data2 to use in linear regression models
data2.scaled <- scale(data2)
head(data2.scaled)
boxplot(data2$sqft_lot)
boxplot(data2$bathrooms)
boxplot(data2$bedrooms)
boxplot(data2$sqft_living)
boxplot(data2$price)

##data$bathrooms <- data$bathrooms(is.na(data$bathrooms))
ggplot(data2, aes(x=sqft_living, y=price, color = bedrooms)) + geom_point()
ggplot(data2, aes(x = sqft_living, y = (price/10000), color = grade)) + geom_point()
data.scaled.price <- scale(price)
data.scaled.sqft_living <- scale(sqft_living)
model1 <- lm(price ~ sqft_living + bedrooms, data = data2.scaled)
summary(model1)
