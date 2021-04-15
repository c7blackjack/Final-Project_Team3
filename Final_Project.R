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

boxplot(data$grade)
##outliers >9 and less than 6


#checking and removing outliers and putting back into data2
data2 <- subset(data2, bedrooms <=5 
                       & bedrooms >= 2 
                       & sqft_living < 3500 
                       & bathrooms < 3.5 
                       & price <= 900000
                       & sqft_lot <=13000
                       & grade <= 9 
                       & grade >=6 )
##Making a scaled version of data2 to use in linear regression models
data2.scaled <- scale(data2)
head(data2.scaled)
boxplot(data2$sqft_lot)
boxplot(data2$bathrooms)
boxplot(data2$bedrooms)
boxplot(data2$sqft_living)
boxplot(data2$price)
boxplot(data$grade)

##data$bathrooms <- data$bathrooms(is.na(data$bathrooms))
ggplot(data2, aes(x=sqft_living, y=price, color = bedrooms)) + geom_point()
ggplot(data2, aes(x = sqft_living, y = (price/10000), color = grade)) + geom_point()
data.scaled.price <- scale(price)
data.scaled.sqft_living <- scale(sqft_living)
model1 <- lm(price ~ sqft_living + bedrooms, data = data2.scaled)
summary(model1)

#testing correlation between variables

# correlation between price and sqft_living
cor_1 <- cor.test(data$price, data$sqft_living, 
                  method = "pearson")
cor_1
sprintf("The correlation Index between price and sqft living is %f" , cor_1$estimate)

#Both have moderate correlation 

#correlation  Index between price and sqft_lot
cor_2 <- cor.test(data$price, data$sqft_lot, 
                  method = "pearson")
cor_2
sprintf("The correlation Index between price and sqft lot is %f" , cor_2$estimate)

#very less correlation almost 0.

# Now testing correlation between price and condition.
cor_3 <- cor.test(data$price, data$condition, 
                  method = "pearson")
cor_3
sprintf("The correlation Index between price and condition is %f" , cor_3$estimate)

#correlation between price and yr_built.
cor_4 <- cor.test(data$price, data$yr_built, 
                  method = "pearson")
cor_4
sprintf("The correlation Index between price and yr_built  is %f" , cor_4$estimate)
ggplot(data, aes(price, yr_built, color= bathrooms)) + geom_point()
ggplot(data, aes(price, yr_renovated, color= bathrooms)) + geom_point()

#Boxplot to check outliers in grade 
#boxplot(data$grade)


#correlation  between price and grade
cor_5 <- cor.test(data$price, data$grade, 
                  method = "pearson")
cor_5
ggplot(data, aes(price, grade , color= bedrooms)) + geom_point()
#boxplot(data$grade)


