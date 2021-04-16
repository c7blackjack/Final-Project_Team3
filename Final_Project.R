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
data2 <- subset(data, select = c(price, bedrooms, bathrooms, sqft_living, sqft_lot, waterfront, view, grade))
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

boxplot(data2$grade)
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
boxplot(data2$grade)

##data$bathrooms <- data$bathrooms(is.na(data$bathrooms))
ggplot(data2, aes(x=sqft_living, y=price, color = bedrooms)) + geom_point()
ggplot(data2, aes(x = sqft_living, y = (price/10000), color = grade)) + geom_point()
data.scaled.price <- scale(price)
data.scaled.sqft_living <- scale(sqft_living)
model1 <- lm(price ~ sqft_living + bedrooms, data = data2.scaled)
summary(model1)

#testing correlation between variables

# correlation between price and sqft_living
cor_1 <- cor.test(data2$price, data2$sqft_living, 
                  method = "pearson")
cor_1
sprintf("The correlation Index between price and sqft living is %f" , cor_1$estimate)

#Both have moderate correlation 

#correlation  Index between price and sqft_lot
cor_2 <- cor.test(data2$price, data2$sqft_lot, 
                  method = "pearson")
cor_2
sprintf("The correlation Index between price and sqft lot is %f" , cor_2$estimate)

#very less correlation almost 0.

# Now testing correlation between price and condition.
cor_3 <- cor.test(data2$price, data2$condition, 
                  method = "pearson")
cor_3
sprintf("The correlation Index between price and condition is %f" , cor_3$estimate)

#correlation between price and yr_built.
cor_4 <- cor.test(data2$price, data2$yr_built, 
                  method = "pearson")
cor_4
sprintf("The correlation Index between price and yr_built  is %f" , cor_4$estimate)
ggplot(data, aes(price, yr_built, color= bathrooms)) + geom_point()
ggplot(data, aes(price, yr_renovated, color= bathrooms)) + geom_point()

#Boxplot to check outliers in grade 
#boxplot(data$grade)


#correlation  between price and grade
cor_5 <- cor.test(data2$price, data2$grade, 
                  method = "pearson")
cor_5
ggplot(data2, aes(price, grade , color= bedrooms)) + geom_point()
#boxplot(data$grade)

#linear regression of price with one predictor variable sqft lot
regr_1= lm(price ~ sqft_lot, data = data2)
summary(regr_1)

#linear regression with two predictor variables i.e grade and sqft_lot
regr_2= lm(price ~ sqft_lot + grade , data = data2)
summary(regr_2)
unique(data2$grade)
as.data.frame(table(data2$grade))
#subsetting data with graede 6
data_g6 <-  data2[ which(data2$grade==6), ]
head(data_g6)
#finding correlation between price and sqftliving of grade 6 data
corr_g6<- cor.test(data_g6$price, data_g6$sqft_living, 
                   method = "pearson")
corr_g6

#subsetting data with grade 7
data_g7 <- data2[ which(data2$grade==7), ]
head(data_g7)
#finding correlation between price and sqftliving of grade 7 data
corr_g7<- cor.test(data_g7$price, data_g7$sqft_living, 
               method = "pearson")
corr_g7

#subsetting data woth grade 8
data_g8 <- data2[ which(data2$grade==8), ]
head(data_g8)
#finding correlation between price and sqftliving of grade 8 data
corr_g8<- cor.test(data_g8$price, data_g8$sqft_living, 
                   method = "pearson")
corr_g8
#subsetting data woth grade 9
data_g9 <- data2[ which(data2$grade==9), ]
head(data_g9)
#finding correlation between price and sqftliving of grade 8 data
corr_g9<- cor.test(data_g9$price, data_g9$sqft_living, 
                   method = "pearson")
corr_g9















