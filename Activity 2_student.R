#Activity 2
#Reading the csv file:
advData <- read.csv("Advertising.csv")
pairs(advData)

#Correlation matrix of the variables
cor(advData)

#Multiple linear regression to predict Sales using TV, Radio and Newspaper
lmSales_All <- lm(Sales ~ TV + Radio + Newspaper , data= advData)
summary(lmSales_All)

