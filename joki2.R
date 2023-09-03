library(moments)
library(dplyr)
library(psych)
#read data
data<-read.csv("https://raw.githubusercontent.com/dat-analytics/data_assess_1_t2_2023/main/z5397591_z5397591-Assessment1Data.csv", encoding="UTF-8")
data
#-------------histogram--------------------#
par(mfrow=c(3,2))
#------ShoppingCart-------------------#
ShoppingAPP = subset(data, C_ShoppingCart =="App")
ShoppingBrowser = subset(data, C_ShoppingCart =="Browser")
hist(ShoppingAPP$C_EquipmentSpend,col = "red",main = "Equipments by App")
hist(ShoppingBrowser$C_EquipmentSpend,col = "blue",main = "Equipments by Browser")
hist(ShoppingAPP$C_ApparelSpend,col = "yellow",main = "Apparel by App")
hist(ShoppingBrowser$C_ApparelSpend,col = "green",main = "Apparel by Brwoser")
hist(ShoppingAPP$C_FootwearSpend,col = "brown",main = "Footwear by App")
hist(ShoppingBrowser$C_FootwearSpend,col = "purple",main = "Footwear by Browser")

#deskriptif statistik#
summary(ShoppingAPP$C_EquipmentSpend)
summary(ShoppingBrowser$C_EquipmentSpend)
summary(ShoppingAPP$C_ApparelSpend)
summary(ShoppingBrowser$C_ApparelSpend)
summary(ShoppingAPP$C_FootwearSpend)
summary(ShoppingBrowser$C_FootwearSpend)

#boxplot
boxplot(ShoppingAPP$C_EquipmentSpend, col = "red",main="eqipment by app")
boxplot.stats(ShoppingAPP$C_EquipmentSpend)$out
boxplot(ShoppingBrowser$C_EquipmentSpend,col="blue",main="equipment by browser")
boxplot.stats(ShoppingBrowser$C_EquipmentSpend)$out
boxplot(ShoppingAPP$C_ApparelSpend,col = "yellow", main="apparel by app")
boxplot.stats(ShoppingAPP$C_ApparelSpend)$out
boxplot(ShoppingBrowser$C_ApparelSpend,col = "green", main="apparel by browser")
boxplot.stats(ShoppingBrowser$C_ApparelSpend)$out
boxplot(ShoppingAPP$C_FootwearSpend,col = "brown", main="footwear by app")
boxplot.stats(ShoppingAPP$C_FootwearSpend)$out
boxplot(ShoppingBrowser$C_FootwearSpend,col = "purple", main="footwear by browser")
boxplot.stats(ShoppingBrowser$C_FootwearSpend)$out
lot(perthhouseprice,las=1,col="yellow",ylim=c(0,2000),names.arg=colnames,xlab = "Number of Bed Rooms", ylab = "Mean Price thousands $",        main="Perth's average housing price by Bed Rooms")

#regression
model<-lm(ShoppingAPP$C_Age~ShoppingAPP$C_EquipmentSpend+ShoppingAPP$C_ApparelSpend+ShoppingAPP$C_FootwearSpend, data = data)
fit<-lm(model)
summary(fit)#summary result
scoef(fit) #vie coefficient estimate
y=fitted(fit) #view fitted values
y
x=residuals(fit) #view ressiduals
x
plot(x, main = "qq residual",type = c("qq"))
#plot
plot(x,y)
plot(fitted(fit))
plot.residuals(residuals(fit))

