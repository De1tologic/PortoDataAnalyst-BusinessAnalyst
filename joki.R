library(psych)
library(ggplot2)
mydata<-read.csv("C:/Users/LENOVO/Downloads/DPBS.csv")
df<-data.frame(mydata$bedrooms,
               mydata$bathrooms,
               mydata$floors)
s1<-sum(mydata$bedrooms)
s2<-sum(mydata$bathrooms)
s3<-sum(mydata$floors)
values<-c(s1,s2,s3)
labels<-c("bedrooms","bathrooms","floors")
explode<-c(0,0.1,0)

plot(mydata, xlab = "x", ylab = "y")  

# Calculate the proportion
proportion <- values / sum(values)
# Create the data frame with values and labels
df_frame <- data.frame(values = values, labels = labels)
# Create the data frame with values, labels, and proportion
df_length <- data.frame(
  values = values[1:3], 
  labels = labels[1:3], 
  proportion = proportion[1:3])

# Calculate the proportion
proportion <- values / sum(values)

# Calculate the proportion
proportion <- values / sum(values)

# Create the data frame with values, labels, and proportion
df_2 <- data.frame(values = values, labels = labels, proportion = proportion)

# Create the pie chart with the proportion variable
pie_chart <- ggplot(df_2, aes(x = "", y = proportion, fill = labels)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  geom_text(aes(label = paste0(round(proportion * 100, 1), "%")), 
            position = position_stack(vjust = 0.8)) +
  theme(legend.position = "lefttop")

# Print the pie chart
print(pie_chart)

#-----histogram------#
# Create a histogram with 30 bins and labeled x-axis
layout(matrix(c(1,2,3),nrow = 1))
hist(mydata$bedrooms, breaks = 30, xlab = "Value of bedrooms", main = "Histogram for Bedrooms",col = "red")
hist(mydata$bathrooms,breaks = 30,xlab="value for bathrooms",main = "Histogram for bathrooms",col="blue")
hist(mydata$floors,breaks = 30,xlab="value for floors",main = "Histogram for floors",col = "green")


#------boxplot--------#
# Membuat box plot dengan label sumbu x dan judul plot

layout(matrix(c(1, 2, 3), nrow = 1))
boxplot(mydata$bedrooms, xlab = "bedrooms", main = "Box Plot for bedrooms",col = "red")
boxplot(mydata$bathrooms, xlab = "bathrooms", main = "Box Plot for bathrooms",col = "red")
boxplot(mydata$floors, xlab = "floors", main = "Box Plot for floors",col  ="red")


#-----summary-----#
describe(df)
dev.new()


# line plot
periode<- c(mydata$price)
plot(mydata$bedrooms, periode, type="l", col="blue", ylab="price")
par(new=TRUE)
plot(mydata$bathrooms,periode, type="l",col="red",  ylab="price")
par(new=TRUE)
plot( mydata$floors,periode, type="l",col="yellow",  ylab="price")


#bubble plot
layout(matrix(c(1, 2, 3), nrow = 1))
df2<-df<-data.frame(mydata$bedrooms,
                    mydata$bathrooms,
                    mydata$floors,
                    mydata$price)
ggplot(df2, aes(x = mydata$bedrooms, y = mydata$bathrooms, size = periode)) +
  geom_point(color="red") +
  xlab("bedrooms") +
  ylab("bathrooms") +
  ggtitle("Bubble Plot for bedrooms")
par(new=TRUE)
ggplot(df2, aes(x = mydata$bathrooms, y = mydata$price, size = periode)) +
  geom_point(color="red") +
  xlab("bedrooms") +
  ylab("price") +
  ggtitle("Bubble Plot for bathrooms")
par(new=TRUE)
ggplot(df2, aes(x = mydata$floors, y = mydata$price, size = periode)) +
  geom_point(color="red") +
  xlab("bedrooms") +
  ylab("price") +
  ggtitle("Bubble Plot for floors")


