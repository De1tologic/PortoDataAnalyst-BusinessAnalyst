#Activity 3:College application predictions
#upload ISLR and leaps packages
library(ISLR)   # To have access to the College data
library(leaps)  # To be able to use regsubsets
#Scatter plots of all variables
pairs(College[,1:10])
pairs(College[,c(2, 11:18)])

#Plotting the number of applications accepted 
# Apps vs accepted
plot(College$Accept, College$Apps)

# Apps vs private
boxplot(Apps ~ Private, data = College)

#splitting the data set into training and test set (50:50)
ndata <- nrow(College)
set.seed(57)
train <- sample(ndata, ndata*0.5)
CollegeTrain <- College[train, ]
CollegeTest <- College[-train, ]

#multiple linear regression
lmAll <- lm (Apps ~., data = CollegeTrain)
summary(lmAll)

#using test set to compute test error of the model
predAppsTest_All <- predict(lmAll, newdata= CollegeTest)
MSE_All <- mean((CollegeTest$Apps - predAppsTest_All)^2)
MSE_All 

#best subset selection to choose the best model for predicting college applications
bestSubset <-regsubsets(Apps ~.,data=CollegeTrain, nvmax =17)
bestSubsetSummary <- summary(bestSubset)

#number of variables in the model selected using C_p, Adjusted R-square and BIC criteria
best_adjr2 <- which.max(bestSubsetSummary$adjr2)
best_rss <- which.min(bestSubsetSummary$rss) #Residual sum of squares
best_cp <- which.min(bestSubsetSummary$cp)       #Mallow's CP
best_bic <- which.min(bestSubsetSummary$bic)     # BIC 

best_adjr2; best_rss; best_cp; best_bic

par(mfrow = c(1,1))
plot(bestSubsetSummary$rss, xlab = "Number of Variables", ylab = "RSS", 
     main = "No variables vs RSS", type = "l")
points(best_rss, bestSubsetSummary$rss[best_rss], col = "red", pch ="x")
plot(bestSubsetSummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", 
     main = "No variables vs Adjusted R^2", type = "l")
points(best_adjr2, bestSubsetSummary$adjr2[best_adjr2], col = "red", pch ="x")

plot(bestSubsetSummary$cp, xlab = "Number of Variables", ylab = "Mallows-CP", 
     main = "No variables vs Mallows-CP", type = "l")
points(best_cp, bestSubsetSummary$cp[best_cp], col = "red", pch ="x")

plot(bestSubsetSummary$bic, xlab = "Number of Variables", ylab = "BIC", 
     main = "No variables vs BIC", type = "l")
points(best_bic, bestSubsetSummary$bic[best_bic], col = "red", pch ="x")
