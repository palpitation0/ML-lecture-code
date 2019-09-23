setwd()
source('RFunctions-1.R')

df=read.csv("Boston.csv",stringsAsFactors = FALSE)
head(df)
names(df) <-c("no","crimeRate","zone","indus","charles","nox","rooms","age",
              "distances","highways","tax","teacherRatio","color","status","cost")
head(df)

df1 <- df %>% dplyr::select("crimeRate","zone","indus","charles","nox","rooms","age",
                            "distances","highways","tax","teacherRatio","color","status","cost")
dim(df1)
# Linear Regression fit
fit <- lm(cost~. ,data=df1)
summary(fit)

library(leaps)
bestFit=regsubsets(cost~.,df1,nvmax=13)
bfSummary=summary(bestFit)

# Plot the Residual Sum of Squares vs number of variables 
plot(bfSummary$rss,xlab="Number of Variables",ylab="RSS",type="l",main="Best fit RSS vs No of features")
# Get the index of the minimum value
a=which.min(bfSummary$rss)
# Mark this in red
points(a,bfSummary$rss[a],col="red",cex=2,pch=20)

# Plot the CP statistic vs Number of variables
plot(bfSummary$cp,xlab="Number of Variables",ylab="Cp",type='l',main="Best fit Cp vs No of features")
# Find the lowest CP value
b=which.min(bfSummary$cp)
# Mark this in red
points(b,bfSummary$cp[b],col="red",cex=2,pch=20)

coef(bestFit,b)
#  Plot the BIC value
plot(bfSummary$bic,xlab="Number of Variables",ylab="BIC",type='l',main="Best fit BIC vs No of Features")
# Find and mark the min value
c=which.min(bfSummary$bic)
points(c,bfSummary$bic[c],col="red",cex=2,pch=20)

# R has some other good plots for best fit
plot(bestFit,scale="r2",main="Rsquared vs No Features")
plot(bestFit,scale="Cp",main="Cp vs NoFeatures")
plot(bestFit,scale="bic",main="BIC vs Features")

##### Ridge regression #####

library(glmnet)
library(dplyr)

# Set X and y as matrices
X=as.matrix(df1[,1:13])
y=df1$cost

# Fit a Ridge model
fitRidge <-glmnet(X,y,alpha=0)

#Plot the model where the coefficient shrinkage is plotted vs log lambda
plot(fitRidge,xvar="lambda",label=TRUE,main= "Ridge regression coefficient shrikage vs log lambda")

# Compute the cross validation error
cvRidge=cv.glmnet(X,y,alpha=0)

#Plot the cross validation error
plot(cvRidge, main="Ridge regression Cross Validation Error (10 fold)")

