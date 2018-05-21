#Data set: education.csv
#Create an OLS regression model to predict the expenditure on public education
#(expend) using the following predictors: urban, income and teen. Validate your model
#with the validation set approach. (Retain 30-35 cases for the training set and the others
 #for the test set.)

educ <-read.csv('education.csv')
str(educ)

library(psych)
pairs.panels(educ)

head(educ)

#Removing the 1st column
educ$X <-NULL

#Check for missisng values 
sapply(educ, function(x) sum(is.na(x)))
ols_model <-lm(expend~urban+income+teen, data=educ)
summary(ols_model)

#urban is not significant 
ols_model1 <-lm(expend~income+teen, data=educ)
summary(ols_model1)

#Only a slightly better adjusted R squared

pred <- predict(ols_model)#calculate mean squared error
mse <-mean((educ$expend-pred)^2)
mse

#Validating the OLS Model

n <-sample(50, 15)
n

#Create the training set 
ols_train <-educ[n,]

ols_test <-educ[-n,]

#Fit the data using the training set 
ols_fit <- lm(expend~urban+income+teen, data=ols_train)
#compute training set mse 
pred <-predict(ols_fit)

#Average squared difference
train_mse <- mean((ols_train$expend-pred)^2)
train_mse

pred2 <-predict(ols_fit, ols_test)
head(pred2)

#Average squared difference
test_mse <- mean((ols_test$expend-pred2)^2)
test_mse

#K-Fold Cross Validation 
library(DAAG)
olsfit2 <- cv.lm(data=educ, form.lm = formula(expend~urban+income+teen), m=10)
# m = The number of folds

##Predicted is the predicted value of the data set without CV
##cvpred is the average predicted value usiing k-fold validation




