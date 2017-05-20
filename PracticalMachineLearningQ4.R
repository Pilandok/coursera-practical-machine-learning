#Q1 Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit (1) a random forest predictor relating the factor variable y to the remaining variables and (2) a boosted predictor using the "gbm" method. Fit these both with the train() command in the caret package.

library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)

rfmodel <- train(y~., data = vowel.train, method = 'rf')
boostmodel <- train(y~., data = vowel.train, method = 'gbm')
rfpredict <- predict(rfmodel,vowel.test)
rfboost <- predict(boostmodel,vowel.test)

#What are the accuracies for the two approaches on the test data set? 
confusionMatrix(vowel.test$y, rfpredict)$overall["Accuracy"]
confusionMatrix(vowel.test$y, rfboost)$overall["Accuracy"]

#What is the accuracy among the test set samples where the two methods agree?

whereagree <- rfpredict==rfboost
sum(rfpredict[whereagree]==vowel.test$y[whereagree])/sum(whereagree)

#Q2

#Load the Alzheimer's data using the following commands
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#Set the seed to 62433 and predict diagnosis with all the other variables using a random forest ("rf"), boosted trees ("gbm") and linear discriminant analysis ("lda") model. 
set.seed(62433)
rfmodel <- train(diagnosis~., data = training, method = 'rf')
gbmmodel <- train(diagnosis~., data = training, method = 'gbm')
ldamodel <- train(diagnosis~., data = training, method = 'lda')
rfpred <- predict(rfmodel, testing)
gbmpred <- predict(gbmmodel, testing)
ldapred <- predict(ldamodel, testing)

#Stack the predictions together using random forests ("rf"). 
combpred <- data.frame(diagnosis = testing$diagnosis, rfpred, gbmpred, ldapred)
rfcombmod <- train(diagnosis~., data = combpred, method = 'rf')
rfcombpred <- predict(rfcombmod, combpred)

#What is the resulting accuracy on the test set? 
#Is it better or worse than each of the individual predictions?
confusionMatrix(rfpred, testing$diagnosis)$overall["Accuracy"]
confusionMatrix(gbmpred, testing$diagnosis)$overall["Accuracy"]
confusionMatrix(ldapred, testing$diagnosis)$overall["Accuracy"]
confusionMatrix(rfcombpred, testing$diagnosis)$overall["Accuracy"]

#Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 233 and fit a lasso model to predict Compressive Strength.
set.seed(233)
lassomodel <- train(CompressiveStrength~., data = training, method = 'lasso')
#Which variable is the last coefficient to be set to zero as the penalty increases? (Hint: it may be useful to look up ?plot.enet).
library(elasticnet)
plot.enet(lassomodel$finalModel, xvar = "penalty", use.color = TRUE)

#Q4
#Load the data on the number of visitors to the instructors blog
#https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv

library(lubridate) # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
#Fit a model using the bats() function in the forecast package to the training time series. Then forecast this model for the remaining time points. For how many of the testing points is the true value within the 95% prediction interval bounds?

batsmodel <- bats(tstrain)
fcast <- forecast(batsmodel, level = 95, h = nrow(testing))
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / nrow(testing)

#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
svmmodel <- svm(CompressiveStrength ~ ., data = training)
svmpred <- predict(svmmodel, testing)
library(forecast)
accuracy(svmpred, testing$CompressiveStrength)


