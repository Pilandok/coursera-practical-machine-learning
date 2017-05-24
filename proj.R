install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
library(caret)
library(randomForest)
library(e1071)
testing <- read.csv("pml-testing.csv")
dat <- read.csv("pml-training.csv")


#NArate <- apply(trainingCl, 2, function(x) sum(is.na(x)))/nrow(trainingCl)
toomuchNA <- apply(dat, 2, function(x) sum(is.na(x)))/nrow(dat)>.96
dat <- dat[,!toomuchNA]
nzvval <- nearZeroVar(dat, saveMetrics= TRUE)$nzv
dat <- dat[,!nzvval]
dat <- dat[,7:59]
dat <- dat[,-6]




#partition
set.seed(300)
inTrain = createDataPartition(dat$classe, p = 3/4)[[1]]
training = dat[ inTrain,]
testing = dat[-inTrain,]

#create models

forpca <- preProcess(training, method = "pca", thresh = 0.95)
pcamodel <- train(classe~., data = training, method = "pca", thresh = 0.95)


rfmodel <- train(classe~., data = training,method = 'rf', do.trace = FALSE)
rfmodel <- randomForest(classe~., data = training)
gbmmodel <- train(classe~., data = training, method = 'gbm')
ldamodel <- train(classe~., data = training, method = 'lda')
rpartmodel <- train(classe~., data = training, method = 'rpart')

rfpred <- predict(rfmodel, testing)
gbmpred <- predict(gbmmodel, testing)
ldapred <- predict(ldamodel, testing)
rpartpred <- predict(rpartmodel, testing)
#rfaccuracy 0.997553
#gbmAccuracy 0.9624796
#ldaAccuracy 0.7065661
confusionMatrix(testing$classe, rfpred)$overall["Accuracy"]
confusionMatrix(testing$classe, gbmpred)$overall["Accuracy"]
confusionMatrix(testing$classe, ldapred)$overall["Accuracy"]
confusionMatrix(testing$classe, rpartpred)$overall["Accuracy"]

#stack the predictions

combpred <- data.frame(classe = testing$classe, rfpred, gbmpred, ldapred)
rfcombmod <- train(classe~., data = combpred, method = 'rf')
rfcombpred <- predict(rfcombmod, combpred)
rfcombpred <- predict(rfcombmod, combpred)
confusionMatrix(testing$classe, rfcombpred)$overall["Accuracy"]
#Accuracy:  0.997553
