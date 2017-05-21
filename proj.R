library(caret)
dat <- read.csv("pml-training.csv")
#testing <- read.csv("pml-testing.csv")



#NArate <- apply(trainingCl, 2, function(x) sum(is.na(x)))/nrow(trainingCl)
toomuchNA <- apply(training, 2, function(x) sum(is.na(x)))/nrow(training)>.97
training <- training[,!toomuchNA]

#partition
set.seed(300)
inTrain = createDataPartition(dat$classe, p = 3/4)[[1]]
training = dat[ inTrain,]
testing = dat[-inTrain,]

#create models

forpca <- preProcess(training, method = "pca", thresh = 0.95)
pcamodel <- train(training, method = "pca", thresh = 0.95)


rfmodel <- train(classe~., data = training,method = 'rf', do.trace = FALSE)
gbmmodel <- train(classe~., data = training, method = 'gbm')
ldamodel <- train(classe~., data = training, method = 'lda')
svmmodel

rfpred <- predict(rfmodel, testing)
gbmpred <- predict(gbmmodel, testing)
ldapred <- predict(ldamodel, testing)