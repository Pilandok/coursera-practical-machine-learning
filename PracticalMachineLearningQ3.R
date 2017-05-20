library(AppliedPredictiveModeling)
library(caret)
data(segmentationOriginal)

set.seed(125)
training = segmentationOriginal[segmentationOriginal$Case=="Train",-2]
testing = segmentationOriginal[segmentationOriginal$Case=="Test",-2]
rpart(Class~., data=training) 

model <- train(Class~., method = "rpart", data = training)

testA <- segmentationOriginal[0,]
testA[1,c("TotalIntenCh2", "FiberWidthCh1", "PerimStatusCh1")] <- c(23000, 10, 2)
testA[2,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(50000, 10, 100)
testA[3,c("TotalIntenCh2", "FiberWidthCh1", "VarIntenCh4")] <- c(57000, 8, 100)
testA[4,c("TotalIntenCh2", "VarIntenCh4", "PerimStatusCh1")] <- c(8, 100, 2)
predict(model$finalModel, testA)

Q3
library(pgmm)
data(olive)
olive = olive[,-1]
olive.model <- train(Area~., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))

predict(olive.model$finalModel, newdata)

Q4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

SAheart$chd <- as.factor(SAheart$chd)
set.seed(8484)
saheart.model <- train(chd~age+alcohol+obesity+tobacco+typea+ldl, method = "glm", family = "binomial", data = trainSA)

testpredict <- predict(saheart.model, testSA)
trainpredict <- predict(saheart.model, trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(testSA$chd, testpredict)
missClass(trainSA$chd, trainpredict)

Q5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
vowel.model <- randomForest(y~., data = vowel.train)
order(varImp(vowel.model), decreasing = TRUE)
