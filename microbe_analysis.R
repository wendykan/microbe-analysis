
setwd('/Users//kanc/Documents/classes//datasci_course_materials/assignment5/')
data = read.csv('seaflow_21min.csv')
summary(data)

# split into train and test dataset
library(caret)
trainIndex = createDataPartition(data$file_id,p=.5,list=FALSE)
trainData=data[trainIndex,]
testData=data[-trainIndex,]

# plot
library(ggplot2)
p=ggplot(data,aes(x=data$chl_small,y=data$pe))
p+geom_point(aes(colour = factor(data$pop)), size=1)

# single tree
library(rpart)
fol = formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small)
model <- rpart(fol, method="class", data=trainData)
test_pred = predict(model,testData,type="class")
sum(test_pred==testData$pop) / length(test_pred)

# RF
library(randomForest)
model = randomForest(fol, data=trainData)
test_pred = predict(model,testData,type="class")
sum(test_pred==testData$pop) / length(test_pred)
importance(model)

# svm
library(e1071)
model <- svm(fol, data=trainData)
test_pred = predict(model,testData,type="class")
sum(test_pred==testData$pop) / length(test_pred)

# confusion matrix
table(pred = test_pred, true = testData$pop)


# clean up
data = data[data$file_id != 208,]
