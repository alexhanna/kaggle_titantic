##Note:
## This is a simple example of predictive modeling using logistic regression in R
## I wrote this for a Kaggle competition on Titanic survivor data

library(tidyverse)
library(caret)
library(caTools)

##training data
train<-read.csv(file.choose())

##out of sample test data w/no Y var  
out<-read.csv(file.choose())

##adding age squared
train$Age2<-train$Age^2

##manually partitioning the data set
n<-round(.8*nrow(train))
set.seed(8675309)

sample <- sample.int(n = nrow(train), size = n, replace = F)

subtrain<-train[sample,]
subtest<-train[-sample,]

##basic logit model with subtrain minus Name and Embarked
out<-glm(Survived ~ Pclass+Sex+Age+Age2+SibSp+Parch+Fare, 
         family="binomial", subtrain)
p<-predict(out, subtest, type="response")
p_class<-ifelse(p>.5, 1, 0)

confusionMatrix(p_class, subtest$Survived)

##ROC Curve
colAUC(p, subtest$Survived, plotROC=TRUE)

##optimizing model with caret
co <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

##removing missing on Age.
train_nomiss<-train %>%
  filter(is.na(Age)==F)

##training the model
opt_out<-train(make.names(factor(Survived)) ~ Pclass+Sex+Age+Age2+SibSp+Parch+Fare+Embarked, train_nomiss, method="glm",
               trControl=co)


outpreds<-predict(opt_out, newdata=out, type="response")


##training a random forest model
rf_out<-train(make.names(factor(Survived)) ~ Pclass+Sex+Age+Age2+SibSp+Parch+Fare+Embarked, 
               data=train_nomiss, method="ranger", tuneLength=10,
               trControl=trainControl(method="cv", number=10, classProbs=TRUE))
