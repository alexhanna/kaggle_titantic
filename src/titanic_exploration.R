library(tidyverse)
library(caret)
library(caTools)

train<-read.csv(file.choose())

##adding age squared
train$Age2<-train$Age*train$Age

n<-round(.8*nrow(dat))
set.seed(8675309)

sample <- sample.int(n = nrow(train), size = n, replace = F)

subtrain<-train[sample,]
subtest<-train[-sample,]
  
#test<-read.csv(file.choose())


##removed embark temporarily as a level was missing from the test sebset
out<-glm(Survived ~ Pclass+Sex+Age+Age2+SibSp+Parch+Fare, 
         family="binomial", subtrain)
p<-predict(out, subtest, type="response")
p_class<-ifelse(p>.5, 1, 0)

confusionMatrix(p_class, subtest$Survived)

##ROC Curve
colAUC(p, subtest$Survived, plotROC=TRUE)

##optimizing model with caret
control <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)


train_nomiss<-train %>%
  filter(is.na(Age)==F)

opt_out<-train(make.names(factor(Survived)) ~ Pclass+Sex+Age+Age2+SibSp+Parch+Fare+Embarked, train_nomiss, method="glm",
               trControl=control)


