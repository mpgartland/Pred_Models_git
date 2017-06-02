#SVM USING R

Churn_Calls <- read.csv("~/Documents/Courses/Data Mining/Week 5/Churn_Calls.csv")

#Popular SVM algo in e1071
library(e1071)
library(MASS)

names(Churn_Calls)

#Build SVM for Churn


#Create test and train 
index <- 1:nrow(Churn_Calls)
testindex <- sample(index, trunc(length(index)/3))
testset <- Churn_Calls[testindex,]
trainset <- Churn_Calls[-testindex,]


#Model with some arguments
model2 <- svm(churn~., data = trainset,kernel = "linear", gamma = 0.1, cost = 10, k=5, type='C')
print(model2)
summary(model2)

#for details on argument
?svm

#USING CARET
library(caret)
actual <- testset[,20] 
svm_predict<-predict(model2, testset,type="response") 
svm_results.matrix <- confusionMatrix(svm_predict, actual, positive="yes") 
print(svm_results.matrix)

#Tuning
tuned <- tune.svm(churn~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)