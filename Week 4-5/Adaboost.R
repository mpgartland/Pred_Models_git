## rpart and mlbench libraries should be loaded
## Comparing the test error of rpart and adaboost.M1
library(rpart)
library(mlbench)

#Set up data and split
data(BreastCancer)
l <- length(BreastCancer[,1])
sub <- sample(1:l,2*l/3)

#Basic Rpart
BC.rpart <- rpart(Class~.,data=BreastCancer[sub,-1], maxdepth=3)
BC.rpart.pred <- predict(BC.rpart,newdata=BreastCancer[-sub,-1],type="class")
tb <-table(BC.rpart.pred,BreastCancer$Class[-sub])
error.rpart <- 1-(sum(diag(tb))/sum(tb))
print(tb)
print(error.rpart)



#This is boosting Rpart. 
library(caret)
library(adabag)
BC.adaboost <- boosting(Class ~.,data=BreastCancer[,-1],mfinal=25, coeflearn='Breiman',boos=TRUE, control=rpart.control(maxdepth=3))
BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata=BreastCancer[-sub,-1])
BC.adaboost.pred$confusion
BC.adaboost.pred$error
BC.adaboost$importance
BC.adaboost$weight


#GLMBOOST
#lesson you can add boosting to most base learners
library(mboost)
#Car data is in mboost


cars.gb <- glmboost(dist ~ speed, data = cars,
                    control = boost_control(mstop = 2000),
                    center = FALSE)
print(cars.gb)

### initial number of boosting iterations
mstop(cars.gb)

### AIC criterion
aic <- AIC(cars.gb, method = "corrected")
print(aic)


