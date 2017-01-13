#Boosted Model
#Example from Caret documentation
#http://topepo.github.io/caret/training.html


library(gbm)
library(caret)

#Sonar Data Set
set.seed(825)
library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

dim(Sonar)

#Test/Train Sonar
set.seed(825)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

#Fit control for CV
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

#Gradient Boosting Model 1
set.seed(825)
gbmFit1 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
print(gbmFit1)

#Grid Search
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                       n.trees = (1:30)*50,
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

nrow(gbmGrid)

#GBM Model 2 with grid search
set.seed(825)
gbmFit2 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaludate:
                 tuneGrid = gbmGrid)
print(gbmFit2)

#plotted model
library(ggplot2)
ggplot(gbmFit2)

#GBM Using Adaboost
#Need to convert target to numeric
library(adabag)
gbm_algorithm <- gbm(Class ~ ., data = training, distribution = "adaboost", n.trees = 5000)
print(gbm_algorithm)