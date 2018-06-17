#A continuous Target

setwd('/Users/mylesgartland/OneDrive - Rockhurst University/Courses/Predictive Models/Pred_Models_git/Week 5/data')
## Step 2: Exploring and preparing the data ----
# read in data and examine structure
concrete <- read.csv("concrete.csv")
str(concrete)



#custom normalization function
#This is called min/max normalization (vs z-score)
#Normalization by Scaling Between 0 and 1 
#Common way for ANN
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply min/max normalization to entire data frame
#note all values are now between 0 and 1
concrete_norm <- as.data.frame(lapply(concrete, normalize))
boxplot(concrete_norm)
# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data 
#Split the dataset into a training and testing sets 70/30
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)

# simple ANN with only a two hidden neurons
concrete_model_1 <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train, hidden = 2, algorithm = "rprop+", learningrate=NULL)
#rprop+ is a backpropagation method called resilient backpropagation. It modifies
#its learning rate on the error.


# visualize the network topology
#note one node in the hidden layer
plot(concrete_model_1)

#table of nuerons and weights
concrete_model_1$result.matrix 

## Step 4: Evaluating model performance ----
# obtain model results
model_results_1 <- compute(concrete_model_1, concrete_test[1:8]) #You are running the training set through the ANN model
# obtain predicted strength values
predicted_strength_1 <- model_results_1$net.result #The prediction of each observation
# examine the correlation between predicted and actual values
cor(predicted_strength_1, concrete_test$strength)

#RMSE
sqrt(mean((concrete_test$strength-predicted_strength_1)^2))

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5,algorithm = "rprop+", learningrate=NULL)

# plot the network
#note 5 neurons in the hidden layer
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

predicted_strength2[1:10]
#what do you notice about the values?

#Return norm value to a regular value
denormalize <- function(x) { 
  return(x*(max(concrete$strength)) - min(concrete$strength))+min(concrete$strength)
}


#look at predicted vs actual
accuracy<-data.frame(denormalize(predicted_strength2),concrete$strength[774:1030])

#plot pred vs actual
plot(denormalize(predicted_strength2),concrete$strength[774:1030])

#Model with two hidden layers
concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = c(5,3), algorithm = "rprop+", learningrate=NULL)

plot(concrete_model3)
