setwd('//Users/mpgartland/Documents/Courses/Predictive Models/Pred_Models_git/Week 5/data')
library("neuralnet")
# you can also see the packages nnet and RSNNS

dataset <- read.csv("creditset.csv")
head(dataset)

## extract a set to train the NN @ 65%
trainset <- dataset[1:1300, ]

## select the test set
testset <- dataset[1301:2000, ]

#NN nodel with one hiddeD layer and 4 nodes, resilient backprop
creditnet <- neuralnet(default10yr ~ LTI +age, trainset, hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1,algorithm = "rprop+")



print(creditnet)

## plot the NN
plot(creditnet, rep = "best")

#weight from models
creditnet$weights

creditnet$result.matrix

#results (notice in probabilities)
creditnet$net.result


## test the resulting output
temp_test <- subset(testset, select = c("LTI", "age"))

#compute the test results through the ANN model 
creditnet.results <- compute(creditnet, temp_test)

#Make a table to predictions and actuals
results <- data.frame(actual = testset$default10yr, 
                      prediction = creditnet.results$net.result)

print(results)

#Round predictions to make easier to read
results$prediction <- round(results$prediction)

#See partial results
results[100:115, ]

#See all results
print(results)

print(creditnet.results)



#USing Gmodels to create a confusion matrix
library(gmodels)
CrossTable(results$actual, results$prediction)


################################################
#Arguments in neural net function
  neuralnet(formula, data, hidden = 1, threshold = 0.01,        
            stepmax = 1e+05, rep = 1, startweights = NULL, 
            learningrate.limit = NULL, 
            learningrate.factor = list(minus = 0.5, plus = 1.2), 
            learningrate=NULL, lifesign = "none", 
            lifesign.step = 1000, algorithm = "rprop+", 
            err.fct = "sse", act.fct = "logistic", 
            linear.output = TRUE, exclude = NULL, 
            constant.weights = NULL, likelihood = FALSE)
#######################################################


#two hidden layers of 2 and 4 nodes. 

creditnet1 <- neuralnet(default10yr ~ LTI +age, trainset, hidden=c(2,4), lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1,algorithm = "rprop+")



print(creditnet1)

plot(creditnet1, rep = "best")

###Try happends are you add/less more hidden node, more variables, and changes in arguments


#In neural networks it is good idea not just normalize data, but also to scale them. 
#This is intended for faster approaching to global minima at error surface. 
#Common way for ANN
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply min/max normalization to entire data frame
#note all values are now between 0 and 1
creditset_norm <- as.data.frame(lapply(creditset, normalize))

