#ANN using a binominal target
set.seed(1234567890)

library("neuralnet")
setwd('~/Documents/Courses/Predictive Models/PM/Week 6/')

creditset <- read.csv("creditset.csv")
head(creditset)

#Common way for ANN
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply min/max normalization to entire data frame
#note all values are now between 0 and 1
creditset_norm <- as.data.frame(lapply(creditset, normalize))

## extract a set to train the NN
trainset <- creditset_norm[1:800, ]

## select the test set
testset <- creditset_norm[801:2000, ]

#ANN with two inputs and 1 output (2 class target)
creditnet <- neuralnet(default10yr ~ LTI + age +income+ loan, trainset, hidden = 4, lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1)

plot(creditnet, rep = "best")

## test the resulting output
#temp_test <- subset(testset, select = c("LTI", "age",))


creditnet.results <- compute(creditnet, testset)

results <- data.frame(actual = testset$default10yr, prediction = creditnet.results$net.result)
results[100:115, ]

#We can round to the nearest integer to improve readability:
  
results$prediction <- round(results$prediction)
results[100:115, ]

library(caret)
#confusion matrix for rpart
actual <- testset$default10yr #created to test the "test" data/
ann_predicted <- predict(creditnet, testset, type="class") 
ann_results.matrix <- confusionMatrix(results$prediction, actual) #the model vs the actual holdout data.
print(ann_results.matrix) #look at my diagnostics
