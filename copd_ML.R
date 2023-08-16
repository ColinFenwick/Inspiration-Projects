# HI 2454 Final Project machine learning model portion
# Colin Fenwick

copd_dt <- read.csv("copd_clean.csv")
View(copd_dt)

# load the caret package for ML
library(caret)

# Part 1: Logistic Regression
# Split the data into training and testing sets
# use 80/20 ratio to split the data
set.seed(1234) # setting the seed for randomness, fixed number when running the below
trainIndex_copd <- createDataPartition(copd_dt$Diagnosis, p = 0.8, list = FALSE) # need list = FALSE otherwise error
head(trainIndex_copd)

trainset_copd <- copd_dt[trainIndex_copd,] # tells R to use train numbers from above
testset_copd <- copd_dt[-trainIndex_copd,] # the '-' tells R to use the numbers not in the train set

fitControl <- trainControl(method = "cv", number = 10, savePredictions = TRUE) # cv = cross validation

# Train a logistic regression model
lr_model_copd <- train(Diagnosis ~ ., data = trainset_copd,
                method = "LogitBoost", trControl = fitControl,
                verbose = FALSE) # the '.' means we are training all of the features
lr_model_copd # to find the "method" for above, google "caret" package 


# Test the machine learning model, Predict the outcome for testing
copd_prediction <- predict(lr_model_copd, newdata = testset_copd)
head(copd_prediction)

# Evaluate machine learning model
xtab <- table(copd_prediction, testset_copd$Diagnosis)
results <- confusionMatrix(xtab)
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")

# Part 2: Support Vector Machine
# Split the data into training and testing sets
# use 80/20 ratio to split the data
set.seed(1234) # setting the seed for randomness, fixed number when running the below
trainIndex_copd1 <- createDataPartition(copd_dt$Diagnosis, p = 0.8, list = FALSE) 
head(trainIndex_copd1)

trainset_copd1 <- copd_dt[trainIndex_copd1,] 

testset_copd1 <- copd_dt[-trainIndex_copd1,] 

fitControl1 <- trainControl(method = "cv", number = 10, savePredictions = TRUE) 

# Train a support vector machine (SVM) model
svm_model_copd <- train(Diagnosis ~ imaginary_min + Smoking + Age, data = trainset_copd,
                  method = "svmLinear3", trControl = fitControl,
                  verbose = FALSE) 

# Test the machine learning model, Predict the outcome for testing
copd_prediction1 <- predict(svm_model_copd, newdata = testset_copd)
head(copd_prediction1)

# Evaluate machine learning model
btab <- table(copd_prediction1, testset_copd$Diagnosis)
results1 <- confusionMatrix(btab)
as.table(results1)
as.matrix(results1)
as.matrix(results1, what = "overall")
as.matrix(results1, what = "classes")

# Data Visualizations
library(ggplot2)
library(tidyverse)

ggplot(data = copd_dt, aes(x = Smoking, y = Age, color = Diagnosis )) + geom_point()
ggplot(data = copd_dt, mapping = aes(x = Age, y = Diagnosis)) + geom_boxplot() 
ggplot(copd_dt, aes(x=Age, color=Diagnosis)) +
  geom_histogram(fill="white")


# T-test to compare to Python Code
t.test(copd_dt$imaginary_min~copd_dt$Diagnosis)
t.test(copd_dt$Age~copd_dt$Diagnosis)

