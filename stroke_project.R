# HI 2021 FINAL PROJECT CODE
# SPRING 2023, Colin Fenwick

# Reading the CSV file
stroke_dt <- read.csv("stroke.csv")
View(stroke_dt)
colnames(stroke_dt)

# PART 1: CLEANING, PROCESSING, and PREPARING THE DATA
# Exploratory Data Analysis
library(dplyr)
summary(stroke_dt)
stroke_dt %>% count(gender)
stroke_dt %>% count(smoking_status)
stroke_dt %>% count(Residence_type)
stroke_dt %>% count(work_type)

# Handle Missing Data 
library(dplyr)
stroke_dt1 <- stroke_dt %>%
  mutate_all(~replace(., . == 'N/A', NA))
View(stroke_dt1) # data with regular NA

# Imputing missing data
missing_dt <- apply(stroke_dt1, MARGIN = 2, function(x){sum(is.na(x))/length(x)*100}) # MARGIN = 2 because this means we are applying to columns
missing_dt # shows the percentage of missing data in each column, bmi missing 3.9% 

new_stroke <- stroke_dt1[,missing_dt < 20] # blank before the comma because we want to keep rows and alter the columns
View(new_stroke) 
apply(new_stroke, 2, function(x){sum(is.na(x))/length(x)*100})

library(ggplot2)
stroke_dt1$bmi <- as.numeric(stroke_dt1$bmi) # convert BMI to numeric
ggplot(data=subset(stroke_dt1, !is.na(bmi)), aes(x=bmi)) +
  geom_histogram(fill = 'blue') # to see the distribution...appears approximately normally distributed

mean(stroke_dt1$bmi, na.rm=TRUE) # check the mean of the column excluding missing values, 28.89324

# Imputing data with the mean 
imputed_bmi <- data.frame(
  original = stroke_dt1$bmi,
  bmi_imputed = replace(stroke_dt1$bmi, is.na(stroke_dt1$bmi), mean(stroke_dt1$bmi, na.rm = TRUE))
)

# To visually see if the imputation disrupted the distribution significantly
original_hist <- ggplot(imputed_bmi, aes(x = original)) +
  geom_histogram(fill = "maroon", color = "black", position = "identity") +
  ggtitle("Original Distribution") +
  theme_classic()

mean_imputedhist <- ggplot(imputed_bmi, aes(x = bmi_imputed)) +
  geom_histogram(fill = "navy", color = "black", position = "identity") +
  ggtitle("Mean-Imputed Distribution") +
  theme_classic()

install.packages("cowplot")
library(cowplot)
plot_grid(original_hist, mean_imputedhist, nrow = 2)

# Combining the imputed data with the original dataframe, use cbind() since the two df have same schema and row order
stroke_dt2 <- cbind(stroke_dt1, imputed_bmi)

stroke_dt3 <- stroke_dt2[ -c(10,13) ] # remove ID, ever_married, old BMI columns

prop.table(table(stroke_dt3$stroke)) # see ratio of stroke and non stroke patients

stroke_pos <- subset(stroke_dt3, stroke == '1') # creating separate df for + stroke and - stroke
summary(stroke_pos)
stroke_neg <- subset(stroke_dt3, stroke == '0')
summary(stroke_neg)

# PART 2 DATA VISUALIZATION 
stroke_dt3$stroke <- as.character(stroke_dt3$stroke)

# histogram
ggplot(stroke_dt3, aes(x=bmi_imputed, color=stroke)) +
  geom_histogram(bins = 20, fill="white") 

# scatter plot (may not include)
ggplot(data = stroke_dt3, mapping = aes(x = age, y = bmi_imputed, color = stroke)) + geom_point(alpha = 0.5) 

# box plot for BMI
pos_box <- ggplot(data = stroke_pos, mapping = aes(x = bmi_imputed, y = stroke)) + geom_boxplot(color = 'dark red') 
pos_box_lab <- pos_box + labs(y = "Stroke", x = "BMI")
neg_box <- ggplot(data = stroke_neg, mapping = aes(x = bmi_imputed, y = stroke)) + geom_boxplot(color = 'darkblue') 
neg_box_lab <- neg_box + labs(y = "No Stroke", x = "BMI")

plot_grid(pos_box_lab, neg_box_lab, nrow = 2) 

# histogram for avg_glucose_level
pos_hist <- ggplot(stroke_pos, aes(x= avg_glucose_level))+
  geom_histogram(color = 'darkred', fill = 'darkred') 
neg_hist <- ggplot(stroke_neg, aes(x = avg_glucose_level))+
  geom_histogram(color = 'darkblue', fill ='darkblue')


plot_grid(pos_hist, neg_hist, nrow = 2) 

# box plot for avg_glucose_level
pos_box_gluc <- ggplot(data = stroke_pos, mapping = aes(x = avg_glucose_level, y = stroke)) + geom_boxplot(color = 'dark red') 
pos_box_lab1 <- pos_box_gluc + labs(y = "Stroke", x = "Avg Glucose Level")
neg_box_gluc <- ggplot(data = stroke_neg, mapping = aes(x = avg_glucose_level, y = stroke)) + geom_boxplot(color = 'darkblue') 
neg_box_lab1 <- neg_box_gluc + labs(y = "No Stroke", x = "Avg Glucose Level")

plot_grid(pos_box_lab1, neg_box_lab1, nrow = 2) 

# to find outliers in bmi_imputed
library(dplyr)
summary(stroke_neg)
stroke_neg %>%
  count(bmi_imputed > '46.6' | bmi_imputed < '9.8')  # find out many outliers there are, 46.6 = Q3 + (1.5 * IQR), 9.8 = Q1 - (1.5 * IQR)

summary(stroke_pos)
stroke_pos %>%
  count(bmi_imputed > '40.75' | bmi_imputed < '18.75') # find out many outliers there are, 40.75 = Q3 + (1.5 * IQR), 18.75 = Q1 - (1.5*IQR)

# to find outliers in avg_glucose_level
library(dplyr)
summary(stroke_neg)
stroke_neg %>%
  count(avg_glucose_level > '166.4') # 166.4 is Q3 + (1.5*IQR)


# creating a combined visual of age, bmi, smoking status, and heart disease 
# see how age is distributed for the stroke pos and stroke neg groups
age <- ggplot(stroke_dt3, aes(x = age)) + 
  geom_histogram(color = 'black', fill = 'white') +
  facet_grid(stroke ~ .) + labs(title = "Age Distribution", x = 
                                  "age", y = "Count")

# see how bmi_imputed is distributed for stroke pos and stroke neg groups
bmi <- ggplot(stroke_dt3, aes(x = bmi_imputed)) +
  geom_histogram(color = 'darkgreen', fill = 'darkgreen') +
  facet_grid(stroke ~ .) + labs(title = "BMI Distribution", x = "bmi", y = "count")

# see how smoking status varies between the groups
smoking_status <- ggplot(stroke_dt3, aes(x= smoking_status))+
  geom_bar(color = 'darkblue', fill = 'darkblue')+
  facet_grid(stroke ~.) + labs (title = 'Count of Patients in Each Smoking Status', x = 'Smoking Status', y =
                                  'count')

# see how heart disease varies between the groups
heart <- ggplot(stroke_dt3, aes(x= heart_disease))+
  geom_bar(color = 'darkred', fill = 'darkred')+
  facet_grid(stroke ~.) + labs (title = 'Count of Patients with Heart Disease', x = 'Heart Disease Status', y =
                                  'count')

plot_grid(age, bmi, smoking_status, heart, nrow = 2) 

# see how residence status and work type are distributed
residence <- ggplot(stroke_dt3, aes(x= Residence_type))+
  geom_bar(color = 'darkblue', fill = 'darkblue')+
  labs (title = 'Count of Patients in Each Residence Type', x = 'Residence', y =
                                  'count')
work <- ggplot(stroke_dt3, aes(x= work_type))+
  geom_bar(color = 'darkblue', fill = 'darkblue')+
  labs (title = 'Count of Patients in Each Work Type', x = 'Work Type', y =
                                  'count')
plot_grid(residence, work, nrow = 2) 


# confirming there are no more NA
new_stroke1 <- stroke_dt3[,missing_dt < 20] # blank before the comma because we want to keep rows and alter the columns
View(new_stroke1)
apply(new_stroke1, 2, function(x){sum(is.na(x))/length(x)*100})


# PART 3 AIM 1 STATISTICAL TESTING
# 2-sample t-test comparing BMI of stroke and non-stroke patients
t.test(stroke_dt3$bmi_imputed~stroke_dt3$stroke)

# 2-sample t-test comparing avg_glucose_level of stroke and non-stroke patients
t.test(stroke_dt3$avg_glucose_level~stroke_dt3$stroke)

# 2-sample t-test comparing ages of stroke and non-stroke patients
t.test(stroke_dt3$age~stroke_dt3$stroke)

# Two-way ANOVA
two.way <- aov(stroke ~ bmi_imputed + hypertension, data = stroke_dt3)
summary(two.way)
interaction <- aov(stroke ~ bmi_imputed*hypertension, data = stroke_dt3)
summary(interaction)



# PART 4 AIM 2 MACHINE LEARNING --> PREPARE FOR ML 
# want to convert 'stroke',  factor to numeric for ML algorithm training and testing
stroke_dt3$stroke = as.numeric(stroke_dt3$stroke)

# Determine how many types of categories are in each feature that is not an integer or numeric
# Remove unwanted features first
stroke_dt3 = subset(stroke_dt3, select = -c(1,6)) # remove ID and ever_married columns


# Model 1: Logistic Regression
# Split the data into training and testing sets
# use 80/20 ratio to split the data

# One-hot encoding of the data
library(caret)
library(mlbench)
dummy <- dummyVars(" ~ .", data=stroke_dt3)
newdata_stroke <- data.frame(predict(dummy, newdata = stroke_dt3)) 
newdata_stroke$stroke[newdata_stroke$stroke == "1"] <- 'stroke'
newdata_stroke$stroke[newdata_stroke$stroke == "0"] <- 'no_stroke'
newdata_stroke$age = as.integer(newdata_stroke$age)

# use 80/20 ratio to split the data
set.seed(1234) # setting the seed for randomness, fixed number when running the below
trainIndex_stroke <- createDataPartition(newdata_stroke$stroke, p = 0.8, list = FALSE) # need list = FALSE otherwise error
head(trainIndex_stroke)

trainset_stroke <- newdata_stroke[trainIndex_stroke,] # tells R to use train numbers from above
testset_stroke <-  newdata_stroke[-trainIndex_stroke,] # the '-' tells R to use the numbers not in the train set

# included over and undersampling due to imbalance in data set
fitControl_down <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "down") # cv = cross validation, 
fitControl_up <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "up") # cv = cross validation


# Train a Boosted Logistic Regression model
lr_model <- train(stroke ~ ., data = trainset_stroke,
                  method = 'LogitBoost', trControl = fitControl_up,
                  verbose = FALSE) 
lr_model 


# Test the machine learning model, Predict the outcome for testing
stroke_prediction <- predict(lr_model, newdata = testset_stroke)
head(stroke_prediction)

# Evaluate machine learning model
lrtab <- table(stroke_prediction, testset_stroke$stroke)
results <- confusionMatrix(lrtab)
as.table(results)
as.matrix(results)
as.matrix(results, what = "overall")
as.matrix(results, what = "classes")


# Model 2: eXtreme Gradient Boosting
# use 80/20 ratio to split the data
set.seed(1234) # setting the seed for randomness, fixed number when running the below
trainIndex_stroke <- createDataPartition(newdata_stroke$stroke, p = 0.8, list = FALSE) # need list = FALSE otherwise error
head(trainIndex_stroke)

trainset_stroke <- newdata_stroke[trainIndex_stroke,] # tells R to use train numbers from above
testset_stroke <-  newdata_stroke[-trainIndex_stroke,] # the '-' tells R to use the numbers not in the train set

fitControl_down <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "down") # cv = cross validation
fitControl_up <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "up") # cv = cross validation

# Train a XGB model
xgb_model <- train(stroke ~ ., data = trainset_stroke,
                  method = 'xgbLinear', trControl = fitControl_down,
                  verbose = FALSE) 
xgb_model 


# Test the machine learning model, Predict the outcome for testing
stroke_prediction2 <- predict(xgb_model, newdata = testset_stroke)
head(stroke_prediction2)

# Evaluate machine learning model
xgbtab <- table(stroke_prediction2, testset_stroke$stroke)
results2 <- confusionMatrix(xgbtab)
as.table(results2)
as.matrix(results2)
as.matrix(results2, what = "overall")
as.matrix(results2, what = "classes")

varImp(xgb_model, scale = FALSE)

# Model 3: Random Forest
# use 80/20 ratio to split the data
set.seed(1234) # setting the seed for randomness, fixed number when running the below
trainIndex_stroke <- createDataPartition(newdata_stroke$stroke, p = 0.8, list = FALSE) # need list = FALSE otherwise error
head(trainIndex_stroke)

trainset_stroke <- newdata_stroke[trainIndex_stroke,] # tells R to use train numbers from above
testset_stroke <-  newdata_stroke[-trainIndex_stroke,] # the '-' tells R to use the numbers not in the train set

fitControl_down <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "down") # cv = cross validation
fitControl_up <- trainControl(method = "cv", number =5, savePredictions = TRUE, sampling = "up") # cv = cross validation

# Train a rf model
rf_model <- train(stroke ~ ., data = trainset_stroke,
                   method = 'parRF', trControl = fitControl_down,
                   verbose = FALSE) 
rf_model 

# Test the machine learning model, Predict the outcome for testing
stroke_prediction3 <- predict(rf_model, newdata = testset_stroke)
head(stroke_prediction3)

# Evaluate machine learning model
rltab <- table(stroke_prediction3, testset_stroke$stroke)
results3 <- confusionMatrix(rltab)
as.table(results3)
as.matrix(results3)
as.matrix(results3, what = "overall")
as.matrix(results3, what = "classes")

varImp(rf_model, scale = FALSE) # to get feature importance

#calculate AUC --> convert stroke back to a numeric
install.packages("ROCR")
library(ROCR)

# Model 1 - Linear Regression
pr_stroke_lr <- prediction(as.numeric(stroke_prediction), testset_stroke$stroke)
pr_stroke_lr<- prediction(as.numeric(stroke_prediction), testset_stroke$stroke)
auc_stroke <- performance(pr_stroke_lr, measure = "auc")
auc_stroke@y.values[[1]]


# Model 3 - XGB
pr_stroke_xgb <- prediction(as.numeric(stroke_prediction2), testset_stroke$stroke)
pr_stroke_xgb <- prediction(as.numeric(stroke_prediction2), testset_stroke$stroke)
auc_stroke_xgb <- performance(pr_stroke_xgb, measure = "auc")
auc_stroke_xgb@y.values[[1]]

# Model 4 - RF
pr_stroke_rf <- prediction(as.numeric(stroke_prediction3), testset_stroke$stroke)
pr_stroke_rf<- prediction(as.numeric(stroke_prediction3), testset_stroke$stroke)
auc_stroke_rf <- performance(pr_stroke_rf, measure = "auc")
auc_stroke_rf@y.values[[1]]
