
#https://towardsdatascience.com/machine-learning-with-r-logistic-regression-152ec20351db

#installing the necessary packages
install.packages("caret")
install.packages("caTools")
install.packages("e1071")
library(dplyr)
library(caret)
library(caTools)
library(e1071)

data_raw <- read_excel("covid_study_v2 (2).xlsx")
head(data)

#there are some missing values:
lapply(data_raw, function(x) { length(which(is.na(x))) })

#figure out how to impute these? nvm
#reading in the imputed data (used MICE imputation)
data <- read.csv("imputed_data.csv")
#need to remove subject.ID column
data <- subset(data, select = -c(Subject.ID) )

#convert all the variables to factors
#when I convert all variables to factors the model doesn't converge, but if I just convert the character ones it works
cat.names = data %>% select_if(is.character) %>% colnames()
all.names = data %>% colnames()
data[,cat.names] = data.frame(lapply(data[cat.names], as.factor))
data[,all.names] = data.frame(lapply(data[all.names], as.factor))


str(data[,cat.names])


#split into training and testing datasets
#maybe use 10 block approach (cross-validation)?
#however, the paper did 60% testing, 10% validation, 30% testing
#going with 60/40
set.seed(123) 
sampleSplit <- sample.split(Y=data$SWAB, SplitRatio=0.6) 
data_train <- subset(x=data, sampleSplit==TRUE) 
data_test <- subset(x=data, sampleSplit==FALSE)
set.seed(NULL)

#samhita's code
intrain <- createDataPartition(y = data$SWAB, p= 0.6, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]
#how similar are the subsets for both methods:
head(data_test)
head(testing)
#not the same, so redo the model with the new code!

set.seed(123) 
intrain <- createDataPartition(y = data$SWAB, p= 0.6, list = FALSE)
data_train <- data[intrain,]
data_test <- data[-intrain,]
set.seed(NULL)




#creating logistic model:
logistic_model <- glm(SWAB ~ ., family=binomial(link='logit'), data=data_train)
summary(logistic_model)

#testing it:
probabs_logistic <- predict(logistic_model, data_test, type='response') 
preds_logistic <- ifelse(probabs_logistic > 0.5, 1, 0)

#confusion matrix, ROC/AUC
confusionMatrix(factor(preds_logistic), factor(data_test$SWAB))
#returned an error bc factors for SWAB were "Positive"/"Negative" instead of 1/0
factor(preds_logistic)
covid_status = data_test$SWAB
positive_indices <- covid_status == "Positive"
as.numeric(positive_indices)
factor(as.numeric(positive_indices))
confusion_matrix_logistic <- confusionMatrix(factor(preds_logistic), factor(as.numeric(positive_indices)))
confusion_matrix_logistic

#testing feature importance
#combining models

logistic_test_data_results <- data_test
logistic_test_data_results$probability <- probabs_logistic
logistic_test_data_results$model_prediction <- preds_logistic
write.csv(logistic_test_data_results, file = "logistic_test_data_results.csv")



