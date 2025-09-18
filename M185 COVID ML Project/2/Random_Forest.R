
#pre-requisite packages
#install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("cowplot")
install.packages("pROC")
install.packages("caret")
install.packages("readr")

#load required libraries
#library(stats)
library(dplyr)
library(ggplot2)
#library(randomForest)
#library(cowplot)
library(readr)
library(pROC)
library(caret)

#load data set
imputed_data <- read_csv("Downloads/imputed_data.csv")


#turn some variables into factors
imputed_data$GENDER <- as.factor(imputed_data$GENDER)
imputed_data$SWAB <- as.factor(imputed_data$SWAB)
drop <- c("Subject ID")
imputed.data = imputed_data[,!(names(imputed_data) %in% drop)]

str(imputed.data)

#partition data set
intrain <- createDataPartition(y = imputed.data$SWAB, p= 0.6, list = FALSE)
training <- imputed.data[intrain,]
testing <- imputed.data[-intrain,]

#random forest model
set.seed(123)
model<-randomForest(SWAB~., data=imputed.data, proximity=TRUE)
model

#plot error rates for different number of trees (max 500)
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate),times=3),
  Type= rep(c("OOB","Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Negative"],
          model$err.rate[,"Positive"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

#Random forest with more trees
set.seed(123)
model<-randomForest(SWAB~., data=imputed.data, ntree=1000, proximity=TRUE)
model

#plot error rates for different number of trees (max 3000)
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate),times=3),
  Type= rep(c("OOB","Healthy", "Unhealthy"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"Negative"],
          model$err.rate[,"Positive"]))
ggplot(data=oob.error.data, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))

#Random forest with less trees
set.seed(123)
model<-randomForest(SWAB~., data=imputed.data, ntree=200, proximity=TRUE)
model
  
#table of probabilities
pred_prob<- predict(model,testing,type="prob")
pred_factor<- predict(model,testing)
predictions<- cbind(pred_prob,as.data.frame(pred_factor))
predictions
write.csv(predictions,"Modelpredictions.csv")







