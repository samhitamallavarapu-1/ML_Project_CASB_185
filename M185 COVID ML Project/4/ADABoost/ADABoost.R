library(vegan)
#install.packages("dummies")
library(dummies)
#install.packages("ada")
library(ada)
library(dplyr)
data <- read.csv(file.choose(), header = TRUE, row.names = 1)
data <- data %>% mutate(
  GENDER = as.factor(GENDER),
  Lymphocytes = as.numeric(Lymphocytes),
  SWAB = as.factor(SWAB),
  AST = as.numeric(AST),
  ALT = as.numeric(ALT),
  ALP = as.numeric(ALP),
  GGT = as.numeric(GGT),
  LDH = as.numeric(LDH)
)
#split into train and testin
set.seed(123)
intrain <- createDataPartition(y = data$SWAB, p= 0.6, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

#using caret
library(caret)
library(MLeval)


fit_control <- trainControl(method="repeatedcv",repeats=3,classProbs = TRUE,savePredictions = TRUE)
fit_grid <- expand.grid(mfinal = (1:3)*3,         # This is new!
                        maxdepth = c(1, 3),       # ...and this
                        coeflearn = c("Breiman")
)
ada_model <- train(SWAB~.,data=training,method="AdaBoost.M1",trControl=fit_control,tuneGrid=fit_grid,verbose=TRUE)

pred_prob <- predict(ada_model,testing,type="prob")
pred_factor <- predict(ada_model,testing)

predictions <- cbind(pred_prob,as.data.frame(pred_factor))
predictions
write.csv(predictions,"ADABoost predictions.csv")

confusionMatrix(testing$SWAB,pred_factor)

ibrary(pROC)

par(pty="s")
pROC_obj <- roc(testing$SWAB,predictions$Positive,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, percent = TRUE,legacy.axes=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE,auc.polygon.col = "#8856a722",xlab = "False Positive Percentage",col = "#8856a7",lwd=4 ,ylab= "True Positive Percentage",show.thres=TRUE)

VIPs <- varImp(ada_model,scale=FALSE)
plot(VIPs, main="ADABoost Variable Importance",color="mediumpurple3")


