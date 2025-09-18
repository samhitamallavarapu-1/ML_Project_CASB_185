#install.packages("e1071")
library(e1071)
#install.packages("caret")
library(caret)
covid <- read.csv(file.choose(new=FALSE))
covid <- covid[,3:17]
dim(covid)
set.seed(123)
#str(covid)
#head(covid)
intrain <- createDataPartition(y = covid$SWAB, p= 0.6, list = FALSE)
training <- covid[intrain,]
testing <- covid[-intrain,]
#dim(training)
#dim(testing)
#anyNA(covid)
#summary(covid)
training[["SWAB"]] = factor(training[["SWAB"]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, savePredictions = TRUE)
svm_Linear <- train(SWAB ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = testing) #just the outcomes
test_prob <- predict(svm_Linear, newdata = testing, type = "prob") #the probabilities
test_pred
test_prob
write.csv(test_prob,"SVM test probability.csv")
write.csv(test_pred,"SVM Outcomes.csv")
confusionMatrix(table(test_pred, testing$SWAB))

#install.packages("MLeval")
library(MLeval)
x <- evalm(svm_Linear)

x$stdres
test_prob
prediction <- cbind(test_prob,as.data.frame(test_pred))
prediction
?roc
result.roc <- roc(testing$SWAB, prediction$Positive) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft",print.auc=TRUE)
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)

#ROC Curves
install.packages("pROC")
library(pROC)

par(pty="s")
pROC_obj <- roc(testing$SWAB,prediction$Positive,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, percent = TRUE,legacy.axes=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE,auc.polygon.col = "#8856a722",xlab = "False Positive Percentage",col = "#8856a7",lwd=4 ,ylab= "True Positive Percentage",show.thres=TRUE)




#for Variable of Importance plot
install.packages("rminer")
library(rminer)
install.packages("glmnet", dependencies=TRUE)
library(glmnet)
model=fit(SWAB~.,training,model="svm")
I = Importance(model,training,method="sens",measure="gradient")
L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
mgraph(L,graph="IMP",leg=names(training),col="mediumpurple",Grid=10) 




#grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
#svm_Linear_Grid <- train(SWAB ~., data = training, method = "svmLinear",
#                         trControl=trctrl,
#                         preProcess = c("center", "scale"),
#                         tuneGrid = grid,
#                         tuneLength = 10)
#svm_Linear_Grid
#plot(svm_Linear_Grid)

#test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
#test_pred_grid

#confusionMatrix(table(test_pred_grid, testing$SWAB))






