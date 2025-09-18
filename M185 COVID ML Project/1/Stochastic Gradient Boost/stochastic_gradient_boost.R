library(caret)
install.packages("gbm")
library(gbm)



fit_control <- trainControl(method="repeatedcv",repeats=3,classProbs = TRUE,savePredictions = TRUE)
#using default grid
sgb_model <- train(SWAB~.,data=data_train,method="gbm",trControl=fit_control,verbose=TRUE)

probabs_sgb_caret <- predict(sgb_model,data_test,type="prob")
prediction_sgb_factor <- predict(sgb_model,data_test)

predictions <- cbind(probabs_sgb_caret,as.data.frame(prediction_sgb_factor))
predictions
write.csv(predictions,"Stochastic gradient boost predictions.csv")

confusionMatrix(data_test$SWAB,prediction_sgb_factor)

library(pROC)
par(pty="s")
pROC_obj <- roc(data_test$SWAB,predictions$Positive,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, percent = TRUE,legacy.axes=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE,auc.polygon.col = "#8856a722",xlab = "False Positive Percentage",col = "#8856A7",lwd=4 ,ylab= "True Positive Percentage",show.thres=TRUE)
VIPs <- varImp(sgb_model,scale=FALSE)
plot(VIPs, main="Stochastic Gradient Boost Variable Importance",color="mediumpurple3")

