
#averaged the probabilities of the models in excel

combined_model_svm_glm_xgb <- read.csv(file = "combined_model_SVM_GLM_XGB.csv")
combined_prediction_factor <- as.factor(combined_model_svm_glm_xgb$combined_prediction)

confusionMatrix(data_test$SWAB, combined_prediction_factor)

pROC_obj <- roc(data_test$SWAB,predictions$Positive,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, percent = TRUE,legacy.axes=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE,auc.polygon.col = "#8856a722",xlab = "False Positive Percentage",col = "#8856A7",lwd=4 ,ylab= "True Positive Percentage",show.thres=TRUE)


#combining SVM with logistic