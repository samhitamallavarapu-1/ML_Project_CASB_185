#Neural Network
library(caret)
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
dim(data)
data <- data[,2:16]
dim(data)
set.seed(123)
intrain <- createDataPartition(y = data$SWAB, p= 0.6, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

str(training)

mlp_grid = expand.grid(layer1 = c(1,2,5,0),
                       layer2 = c(1,2,5,10),
                       layer3 = c(1,2,5,10))
fit_control <- trainControl(method="repeatedcv",repeats=3)

mlp_fit = train(SWAB~.,training,method = "mlpML", preProc =  c('center', 'scale', 'knnImpute', 'pca'),
                       trControl = fit_control, tuneGrid=mlp_grid)
mlp_fit

pred_prob <- predict(mlp_fit,testing,type="prob")
pred_factor <- predict(mlp_fit,testing)

str(training)


predictions <- cbind(pred_prob,as.data.frame(pred_factor))
predictions
write.csv(predictions,"Multilayer Perceptron predictions.csv")
testing$SWAB <- as.factor(testing$SWAB)

confusionMatrix(testing$SWAB,pred_factor)

library(pROC)

par(pty="s")
pROC_obj <- roc(testing$SWAB,predictions$Positive,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, percent = TRUE,legacy.axes=TRUE,auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE,auc.polygon.col = "#8856a722",xlab = "False Positive Percentage",col = "#8856a7",lwd=4 ,ylab= "True Positive Percentage",show.thres=TRUE)

roc_imp2 <- varImp(mlp_fit,scale=FALSE)
plot(roc_imp2)
