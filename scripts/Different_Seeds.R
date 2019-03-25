set.seed(5000)
train.size <- 0.8
train.index <- sample.int(length(turnover_dummies$left), round(length(turnover_dummies$left) * train.size))
trainingData <- turnover_dummies[train.index,]
testData <- turnover_dummies[-train.index,]

trainingData$left <- as.character(trainingData$left)
trainingData$left <- factor(trainingData$left,levels = c(0,1), label = c(0, 1))
testData$left <- as.character(testData$left)
testData$left <- factor(testData$left,levels = c(0,1), label = c(0, 1))

###############################################################################################
train_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = TRUE
)

model_4 <- train(left ~ poly(satisfaction_level,11) + poly(last_evaluation,7) + poly(average_montly_hours,9)+
                   sales_management + sales_RandD + salary_medium + number_project_1 + number_project_2 + 
                   number_project_3 + time_spend_company_1 + time_spend_company_2 + time_spend_company_3 + 
                   Work_accident_0 , 
                 data=trainingData, 
                 trControl=train_control, 
                 method="glm",
                 family="binomial")
summary(model_4)
model_4$results[2]
###############################################################################################
fitted.results_4 <- predict(model_4,newdata=testData,type = 'prob')
fitted.results_4 <- ifelse(fitted.results_4$`0` > 0.4,0,1)
# table(fitted.results_4,testData$left)
misClasificError_4 <- mean(fitted.results_4 != testData$left)
print(paste('Accuracy',1-misClasificError_4))

