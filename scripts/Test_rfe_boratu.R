############################## RANDOM FOREST TO CHOOSE ########################################
library(party)
cf1 <- cforest(left ~ . , data= trainingData, control=cforest_unbiased(mtry=2,ntree=50))
var_imp_rf <- varimp(cf1, conditional=TRUE)

############################## BORUTA ########################################
library(Boruta)
boruta_output <- Boruta(left ~ ., data=trainingData, doTrace=2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

library(Boruta)
boruta_output_1 <- Boruta(left ~ ., data=trainingData, doTrace=2)
boruta_signif_1 <- names(boruta_output_1$finalDecision[boruta_output_1$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output_1, cex.axis=.7, las=2, xlab="", main="Variable Importance")

library(Boruta)
boruta_output_2 <- Boruta(left ~ . - sales_hr - sales_product_mng - sales_marketing - sales_IT, data=trainingData, doTrace=2)
boruta_signif_2 <- names(boruta_output_2$finalDecision[boruta_output_2$finalDecision %in% c("Confirmed", "Tentative")])
plot(boruta_output_2, cex.axis=.7, las=2, xlab="", main="Variable Importance")
############################## RFE ########################################
control <- rfeControl(functions=rfFuncs, method='cv', number=10)
rfe <- rfe(df.new[,2:7], df.new[,1], rfeControl=control)
predictors(rfe)
