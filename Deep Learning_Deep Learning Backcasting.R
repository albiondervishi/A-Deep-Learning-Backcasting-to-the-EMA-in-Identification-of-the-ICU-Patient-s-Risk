
library(caret)
library(pROC)
library(ggplot2)
library(grid)
library(Epi)
library("lattice")
library("ROCR")
library(h2o)
library(jsonlite)
library (plyr)
library(dplyr)


#------------------------------------------------------------------------------------------------------------#

AUC_model <- function(DL_RTest) {  
  roc <- plot.roc(DL_RTest$FLAG,DL_RTest$RISK, main="Confidence intervals", percent=FALSE,
                  ci=TRUE, # compute AUC (of AUC by default)
                  print.auc=FALSE) # print the AUC (will contain the CI))
  
  Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                        "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                        "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                        "precision", "recall"),best.method="closest.topleft" )
  
  specificity <- Best_closest.topleft_roc[2]
  
  sensitivity <- Best_closest.topleft_roc[3]
  
  accuracy <- Best_closest.topleft_roc[4]
  
  auc <- roc$auc
  CI <- as.vector(roc$ci)
  
  Stat <- data.frame( AUC=auc, CI95lower=CI[1], CI95upper=CI[3], Specificity=specificity, Sensitivity=sensitivity, 
                      Accuracy=accuracy)
  Stat
}

##########################################################################################################################################
##########################################################################################################################################


load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)

set.seed(141)
sample <- sample.int(n = nrow(Data_last_0_8H_Val_LAB), size = floor(.60*nrow(Data_last_0_8H_Val_LAB)))
train_last_0_8H_LEMA <- Data_last_0_8H_Val_LAB[sample, ]
test_last_0_8H_LEMA  <- Data_last_0_8H_Val_LAB[-sample, ]



h2o.init()
h2o_train_last_0_8H_LEMA = as.h2o(train_last_0_8H_LEMA)
h2o_test_last_0_8H_LEMA  = as.h2o(test_last_0_8H_LEMA)



Last_0_8H_LEMA_deepL <- h2o.deeplearning(x = 1:8, y = 9, training_frame = h2o_train_last_0_8H_LEMA, seed=123456)
pred_last_0_8H_LEMA_deepL_test<- as.data.frame(h2o.predict(Last_0_8H_LEMA_deepL, h2o_test_last_0_8H_LEMA))
last_0_8H_LEMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last_0_8H_LEMA), RISK=pred_last_0_8H_LEMA_deepL_test$p1)


last_0_8H_LEMA_deepLstat <- AUC_model(last_0_8H_LEMA_deepL_RTest)



##########################################################################################################################################
# last 1 Day analyse with deep Leanring >>>EMA Day1 (Electrolyte, Metabolic and Acido-basic values)

load('Data_last_8_24H_Val_LAB.RData')
Data_last_8_24H_Val_LAB$FLAG <- as.factor(Data_last_8_24H_Val_LAB$FLAG)



set.seed(41)
sample <- sample.int(n = nrow(Data_last_8_24H_Val_LAB), size = floor(.60*nrow(Data_last_8_24H_Val_LAB)))
train_last_8_24H_EMA <- Data_last_8_24H_Val_LAB[sample, ]
test_last_8_24H_EMA  <- Data_last_8_24H_Val_LAB[-sample, ]
h2o.init()
h2o_train_last_8_24H_EMA = as.h2o(train_last_8_24H_EMA)
h2o_test_last_8_24H_EMA  = as.h2o(test_last_8_24H_EMA)



last_8_24H_EMA_deepL <- h2o.deeplearning(x = 1:8, y = 9, training_frame = h2o_train_last_8_24H_EMA, seed=123456)
# now make a prediction

pred_last_8_24H_EMA_test<- as.data.frame(h2o.predict(last_8_24H_EMA_deepL, h2o_test_last_8_24H_EMA))
last_8_24H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last_8_24H_EMA), RISK=pred_last_8_24H_EMA_test$p1)

Last_8_24H_EMA_deepL_stat <- AUC_model(last_8_24H_EMA_deepL_RTest)

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


#Backcasting EMA 8_24H to the Point LEMA 0_8H


Backcasting_pred_last_8_24H_EMA_test<- as.data.frame(h2o.predict(Last_0_8H_LEMA_deepL, h2o_test_last_8_24H_EMA))
Backcasting_last_8_24H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last_8_24H_EMA), RISK=Backcasting_pred_last_8_24H_EMA_test$p1)
Backcasting_Last_8_24H_EMA_deepL_stat <- AUC_model(Backcasting_last_8_24H_EMA_deepL_RTest)


#######################################################################################################################
#######################################################################################################################

load('Data_last_24_48H_Val_LAB.RData')
Data_last_24_48H_Val_LAB$FLAG <- as.factor(Data_last_24_48H_Val_LAB$FLAG)


set.seed(41)
sample <- sample.int(n = nrow(Data_last_24_48H_Val_LAB), size = floor(.75*nrow(Data_last_24_48H_Val_LAB)))
train_last_24_48H_EMA <- Data_last_24_48H_Val_LAB[sample, ]
test_last_24_48H_EMA  <- Data_last_24_48H_Val_LAB[-sample, ]
h2o.init()
h2o_train_last_24_48H_EMA = as.h2o(train_last_24_48H_EMA)
h2o_test_last_24_48H_EMA  = as.h2o(test_last_24_48H_EMA)



last_24_48H_EMA_deepL <- h2o.deeplearning(x = 1:8, y = 9, training_frame = h2o_train_last_24_48H_EMA, seed=123456)
# now make a prediction

pred_last_24_48H_EMA_test<- as.data.frame(h2o.predict(last_24_48H_EMA_deepL, h2o_test_last_24_48H_EMA))
last_24_48H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last_24_48H_EMA), RISK=pred_last_24_48H_EMA_test$p1)

Last_24_48H_EMA_deepL_stat <- AUC_model(last_24_48H_EMA_deepL_RTest)

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


#Backcasting EMA 24-48 Hours to the Point LEMA 0_8 Hours

Backcasting_pred_last_24_48H_EMA_test<- as.data.frame(h2o.predict(Last_0_8H_LEMA_deepL, h2o_test_last_24_48H_EMA))
Backcasting_last_24_48H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last_24_48H_EMA), RISK=Backcasting_pred_last_24_48H_EMA_test$p1)
Backcasting_Last_24_48H_EMA_deepL_stat <- AUC_model(Backcasting_last_24_48H_EMA_deepL_RTest)


#################################################################################################################
#################################################################################################################




load('Data_last_48_72H_Val_LAB.RData')
Data_last_48_72H_Val_LAB$FLAG <- as.factor(Data_last_48_72H_Val_LAB$FLAG)
dim(Data_last_48_72H_Val_LAB)

set.seed(41)
sample <- sample.int(n = nrow(Data_last_48_72H_Val_LAB), size = floor(.60*nrow(Data_last_48_72H_Val_LAB)))
train_last48_72H_EMA <- Data_last_48_72H_Val_LAB[sample, ]
test_last48_72H_EMA <- Data_last_48_72H_Val_LAB[-sample, ]
h2o.init()
h2o_train_last48_72H_EMA = as.h2o(train_last48_72H_EMA)
h2o_test_last48_72H_EMA  = as.h2o(test_last48_72H_EMA)


last_48_72H_EMA_deepL <- h2o.deeplearning(x = 1:8, y = 9, training_frame = h2o_train_last48_72H_EMA, seed=123456)
# now make a prediction

pred_last_48_72H_EMA_test<- as.data.frame(h2o.predict(last_48_72H_EMA_deepL, h2o_test_last48_72H_EMA))
last_48_72H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last48_72H_EMA), RISK=pred_last_48_72H_EMA_test$p1)

Last_48_72H_EMA_deepL_stat <- AUC_model(last_48_72H_EMA_deepL_RTest)

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


#Backcasting EMA 48_72 Hours to the Point LEMA 0_8 Hours

Backcasting_pred_last_48_72H_EMA_test<- as.data.frame(h2o.predict(Last_0_8H_LEMA_deepL, h2o_test_last48_72H_EMA))
Backcasting_last_48_72H_EMA_deepL_RTest <- data.frame(Test <- as.data.frame(h2o_test_last48_72H_EMA), RISK=Backcasting_pred_last_48_72H_EMA_test$p1)
Backcasting_Last_48_72H_EMA_deepL_stat <- AUC_model(Backcasting_last_48_72H_EMA_deepL_RTest)


##########################################################################################################################
##########################################################################################################################



