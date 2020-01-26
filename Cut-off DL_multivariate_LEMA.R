

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


#-----------------------------------------------------------------------------------------------------------



roc <- roc(last_0_8H_LEMA_deepL_RTest$FLAG,last_0_8H_LEMA_deepL_RTest$RISK, main="Confidence intervals", percent=FALSE,
           ci=TRUE, # compute AUC (of AUC by default)
           print.auc=FALSE) # print the AUC (will contain the CI))



Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                      "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                      "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                      "precision", "recall"),best.method="closest.topleft" )

Threshold_Best_closest.topleft_roc <- Best_closest.topleft_roc[1]


Threshold_values_deep_L_multivariate <- subset(last_0_8H_LEMA_deepL_RTest, last_0_8H_LEMA_deepL_RTest$RISK<=Threshold_Best_closest.topleft_roc)
summary(Threshold_values_deep_L_multivariate)

