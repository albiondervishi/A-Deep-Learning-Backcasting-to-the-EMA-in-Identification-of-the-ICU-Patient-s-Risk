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



load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Lac_data0_8H <- data.frame(LACTATE=Data_last_0_8H_Val_LAB$LACTATE, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(41)
sample <- sample.int(n = nrow(Lac_data0_8H), size = floor(.60*nrow(Lac_data0_8H)))
train_Lac_data0_8H <- Lac_data0_8H[sample, ]
test_Lac_data0_8H  <- Lac_data0_8H[-sample, ]


h2o.init()
h2o_train_Lac_data0_8H = as.h2o(train_Lac_data0_8H)
h2o_test_Lac_data0_8H  = as.h2o(test_Lac_data0_8H)

LACTATE_dl_0_8H <- h2o.deeplearning(x = 1, y = 2, training_frame = h2o_train_Lac_data0_8H, seed=123456) 
# now make a prediction

pred_LACTATE_test<- as.data.frame(h2o.predict(LACTATE_dl_0_8H, h2o_test_Lac_data0_8H))
LACTATE_DL_RTest <- data.frame(Test <- as.data.frame(test_Lac_data0_8H), RISK=pred_LACTATE_test$p1)

pred_LACTATE_train<- as.data.frame(h2o.predict(LACTATE_dl_0_8H, h2o_train_Lac_data0_8H))
LACTATE_DL_Rtrain <- data.frame(Train <- as.data.frame(train), RISK=pred_LACTATE_train$p1)

#########################################################################################################


load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Ph_data <- data.frame(PH=Data_last_0_8H_Val_LAB$PH, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(41)
sample <- sample.int(n = nrow(Ph_data), size = floor(.60*nrow(Ph_data)))
train <- Ph_data[sample, ]
test  <- Ph_data[-sample, ]

h2o.init()
train = as.h2o(train)
test  = as.h2o(test)

PH_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train, seed=123456) 
# now make a prediction

pred_PH_test<- as.data.frame(h2o.predict(PH_dl, test))
PH_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_PH_test$p1)

pred_PH_train<- as.data.frame(h2o.predict(PH_dl, train))
PH_DL_Rtrain <- data.frame(Train <- as.data.frame(train), RISK=pred_PH_train$p1)

#########################################################################################################



load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
HCO3_data <- data.frame(BICARBONATE=Data_last_0_8H_Val_LAB$BICARBONATE, FLAG=Data_last_0_8H_Val_LAB$FLAG)

set.seed(41)
sample <- sample.int(n = nrow(HCO3_data), size = floor(.60*nrow(HCO3_data)))
train <- HCO3_data[sample, ]
test  <- HCO3_data[-sample, ]


h2o.init()
train = as.h2o(train)
test  = as.h2o(test)


BICARBONATE_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train, seed=123456) 
# now make a prediction

pred_BICARBONATE_test<- as.data.frame(h2o.predict(BICARBONATE_dl, test))
BICARBONATEDL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_BICARBONATE_test$p1)

pred_BICARBONATE_train<- as.data.frame(h2o.predict(BICARBONATE_dl, train))
BICARBONATE_DL_Rtrain <- data.frame(Train <- as.data.frame(train), RISK=pred_BICARBONATE_train$p1)

#######################################################################################################################################


load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
K_data <- data.frame(POTASSIUM=Data_last_0_8H_Val_LAB$POTASSIUM, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(73) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(K_data), size = floor(.60*nrow(K_data)))
train <- K_data[sample, ]
test  <- K_data[-sample, ]


h2o.init()

train <- as.h2o(train)
test <- as.h2o(test)

POTASSIUM_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train, seed=123456)
# Prediction performance in AUC
predTest_POTASSIUM<- as.data.frame(h2o.predict(POTASSIUM_dl, test))
POTASSIUM_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=predTest_POTASSIUM$p1)

# Potassium risk  derived from Deep learning regression of the Trained Data 
predTrain_POTASSIUM<- as.data.frame(h2o.predict(POTASSIUM_dl, train))
POTASSIUM_DL_RTrain <- data.frame(K_dataT <- as.data.frame(train), RISK=predTrain_POTASSIUM$p1)

#################################################################################################################################


load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Ca_data <- data.frame(CALCIUM=Data_last_0_8H_Val_LAB$CALCIUM, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(73) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(Ca_data), size = floor(.60*nrow(Ca_data)))
train <- Ca_data[sample, ]
test  <- Ca_data[-sample, ]



h2o.init()

train <- as.h2o(train)
test <- as.h2o(test)

CALCIUM_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train, seed=123456)
# now make a prediction

pred_CALCIUM_test<- as.data.frame(h2o.predict(CALCIUM_dl, test))
CALCIUM_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_CALCIUM_test$p1)

pred_CALCIUM_train<- as.data.frame(h2o.predict(CALCIUM_dl, train))
CALCIUM_DL_Rtrain <- data.frame(Test <- as.data.frame(train), RISK=pred_CALCIUM_train$p1)

#####################################################################################################################################

load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Glu_data <- data.frame(GLUCOSE=Data_last_0_8H_Val_LAB$GLUCOSE, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(41)
sample <- sample.int(n = nrow(Glu_data), size = floor(.75*nrow(Glu_data)))
train <- Glu_data[sample, ]
test  <- Glu_data[-sample, ]

h2o.init()
train = as.h2o(train)
test  = as.h2o(test)


GLUCOSE_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train,  seed=123456)
# now make a prediction

pred_GLUCOSE_test<- as.data.frame(h2o.predict(GLUCOSE_dl, test))
GLUCOSE_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_GLUCOSE_test$p1)
pred_GLUCOSE_train<- as.data.frame(h2o.predict(GLUCOSE_dl, train))
GLUCOSE_DL_Rtrain <- data.frame(Train <- as.data.frame(train), RISK=pred_GLUCOSE_train$p1)
#######################################################################################################################################



load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Na_data <- data.frame(SODIUM=Data_last_0_8H_Val_LAB$SODIUM, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(41)
sample <- sample.int(n = nrow(Na_data), size = floor(.60*nrow(Na_data)))
train <- Na_data[sample, ]
test  <- Na_data[-sample, ]


h2o.init()
train = as.h2o(train)
test  = as.h2o(test)


SODIUM_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train,  seed=123456)
# now make a prediction

pred_SODIUM_test<- as.data.frame(h2o.predict(SODIUM_dl, test))
SODIUM_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_SODIUM_test$p1)

pred_SODIUM_train<- as.data.frame(h2o.predict(SODIUM_dl, train))
SODIUM_DL_Rtrain <- data.frame(Train <- as.data.frame(train), RISK=pred_SODIUM_train$p1)
#########################################################################################################


load('Data_last_0_8H_Val_LAB.RData')
Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)
Cl_data <- data.frame(CHLORIDE=Data_last_0_8H_Val_LAB$CHLORIDE, FLAG=Data_last_0_8H_Val_LAB$FLAG)


set.seed(41) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 60% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(Cl_data), size = floor(.60*nrow(Cl_data)))
train <- Cl_data[sample, ]
test  <- Cl_data[-sample, ]


h2o.init()

train <- as.h2o(train)
test <- as.h2o(test)


CHLORIDE_dl <- h2o.deeplearning(x = 1, y = 2, training_frame = train,  seed=123456)
# now make a prediction
pred_CHLORIDE_test<- as.data.frame(h2o.predict(CHLORIDE_dl, test))
CHLORIDE_DL_RTest <- data.frame(Test <- as.data.frame(test), RISK=pred_CHLORIDE_test$p1)

pred_CHLORIDE_train<- as.data.frame(h2o.predict(CHLORIDE_dl, train))
CHLORIDE_DL_Rtrain <- data.frame(Test <- as.data.frame(train), RISK=pred_CHLORIDE_train$p1)

#################################################################################################################################


