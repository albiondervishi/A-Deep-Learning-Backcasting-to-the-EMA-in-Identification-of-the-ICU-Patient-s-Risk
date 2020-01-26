

library(ggfortify)
library(corrplot)
library(ggplot2)
library(gridBase)
library(gridExtra)
library(PASWR2)
library(party)
library(repmis)
library(mgcv)
library(rpart)
library(party)
library(rpart.plot)
library(DT)
library(rpart)
library(rpart.plot)
library(randomForest)
library(webshot)
library(markdown)
library(knitr)
library(knitLatex)
library(knitrBootstrap)
library(datasets)
library(ggplot2) 
library(aod)
library(epiR)
library(PredictABEL)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
library(gridExtra)
library(dplyr)
library(lattice)
library(ggplot2)
library(cluster)
library(mlbench)
library(randomForest)
library(forestFloor)
library(AUC)
library(gnm)
library(pROC)
library(Epi)
library(caTools)
library(lubridate)
library(Distance)
library(ResourceSelection)
library(PredictABEL)
library(e1071)
library(splitstackshape)
library(dplyr)
library(tidyr)
library(downloader)
library(reshape)
library(reshape2)
library(sjmisc)
library(stringi)
library(forecast)
library(plyr)
library(TTR)
library(earth)
library(gbm)
library(splines)
library(parallel)
library(papeR)
library(data.table) 
library(rfUtilities)
library(sp)
library("papeR")
library(xts)
library(zoo)
library("arsenal")
library(timeSeries)
library("chron")
library(finalfit)

#######################################################################################################

# Extracting the survivors/non-survivors from the ADMISSIONS file
ADMISSIONS <- read.csv("ADMISSIONS.csv")

#------------------------------------------------------------------------------------------------------#
# Extracting patients length of staying in the  ICU from ICUSTAYS file.
ICUSTAYS <- read.csv("ICUSTAYS.csv")

# Extracting patients transfer

TRANSFERS <- read.csv("TRANSFERS.csv")

#--------------------------------------------------------------------------------------------------------#
# Extracting patients age from PATIENTS  and ADMISSIONS file.

PATIENTS <- read.csv("PATIENTS.csv")

PatDOB <- data.frame(SUBJECT_ID=PATIENTS[,2], TIME=TIME <- ymd_hms(PATIENTS[,4]))
PatADT <-data.frame(SUBJECT_ID= ADMISSIONS[,2], ADMITTIME=ADMITTIME <- ymd_hms(ADMISSIONS[,4])) 
Data <- data.frame(PatDOB, PatADT=PatADT[match(PatDOB$SUBJECT_ID, PatADT$SUBJECT_ID), 2])

INTERVAL <- interval(Data$TIME, Data$PatADT ) 
Age <- INTERVAL / dyears(1)
AGE <- data.frame(cbind(SUBJECT_ID=Data[,1],Age))
AGE <- subset(AGE, AGE$Age<89 & AGE$Age>15)

###############################################################################################################



load('LABEVENTS.RData')

LABEVENTS <- LABEVENTS[ ,c(2:9)]

ICUSTAYS_ADMISSIONS<- data.frame(left_join(ICUSTAYS, ADMISSIONS, by = c( 'HADM_ID'='HADM_ID' ))) 
str(ICUSTAYS_ADMISSIONS)

ICUSTAYS_ADMISSIONS_FLAG_UNSUV <- subset(ICUSTAYS_ADMISSIONS, ICUSTAYS_ADMISSIONS$DISCHARGE_LOCATION=="DEAD/EXPIRED")

ICUSTAYS_ADMISSIONS_FLAG_UNSUV$OUTTIME <- as.Date(as.character(ICUSTAYS_ADMISSIONS_FLAG_UNSUV$OUTTIME), format="%Y-%m-%d %H:%M:%S")
ICUSTAYS_ADMISSIONS_FLAG_UNSUV$DEATHTIME <- as.Date(as.character(ICUSTAYS_ADMISSIONS_FLAG_UNSUV$DEATHTIME), format="%Y-%m-%d %H:%M:%S")

ICUSTAYS_ADMISS_FLAG_UNSUV <- data.frame(ICUSTAYS_ADMISSIONS_FLAG_UNSUV,DEATH_OUTTIME=DEATH_OUTTIME <-ICUSTAYS_ADMISSIONS_FLAG_UNSUV$OUTTIME == ICUSTAYS_ADMISSIONS_FLAG_UNSUV$DEATHTIME)
ICUSTAYS_ADMISS_FLAG_UNSUV <- subset(ICUSTAYS_ADMISS_FLAG_UNSUV, ICUSTAYS_ADMISS_FLAG_UNSUV$DEATH_OUTTIME=="TRUE")



ICUSTAYS_ADMISS_UNSUV <- data.frame(HADM_ID=ICUSTAYS_ADMISS_FLAG_UNSUV$HADM_ID, HOSPITAL_EXPIRE_FLAG=ICUSTAYS_ADMISS_FLAG_UNSUV$HOSPITAL_EXPIRE_FLAG)

LABEVENTS_UNSUV <- left_join(LABEVENTS, ICUSTAYS_ADMISS_UNSUV, by = c('HADM_ID'='HADM_ID')) 
dim(LABEVENTS_UNSUV)


TIME_ORDER <- function(DATA, x){ 
  DATA <- subset(DATA,DATA$ITEMID==x)
  DATA <- DATA[order(DATA$HADM_ID, as.Date(DATA$CHARTTIME)),]
  DATA
}


SODIUMdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50983)
POTASSIUMdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50971)
CHLORIDEdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50902)
BICARBONATEdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50882)
GLUCOSEdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50931)
CALCIUMdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50893)
BASEEXCESSdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50802)
LACTATEdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50813)
PHdataN <- TIME_ORDER(LABEVENTS_UNSUV, 50820)



# Last values


Last_value_selecting <- function (data){
  
  data <- data.frame(data[ ,c(1:9)])
  
  
  TAIL_VALUE_1_DAY <- function(data){ 
    if ( length(data) < 1) {return(NA)} 
    else if (length(data) >=1) {
      dataxts <- xts(x=data, order.by=as.POSIXct(data$CHARTTIME))
      data_tail_h <- last(dataxts, n = "1 days")
      data_tail_h <- data_tail_h[rev(1:nrow(data_tail_h))[1],]
      data_tail_h <- as.data.table(data_tail_h)
      data_tail_h <- data.frame(data_tail_h)
    }
    data_tail_h
  }
  
  
  
  split_list_data <- split(data, data$HADM_ID)
  
  
  l_res <- lapply(split_list_data, FUN = TAIL_VALUE_1_DAY)
  
  res_data = rbindlist(l_res)
  res_data
}

## INTERVAL BETWEEN  LAST VALUES AND DEATH TIME.  If you want to select hours change the hours in this row DATA[,11]<=24 & DATA[,11]>=12
Selecting_N_hour <- function (DATA, ADMISSIONS){ 
  L_DATA_DEATHTIME <- data.frame(DATA, DEATHTIME=ADMISSIONS[match(DATA$HADM_ID, ADMISSIONS$HADM_ID), 6])
  INTERVAL_L_DATA_DEATHTIME <- interval(ymd_hms(L_DATA_DEATHTIME$CHARTTIME), ymd_hms(L_DATA_DEATHTIME$DEATHTIME))
  hours_INTERVAL_L_DATA_DEATHTIME  <- INTERVAL_L_DATA_DEATHTIME / dhours(1)
  DATA<- data.frame(DATA, hours_INTERVAL_L_DATA_DEATHTIME)
  DATA <- subset(DATA, DATA[,11]<=8 & DATA[,11]>=0)
  DATA
}


Last_valuesN_SODIUM <- Last_value_selecting(SODIUMdataN)
colnames(Last_valuesN_SODIUM)[colnames(Last_valuesN_SODIUM) == 'VALUENUM'] <- 'SODIUM'
Last_valuesN_SODIUM <- Selecting_N_hour(Last_valuesN_SODIUM, ADMISSIONS)
summary(Last_valuesN_SODIUM); sd(Last_valuesN_SODIUM$hours_INTERVAL_L_DATA_DEATHTIME)

Last_valuesN_POTASSIUM <- Last_value_selecting(POTASSIUMdataN)
colnames(Last_valuesN_POTASSIUM)[colnames(Last_valuesN_POTASSIUM) == 'VALUENUM'] <- 'POTASSIUM'
Last_valuesN_POTASSIUM <- Selecting_N_hour(Last_valuesN_POTASSIUM, ADMISSIONS)
summary(Last_valuesN_POTASSIUM); sd(Last_valuesN_POTASSIUM$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_CHLORIDE <- Last_value_selecting(CHLORIDEdataN)
colnames(Last_valuesN_CHLORIDE)[colnames(Last_valuesN_CHLORIDE) == 'VALUENUM'] <- 'CHLORIDE'
Last_valuesN_CHLORIDE <- Selecting_N_hour(Last_valuesN_CHLORIDE, ADMISSIONS)
summary(Last_valuesN_CHLORIDE); sd(Last_valuesN_CHLORIDE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_BICARBONATE <- Last_value_selecting(BICARBONATEdataN)
colnames(Last_valuesN_BICARBONATE)[colnames(Last_valuesN_BICARBONATE) == 'VALUENUM'] <- 'BICARBONATE'
Last_valuesN_BICARBONATE <- Selecting_N_hour(Last_valuesN_BICARBONATE, ADMISSIONS)
summary(Last_valuesN_BICARBONATE); sd(Last_valuesN_BICARBONATE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_GLUCOSE <- Last_value_selecting(GLUCOSEdataN)
colnames(Last_valuesN_GLUCOSE)[colnames(Last_valuesN_GLUCOSE) == 'VALUENUM'] <- 'GLUCOSE'
Last_valuesN_GLUCOSE <- Selecting_N_hour(Last_valuesN_GLUCOSE, ADMISSIONS)
summary(Last_valuesN_GLUCOSE); sd(Last_valuesN_GLUCOSE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_CALCIUM <- Last_value_selecting(CALCIUMdataN)
colnames(Last_valuesN_CALCIUM)[colnames(Last_valuesN_CALCIUM) == 'VALUENUM'] <- 'CALCIUM'
Last_valuesN_CALCIUM <- Selecting_N_hour(Last_valuesN_CALCIUM, ADMISSIONS)
summary(Last_valuesN_CALCIUM); sd(Last_valuesN_CALCIUM$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_LACTATE <- Last_value_selecting(LACTATEdataN)
colnames(Last_valuesN_LACTATE)[colnames(Last_valuesN_LACTATE) == 'VALUENUM'] <- 'LACTATE'
Last_valuesN_LACTATE <- Selecting_N_hour(Last_valuesN_LACTATE, ADMISSIONS)
summary(Last_valuesN_LACTATE); sd(Last_valuesN_LACTATE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_PH <- Last_value_selecting(PHdataN)
colnames(Last_valuesN_PH)[colnames(Last_valuesN_PH) == 'VALUENUM'] <- 'PH'
Last_valuesN_PH <- Selecting_N_hour(Last_valuesN_PH, ADMISSIONS)
summary(Last_valuesN_PH); sd(Last_valuesN_PH$hours_INTERVAL_L_DATA_DEATHTIME)


DATA_Last_0_8HoursvaluesN<-join_all(list( Last_valuesN_SODIUM[,c(3,7)], Last_valuesN_POTASSIUM[,c(3,7)],Last_valuesN_CHLORIDE[,c(3,7)], Last_valuesN_BICARBONATE[,c(3,7)],
                                          Last_valuesN_GLUCOSE[,c(3,7)], Last_valuesN_CALCIUM[,c(3,7)], 
                                          Last_valuesN_LACTATE[,c(3,7)],Last_valuesN_PH[,c(3,7)]), by='HADM_ID', type='left')

DATA_Last_0_8HoursvaluesN<-DATA_Last_0_8HoursvaluesN[complete.cases(DATA_Last_0_8HoursvaluesN),]

DATA_Last_0_8HoursvaluesN<-join_all(list(DATA_Last_0_8HoursvaluesN[,c(1:30)], as.data.frame(PATIENTS[,c(2,3)]), as.data.frame(AGE)), by='SUBJECT_ID', type='left')
DATA_Last_0_8HoursvaluesN<-DATA_Last_0_8HoursvaluesN[complete.cases(DATA_Last_0_8HoursvaluesN),]


#######################################################################################################################
#######################################################################################################################

# When you need to extract labs in different Days>>change in the funcion nummber of days >>>n = "3 days" 

N_hour_selecting <- function (data){
  
  data <- data.frame(data[ ,c(1:9)])
  
  
  TAIL_VALUE_1_DAY <- function(data){ 
    if ( length(data) < 1) {return(NA)} 
    else if (length(data) >=1) {
      dataxts <- xts(x=data, order.by=as.POSIXct(data$CHARTTIME))
      data_tail_h <- first(last(dataxts, n = "1 days"))
      data_tail_h <- as.data.table(data_tail_h)
      data_tail_h <- data.frame(data_tail_h)
    }
    data_tail_h
  }
  
  
  split_list_data <- split(data, data$HADM_ID)
  
  
  l_res <- lapply(split_list_data, FUN = TAIL_VALUE_1_DAY)
  
  res_data = rbindlist(l_res)
  res_data
}




Selecting_N_hour <- function (DATA, ADMISSIONS){ 
  L_DATA_DEATHTIME <- data.frame(DATA, DEATHTIME=ADMISSIONS[match(DATA$HADM_ID, ADMISSIONS$HADM_ID), 6])
  INTERVAL_L_DATA_DEATHTIME <- interval(ymd_hms(L_DATA_DEATHTIME$CHARTTIME), ymd_hms(L_DATA_DEATHTIME$DEATHTIME))
  hours_INTERVAL_L_DATA_DEATHTIME  <- INTERVAL_L_DATA_DEATHTIME / dhours(1)
  DATA<- data.frame(DATA, hours_INTERVAL_L_DATA_DEATHTIME)
  DATA <- subset(DATA, DATA[,11]<=24 & DATA[,11]>=8)
  DATA
}



Last_valuesN_SODIUM <- N_hour_selecting(SODIUMdataN)
colnames(Last_valuesN_SODIUM)[colnames(Last_valuesN_SODIUM) == 'VALUENUM'] <- 'SODIUM'
Last_valuesN_SODIUM <- Selecting_N_hour(Last_valuesN_SODIUM, ADMISSIONS)
summary(Last_valuesN_SODIUM); sd(Last_valuesN_SODIUM$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_POTASSIUM <- N_hour_selecting(POTASSIUMdataN)
colnames(Last_valuesN_POTASSIUM)[colnames(Last_valuesN_POTASSIUM) == 'VALUENUM'] <- 'POTASSIUM'
Last_valuesN_POTASSIUM <- Selecting_N_hour(Last_valuesN_POTASSIUM, ADMISSIONS)
summary(Last_valuesN_POTASSIUM); sd(Last_valuesN_POTASSIUM$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_CHLORIDE <- N_hour_selecting(CHLORIDEdataN)
colnames(Last_valuesN_CHLORIDE)[colnames(Last_valuesN_CHLORIDE) == 'VALUENUM'] <- 'CHLORIDE'
Last_valuesN_CHLORIDE <- Selecting_N_hour(Last_valuesN_CHLORIDE, ADMISSIONS)
summary(Last_valuesN_CHLORIDE); sd(Last_valuesN_CHLORIDE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_BICARBONATE <- N_hour_selecting(BICARBONATEdataN)
colnames(Last_valuesN_BICARBONATE)[colnames(Last_valuesN_BICARBONATE) == 'VALUENUM'] <- 'BICARBONATE'
Last_valuesN_BICARBONATE <- Selecting_N_hour(Last_valuesN_BICARBONATE, ADMISSIONS)
summary(Last_valuesN_BICARBONATE); sd(Last_valuesN_BICARBONATE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_GLUCOSE <- N_hour_selecting(GLUCOSEdataN)
colnames(Last_valuesN_GLUCOSE)[colnames(Last_valuesN_GLUCOSE) == 'VALUENUM'] <- 'GLUCOSE'
Last_valuesN_GLUCOSE <- Selecting_N_hour(Last_valuesN_GLUCOSE, ADMISSIONS)
summary(Last_valuesN_GLUCOSE); sd(Last_valuesN_GLUCOSE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_CALCIUM <- N_hour_selecting(CALCIUMdataN)
colnames(Last_valuesN_CALCIUM)[colnames(Last_valuesN_CALCIUM) == 'VALUENUM'] <- 'CALCIUM'
Last_valuesN_CALCIUM <- Selecting_N_hour(Last_valuesN_CALCIUM, ADMISSIONS)
summary(Last_valuesN_CALCIUM); sd(Last_valuesN_CALCIUM$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_LACTATE <- N_hour_selecting(LACTATEdataN)
colnames(Last_valuesN_LACTATE)[colnames(Last_valuesN_LACTATE) == 'VALUENUM'] <- 'LACTATE'
Last_valuesN_LACTATE <- Selecting_N_hour(Last_valuesN_LACTATE, ADMISSIONS)
summary(Last_valuesN_LACTATE); sd(Last_valuesN_LACTATE$hours_INTERVAL_L_DATA_DEATHTIME)


Last_valuesN_PH <- N_hour_selecting(PHdataN)
colnames(Last_valuesN_PH)[colnames(Last_valuesN_PH) == 'VALUENUM'] <- 'PH'
Last_valuesN_PH <- Selecting_N_hour(Last_valuesN_PH, ADMISSIONS)
summary(Last_valuesN_PH); sd(Last_valuesN_PH$hours_INTERVAL_L_DATA_DEATHTIME)


DATA_Last8_24HoursvaluesN<-join_all(list( Last_valuesN_SODIUM[,c(3,7)],  Last_valuesN_POTASSIUM[,c(3,7)],Last_valuesN_CHLORIDE[,c(3,7)], Last_valuesN_BICARBONATE[,c(3,7)],
                                          Last_valuesN_GLUCOSE[,c(3,7)], Last_valuesN_CALCIUM[,c(3,7)], 
                                          Last_valuesN_LACTATE[,c(3,7)],Last_valuesN_PH[,c(3,7)]), by='HADM_ID', type='left')

DATA_Last8_24HoursvaluesN<-DATA_Last8_24HoursvaluesN[complete.cases(DATA_Last8_24HoursvaluesN),]



DATA_Last8_24HoursvaluesN<-join_all(list(DATA_Last8_24HoursvaluesN, as.data.frame(PATIENTS[,c(2,3)]), as.data.frame(AGE)), by='SUBJECT_ID', type='left')
DATA_Last8_24HoursvaluesN<-DATA_Last8_24HoursvaluesN[complete.cases(DATA_Last8_24HoursvaluesN),]

# Change the day function to 2
DATA_Last24_48HoursvaluesN<-join_all(list(DATA_Last24_48HoursvaluesN, as.data.frame(PATIENTS[,c(2,3)]), as.data.frame(AGE)), by='SUBJECT_ID', type='left')
DATA_Last24_48HoursvaluesN<-DATA_Last24_48HoursvaluesN[complete.cases(DATA_Last24_48HoursvaluesN),]

# Change the day function to 3
DATA_Last48_72HoursvaluesN<-join_all(list(DATA_Last48_72HoursvaluesN, as.data.frame(PATIENTS[,c(2,3)]), as.data.frame(AGE)), by='SUBJECT_ID', type='left')
DATA_Last48_72HoursvaluesN<-DATA_Last48_72HoursvaluesN[complete.cases(DATA_Last48_72HoursvaluesN),]



#######################################################################################################################
#######################################################################################################################


# SURVIVAL DATASUBSET 

ICUSTAYS_ADMISSIONS<- data.frame(left_join(ICUSTAYS, ADMISSIONS, by = c( 'HADM_ID'='HADM_ID' ))) 
ICUSTAYS_ADMISSIONS_FLAG_SUV <- subset(ICUSTAYS_ADMISSIONS, ICUSTAYS_ADMISSIONS$HOSPITAL_EXPIRE_FLAG=="0")

str(ICUSTAYS_ADMISSIONS_FLAG_SUV)

ICUSTAYS_ADMISS_SUV <- data.frame(HADM_ID=ICUSTAYS_ADMISSIONS_FLAG_SUV$HADM_ID, HOSPITAL_EXPIRE_FLAG=ICUSTAYS_ADMISSIONS_FLAG_SUV$HOSPITAL_EXPIRE_FLAG, OUTTIME=ICUSTAYS_ADMISSIONS_FLAG_SUV$OUTTIME)

LABEVENTS_SUV <- left_join(LABEVENTS,ICUSTAYS_ADMISS_SUV, by = c('HADM_ID'='HADM_ID')) 

dim(LABEVENTS_SUV)


OUTTIMEicuCUTTOFF <- function(DATA) { 
  Select_icuOUTTIME <- function(DATA) { 
    DATA <- subset(DATA,  as.Date(as.character(DATA$CHARTTIME), "%Y-%m-%d %H:%M:%S") <= as.Date(as.character(DATA$OUTTIME), "%Y-%m-%d %H:%M:%S") )
    DATA
  }
  split_list_data <- split(DATA, DATA$HADM_ID)
  l_res <- lapply(split_list_data, FUN = Select_icuOUTTIME)
  res_data = rbindlist(l_res)
  res_data
  
}

LABEVENTS_SUV <- OUTTIMEicuCUTTOFF(LABEVENTS_SUV)






TIME_ORDER <- function(DATA, x){ 
  DATA <- subset(DATA,DATA$ITEMID==x)
  DATA <- DATA[order(DATA$HADM_ID, as.Date(DATA$CHARTTIME)),]
  DATA
}


SODIUMdataS<- TIME_ORDER(LABEVENTS_SUV, 50983)
POTASSIUMdataS <- TIME_ORDER(LABEVENTS_SUV, 50971)
CHLORIDEdataS <- TIME_ORDER(LABEVENTS_SUV, 50902)
BICARBONATEdataS <- TIME_ORDER(LABEVENTS_SUV, 50882)
GLUCOSEdataS <- TIME_ORDER(LABEVENTS_SUV, 50931)
CALCIUMdataS <- TIME_ORDER(LABEVENTS_SUV, 50893)
LACTATEdataS <- TIME_ORDER(LABEVENTS_SUV, 50813)
PHdataS <- TIME_ORDER(LABEVENTS_SUV, 50820)



# Last values
Last_value_selecting <- function (data){
  
  data <- data.frame(data)
  TAIL_VALUE_1_DAY <- function(data){ 
    if ( length(data) < 1) {return(NA)} 
    else if (length(data) >=1) {
      dataxts <- xts(x=data, order.by=as.POSIXct(data$CHARTTIME))
      data_tail_h <- last(dataxts, n = "1 days")
      data_tail_h <- data_tail_h[rev(1:nrow(data_tail_h))[1],]
      data_tail_h <- as.data.table(data_tail_h)
      data_tail_h <- data.frame(data_tail_h)
    }
    data_tail_h
  }
  
  split_list_data <- split(data, data$HADM_ID)
  l_res <- lapply(split_list_data, FUN = TAIL_VALUE_1_DAY)
  res_data = rbindlist(l_res)
  res_data
}




Selecting_S_hour <- function (DATA){ 
  INTERVAL_L_DATA_OUTTIME <- interval(ymd_hms(DATA$CHARTTIME), ymd_hms(DATA$OUTTIME))
  hours_INTERVAL_L_DATA_OUTTIME  <- INTERVAL_L_DATA_OUTTIME / dhours(1)
  DATA <- data.frame(DATA, hours_INTERVAL_L_DATA_OUTTIME=hours_INTERVAL_L_DATA_OUTTIME)
  DATA <- subset(DATA, DATA$hours_INTERVAL_L_DATA_OUTTIME<=24)
  DATA
}



Last_valuesS_SODIUM <- Last_value_selecting(SODIUMdataS)
colnames(Last_valuesS_SODIUM)[colnames(Last_valuesS_SODIUM) == 'VALUE'] <- 'SODIUM'
Last_valuesS_SODIUM <- Selecting_S_hour(Last_valuesS_SODIUM)
summary(Last_valuesS_SODIUM); sd(Last_valuesS_SODIUM$hours_INTERVAL_L_DATA_OUTTIME)


Last_valuesS_POTASSIUM <- Last_value_selecting(POTASSIUMdataS)
colnames(Last_valuesS_POTASSIUM)[colnames(Last_valuesS_POTASSIUM) == 'VALUE'] <- 'POTASSIUM'
Last_valuesS_POTASSIUM <- Selecting_S_hour(Last_valuesS_POTASSIUM)
summary(Last_valuesS_POTASSIUM); sd(Last_valuesS_POTASSIUM$hours_INTERVAL_L_DATA_OUTTIME)


Last_valuesS_CHLORIDE <- Last_value_selecting(CHLORIDEdataS)
colnames(Last_valuesS_CHLORIDE)[colnames(Last_valuesS_CHLORIDE) == 'VALUE'] <- 'CHLORIDE'
Last_valuesS_CHLORIDE <- Selecting_S_hour(Last_valuesS_CHLORIDE)
summary(Last_valuesS_CHLORIDE); sd(Last_valuesS_CHLORIDE$hours_INTERVAL_L_DATA_OUTTIME)


Last_valuesS_BICARBONATE <- Last_value_selecting(BICARBONATEdataS)
colnames(Last_valuesS_BICARBONATE)[colnames(Last_valuesS_BICARBONATE) == 'VALUE'] <- 'BICARBONATE'
Last_valuesS_BICARBONATE <- Selecting_S_hour(Last_valuesS_BICARBONATE)
summary(Last_valuesS_BICARBONATE); sd(Last_valuesS_BICARBONATE$hours_INTERVAL_L_DATA_OUTTIME)

Last_valuesS_GLUCOSE <- Last_value_selecting(GLUCOSEdataS)
colnames(Last_valuesS_GLUCOSE)[colnames(Last_valuesS_GLUCOSE) == 'VALUE'] <- 'GLUCOSE'
Last_valuesS_GLUCOSE <- Selecting_S_hour(Last_valuesS_GLUCOSE)
summary(Last_valuesS_GLUCOSE); sd(Last_valuesS_GLUCOSE$hours_INTERVAL_L_DATA_OUTTIME)

Last_valuesS_CALCIUM <- Last_value_selecting(CALCIUMdataS)
colnames(Last_valuesS_CALCIUM)[colnames(Last_valuesS_CALCIUM) == 'VALUE'] <- 'CALCIUM'
Last_valuesS_CALCIUM <- Selecting_S_hour(Last_valuesS_CALCIUM)
summary(Last_valuesS_CALCIUM); sd(Last_valuesS_CALCIUM$hours_INTERVAL_L_DATA_OUTTIME)


Last_valuesS_LACTATE <- Last_value_selecting(LACTATEdataS)
colnames(Last_valuesS_LACTATE)[colnames(Last_valuesS_LACTATE) == 'VALUE'] <- 'LACTATE'
Last_valuesS_LACTATE <- Selecting_S_hour(Last_valuesS_LACTATE)
summary(Last_valuesS_LACTATE); sd(Last_valuesS_LACTATE$hours_INTERVAL_L_DATA_OUTTIME)

Last_valuesS_PH <- Last_value_selecting(PHdataS)
colnames(Last_valuesS_PH)[colnames(Last_valuesS_PH) == 'VALUE'] <- 'PH'
Last_valuesS_PH <- Selecting_S_hour(Last_valuesS_PH)
summary(Last_valuesS_PH); sd(Last_valuesS_PH$hours_INTERVAL_L_DATA_OUTTIME)



DATA_Last_HvaluesS<-join_all(list(Last_valuesS_SODIUM[,c(2,8)], Last_valuesS_POTASSIUM[,c(2,8)], Last_valuesS_CHLORIDE[,c(2,8)], Last_valuesS_BICARBONATE[,c(2,8)], 
                                  Last_valuesS_GLUCOSE[,c(2,8)], Last_valuesS_CALCIUM[,c(2,8)], 
                                  Last_valuesS_LACTATE[,c(2,8)], Last_valuesS_PH[,c(2,8)]), by='HADM_ID', type='left')

DATA_Last_HvaluesS<-DATA_Last_HvaluesS[complete.cases(DATA_Last_HvaluesS),]


DATA_Last_HvaluesS<-join_all(list(DATA_Last_HvaluesS, as.data.frame(PATIENTS[,c(2,3)]), as.data.frame(AGE)), by='SUBJECT_ID', type='left')
DATA_Last_HvaluesS<-DATA_Last_HvaluesS[complete.cases(DATA_Last_HvaluesS),]


#######################################################################################################################
#######################################################################################################################

load('DATA_Last_0_8HoursvaluesN.RData')

Data_Last_0_8H_N <- data.frame(SODIUM=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$POTASSIUM)), 
                               CHLORIDE=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$BICARBONATE)),
                               GLUCOSE=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$CALCIUM)), 
                               Age=as.numeric(as.character(DATA_Last_HvaluesS$Age)), GENDER=DATA_Last_0_8HoursvaluesN$GENDER,
                               LACTATE=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$LACTATE)), PH=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$PH)), FLAG=as.numeric(as.character(DATA_Last_0_8HoursvaluesN$HOSPITAL_EXPIRE_FLAG)))


Data_S <- data.frame(SODIUM=as.numeric(as.character(DATA_Last_HvaluesS$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last_HvaluesS$POTASSIUM)), 
                     CHLORIDE=as.numeric(as.character(DATA_Last_HvaluesS$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last_HvaluesS$BICARBONATE)),
                     GLUCOSE=as.numeric(as.character(DATA_Last_HvaluesS$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last_HvaluesS$CALCIUM)), 
                     Age=as.numeric(as.character(DATA_Last_HvaluesS$Age)), GENDER=DATA_Last_HvaluesS$GENDER,
                     LACTATE=as.numeric(as.character(DATA_Last_HvaluesS$LACTATE)), PH=as.numeric(as.character(DATA_Last_HvaluesS$PH)), FLAG=as.numeric(as.character(DATA_Last_HvaluesS$HOSPITAL_EXPIRE_FLAG)))

Data_last_0_8H_Val_LAB <- rbind(Data_S, Data_Last_0_8H_N)
summary(Data_last_0_8H_Val_LAB)
Data_last_0_8H_Val_LAB<-Data_last_0_8H_Val_LAB[complete.cases(Data_last_0_8H_Val_LAB),]


#######################################################################################################################
#######################################################################################################################

load('DATA_Last8_24HoursvaluesN.RData')

Data_N_8_24Hours <- data.frame(SODIUM=as.numeric(as.character(DATA_Last8_24HoursvaluesN$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last8_24HoursvaluesN$POTASSIUM)), 
                               CHLORIDE=as.numeric(as.character(DATA_Last8_24HoursvaluesN$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last8_24HoursvaluesN$BICARBONATE)),
                               GLUCOSE=as.numeric(as.character(DATA_Last8_24HoursvaluesN$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last8_24HoursvaluesN$CALCIUM)), 
                               Age=DATA_Last8_24HoursvaluesN$Age, GENDER=DATA_Last8_24HoursvaluesN$GENDER,
                               LACTATE=as.numeric(as.character(DATA_Last8_24HoursvaluesN$LACTATE)), PH=as.numeric(as.character(DATA_Last8_24HoursvaluesN$PH)), FLAG=as.numeric(as.character(DATA_Last8_24HoursvaluesN$HOSPITAL_EXPIRE_FLAG)))


Data_S <- data.frame(SODIUM=as.numeric(as.character(DATA_Last_HvaluesS$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last_HvaluesS$POTASSIUM)), 
                     CHLORIDE=as.numeric(as.character(DATA_Last_HvaluesS$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last_HvaluesS$BICARBONATE)),
                     GLUCOSE=as.numeric(as.character(DATA_Last_HvaluesS$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last_HvaluesS$CALCIUM)), 
                     Age=DATA_Last_HvaluesS$Age, GENDER=DATA_Last_HvaluesS$GENDER,GENDER=DATA_Last_HvaluesS$GENDER,
                     LACTATE=as.numeric(as.character(DATA_Last_HvaluesS$LACTATE)), PH=as.numeric(as.character(DATA_Last_HvaluesS$PH)), FLAG=as.numeric(as.character(DATA_Last_HvaluesS$HOSPITAL_EXPIRE_FLAG)))

Data_last_8_24H_Val_LAB <- rbind(Data_S, Data_N_8_24Hours)
Data_last_8_24H_Val_LAB<-Data_last_8_24H_Val_LAB[complete.cases(Data_last_8_24H_Val_LAB),]


#######################################################################################################################
#######################################################################################################################

load('DATA_Last24_48HoursvaluesN.RData')


Data_N_24_48Hours <- data.frame(SODIUM=as.numeric(as.character(DATA_Last24_48HoursvaluesN$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last24_48HoursvaluesN$POTASSIUM)), 
                                CHLORIDE=as.numeric(as.character(DATA_Last24_48HoursvaluesN$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last24_48HoursvaluesN$BICARBONATE)),
                                GLUCOSE=as.numeric(as.character(DATA_Last24_48HoursvaluesN$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last24_48HoursvaluesN$CALCIUM)), 
                                Age=DATA_Last24_48HoursvaluesN$Age, GENDER=DATA_Last24_48HoursvaluesN$GENDER,
                                LACTATE=as.numeric(as.character(DATA_Last24_48HoursvaluesN$LACTATE)), PH=as.numeric(as.character(DATA_Last24_48HoursvaluesN$PH)), FLAG=as.numeric(as.character(DATA_Last24_48HoursvaluesN$HOSPITAL_EXPIRE_FLAG)))


Data_S <- data.frame(SODIUM=as.numeric(as.character(DATA_Last_HvaluesS$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last_HvaluesS$POTASSIUM)), 
                     CHLORIDE=as.numeric(as.character(DATA_Last_HvaluesS$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last_HvaluesS$BICARBONATE)),
                     GLUCOSE=as.numeric(as.character(DATA_Last_HvaluesS$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last_HvaluesS$CALCIUM)), 
                     Age=DATA_Last_HvaluesS$Age, GENDER=DATA_Last_HvaluesS$GENDER,
                     LACTATE=as.numeric(as.character(DATA_Last_HvaluesS$LACTATE)), PH=as.numeric(as.character(DATA_Last_HvaluesS$PH)), FLAG=as.numeric(as.character(DATA_Last_HvaluesS$HOSPITAL_EXPIRE_FLAG)))

Data_last_24_48H_Val_LAB <- rbind(Data_S, Data_N_24_48Hours)
Data_last_24_48H_Val_LAB<-Data_last_24_48H_Val_LAB[complete.cases(Data_last_24_48H_Val_LAB),]


#######################################################################################################################
#######################################################################################################################


load('DATA_Last48_72HoursvaluesN.RData')

Data_N_48_72Hours <- data.frame(SODIUM=as.numeric(as.character(DATA_Last48_72HoursvaluesN$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last48_72HoursvaluesN$POTASSIUM)), 
                                CHLORIDE=as.numeric(as.character(DATA_Last48_72HoursvaluesN$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last48_72HoursvaluesN$BICARBONATE)),
                                GLUCOSE=as.numeric(as.character(DATA_Last48_72HoursvaluesN$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last48_72HoursvaluesN$CALCIUM)), 
                                Age=DATA_Last48_72HoursvaluesN$Age, GENDER=DATA_Last48_72HoursvaluesN$GENDER,
                                LACTATE=as.numeric(as.character(DATA_Last48_72HoursvaluesN$LACTATE)), PH=as.numeric(as.character(DATA_Last48_72HoursvaluesN$PH)), FLAG=as.numeric(as.character(DATA_Last48_72HoursvaluesN$HOSPITAL_EXPIRE_FLAG)))



Data_S <- data.frame(SODIUM=as.numeric(as.character(DATA_Last_HvaluesS$SODIUM)), POTASSIUM=as.numeric(as.character(DATA_Last_HvaluesS$POTASSIUM)), 
                     CHLORIDE=as.numeric(as.character(DATA_Last_HvaluesS$CHLORIDE)),BICARBONATE=as.numeric(as.character(DATA_Last_HvaluesS$BICARBONATE)),
                     GLUCOSE=as.numeric(as.character(DATA_Last_HvaluesS$GLUCOSE)), CALCIUM=as.numeric(as.character(DATA_Last_HvaluesS$CALCIUM)), 
                     Age=DATA_Last_HvaluesS$Age, GENDER=DATA_Last_HvaluesS$GENDER,
                     LACTATE=as.numeric(as.character(DATA_Last_HvaluesS$LACTATE)), PH=as.numeric(as.character(DATA_Last_HvaluesS$PH)), FLAG=as.numeric(as.character(DATA_Last_HvaluesS$HOSPITAL_EXPIRE_FLAG)))

Data_last_48_72H_Val_LAB <- rbind(Data_S, Data_N_48_72Hours)
Data_last_48_72H_Val_LAB<-Data_last_48_72H_Val_LAB[complete.cases(Data_last_48_72H_Val_LAB),]


#######################################################################################################################
#######################################################################################################################


Data_last_0_8H_Val_LAB$FLAG <- as.factor(Data_last_0_8H_Val_LAB$FLAG)

explanatory=c("Age", "GENDER","LACTATE", "PH",  "BICARBONATE","POTASSIUM","CALCIUM", 
              "GLUCOSE","SODIUM","CHLORIDE")
dependent = "FLAG"

Data_last_0_8H_Val_LAB %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE)



Data_last_8_24H_Val_LAB$FLAG <- as.factor(Data_last_8_24H_Val_LAB$FLAG)

explanatory=c("Age", "GENDER","LACTATE", "PH",  "BICARBONATE","POTASSIUM","CALCIUM", 
              "GLUCOSE","SODIUM","CHLORIDE")
dependent = "FLAG"

Data_last_8_24H_Val_LAB %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE)




Data_last_24_48H_Val_LAB$FLAG <- as.factor(Data_last_24_48H_Val_LAB$FLAG)

explanatory=c("Age", "GENDER","LACTATE", "PH",  "BICARBONATE","POTASSIUM","CALCIUM", 
              "GLUCOSE","SODIUM","CHLORIDE")
dependent = "FLAG"

Data_last_24_48H_Val_LAB %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE)




Data_last_48_72H_Val_LAB$FLAG <- as.factor(Data_last_48_72H_Val_LAB$FLAG)

explanatory=c("Age", "GENDER","LACTATE", "PH",  "BICARBONATE","POTASSIUM","CALCIUM", 
              "GLUCOSE","SODIUM","CHLORIDE")
dependent = "FLAG"

Data_last_48_72H_Val_LAB %>%
  summary_factorlist(dependent, explanatory,
                     p=TRUE, add_dependent_label=TRUE)






