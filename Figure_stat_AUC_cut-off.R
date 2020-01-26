
# Plot function for cut-off Roc 

PLOT <- function(DL_RTest, DL_Rtrain, unit) {  
  
  
  roc <- roc(DL_RTest$FLAG,DL_RTest$RISK, main="Confidence intervals", percent=FALSE,
             ci=TRUE, # compute AUC (of AUC by default)
             print.auc=FALSE) # print the AUC (will contain the CI))
  
  
  
  Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                        "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                        "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                        "precision", "recall"),best.method="closest.topleft" )
  
  Threshold_Best_closest.topleft_roc <- Best_closest.topleft_roc[1]
  
  
  Data_NS <- subset(DL_Rtrain, DL_Rtrain[,2]==1)
  Data_S <- subset(DL_Rtrain, DL_Rtrain[,2]==0)
  Data_DL_threshold  <- subset(DL_Rtrain[,1], DL_Rtrain[,3]<Threshold_Best_closest.topleft_roc)
  
  
  par(mar = c(4, 4, 4, 4))
  
  plot(density(Data_S[,1]), pch=15, axes=FALSE, xlab="",xlim=range(Data_NS[,1]), ylim=c(0,max(g <- density(Data_S[,1])$y)), ylab="",main=" ", lwd = 1, col = "gray75")
  polygon(density(Data_S[,1]), col="gray90", border="gray90")
  
  
  polygon(density(Data_NS[,1]), col="gray80", border="gray80",ylim=c(0,max(g <- density(Data_NS[,1])$y)), alpha = 0.1)
  lines(density(Data_S[,1]), pch=15,  xlab="", ylab="", axes=FALSE, lwd = 1.5, type="l", col="gray90")
  
  
  par(new=TRUE)
  plot(DL_Rtrain[,1],  DL_Rtrain[,3], col = "black", pch = 21, bg = "grey", cex = 1, axes=FALSE, xlim=range(DL_Rtrain[,1]), ylim=c(0,1), xlab="", ylab="", main=" ")
  axis(2, ylim=c(0,1),col="black",las=0.5)  ## las=1 makes horizontal labels
  axis(1, DL_Rtrain[,1])
  axis(4, ylim=c(0, 1, by = 0.1),col="black", las=0.5)
  
  
  lines(range(DL_Rtrain[,1]), c(Threshold_Best_closest.topleft_roc, Threshold_Best_closest.topleft_roc), lwd = 1, col = "gray31")
  
  
  lines(c(min(Data_DL_threshold),min(Data_DL_threshold)) , c(-0.5,Threshold_Best_closest.topleft_roc), lwd = 0.5, col = "gray31")
  
  lines( c(max(Data_DL_threshold), max(Data_DL_threshold)), c(-0.5,Threshold_Best_closest.topleft_roc), lwd = 0.5, col = "gray31")
  arrows(x0 = min(Data_DL_threshold), y0 = 0, x1 = max(Data_DL_threshold), y1 = 0, angle = 90, code = 3, length = 0.08, lwd = 3,  col="green3")
  
  
  mtext(side = 1, line = 2, paste(unit), lwd = 0.3,  col="gray31")
  mtext(side = 2, line = 2, 'DL risk probability', lwd = 0.3,  col="gray31")
  mtext(side = 3, line = 2, 'Sensitivity/Specificity', lwd = 0.3,  col="gray31")
  mtext(side = 4, line = 2, paste("Probability cut-off", round(Threshold_Best_closest.topleft_roc,2)), lwd = 0.3,  col="gray31")
  
  
  box()
  
  par(new=TRUE)
  plot(roc$specificities, roc$thresholds, type="l", 
       col="firebrick", xlab=" ", ylab=" ", ann=FALSE, axes=FALSE, ylim = c(0,1) ,  xlim = c(0,1)) 
  
  lines(roc$sensitivities, roc$thresholds, ylim = c(0,1) , xlim = c(0,1), type="l", col = "dodgerblue3")
  axis(3, ylim=c(0,1, by = 1,0),col="black",las=0.5)
  
  
}

#----------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------#

# Stat function outcome

STAT_model <- function(DL_RTest, DL_Rtrain) {  
  roc <- roc(DL_RTest$FLAG,DL_RTest$RISK, main="Confidence intervals", percent=FALSE,
             ci=TRUE, # compute AUC (of AUC by default)
             print.auc=FALSE) # print the AUC (will contain the CI))
  
  Best_closest.topleft_roc <- coords(roc, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy",
                                                        "tn", "tp", "fn", "fp", "npv", "ppv", "1-specificity",
                                                        "1-sensitivity", "1-accuracy", "1-npv", "1-ppv",
                                                        "precision", "recall"),best.method="closest.topleft" )
  
  Threshold_Best_closest.topleft_roc <- Best_closest.topleft_roc[1]
  
  Data_NS <- subset(DL_Rtrain, DL_Rtrain[,2]==1)
  Data_S <- subset(DL_Rtrain, DL_Rtrain[,2]==0)
  Data_DL_threshold  <- subset(DL_Rtrain[,1], DL_Rtrain[,3]<Threshold_Best_closest.topleft_roc)
  
  Threshold_Best_closest.topleft <- Best_closest.topleft_roc[1]
  
  specificity <- Best_closest.topleft_roc[2]
  
  sensitivity <- Best_closest.topleft_roc[3]
  
  accuracy <- Best_closest.topleft_roc[4]
  
  summary_threshold_values <- summary(Data_DL_threshold)
  min_threshold_values <- as.vector(summary_threshold_values[1])
  max_threshold_values <- as.vector(summary_threshold_values[6])
  
  auc <- roc$auc
  CI <- as.vector(roc$ci)
  
  Stat <- data.frame( AUC=auc, CI95lower=CI[1], CI95upper=CI[3],Cutoff_probabilityt=Threshold_Best_closest.topleft, Specificity=specificity, Sensitivity=sensitivity, 
                      Accuracy=accuracy, min_threshold_values=min_threshold_values, max_threshold_values=max_threshold_values)
  Stat
}


##------------------------------------------------------------------------------------------------------------------##

# Stat univariante model cut-off
LACTATE_model_stat<- STAT_model(LACTATE_DL_RTest, LACTATE_DL_Rtrain)
PH_model_stat <- STAT_model(PH_DL_RTest, PH_DL_Rtrain)
BICARBONATE_model_stat<- STAT_model(BICARBONATEDL_RTest, BICARBONATE_DL_Rtrain)


POTASSIUM_model_stat <- STAT_model(POTASSIUM_DL_RTest, POTASSIUM_DL_RTrain)
CALCIUM_model_stat <- STAT_model(CALCIUM_DL_RTest, CALCIUM_DL_Rtrain)
GLUCOSE_model_stat <- STAT_model(GLUCOSE_DL_RTest, GLUCOSE_DL_Rtrain)
SODIUM_model_stat <- STAT_model(SODIUM_DL_RTest, SODIUM_DL_Rtrain)
CHLORIDE_model_stat <- STAT_model(CHLORIDE_DL_RTest, CHLORIDE_DL_Rtrain)


#Plot univariante model

#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§

par(oma = c(1.5, 0, 0, 0), mfrow = c(2, 2))

LACTATE<- PLOT(LACTATE_DL_RTest, LACTATE_DL_Rtrain, unit <-"Blood Lactate levels (mmol/L)")
PH <- PLOT(PH_DL_RTest, PH_DL_Rtrain, unit <-"Blood PH levels (Units)")
BICARBONATE<- PLOT(BICARBONATEDL_RTest, BICARBONATE_DL_Rtrain, unit <-"Blood Bicarbonate levels (mEq/L)")
POTASSIUM <- PLOT(POTASSIUM_DL_RTest, POTASSIUM_DL_RTrain, unit <-"Blood Potassium levels (mEq/L)")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottomleft", c("Survivors",  "Non-Survivors"), xpd = TRUE, horiz = TRUE, inset = c(0.03, -0.03), 
       bty = "n", pch = c(15, 15), col = c("gray90","gray80"  ),pt.cex=2.5, cex = 1)

legend("bottomright", c("Specificity", "Sensitivity"), xpd = TRUE, horiz = TRUE, inset = c(-0.09, -0.03), 
       bty = "n", lty=c(1, 1) , col = c("firebrick","dodgerblue3"), cex = 1)

#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§



par(oma = c(1.5, 0, 0, 0), mfrow = c(2, 2))

CALCIUM <- PLOT(CALCIUM_DL_RTest, CALCIUM_DL_Rtrain, unit <-"Blood Calcium levels (mg/dL)")
GLUCOSE <- PLOT(GLUCOSE_DL_RTest, GLUCOSE_DL_Rtrain, unit <-"Blood Glucose levels (mg/dL)")
CHLORIDE <- PLOT(CHLORIDE_DL_RTest, CHLORIDE_DL_Rtrain, unit <-"Blood Chloride levels (mEq/L)")
SODIUM <- PLOT(SODIUM_DL_RTest, SODIUM_DL_Rtrain, unit <-" Blood Sodium levels (mEq/L)")

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottomleft", c("Survivors",  "Non-Survivors"), xpd = TRUE, horiz = TRUE, inset = c(0.03, -0.03), 
       bty = "n", pch = c(15, 15), col = c("gray90","gray80"  ),pt.cex=2.5, cex = 1)

legend("bottomright", c("Specificity", "Sensitivity"), xpd = TRUE, horiz = TRUE, inset = c(-0.09, -0.03), 
       bty = "n", lty=c(1, 1) , col = c("firebrick","dodgerblue3"), cex = 1)


#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


# Auc model evaluation


AUC_model <- function(DL_RTest) {  
  roc <- roc(DL_RTest$FLAG,DL_RTest$RISK, main="Confidence intervals", percent=FALSE,
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


#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
#§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§


