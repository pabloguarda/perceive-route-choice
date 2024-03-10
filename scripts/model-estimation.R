#######################################################################################
###################### DCM USING REAL PARTICIPANTS CHOICE DATA ########################
#######################################################################################
#   1) Data formatting ------------------------------------------------------

#     i) Build dataset --------------------------------------------------------
cols_temp <- c("scenario","participantId","experimentType","gender","city","country","absoluteOrderScenario"
               ,"experimentalCondition","experimentalBlock","reactionTime","choice","certaintyLevel"
               ,"rationalRouteChosen","irrationalRouteChosen","waitingTimeImportance","travelTimeImportance"
               ,"journeyTimeImportance","waitingTimeReliabilityImportance","travelTimeReliabilityImportance"
               ,"timeCounting","missingExperiencedScenario","missingChoice","route","alternative","averageWaitingTime"
               ,"averageTravelTime","averageJourneyTime","variabilityWaitingTime","variabilityTravelTime"
               ,"variabilityJourneyTime","choiceId","alternativeChosen","alternativeChosenCertaintyLevel"
)

data <- DCMLongDataset[,cols_temp]

rational_alternatives <- subset(data,alternative== "rational")

rational_alternatives <- plyr::rename(rational_alternatives,c(averageWaitingTime="averageWaitingTimeR"))
rational_alternatives <- plyr::rename(rational_alternatives,c(averageTravelTime="averageTravelTimeR"))
rational_alternatives <- plyr::rename(rational_alternatives,c(averageJourneyTime="averageJourneyTimeR"))
rational_alternatives <- plyr::rename(rational_alternatives,c(variabilityWaitingTime="variabilityWaitingTimeR"))
rational_alternatives <- plyr::rename(rational_alternatives,c(variabilityTravelTime="variabilityTravelTimeR"))
rational_alternatives <- plyr::rename(rational_alternatives,c(variabilityJourneyTime="variabilityJourneyTimeR"))

irrational_alternatives <- subset(data,alternative== "irrational")

irrational_alternatives <- plyr::rename(irrational_alternatives,c(averageWaitingTime="averageWaitingTimeI"))
irrational_alternatives <- plyr::rename(irrational_alternatives,c(averageTravelTime="averageTravelTimeI"))
irrational_alternatives <- plyr::rename(irrational_alternatives,c(averageJourneyTime="averageJourneyTimeI"))
irrational_alternatives <- plyr::rename(irrational_alternatives,c(variabilityWaitingTime="variabilityWaitingTimeI"))
irrational_alternatives <- plyr::rename(irrational_alternatives,c(variabilityTravelTime="variabilityTravelTimeI"))
irrational_alternatives <- plyr::rename(irrational_alternatives,c(variabilityJourneyTime="variabilityJourneyTimeI"))

DCMData <- merge(rational_alternatives,irrational_alternatives
                 ,by = c("scenario","participantId","experimentType"))

#- Remove .x .y from variable names 

#Remove duplicated variables (e.g.those ended in [.y])

colnames1 <- colnames(DCMData)
colnames.x <- list()
count <- 0

for(i in 1:length(colnames1)){
  if(str_detect(colnames1[i],"[.]y")){
    count <- count + 1
    colnames.x[count] <- colnames1[i]
  }
}

DCMData <- remove.vars(DCMData,colnames.x)

#Remove .x from column labels
colnames(DCMData) <- str_replace(colnames(DCMData),"[.]x","")

#Remove dupplicated columns
DCMData <- DCMData[,unique(colnames(DCMData))]


#     ii) Processing real dataset -----------------------------------------------------

cols <- c("averageWaitingTimeR","averageTravelTimeR","variabilityWaitingTimeR","variabilityTravelTimeR"
          ,"averageWaitingTimeI","averageTravelTimeI","variabilityWaitingTimeI","variabilityTravelTimeI"
          ,"choice"
          ,"experimentType","experimentalCondition","scenario","participantId", "city"
)

DCMData <- DCMData[,cols]

simulatedData <-subset(DCMData, scenario %in% c("01","02","03","04","05","06","07","08"))

realChoiceData <- simulatedData

realChoiceData$realChoice <- ifelse(realChoiceData$choice == "rational",1,2)
realChoiceData$choice <- ifelse(realChoiceData$choice == "rational",1,2)

realChoiceData$experimentType <- ifelse(realChoiceData$experimentType == "descriptive","numeric","animated")

realChoiceData$w1 <- realChoiceData$averageWaitingTimeR
realChoiceData$v1 <- realChoiceData$averageTravelTimeR

realChoiceData$w2 <- realChoiceData$averageWaitingTimeI
realChoiceData$v2 <- realChoiceData$averageTravelTimeI



DCMDataRaw <- realChoiceData[,c("experimentType","experimentalCondition","w1","w2","v1","v2","choice","realChoice","scenario","city","participantId")]

#   2) Estimation -----------------------------------------------------------
#      Functions ---------------------------------------------------------------

logitProbability <- function(VA,VB){
  
  return(exp(VA)/(exp(VA)+exp(VB)))
  
}

#Grid search
MLEGridSearchOverTimePerceptionRealData <- function(exponents, data, logitParameters, simulationParameters){
  
  estimations <- data.frame()
  
  tempRealSimulationParametersNumeric <- simulationParameters
  
  for(i in 1:length(exponents)){
    
    exponent <- exponents[i]
    
    tempRealSimulationParametersNumeric[["cw"]] <- exponent
    tempRealSimulationParametersNumeric[["cv"]] <- exponent
    
    DCMNumeric <- LogitTimePerception(DCMData = data
                                      , logitParameters = logitParameters, method = "BHHH"
                                      , simulationParameters = tempRealSimulationParametersNumeric
                                      , sameCurvature = FALSE, sameTimeWeighting = FALSE, knownTimePerception = TRUE, boxCox = FALSE)
    
    # #Without grid search
    # 
    # DCMNumeric_NGS <- LogitTimePerception(DCMData = data
    #                                       , logitParameters = logitParameters, method = "BHHH"
    #                                       , simulationParameters = tempRealSimulationParametersNumeric
    #                                       , sameCurvature = TRUE, sameTimeWeighting = FALSE, knownTimePerception = FALSE)
    
    
    temp <- data.frame(cw = tempRealSimulationParametersNumeric[["cw"]], cv = tempRealSimulationParametersNumeric[["cv"]]
                       , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                       , ratioWV = DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]
                       , ll = logLik(DCMNumeric[[2]])
    )
    estimations <- rbind(estimations,temp)
    
    # numbers[[seed]] <- number
  }
  
  
  
  
  # return(numbers)
  return(estimations)
}


# Loglikelihood ratio test 

LogLikTimeTest <<- function(model_full, model_null){
  
  logLik_null <- logLik(model_null)
  logLik_full <- logLik(model_full)
  
  # Calculate the test statistic
  LRT_statistic <- 2 * (logLik_full - logLik_null)
  
  # Degrees of freedom
  df <- length(coef(model_full)) - length(coef(model_null))
  
  # Compute the p-value
  p_value <- pchisq(LRT_statistic, df, lower.tail = FALSE)
  
  cat("LRT Statistic:", LRT_statistic, "\n")
  cat("P-value:", p_value, "\n")
  
}

#     i) All ------------------------------------------------------------------
#       * Numeric -------------------------------------------------------------

#         - Control ---------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNCFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNCFreeC

#           + c grid search (GS) -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, 

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 
exponentsNumeric <- seq(0.1,1.5,0.01)
DCMResultsNCGSC = MLEGridSearchOverTimePerceptionRealData(exponents = exponentsNumeric
                                                          , data =  DCMData0
                                                          , logitParameters = logitParameters0
                                                          , simulationParameters = simulationParameters
)


maxLL_GS <- max(DCMResultsNCGSC$ll)
cMaxLL_GS <- DCMResultsNCGSC[which(DCMResultsNCGSC$ll == maxLL_GS),]$cw



#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNCFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                            , simulationParameters = simulationParameters
                                            , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNCFixedC09



#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNCFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                          , simulationParameters = simulationParameters
                                          , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNCFixedC

-1.558563/-1.285038




#         - Treatment ---------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNTFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNTFreeC


#           + c grid search (GS) -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, 

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 
exponentsNumeric <- seq(0.1,1.5,0.01)
DCMResultsNTGSC = MLEGridSearchOverTimePerceptionRealData(exponents = exponentsNumeric
                                                          , data =  DCMData0
                                                          , logitParameters = logitParameters0
                                                          , simulationParameters = simulationParameters
)


maxLL_GS <- max(DCMResultsNTGSC$ll)
cMaxLL_GS <- DCMResultsNTGSC[which(DCMResultsNTGSC$ll == maxLL_GS),]$cw
#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNTFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                            , simulationParameters = simulationParameters
                                            , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNTFixedC09
#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNTFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                          , simulationParameters = simulationParameters
                                          , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNTFixedC



#         - Control and Treatment -------------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                        , simulationParameters = simulationParameters
                                        , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNFreeC
#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                           , simulationParameters = simulationParameters
                                           , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNFixedC09
#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNFixedC

#       * Animated ----------------------------------------------------------------
#         - Control ---------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1.25,thetaPV = 1.25, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsACFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFreeC

#           + c grid search (GS) -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, 

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 
exponentsAnimated <- seq(0.1,1.5,0.01)
DCMResultsACGSC = MLEGridSearchOverTimePerceptionRealData(exponents = exponentsAnimated
                                                          , data =  DCMData0
                                                          , logitParameters = logitParameters0
                                                          , simulationParameters = simulationParameters
)


maxLL_GS <- max(DCMResultsACGSC$ll)
cMaxLL_GS <- DCMResultsACGSC[which(DCMResultsACGSC$ll == maxLL_GS),]$cw
#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsACFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                            , simulationParameters = simulationParameters
                                            , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedC09

#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsACFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                          , simulationParameters = simulationParameters
                                          , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedC






#         - Treatment ---------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsATFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsATFreeC
#           + c grid search (GS) -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, 

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 
exponentsAnimated <- seq(0.1,1.5,0.01)
DCMResultsATGSC = MLEGridSearchOverTimePerceptionRealData(exponents = exponentsAnimated
                                                          , data =  DCMData0
                                                          , logitParameters = logitParameters0
                                                          , simulationParameters = simulationParameters
)


maxLL_GS <- max(DCMResultsATGSC$ll)
cMaxLL_GS <- DCMResultsATGSC[which(DCMResultsATGSC$ll == maxLL_GS),]$cw
#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsATFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                            , simulationParameters = simulationParameters
                                            , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsATFixedC09



#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsATFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                          , simulationParameters = simulationParameters
                                          , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsATFixedC




#         - Control and Treatment -------------------------------------------------------------------
#           + c estimated -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 1,thetaPV = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsAFreeC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                        , simulationParameters = simulationParameters
                                        , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAFreeC
#           + c = 0.9 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  0.9, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsAFixedC09 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                           , simulationParameters = simulationParameters
                                           , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAFixedC09
#           + c = 1 -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw,experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsAFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                         , simulationParameters = simulationParameters
                                         , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAFixedC

#       * Summary tables -------------------------------------------------------

#Numeric condition
DCMResultsNCFixedC09
DCMResultsNCFixedC
DCMResultsNTFixedC09
DCMResultsNTFixedC

#Animated Condition
DCMResultsACFixedC09
DCMResultsACFixedC
DCMResultsATFixedC09
DCMResultsATFixedC

#Variables
DCMResultsNCFixedC09[[1]][,"estimate"]
DCMResultsNCFixedC09[[1]][,"ttest"]
DCMResultsNCFixedC09[[2]]$maximum
AIC(DCMResultsNCFixedC09[[2]])

DCMResultsNCFixedC09[[1]][,"estimate"][["thetaW"]]
DCMResultsNCFixedC09[[1]][,"estimate"][["thetaV"]]
# DCMResultsNCFixedC09[[1]][,"estimate"][["thetaP"]]
DCMResultsNCFixedC09[[1]][,"estimate"][["thetaW"]]/DCMResultsNCFixedC09[[1]][,"estimate"][["thetaV"]]

#Summary table of estimation results

#-Animated choices
DCMModelsAnimatedChoices <- list(DCMResultsACFixedC09,DCMResultsACFixedC,DCMResultsATFixedC09,DCMResultsATFixedC)
DCMModelsNamesAnimatedChoices <- list("DCMResultsACFixedC09","DCMResultsACFixedC","DCMResultsATFixedC09","DCMResultsATFixedC")
summmaryTableDCMAnimatedChoices <- data.frame()

for(i in 1:length(DCMModelsAnimatedChoices)){
  
  model <- DCMModelsAnimatedChoices[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model <-  DCMModelsNamesAnimatedChoices[[i]]
  modelRow$thetaW <-  paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV <-  paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  
  #T test for c is respect to 1 
  # tTestC1 <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/DCMResultsACFreeC[[1]][,"se"][["thetaP"]],1))
  # modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTestC1,")",sep=""))
  modelRow$c <- ifelse(str_detect(modelRow$model,"FixedC09"),"0.900 (fixed)", "1.000 (fixed)")
  
  modelRow$ratioWV <- round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll <- round(model[[2]]$maximum,1)
  modelRow$AIC <-  round(AIC(model[[2]]),1)
  modelRow$obs = 144 #dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMAnimatedChoices <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMAnimatedChoices <- rbind(summmaryTableDCMAnimatedChoices,modelRow)
}

View(t(summmaryTableDCMAnimatedChoices))

#-Numeric choices

DCMModelsNumericChoices <- list(DCMResultsNCFixedC09,DCMResultsNCFixedC,DCMResultsNTFixedC09,DCMResultsNTFixedC)
DCMModelsNamesNumericChoices <- list("DCMResultsNCFixedC09","DCMResultsNCFixedC","DCMResultsNTFixedC09","DCMResultsNTFixedC")
summmaryTableDCMNumericChoices <- data.frame()


for(i in 1:length(DCMModelsNumericChoices)){
  
  model <- DCMModelsNumericChoices[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesNumericChoices[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # #T test for c is respect to 1 
  # tTestC1 <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/model[[1]][,"se"][["thetaP"]],1))
  # modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTestC1,")",sep=""))
  modelRow$c <- ifelse(str_detect(modelRow$model,"FixedC09"),"0.900 (fixed)", "1.000 (fixed)")
  
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = 144 #dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMNumericChoices <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMNumericChoices <- rbind(summmaryTableDCMNumericChoices,modelRow)
}

# View(t(summmaryTableDCMNumericChoices))






#     ii) Analysis by city ------------------------------------------------------------------
#       * Numeric -------------------------------------------------------------

#         - Control ---------------------------------------------------------------
#           + Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNCFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                  , simulationParameters = simulationParameters
                                                  , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNCFixedCSantiago

-1.558563/-1.285038

#           + London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNCFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                , simulationParameters = simulationParameters
                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNCFixedCLondon

-1.558563/-1.285038




#         - Treatment ---------------------------------------------------------------
#           + Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "numeric"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNTFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                  , simulationParameters = simulationParameters
                                                  , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNTFixedCSantiago



#           + London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "numeric"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNTFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                , simulationParameters = simulationParameters
                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNTFixedCLondon



#       * Animated ----------------------------------------------------------------
#         - Control ---------------------------------------------------------------
#           + Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsACFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                  , simulationParameters = simulationParameters
                                                  , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedCSantiago



# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 1, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsACFixedCGenericTravelTimeSantiago <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                   , simulationParameters = simulationParameters
                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedCGenericTravelTimeSantiago


LogLikTimeTest(DCMResultsACFixedCSantiago[[2]],DCMResultsACFixedCGenericTravelTimeSantiago[[2]])


#           + London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsACFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                , simulationParameters = simulationParameters
                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedCLondon


# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 1, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsACFixedCGenericTravelTimeLondon <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                   , simulationParameters = simulationParameters
                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsACFixedCGenericTravelTimeLondon


LogLikTimeTest(DCMResultsACFixedCLondon[[2]],DCMResultsACFixedCGenericTravelTimeLondon[[2]])


#         - Treatment ---------------------------------------------------------------
#           + Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "animated"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsATFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                  , simulationParameters = simulationParameters
                                                  , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsATFixedCSantiago

#           + London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "animated"  & experimentalCondition == "treatment")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsATFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                , simulationParameters = simulationParameters
                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsATFixedCLondon

#       * Summary tables -------------------------------------------------------

#Animated Choices
DCMResultsACFixedCSantiago
DCMResultsACFixedCLondon
DCMResultsATFixedCSantiago
DCMResultsATFixedCLondon


#Numeric choices
DCMResultsNCFixedCSantiago
DCMResultsNCFixedCLondon
DCMResultsNTFixedCSantiago
DCMResultsNTFixedCLondon

#Summary table of estimation results

#-Animated choices
DCMModelsAnimatedChoicesByCity <- list(DCMResultsACFixedCSantiago,DCMResultsACFixedCLondon,DCMResultsATFixedCSantiago,DCMResultsATFixedCLondon)
DCMModelsNamesAnimatedChoicesByCity <- list("DCMResultsACFixedCSantiago","DCMResultsACFixedCLondon","DCMResultsATFixedCSantiago","DCMResultsATFixedCLondon")
summmaryTableDCMAnimatedChoicesByCity <- data.frame()

for(i in 1:length(DCMModelsAnimatedChoicesByCity)){
  
  model <- DCMModelsAnimatedChoicesByCity[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesAnimatedChoicesByCity[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # modelRow$c =ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)",paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",round(model[[1]][,"ttest"][["thetaP"]],1),")",sep=""))[[1]]
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMAnimatedChoicesByCity <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMAnimatedChoicesByCity <- rbind(summmaryTableDCMAnimatedChoicesByCity,modelRow)
}

# View(t(summmaryTableDCMAnimatedChoicesByCity))

#-Numeric choices

DCMModelsNumericChoicesByCity <- list(DCMResultsNCFixedCSantiago,DCMResultsNCFixedCLondon,DCMResultsNTFixedCSantiago,DCMResultsNTFixedCLondon)
DCMModelsNamesNumericChoicesByCity <- list("DCMResultsNCFixedCSantiago","DCMResultsNCFixedCLondon","DCMResultsNTFixedCSantiago","DCMResultsNTFixedCLondon")
summmaryTableDCMNumericChoicesByCity <- data.frame()

for(i in 1:length(DCMModelsNumericChoices)){
  
  model <- DCMModelsNumericChoicesByCity[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesNumericChoicesByCity[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # modelRow$c =ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)",paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",round(model[[1]][,"ttest"][["thetaP"]],1),")",sep=""))[[1]]
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMNumericChoicesByCity <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMNumericChoicesByCity <- rbind(summmaryTableDCMNumericChoicesByCity,modelRow)
}

View(t(summmaryTableDCMNumericChoicesByCity))

#     iii) Analysis by city and experiment [Paper] ------------------------------------------------------------------
#       * Animated ----------------------------------------------------------------
#         - Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsAnimatedFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                   , simulationParameters = simulationParameters
                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCSantiago

# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 1, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsAnimatedFixedCGenericTravelTimeSantiago <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                   , simulationParameters = simulationParameters
                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCGenericTravelTimeSantiago 


LogLikTimeTest(DCMResultsAnimatedFixedCSantiago[[2]],DCMResultsAnimatedFixedCGenericTravelTimeSantiago [[2]])


#         - London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 0, thetaV = 0, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 1, sw = 0, sv= 0) 

DCMResultsAnimatedFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                 , simulationParameters = simulationParameters
                                                                 , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCLondon

# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 0, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsAnimatedFixedCGenericTravelTimeLondon <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                         , simulationParameters = simulationParameters
                                                                         , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCGenericTravelTimeLondon 


LogLikTimeTest(DCMResultsAnimatedFixedCLondon[[2]],DCMResultsAnimatedFixedCGenericTravelTimeLondon[[2]])

#       * Numeric ----------------------------------------------------------------
#         - Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsNumericFixedCSantiago<- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                 , simulationParameters = simulationParameters
                                                                 , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCSantiago

# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 0, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsNumericFixedCGenericTravelTimeSantiago <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                      , simulationParameters = simulationParameters
                                                                      , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCGenericTravelTimeSantiago 


LogLikTimeTest(DCMResultsNumericFixedCSantiago[[2]],DCMResultsNumericFixedCGenericTravelTimeSantiago[[2]])

#         - London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNumericFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                , simulationParameters = simulationParameters
                                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCLondon

# Model with generic parameter for travel time 

DCMData0_copy = DCMData0
DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2

logitParameters0 <- c(thetaW = 0, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0)

DCMResultsNumericFixedCGenericTravelTimeLondon <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
                                                                       , simulationParameters = simulationParameters
                                                                       , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCGenericTravelTimeLondon 


LogLikTimeTest(DCMResultsNumericFixedCLondon[[2]],DCMResultsNumericFixedCGenericTravelTimeLondon[[2]])

#       * Summary table -------------------------------------------------------

#Animated Choices
DCMResultsAnimatedFixedCSantiago
DCMResultsAnimatedFixedCLondon

#Numeric choices
DCMResultsNumericFixedCSantiago
DCMResultsNumericFixedCLondon

#Summary table of estimation results
DCMModelsByCity <- list(DCMResultsNumericFixedCSantiago,DCMResultsAnimatedFixedCSantiago,DCMResultsNumericFixedCLondon,DCMResultsAnimatedFixedCLondon)
DCMModelsNamesByCity <- list("DCMResultsNumericFixedCSantiago","DCMResultsAnimatedFixedCSantiago","DCMResultsNumericFixedCLondon","DCMResultsAnimatedFixedCLondon")
summmaryTableDCMByCity <- data.frame()

for(i in 1:length(DCMModelsByCity)){
  
  model <- DCMModelsByCity[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesByCity[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  # modelRow$thetaWT = paste(round(model[[1]][,"estimate"][["thetaWT"]],3), " (",round(model[[1]][,"ttest"][["thetaWT"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  # modelRow$ratioWVT = round((model[[1]][,"estimate"][["thetaW"]]+model[[1]][,"estimate"][["thetaWT"]])/model[[1]][,"estimate"][["thetaV"]],3)
  # modelRow$deltaRatioWV = with(modelRow,ratioWVT-ratioWV)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMByCity <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMByCity <- rbind(summmaryTableDCMByCity,modelRow)
}

View(t(summmaryTableDCMByCity))




#     iv) Analysis by city and experiment (with interaction) [Paper] ------------------------------------------------------------------
#       * Animated ----------------------------------------------------------------
#         - Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsAnimatedFixedCSantiagoInteraction <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                   , simulationParameters = simulationParameters
                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCSantiagoInteraction
#         - London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 1, sw = 0, sv= 0) 

DCMResultsAnimatedFixedCLondonInteraction <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                 , simulationParameters = simulationParameters
                                                                 , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsAnimatedFixedCLondonInteraction

#       * Numeric ----------------------------------------------------------------
#         - Santiago -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsNumericFixedCSantiagoInteraction<- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                 , simulationParameters = simulationParameters
                                                                 , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCSantiagoInteraction

#         - London -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMResultsNumericFixedCLondonInteraction <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                                , simulationParameters = simulationParameters
                                                                , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
DCMResultsNumericFixedCLondonInteraction




#       * Summary table -------------------------------------------------------

#Animated Choices
DCMResultsAnimatedFixedCSantiagoInteraction
DCMResultsAnimatedFixedCLondonInteraction

#Numeric choices
DCMResultsNumericFixedCSantiagoInteraction
DCMResultsNumericFixedCLondonInteraction

#Summary table of estimation results
DCMModelsByCity <- list(DCMResultsNumericFixedCSantiagoInteraction,DCMResultsAnimatedFixedCSantiagoInteraction,DCMResultsNumericFixedCLondonInteraction,DCMResultsAnimatedFixedCLondonInteraction)
DCMModelsNamesByCity <- list("DCMResultsNumericFixedCSantiagoInteraction","DCMResultsAnimatedFixedCSantiagoInteraction","DCMResultsNumericFixedCLondonInteraction","DCMResultsAnimatedFixedCLondonInteraction")
summmaryTableDCMByCity <- data.frame()

for(i in 1:length(DCMModelsByCity)){
  
  model <- DCMModelsByCity[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA,thetaWT = NA, ratioWV =NA, ratioWVT = NA, deltaRatioWV = NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesByCity[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaWT = paste(round(model[[1]][,"estimate"][["thetaWT"]],3), " (",round(model[[1]][,"ttest"][["thetaWT"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ratioWVT = round((model[[1]][,"estimate"][["thetaW"]]+model[[1]][,"estimate"][["thetaWT"]])/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$deltaRatioWV = with(modelRow,ratioWVT-ratioWV)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMByCity <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMByCity <- rbind(summmaryTableDCMByCity,modelRow)
}

# View(t(summmaryTableDCMByCity))



#   3) Box-cox estimation ---------------------------------------------------
#      * All ---------------------------------------------------------------------


#Animated Control

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxCAnimatedControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                    , simulationParameters = simulationParameters
                                                    , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCAnimatedControlAll

DCMConventionalCAnimatedControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                          , simulationParameters = simulationParameters
                                                          , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCAnimatedControlAll

#Animated

DCMData0 <- subset(DCMDataRaw,experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxCAnimatedAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                    , simulationParameters = simulationParameters
                                                    , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCAnimatedAll

DCMConventionalCAnimatedAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                          , simulationParameters = simulationParameters
                                                          , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCAnimatedAll




#Numeric control

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

DCMBoxCoxCNumericControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                   , simulationParameters = simulationParameters
                                                   , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCNumericControlAll

DCMConventionalCNumericControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                         , simulationParameters = simulationParameters
                                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCNumericControlAll

# Numeric
DCMData0 <- subset(DCMDataRaw,experimentType == "numeric")
DCMData0$choice <- DCMData0$realChoice

DCMBoxCoxCNumericAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                   , simulationParameters = simulationParameters
                                                   , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCNumericAll

DCMConventionalCNumericAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                         , simulationParameters = simulationParameters
                                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCNumericAll


#        + Summary table (Animated vs Numeric) -------------------------------------------------------

#Animated
DCMBoxCoxCAnimatedAll
DCMConventionalCAnimatedAll

#Numeric
DCMBoxCoxCNumericAll
DCMConventionalCNumericAll

#Summary table of estimation results
DCMModelsExperiment <- list(DCMConventionalCAnimatedAll,DCMConventionalCNumericAll,DCMBoxCoxCAnimatedAll,DCMBoxCoxCNumericAll)
DCMModelsNamesExperiment <- list("DCMConventionalCAnimatedAll","DCMConventionalCNumericAll","DCMBoxCoxCAnimatedAll","DCMBoxCoxCNumericAll")
summmaryTableDCMExperiment <- data.frame()

for(i in 1:length(DCMModelsExperiment)){
  
  model <-DCMModelsExperiment[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesExperiment[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # #T test for c is respect to 1 
  # tTest <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/model[[1]][,"se"][["thetaP"]],1))
  
  #T test respecto to 0
  tTest <- round(model[[1]][,"ttest"][["thetaP"]],1)
  
  modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTest,")",sep=""))
  
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMExperiment <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMExperiment <- rbind(summmaryTableDCMExperiment,modelRow)
}

View(t(summmaryTableDCMExperiment))


#      * Control Condition ---------------------------------------------------------------------


#Animated Control

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxCAnimatedControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                    , simulationParameters = simulationParameters
                                                    , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCAnimatedControlAll

DCMConventionalCAnimatedControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                          , simulationParameters = simulationParameters
                                                          , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCAnimatedControlAll


#Numeric control

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")
DCMData0$choice <- DCMData0$realChoice

DCMBoxCoxCNumericControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                   , simulationParameters = simulationParameters
                                                   , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCNumericControlAll

DCMConventionalCNumericControlAll <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                         , simulationParameters = simulationParameters
                                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCNumericControlAll


#        + Summary table (Animated vs Numeric Control) -------------------------------------------------------

#Animated
DCMBoxCoxCAnimatedControlAll
DCMConventionalCAnimatedControlAll

#Numeric
DCMBoxCoxCNumericControlAll
DCMConventionalCNumericControlAll

#Summary table of estimation results
DCMModelsExperiment <- list(DCMConventionalCAnimatedControlAll,DCMConventionalCNumericControlAll,DCMBoxCoxCAnimatedControlAll,DCMBoxCoxCNumericControlAll)
DCMModelsNamesExperiment <- list("DCMConventionalCAnimatedControlAll","DCMConventionalCNumericControlAll","DCMBoxCoxCAnimatedControlAll","DCMBoxCoxCNumericControlAll")
summmaryTableDCMExperiment <- data.frame()

for(i in 1:length(DCMModelsExperiment)){
  
  model <-DCMModelsExperiment[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = NA, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesExperiment[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # #T test for c is respect to 1 
  # tTest <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/model[[1]][,"se"][["thetaP"]],1))
  
  #T test respecto to 0
  tTest <- round(model[[1]][,"ttest"][["thetaP"]],1)
  
  modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTest,")",sep=""))
  
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMExperiment <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMExperiment <- rbind(summmaryTableDCMExperiment,modelRow)
}

View(t(summmaryTableDCMExperiment))

#       * By city and between subjects condition (animated vs numeric) [Paper] ----------
#         - Santiago ----------------------------------------------------------------
#           $ Animated --------------------------------------------------------------



#Animated Santiago

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & city == "Santiago")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxAnimatedSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                    , simulationParameters = simulationParameters
                                                    , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxAnimatedSantiago

# # Generic 
# 
# DCMData0_copy = DCMData0
# DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
# DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2
# 
# logitParameters0 <- c(thetaW = 1, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
#                       , thetaPW = 0,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1
# 
# simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
#                           , betaWeight =1,awt = 0, sw = 0, sv= 0) 
# 
# DCMBoxCoxAnimatedGenericTravelTimeSantiago <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
#                                                                    , simulationParameters = simulationParameters
#                                                                    , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
# DCMBoxCoxAnimatedGenericTravelTimeSantiago
# 
# 
# LogLikTimeTest(DCMBoxCoxAnimatedSantiago[[2]],DCMBoxCoxAnimatedGenericTravelTimeSantiago[[2]])

LogLikTimeTest(DCMBoxCoxAnimatedSantiago[[2]], DCMResultsAnimatedFixedCGenericTravelTimeSantiago[[2]]) 


#           $ Numeric ---------------------------------------------------------------

#Numeric Santiago

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & city == "Santiago")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxNumericSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                   , simulationParameters = simulationParameters
                                                   , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxNumericSantiago

# Generic 
# DCMData0_copy = DCMData0
# DCMData0_copy$w1 = DCMData0_copy$w1 + DCMData0_copy$v1
# DCMData0_copy$w2 = DCMData0_copy$w2 + DCMData0_copy$v2
# 
# logitParameters0 <- c(thetaW = 1, thetaV = NULL, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
#                       , thetaPW = 0,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1
# 
# simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
#                           , betaWeight =1,awt = 0, sw = 0, sv= 0) 
# 
# DCMBoxCoxNumericGenericTravelTimeSantiago <- LogitTimePerception(DCMData = DCMData0_copy, logitParameters = logitParameters0, method = "BHHH"
#                                                                   , simulationParameters = simulationParameters
#                                                                   , sameCurvature = FALSE, sameTimeWeighting = FALSE, boxCox = FALSE)
# DCMBoxCoxNumericGenericTravelTimeSantiago


LogLikTimeTest(DCMBoxCoxNumericSantiago[[2]], DCMResultsNumericFixedCGenericTravelTimeSantiago[[2]])



#         - London ----------------------------------------------------------------
#           $ Animated ----------------------------------------------------------------



#Animated London

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & city == "London")
DCMData0$choice <- DCMData0$realChoice

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 0.9, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 

DCMBoxCoxAnimatedLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                               , simulationParameters = simulationParameters
                                               , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxAnimatedLondon


LogLikTimeTest(DCMBoxCoxAnimatedLondon[[2]], DCMResultsAnimatedFixedCGenericTravelTimeLondon[[2]]) 


#           $ Numeric ---------------------------------------------------------------


# Numeric London
DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & city == "London")
DCMData0$choice <- DCMData0$realChoice

DCMBoxCoxNumericLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                            , simulationParameters = simulationParameters
                                            , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxNumericLondon

LogLikTimeTest(DCMBoxCoxNumericLondon[[2]], DCMResultsNumericFixedCGenericTravelTimeLondon[[2]]) 


#        + Summary table  -------------------------------------------------------

#Animated
DCMBoxCoxAnimatedSantiago
DCMBoxCoxAnimatedLondon

#Numeric
DCMBoxCoxNumericSantiago
DCMBoxCoxNumericLondon

#Summary table of estimation results
DCMModelsExperiment <- list(DCMBoxCoxNumericSantiago,DCMBoxCoxAnimatedSantiago,DCMBoxCoxNumericLondon,DCMBoxCoxAnimatedLondon)
DCMModelsNamesExperiment <- list("DCMBoxCoxNumericSantiago","DCMBoxCoxAnimatedSantiago",",DCMBoxCoxNumericLondon","DCMBoxCoxAnimatedLondon")
summmaryTableDCMExperiment <- data.frame()

for(i in 1:length(DCMModelsExperiment)){
  
  model <-DCMModelsExperiment[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = NA, ratioWV =NA, tTest1= NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesExperiment[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  # #T test for c is respect to 1 
  tTest1 <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/model[[1]][,"se"][["thetaP"]],1))
  
  #T test respecto to 0
  tTest <- round(model[[1]][,"ttest"][["thetaP"]],1)
  
  modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTest,")",sep=""))
  
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$tTest1 =round(tTest1,1)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMExperiment <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMExperiment <- rbind(summmaryTableDCMExperiment,modelRow)
}

View(t(summmaryTableDCMExperiment))



#      * Participants who pass the perception test -----------------------------------

# #Parameters included in the estimation are not null
# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
#                       , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1
# 
# #Set the initial values (cw set the initial value for c)
# simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
#                           , betaWeight =1,awt = 0, sw = 0, sv= 0) 

#Animated

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")

rationalParticipantsControlCondition <- subset(manipulationChecksParticipantsS1S2Rational,experimentalCondition == "control")

dim(rationalParticipantsControlCondition)[1]

DCMData0 <- subset(DCMData0, participantId %in% manipulationChecksParticipantsS1S2Rational$participantId)

DCMBoxCoxCAnimatedControlPassTest <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                         , simulationParameters = simulationParameters
                                                         , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxCAnimatedControlPassTest

DCMConventionalCAnimatedControlPassTest <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                               , simulationParameters = simulationParameters
                                                               , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCAnimatedControlPassTest

#Numeric

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")

rationaParticipantsControlCondition <- subset(manipulationChecksParticipantsS1S2Rational,experimentalCondition == "control")

dim(rationaParticipantsControlCondition)[1]

DCMData0 <- subset(DCMData0, participantId %in% manipulationChecksParticipantsS1S2Rational$participantId)

DCMBoxCoxCNumericControlPassTest <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                        , simulationParameters = simulationParameters
                                                        , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)


DCMBoxCoxCNumericControlPassTest

DCMConventionalCNumericControlPassTest <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                              , simulationParameters = simulationParameters
                                                              , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalCNumericControlPassTest

#        + Summary table (Animated vs Numeric) -------------------------------------------------------

#Animated
DCMBoxCoxCAnimatedControlPassTest
DCMConventionalCAnimatedControlPassTest

#Numeric
DCMBoxCoxCNumericControlPassTest
DCMConventionalCNumericControlPassTest

#Summary table of estimation results
DCMModelsByTestAndExperiment <- list(DCMConventionalCAnimatedControlPassTest,DCMConventionalCNumericControlPassTest,DCMBoxCoxCAnimatedControlPassTest,DCMBoxCoxCNumericControlPassTest)
DCMModelsNamesTestAndExperiment <- list("DCMConventionalCAnimatedControlPassTest","DCMConventionalCNumericControlPassTest","DCMBoxCoxCAnimatedControlPassTest","DCMBoxCoxCNumericControlPassTest")
summmaryTableDCMByTestAndExperiment <- data.frame()

for(i in 1:length(DCMModelsByTestAndExperiment)){
  
  model <-DCMModelsByTestAndExperiment[[i]]
  modelRow <- data.frame(model = NA, thetaW = NA,thetaV = NA, c = 0, ratioWV =NA, ll = NA, AIC = NA, obs = NA)
  modelRow$model = DCMModelsNamesTestAndExperiment[[i]]
  modelRow$thetaW = paste(round(model[[1]][,"estimate"][["thetaW"]],3), " (",round(model[[1]][,"ttest"][["thetaW"]],1),")",sep="")
  modelRow$thetaV = paste(round(model[[1]][,"estimate"][["thetaV"]],3), " (",round(model[[1]][,"ttest"][["thetaV"]],1),")",sep="")
  #T test for c is respect to 1 
  tTestC1 <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"",round((model[[1]][,"estimate"][["thetaP"]] - 1)/model[[1]][,"se"][["thetaP"]],1))
  modelRow$c <- ifelse(is.na(model[[1]][,"estimate"]["thetaP"]),"1.000 (fixed)", paste(round(model[[1]][,"estimate"][["thetaP"]],3)," (",tTestC1,")",sep=""))
  
  modelRow$ratioWV = round(model[[1]][,"estimate"][["thetaW"]]/model[[1]][,"estimate"][["thetaV"]],3)
  modelRow$ll =round(model[[2]]$maximum,1)
  modelRow$AIC = round(AIC(model[[2]]),1)
  modelRow$obs = dim(DCMData0)[1]
  
  if(i == 1){
    summmaryTableDCMByTestAndExperiment <- modelRow
  }
  
  if(i > 1)
    summmaryTableDCMByTestAndExperiment <- rbind(summmaryTableDCMByTestAndExperiment,modelRow)
}

View(t(summmaryTableDCMByTestAndExperiment))

#      * Participants who do not pass the perception test -----------------------------------

#Parameters included in the estimation are not null
logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = 0,thetaPV = 0, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

#Set the initial values (cw set the initial value for c)
simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1.5, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


#Animated

DCMData0 <- subset(DCMDataRaw,experimentType == "animated" & experimentalCondition == "control")

rationaParticipantsControlCondition <- subset(manipulationChecksParticipantsS1S2Rational,experimentalCondition == "control")

36-dim(rationaParticipantsControlCondition)[1]

DCMData0 <- subset(DCMData0, !(participantId %in% manipulationChecksParticipantsS1S2Rational$participantId))

DCMBoxCoxC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "SANN"
                                  , simulationParameters = simulationParameters
                                  , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxC

DCMConventionalC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "SANN"
                                        , simulationParameters = simulationParameters
                                        , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalC

#Numeric

DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "control")

rationaParticipantsControlCondition <- subset(manipulationChecksParticipantsS1S2Rational,experimentalCondition == "control")

dim(rationaParticipantsControlCondition)[1]

DCMData0 <- subset(DCMData0, !(participantId %in% manipulationChecksParticipantsS1S2Rational$participantId))

DCMBoxCoxC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                  , simulationParameters = simulationParameters
                                  , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = TRUE)

DCMBoxCoxC

DCMConventionalC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                        , simulationParameters = simulationParameters
                                        , sameCurvature = TRUE, sameTimeWeighting = FALSE, boxCox = FALSE)

DCMConventionalC

#   4) Behavioral parameters -----------------------------------------------
#   i) Table ------------------------------------------------------------------
#     + Santiago ----------------------------------------------------------------
#       - bw/bv -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "Santiago" & experimentType == "numeric") # & experimentalCondition == "control"
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsNumericFixedCSantiago <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                       , simulationParameters = simulationParameters
                                                       , sameCurvature = FALSE, sameTimeWeighting = FALSE)
DCMResultsNumericFixedCSantiago

bwbvRatioSantiago <- DCMResultsNumericFixedCSantiago[[1]][,"estimate"][["thetaW"]]/DCMResultsNumericFixedCSantiago[[1]][,"estimate"][["thetaV"]]
bwbvRatioSantiago 

#       - aw/av -----------------------------------------------------------------

awavRatioSantiago <- 1/bwbvRatioSantiago*DCMResultsAnimatedFixedCSantiagoInteraction[[1]][,"estimate"][["thetaW"]]/DCMResultsAnimatedFixedCSantiagoInteraction[[1]][,"estimate"][["thetaV"]]

#       - awt/avt -----------------------------------------------------------------

#We express it in preference units, that is why it is divided by the estimate of theta W (aw x bw) obtained from the animated experiment in the control condition
awtbwSantiago <- 1+DCMResultsAnimatedFixedCSantiagoInteraction[[1]][,"estimate"][["thetaWT"]]/DCMResultsAnimatedFixedCSantiagoInteraction[[1]][,"estimate"][["thetaW"]]

#     + London ----------------------------------------------------------------
#       - bw/bv -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, city == "London" & experimentType == "numeric") # & experimentalCondition == "control"
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsNumericFixedCLondon <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                     , simulationParameters = simulationParameters
                                                     , sameCurvature = FALSE, sameTimeWeighting = FALSE)
DCMResultsNumericFixedCLondon

bwbvRatioLondon <- DCMResultsNumericFixedCLondon[[1]][,"estimate"][["thetaW"]]/DCMResultsNumericFixedCLondon[[1]][,"estimate"][["thetaV"]]
bwbvRatioLondon 


#       - aw/av -----------------------------------------------------------------

awavRatioLondon <- 1/bwbvRatioLondon*DCMResultsAnimatedFixedCLondonInteraction[[1]][,"estimate"][["thetaW"]]/DCMResultsAnimatedFixedCLondonInteraction[[1]][,"estimate"][["thetaV"]]

#       - awt/avt -----------------------------------------------------------------

awtbwLondon <- 1+DCMResultsAnimatedFixedCLondonInteraction[[1]][,"estimate"][["thetaWT"]]/DCMResultsAnimatedFixedCLondonInteraction[[1]][,"estimate"][["thetaW"]]

#     + All ----------------------------------------------------------------
#       - bw/bv -----------------------------------------------------------------

DCMData0 <- subset(DCMDataRaw, experimentType == "numeric") # & experimentalCondition == "control"
DCMData0$choice <- DCMData0$realChoice

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsNumericFixedC <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                               , simulationParameters = simulationParameters
                                               , sameCurvature = FALSE, sameTimeWeighting = FALSE)
DCMResultsNumericFixedC

bwbvRatio <- DCMResultsNumericFixedC[[1]][,"estimate"][["thetaW"]]/DCMResultsNumericFixedC[[1]][,"estimate"][["thetaV"]]
bwbvRatio 


#       - aw/av -----------------------------------------------------------------


DCMData0 <- subset(DCMDataRaw, experimentType == "animated")
DCMData0$choice <- DCMData0$realChoice

#bw = -1.558563
#bv = -1.285038

logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
                      , thetaPW = NULL,thetaPV = NULL, thetaWT = 1, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1

# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)

simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
                          , betaWeight =1,awt = 0, sw = 0, sv= 0) 


DCMResultsAnimatedFixedCInteraction<- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
                                                          , simulationParameters = simulationParameters
                                                          , sameCurvature = FALSE, sameTimeWeighting = FALSE)
DCMResultsAnimatedFixedCInteraction

awavRatio <- 1/bwbvRatio*DCMResultsAnimatedFixedCInteraction[[1]][,"estimate"][["thetaW"]]/DCMResultsAnimatedFixedCInteraction[[1]][,"estimate"][["thetaV"]]

#       - awt/avt -----------------------------------------------------------------

awtbw <- 1+DCMResultsAnimatedFixedCInteraction[[1]][,"estimate"][["thetaWT"]]/DCMResultsAnimatedFixedCInteraction[[1]][,"estimate"][["thetaW"]]


#     + Summary table -------------------------------------------------------

bwbvRatioSantiago 
awavRatioSantiago
awtbwSantiago

bwbvRatioLondon 
awavRatioLondon
awtbwLondon

bwbvRatio
awavRatio
awtbw


summaryTableBehavioralParameters <- data.frame(sample = NA, awavRatio = NA,bwbvRatio = NA,awtbw = NA)


#     ii) Plot of effect of time perception scales (aw,av) and treatment over ratio of estimated preference parameters (thetaW,thetaV) --------------------------------------------------------------------
#       - Plot theme ---------------------------------------------------------

themePlotsBehavioralParametersPaper1 <- theme(panel.grid.major = element_blank()
                                              , panel.grid.minor = element_blank()
                                              ,panel.background = element_blank()
                                              , axis.line = element_line(colour = "black")
                                              ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
                                              ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                                              ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                                              ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                                              ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                                              ,plot.margin = unit(c(0.3,1,0.3,0.3), "cm") #Tamaño de la imagen en que está contenido el gráfico, #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                                              # ,plot.margin=unit(c(1,1,1.5,1.2),"cm")
                                              ,legend.text.align = 0
                                              # ,legend.title.align = 0
                                              ,legend.title = element_text(size=18)
                                              # ,legend.title=element_blank()
                                              ,legend.background = element_rect(colour = 'black', fill = "transparent" )
                                              ,legend.box.background = element_blank()
                                              ,legend.key = element_rect(colour="black", fill = "transparent" )
                                              ,legend.key.size= unit(1.1,"cm")
                                              ,legend.text=element_text(size=14)
                                              ,legend.position="bottom"
                                              
)

#       - General parameters ------------------------------------------------------
#         + All -------------------------------------------------------------------

#Parameters
ratioAWAVBehavioralParametersAll <- seq(0,1, 0.01) #xAxis
ratioBWBVBehavioralParametersAll <- bwbvRatio #Slope control
treatmentEffectBehavioralParametersAll <- awtbw #1.5 #Slope treatment

#Ratio Thetas 
ratioTWTVControlBehavioralParametersAll <- ratioBWBVBehavioralParametersAll*seq(0,1, 0.01) #Control
ratioTWTVTreatmentBehavioralParametersAll <- treatmentEffectBehavioralParametersAll*ratioBWBVBehavioralParametersAll*seq(0,1, 0.01) #Treatment

#Cutoff point ratio aw & av in animated (control) and numeric condition 
awAnimatedBehavioralParametersAll <- awavRatio #1
avAnimatedBehavioralParametersAll <- 1 #2 #This is equivalent to rqatio of 2

awNumericBehavioralParametersAll <- 1
avNumericBehavioralParametersAll <- 1


#           + Santiago ----------------------------------------------------------------

#Parameters
ratioAWAVBehavioralParametersSantiago <- seq(0,1, 0.01) #xAxis
ratioBWBVBehavioralParametersSantiago <- bwbvRatioSantiago #Slope control
treatmentEffectBehavioralParametersSantiago <- awtbwSantiago #1.5 #Slope treatment

#Ratio Thetas 
ratioTWTVControlBehavioralParametersSantiago <- ratioBWBVBehavioralParametersSantiago*seq(0,1, 0.01) #Control
ratioTWTVTreatmentBehavioralParametersSantiago <- treatmentEffectBehavioralParametersSantiago*ratioBWBVBehavioralParametersSantiago*seq(0,1, 0.01) #Treatment

#Cutoff point ratio aw & av in animated (control) and numeric condition 
awAnimatedBehavioralParametersSantiago <- awavRatioSantiago #1
avAnimatedBehavioralParametersSantiago <- 1 #2 #This is equivalent to rqatio of 2

awNumericBehavioralParametersSantiago <- 1
avNumericBehavioralParametersSantiago <- 1



#           + London ------------------------------------------------------------------

#Parameters
ratioAWAVBehavioralParametersLondon <- seq(0,1, 0.01) #xAxis
ratioBWBVBehavioralParametersLondon <- bwbvRatioLondon #Slope control
treatmentEffectBehavioralParametersLondon <- awtbwLondon #1.5 #Slope treatment

#Ratio Thetas 
ratioTWTVControlBehavioralParametersLondon <- ratioBWBVBehavioralParametersLondon*seq(0,1, 0.01) #Control
ratioTWTVTreatmentBehavioralParametersLondon <- treatmentEffectBehavioralParametersLondon*ratioBWBVBehavioralParametersLondon*seq(0,1, 0.01) #Treatment

#Cutoff point ratio aw & av in animated (control) and numeric condition 
awAnimatedBehavioralParametersLondon <- awavRatioLondon #1
avAnimatedBehavioralParametersLondon <- 1 #2 #This is equivalent to rqatio of 2

awNumericBehavioralParametersLondon <- 1
avNumericBehavioralParametersLondon <- 1


#       a) Numeric vs Animated in Control Condition  -----------------------------

#Data for plot 
dataBehavioralParametersPlotA <- rbind(data.frame(ratioAWAV = ratioAWAVBehavioralParametersSantiago,ratioTWTV = ratioTWTVControlBehavioralParametersSantiago, type = "Santiago")
                                       ,data.frame(ratioAWAV = ratioAWAVBehavioralParametersLondon,ratioTWTV = ratioTWTVControlBehavioralParametersLondon, type = "London")
                                       ,data.frame(ratioAWAV = ratioAWAVBehavioralParametersAll,ratioTWTV = ratioTWTVControlBehavioralParametersAll, type = "All")
)

#i) Santiago

#Cutoff point ratio aw & av in animated (control) and numeric condition 
ratioAWAVAnimatedBehavioralParametersSantiago <- awAnimatedBehavioralParametersSantiago/avAnimatedBehavioralParametersSantiago
ratioAWAVNumericBehavioralParametersSantiago <- awNumericBehavioralParametersSantiago/avNumericBehavioralParametersSantiago

#Add points of interest
dataPlotAPointsBehavioralParametersSantiago <- rbind(data.frame(ratioAWAV = ratioAWAVNumericBehavioralParametersSantiago
                                                                ,ratioTWTV = ratioBWBVBehavioralParametersSantiago*ratioAWAVNumericBehavioralParametersSantiago
                                                                , type = "NC")
                                                     ,data.frame(ratioAWAV = ratioAWAVAnimatedBehavioralParametersSantiago
                                                                 ,ratioTWTV = ratioBWBVBehavioralParametersSantiago*ratioAWAVAnimatedBehavioralParametersSantiago
                                                                 , type = "AC")
)

#ii) London

ratioAWAVAnimatedBehavioralParametersLondon <- awAnimatedBehavioralParametersLondon/avAnimatedBehavioralParametersLondon
ratioAWAVNumericBehavioralParametersLondon <- awNumericBehavioralParametersLondon/avNumericBehavioralParametersLondon

#Add points of interest
dataPlotAPointsBehavioralParametersLondon <- rbind(data.frame(ratioAWAV = ratioAWAVNumericBehavioralParametersLondon
                                                              ,ratioTWTV = ratioBWBVBehavioralParametersLondon*ratioAWAVNumericBehavioralParametersLondon
                                                              , type = "NC")
                                                   ,data.frame(ratioAWAV = ratioAWAVAnimatedBehavioralParametersLondon
                                                               ,ratioTWTV = ratioBWBVBehavioralParametersLondon*ratioAWAVAnimatedBehavioralParametersLondon
                                                               , type = "AC")
)


#iii) All

ratioAWAVAnimatedBehavioralParametersAll <- awAnimatedBehavioralParametersAll/avAnimatedBehavioralParametersAll
ratioAWAVNumericBehavioralParametersAll <- awNumericBehavioralParametersAll/avNumericBehavioralParametersAll

#Add points of interest
dataPlotAPointsBehavioralParametersAll <- rbind(data.frame(ratioAWAV = ratioAWAVNumericBehavioralParametersAll
                                                           ,ratioTWTV = ratioBWBVBehavioralParametersAll*ratioAWAVNumericBehavioralParametersAll
                                                           , type = "NC")
                                                ,data.frame(ratioAWAV = ratioAWAVAnimatedBehavioralParametersAll
                                                            ,ratioTWTV = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll
                                                            , type = "AC")
)

dataPlotAPointsBehavioralParametersLegend <- rbind(data.frame(ratioAWAV = 10
                                                              ,ratioTWTV = 10
                                                              , type = "NC")
                                                   ,data.frame(ratioAWAV = 10
                                                               ,ratioTWTV = 10
                                                               , type = "AC")
)

ggplot(dataBehavioralParametersPlotA, aes(x = ratioAWAV, y = ratioTWTV)) +
  geom_line(data = dataBehavioralParametersPlotA, aes(colour = type))+
  
  #i) Santiago
  #Numeric condition
  geom_segment(x = ratioAWAVNumericBehavioralParametersSantiago, y = ratioBWBVBehavioralParametersAll*ratioAWAVNumericBehavioralParametersAll, xend = ratioAWAVNumericBehavioralParametersSantiago, yend = ratioBWBVBehavioralParametersSantiago*ratioAWAVNumericBehavioralParametersSantiago
               , colour = "red",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersSantiago*ratioAWAVNumericBehavioralParametersSantiago, xend = ratioAWAVNumericBehavioralParametersSantiago, yend = ratioBWBVBehavioralParametersSantiago*ratioAWAVNumericBehavioralParametersSantiago
  ),linetype="dotted", colour = "red", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersSantiago[1,], shape = 21, colour = "red", fill = "red", size = 2) +
  
  #Animated (control) condition
  geom_segment(x = awAnimatedBehavioralParametersSantiago/avAnimatedBehavioralParametersSantiago, y = 0
               , xend = ratioAWAVAnimatedBehavioralParametersSantiago, yend = ratioBWBVBehavioralParametersSantiago*ratioAWAVAnimatedBehavioralParametersSantiago
               , colour = "red",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersSantiago*ratioAWAVAnimatedBehavioralParametersSantiago
                   , xend = ratioAWAVAnimatedBehavioralParametersSantiago, yend = ratioBWBVBehavioralParametersSantiago*ratioAWAVAnimatedBehavioralParametersSantiago
  ),linetype = "dotted", colour = "red", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersSantiago[2,], shape = 22, colour = "red", fill = "red", size = 2) +
  
  #ii) London
  #Numeric condition
  geom_segment(x = ratioAWAVNumericBehavioralParametersLondon, y = 0, xend = ratioAWAVNumericBehavioralParametersLondon
               , yend = ratioBWBVBehavioralParametersLondon*ratioAWAVNumericBehavioralParametersLondon
               , colour = "blue",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersLondon*ratioAWAVNumericBehavioralParametersLondon, xend = ratioAWAVNumericBehavioralParametersLondon, yend = ratioBWBVBehavioralParametersLondon*ratioAWAVNumericBehavioralParametersLondon
  ),linetype="dotted", colour = "blue", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersLondon[1,], shape = 21, colour = "blue", fill = "blue", size = 2) +
  
  #Animated (control) condition
  geom_segment(x = awAnimatedBehavioralParametersLondon/avAnimatedBehavioralParametersLondon, y = 0
               , xend = ratioAWAVAnimatedBehavioralParametersLondon, yend = ratioBWBVBehavioralParametersLondon*ratioAWAVAnimatedBehavioralParametersLondon
               , color = "blue",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersLondon*ratioAWAVAnimatedBehavioralParametersLondon, xend = ratioAWAVAnimatedBehavioralParametersLondon, yend = ratioBWBVBehavioralParametersLondon*ratioAWAVAnimatedBehavioralParametersLondon
  ),linetype = "dotted", colour = "blue", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersLondon[2,], shape = 22, colour = "blue", fill = "blue", size = 2) +
  
  #iii) All
  #Numeric condition
  geom_segment(x = ratioAWAVNumericBehavioralParametersAll, y = ratioBWBVBehavioralParametersLondon*ratioAWAVNumericBehavioralParametersLondon, xend = ratioAWAVNumericBehavioralParametersAll, yend = ratioBWBVBehavioralParametersAll*ratioAWAVNumericBehavioralParametersAll
               , color = "black",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersAll*ratioAWAVNumericBehavioralParametersAll, xend = ratioAWAVNumericBehavioralParametersAll, yend = ratioBWBVBehavioralParametersAll*ratioAWAVNumericBehavioralParametersAll
  ),linetype="dotted", colour = "black", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersAll[1,], shape = 21, colour = "black", fill = "black", size = 2) +
  
  #Animated (control) condition
  geom_segment(x = awAnimatedBehavioralParametersAll/avAnimatedBehavioralParametersAll, y = 0
               , xend = ratioAWAVAnimatedBehavioralParametersAll, yend = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll
               , color = "black",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll, xend = ratioAWAVAnimatedBehavioralParametersAll, yend = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll
  ),linetype = "dotted", colour = "black", lwd = 0.3)+
  geom_point(data = dataPlotAPointsBehavioralParametersAll[2,], shape = 22, colour = "black", fill = "black", size = 2) +
  
  #Legend
  geom_point(data = dataPlotAPointsBehavioralParametersLegend[2,], aes(shape = "AnimatedPoint"), colour = "black", size = 2) +
  geom_point(data = dataPlotAPointsBehavioralParametersLegend[2,], aes(shape = "NumericPoint"), colour = "black", size = 2) +
  
  
  scale_color_manual(name = "", values = c(Santiago = "red",London = "blue", All="black")
                     , labels = c("Santiago","London", "All"))+
  
  # scale_color_manual(name = "", values = c(AnimatedPoint = "gray",NumericPoint = "black")
  #                    , labels = c("Animated-Control","Numeric-Control"))+
  scale_shape_manual(name = "", values = c(NumericPoint = 21,AnimatedPoint = 22), labels = c(expression(paste("AC ", (a[w] < a[v]))),expression(paste("NC ", (a[w] == a[v])))))+
  # scale_linetype_manual(name = "", values = c(Santiago = "solid", London = "solid", All = "solid"), labels = c("Animated-Control", "Numeric-Control"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0,1.05,0.1), limits = c(0,1.05)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(seq(0,1.35,0.1)), limits = c(0,1.35)) +
  labs(x = expression(x = a[w]/a[v]), y = expression(hat(theta)[w]/hat(theta)[v]==(beta[w]/beta[v])*(a[w]/a[v])))+
  themePlotsBehavioralParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/WaitingTimeFiller/BehavioralParametersPlotA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#       b) Control vs Treatment in Animated Condition  -----------------------------

#Cutoff point ratio aw & av in animated control and treatment conditions 
awAnimatedBehavioralParametersB <- awAnimatedBehavioralParametersAll
avAnimatedBehavioralParametersB <- avAnimatedBehavioralParametersAll
ratioAWAVAnimatedBehavioralParametersB <- awAnimatedBehavioralParametersAll/avAnimatedBehavioralParametersAll

#Data for plot 
dataPlotBehavioralParametersB <- rbind(data.frame(ratioAWAV = ratioAWAVBehavioralParametersAll,ratioTWTV = ratioTWTVControlBehavioralParametersAll, type = "Control"),
                                       data.frame(ratioAWAV = ratioAWAVBehavioralParametersAll,ratioTWTV = ratioTWTVTreatmentBehavioralParametersAll, type = "Treatment")
)

# dataPlotBehavioralParametersB <- cbind.data.frame(ratioAWAV = dataPlotBehavioralParametersB$ratioAWAV
#                                   , control = subset(dataPlotBehavioralParametersB, type == "Control")$ratioTWTV
#                                   , treatment = subset(dataPlotBehavioralParametersB, type == "Treatment")$ratioTWTV
# )
#Add points of interest
dataPlotBPointsBehavioralParametersAll <- rbind(data.frame(ratioAWAV = ratioAWAVAnimatedBehavioralParametersAll
                                                           ,ratioTWTV = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll
                                                           , type = "AC")
                                                ,data.frame(ratioAWAV = ratioAWAVAnimatedBehavioralParametersAll
                                                            ,ratioTWTV = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersAll*treatmentEffectBehavioralParametersAll
                                                            , type = "AT")
)

dataPlotBPointsBehavioralParametersLegend <- rbind(data.frame(ratioAWAV = 10
                                                              ,ratioTWTV = 10
                                                              , type = "NC")
                                                   ,data.frame(ratioAWAV = 10
                                                               ,ratioTWTV = 10
                                                               , type = "AC")
)

ggplot(data=dataPlotBehavioralParametersB, aes(x = ratioAWAV, y = ratioTWTV)) +
  geom_line(data = dataPlotBehavioralParametersB, aes(colour = type))+
  # geom_line(aes(y = control),aes(colour = "A"),linetype = "solid")+
  # geom_line(aes(y = treatment),aes(colour = "B"),linetype = "solid")+
  
  #Animated control condition
  geom_segment(x = ratioAWAVAnimatedBehavioralParametersB, y = 0, xend = ratioAWAVAnimatedBehavioralParametersB, yend = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB
               , color = "black", linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB, xend = ratioAWAVAnimatedBehavioralParametersB
                   , yend = ratioBWBVBehavioralParametersAll*awAnimatedBehavioralParametersB/avAnimatedBehavioralParametersB)
               , color = "black",linetype="dotted", lwd = 0.3)+
  geom_point(data = dataPlotBPointsBehavioralParametersAll[1,],  y =dataPlotBPointsBehavioralParametersAll[1,]$ratioTWTV, shape = 24, colour = "black", fill = "black", size = 2) +
  
  #Animated treatment condition
  geom_segment(x = ratioAWAVAnimatedBehavioralParametersB, y = ratioBWBVBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB
               , xend = ratioAWAVAnimatedBehavioralParametersB, yend = ratioBWBVBehavioralParametersAll*treatmentEffectBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB
               , color = "lightgray",linetype="dotted", lwd = 0.3)+
  geom_segment(aes(x = 0, y =  ratioBWBVBehavioralParametersAll*treatmentEffectBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB
                   , xend = ratioAWAVAnimatedBehavioralParametersB, yend = ratioBWBVBehavioralParametersAll*treatmentEffectBehavioralParametersAll*ratioAWAVAnimatedBehavioralParametersB
  ), color = "gray",linetype="dotted", lwd = 0.3)+
  geom_point(data = dataPlotBPointsBehavioralParametersAll[2,], y =dataPlotBPointsBehavioralParametersAll[2,]$ratioTWTV,   shape = 24, colour = "gray", fill = "gray", size = 2) +
  
  scale_color_manual(name = "", values=c(Control = "black", Treatment = "gray"), labels = c(expression(paste("AC ", (alpha[w] == 0))),expression(paste("AT ", (alpha[w] == 0.111)))))+
  # scale_linetype_manual(name = "", values=c(A = "dashed", B = "dashed"), labels = c("Animated-Control","Animated-Treatment"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(0,max(dataPlotBehavioralParametersB$ratioAWAV),0.1)) +
  scale_y_continuous(expand = c(0, 0), breaks = c(seq(0,1.2,0.1)), limits = c(0,1.2)) +
  labs(x = expression(x = a[w]/a[v]), y = expression(hat(theta)[w]/hat(theta)[v]==(1+alpha[w])(beta[w]/beta[v])*(a[w]/a[v])))+
  themePlotsBehavioralParametersPaper1
# guides(colour = guide_legend(override.aes = list(linetype = 1)))

ggsave(str_c("export","/figures/Paper1/Simulations/WaitingTimeFiller/BehavioralParametersPlotB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#   5) Logit Response ------------------------------------------------------


# * Plot theme ------------------------------------------------------------

themePlotsLRPaper1 <- theme(panel.grid.major = element_blank()
                            , panel.grid.minor = element_blank()
                            ,panel.background = element_blank()
                            , axis.line = element_line(colour = "black")
                            ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
                            ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                            ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                            ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                            ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                            ,plot.margin = unit(c(0.3,1,0.3,0.3), "cm") #Tamaño de la imagen en que está contenido el gráfico, #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
                            # ,plot.margin=unit(c(1,1,1.5,1.2),"cm")
                            ,legend.text.align = 0
                            # ,legend.title.align = 0
                            # ,legend.title = element_text(size=18)
                            , legend.title = element_blank()
                            ,legend.spacing.x = unit(0.2, 'cm')
                            # ,legend.title=element_blank()
                            # , legend.key.width = 
                            # , legend.margin = margin(r = 2, unit='cm')
                            ,legend.background = element_rect(colour = 'black', fill = "transparent" )
                            ,legend.box.background = element_blank()
                            ,legend.key = element_rect(colour="black", fill = "transparent" )
                            ,legend.key.size= unit(1.1,"cm")
                            ,legend.text=element_text(size=14)
                            ,legend.position="bottom"
                            
)
#   * Functions -------------------------------------------------------------

ValueFunctionsPlotPT <- function(modelId = "", figuresFolder, parameters){
  
  # e <- new.env()
  
  p <<-  as.numeric(p)
  llw <<- as.numeric(PTEstimates[["llw"]])
  lgw <<-  as.numeric(PTEstimates[["lgw"]])
  llt <<-  as.numeric(PTEstimates[["llt"]])
  lgt <<-  as.numeric(PTEstimates[["lgt"]])
  at <<-   as.numeric(PTEstimates[["at"]])
  aw <<-   as.numeric(PTEstimates[["aw"]])
  reft <<-   as.numeric(PTEstimates[["reft"]])
  refw <<-   as.numeric(PTEstimates[["refw"]])
  
  thetaw<<- MNLEstimates[["averageWaitingTime"]]
  thetat<<- MNLEstimates[["averageTravelTime"]]
  
  xAxis <- c("Time outcome [min]")
  yAxis <- c("Subjective Utility")
  
  pdf(paste(figuresFolder,"ValueFunctionsPT",modelId,".pdf",sep=""))
  
  par(mar=c(12.1, 8.1, 8.1, 8.1), xpd=TRUE)
  
  x_min <- 0; x_max <- 10; y_min <- -20; y_max <- 20; 
  # plot(x=x_min:x_max,y=seq(y_min,y_max, (y_max-y_min)/(x_max-x_min)),type='n',xlim=c(x_min,x_max),ylim=c(y_min,y_max),xlab = "Waiting Time",ylab = "MRS", xaxs="i", yaxs="i")
  
  curve(-llt*(x-reft)^(at),from = reft, to = 10,xlim = c(x_min,x_max),ylim=c(y_min,y_max),col = "red",xlab = xAxis,ylab = yAxis)
  curve(lgt*(reft-x)^(at),from = 0, to = reft, add = T,col = "red",xlab = xAxis,ylab = yAxis)
  lines(x=c(0,x_max),y=c(0,thetat*x_max),lty = 2, col = "red") 
  lines(x =c(reft,reft), y = c(y_min,y_max), lty = 3, col = "red") #Reference point in-vehicle time
  
  curve(-llw*(x-refw)^(aw),from = refw, to = 10, add = T,col = "blue",xlab = xAxis,ylab = yAxis)
  curve(lgw*(refw-x)^(aw),from = 0, to = refw, add = T,col = "blue",xlab = xAxis,ylab = yAxis)
  lines(x=c(0,x_max),y=c(0,thetaw*x_max),lty = 2, col =  "blue") 
  lines(x =c(refw,refw), y = c(y_min,y_max), lty = 3, col =  "blue") #Reference point waiting time
  
  
  # legend("bottom",inset=c(0,-0.5), c("In-vehicle time","Waiting time")
  #        # , title="Model"
  #        , lty=c(1,1)
  #        , ncol=2
  #        ,col = c("red","blue")
  # )
  
  dev.off()
}


LogitResponseFunction <- function(parameters, interval_w2, v2, w1, v1, scaleParameter = 1){
  
  
  bw <- parameters[["bw"]]
  bv <- parameters[["bv"]]
  aw <- parameters[["aw"]]
  av <- parameters[["av"]]
  c <- parameters[["c"]]
  V1 <- aw*bw*(w1)^c+av*bv*(v1)^c
  
  dataPlot <- data.frame(w2 = NA, deltaW2W1= NA, p1 = NA)
  
  for(w2 in interval_w2){
    
    V2 <- aw*bw*(w2)^c+av*bv*(v2)^c
    
    p1Logit <- 1/(1+exp(V2-V1))
    dataPlot <- rbind(dataPlot, data.frame(w2 = w2, deltaW2W1 = w2-w1, p1 = p1Logit))
  }
  
  return(dataPlot[-1,])
  
}

#   * Scenarios ---------------------------------------------------------------


#     i) S1: C = {Fixed = 0.9, Fixed =1} in Animated and Numeric Control ------------------------------------------------------
#       - In Sample (Paper) -------------------------------------------------------------
v2S1 <- 4
w1S1 <- 4
v1S1 <- 6
intervalW2S1 <- seq(0,10,0.01)
#         + Numeric Control (x2)  ------------------------------------------------------

# #C estimated
# parametersLRNCFreeC <- list()
# parametersLRNCFreeC[["bw"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaW"]]
# parametersLRNCFreeC[["bv"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaV"]]
# parametersLRNCFreeC[["aw"]] = 1
# parametersLRNCFreeC[["av"]] = 1
# parametersLRNCFreeC[["c"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaP"]]
# LRNCFreeCS1 <- LogitResponseFunction(parameters = parametersLRNCFreeC, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# # plot(LRNCFreeCS1)

#C = 0.9
parametersLRNCFixedC09 <- list()
parametersLRNCFixedC09[["bw"]] = DCMResultsNCFixedC09[[1]][,"estimate"][["thetaW"]]
parametersLRNCFixedC09[["bv"]] = DCMResultsNCFixedC09[[1]][,"estimate"][["thetaV"]]
parametersLRNCFixedC09[["aw"]] = 1
parametersLRNCFixedC09[["av"]] = 1
parametersLRNCFixedC09[["c"]] = 0.9
LRNCFixedC09S1 <- LogitResponseFunction(parameters = parametersLRNCFixedC09, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# plot(LRNCFixedCS1)


#C = 1
parametersLRNCFixedC <- list()
parametersLRNCFixedC[["bw"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRNCFixedC[["bv"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRNCFixedC[["aw"]] = 1
parametersLRNCFixedC[["av"]] = 1
parametersLRNCFixedC[["c"]] = 1
LRNCFixedCS1 <- LogitResponseFunction(parameters = parametersLRNCFixedC, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# plot(LRNCFixedCS1)


#         + Animated Control (x2)  ------------------------------------------------------

# #C estimated
# parametersLRACFreeC <- list()
# parametersLRACFreeC[["bw"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaW"]]
# parametersLRACFreeC[["bv"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaV"]]
# parametersLRACFreeC[["aw"]] = 1
# parametersLRACFreeC[["av"]] = 1
# parametersLRACFreeC[["c"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaP"]]
# LRACFreeCS1 <- LogitResponseFunction(parameters = parametersLRACFreeC, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# # plot(LRACFreeCS1)

#C = 0.9
parametersLRACFixedC09 <- list()
parametersLRACFixedC09[["bw"]] = DCMResultsACFixedC09[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedC09[["bv"]] = DCMResultsACFixedC09[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedC09[["aw"]] = 1
parametersLRACFixedC09[["av"]] = 1
parametersLRACFixedC09[["c"]] = 0.9
LRACFixedC09S1 <- LogitResponseFunction(parameters = parametersLRACFixedC09, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# plot(LRACFixedC09S1)

#C = 1
parametersLRACFixedC <- list()
parametersLRACFixedC[["bw"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedC[["bv"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedC[["aw"]] = 1
parametersLRACFixedC[["av"]] = 1
parametersLRACFixedC[["c"]] = 1
LRACFixedCS1 <- LogitResponseFunction(parameters = parametersLRACFixedC, interval_w2 = intervalW2S1, v2 = v2S1, w1 = w1S1, v1 = v1S1)
# plot(LRACFixedCS1)

#         + Combined Plot (x4) -----------------------------------------------------------

estimatesPlotsLRS1 <- rbind(data.frame(deltaW = LRNCFixedCS1$deltaW2W1, p1 = LRNCFixedCS1$p1, type = "NC Fixed C")
                            , data.frame(deltaW = LRNCFixedC09S1$deltaW2W1, p1 = LRNCFixedC09S1$p1, type = "NC C = 0.9")
                            ,data.frame(deltaW = LRACFixedCS1$deltaW2W1, p1 = LRACFixedCS1$p1, type = "AC Fixed C")
                            ,data.frame(deltaW = LRACFixedC09S1$deltaW2W1, p1 = LRACFixedC09S1$p1, type = "AC C = 0.9")
)

estimatesPlotsLRS1$type


ggplot(estimatesPlotsLRS1, aes(x = deltaW, y = p1, color = interaction(type), linetype = interaction(type))) +
  geom_line(aes(group = type))+
  scale_color_manual(name = "", values=c("black","black", "gray", "gray")
                     , labels = c("NC (c = 1)","NC (c = 0.9)","AC (c = 1)","AC (c = 0.9)"))+ 
  scale_linetype_manual(name = "", values=c("solid","dotdash","solid","dotdash")
                        , labels = c("NC (c = 1)","NC (c = 0.9)","AC (c = 1)","AC (c = 0.9)"))+ 
  # geom_hline(yintercept = with(simulationParametersNumeric,bw),color = "red", linetype="dashed")+
  # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
  # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
  # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsLRS1$p1),1)) +
  # labs(x = expression(paste(Delta,w)), y = expression(P[1]))+
  labs(x = expression(paste(Delta,w==w[2]-bar(w)[1])), y = expression(paste(P[1],"=[",1+exp(V[2]-V[1]),"]",""^{-1})))+
  themePlotsLRPaper1

ggsave(str_c("export","/figures/Paper1/LogitResponse/LR1.pdf")
       , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# #       - Out of Sample -------------------------------------------------------------
# v2OutS1 <- 40
# w1OutS1 <- 40
# v1OutS1 <- 60
# intervalW2OutS1 <- seq(50,80,0.01)
# #         + Numeric Control  ------------------------------------------------------
# 
# #C = 1
# parametersLRNCFixedC <- list()
# parametersLRNCFixedC[["bw"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaW"]]
# parametersLRNCFixedC[["bv"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaV"]]
# parametersLRNCFixedC[["aw"]] = 1
# parametersLRNCFixedC[["av"]] = 1
# parametersLRNCFixedC[["c"]] = 1
# LRNCFixedCOutS1 <- LogitResponseFunction(parameters = parametersLRNCFixedC,  interval_w2 = intervalW2OutS1, v2 = v2OutS1, w1 = w1OutS1, v1 = v1OutS1)
# # plot(LRNCFixedCS1)
# 
# #C estimated
# parametersLRNCFreeC <- list()
# parametersLRNCFreeC[["bw"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaW"]]
# parametersLRNCFreeC[["bv"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaV"]]
# parametersLRNCFreeC[["aw"]] = 1
# parametersLRNCFreeC[["av"]] = 1
# parametersLRNCFreeC[["c"]] = DCMResultsNCFreeC[[1]][,"estimate"][["thetaP"]]
# LRNCFreeCOutS1 <- LogitResponseFunction(parameters = parametersLRNCFreeC, interval_w2 = intervalW2OutS1, v2 = v2OutS1, w1 = w1OutS1, v1 = v1OutS1)
# # plot(LRNCFreeCS1)
# 
# 
# #         + Animated Control  ------------------------------------------------------
# 
# #C = 1
# parametersLRACFixedC <- list()
# parametersLRACFixedC[["bw"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaW"]]
# parametersLRACFixedC[["bv"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaV"]]
# parametersLRACFixedC[["aw"]] = 1
# parametersLRACFixedC[["av"]] = 1
# parametersLRACFixedC[["c"]] = 1
# LRACFixedCOutS1 <- LogitResponseFunction(parameters = parametersLRACFixedC, interval_w2 = intervalW2OutS1, v2 = v2OutS1, w1 = w1OutS1, v1 = v1OutS1)
# # plot(LRACFixedCS1)
# 
# #C estimated
# parametersLRACFreeC <- list()
# parametersLRACFreeC[["bw"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaW"]]
# parametersLRACFreeC[["bv"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaV"]]
# parametersLRACFreeC[["aw"]] = 1
# parametersLRACFreeC[["av"]] = 1
# parametersLRACFreeC[["c"]] = DCMResultsACFreeC[[1]][,"estimate"][["thetaP"]]
# LRACFreeCOutS1 <- LogitResponseFunction(parameters = parametersLRACFreeC, interval_w2 = intervalW2OutS1, v2 = v2OutS1, w1 = w1OutS1, v1 = v1OutS1)
# # plot(LRACFreeCS1)
# 
# 
# 
# 
# 
# # ValueFunctionsPlotPT(MNLEstimates = MNLResultsAll$coefficients, PTEstimates = PTResultsAllCurvature$estimate
# #                      , modelId = "ResultsAllCurvature", figuresFolder = str_c(project,"/export/figures/ProspectTheory/ValueFunctions/"))
# 
# 
# #         + Combined Plot -----------------------------------------------------------
# 
# LRNCFixedCS1
# LRACFixedCS1
# 
# estimatesPlotsLROutS1 <- rbind(data.frame(deltaW = LRNCFixedCOutS1$deltaW2W1, p1 = LRNCFixedCOutS1$p1, type = "NC Fixed C")
#                             ,data.frame(deltaW = LRNCFreeCOutS1$deltaW2W1, p1 = LRNCFreeCOutS1$p1, type = "NC Free C")
# )
# 
# ggplot(estimatesPlotsLROutS1, aes(x = deltaW, y = p1, color = interaction(type), linetype = interaction(type))) +
#   geom_line(aes(group = type))+
#   scale_color_manual(name = "", values=c("black", "lightgray")
#                      , labels = c(expression(paste(hat(theta)[w],"  ")),expression(paste(hat(theta)[v],"  "))))+ 
#   scale_linetype_manual(name = "", values=c("solid", "solid")
#                         , labels = c(expression(paste(hat(theta)[w],"  ")),expression(paste(hat(theta)[v],"  ")),expression(paste(hat(theta)[w]/hat(theta)[v]," "))))+ 
#   # geom_hline(yintercept = with(simulationParametersNumeric,bw),color = "red", linetype="dashed")+
#   # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
#   # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
#   # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsLROutS1$p1),1)) +
#   labs(x = expression(paste(Delta,w)), y = expression(P[1]))+
#   themePlotsDCM1APaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/GridSearch/DCM1A.pdf")
#        , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#     ii) S2: Numeric vs Animated Control vs Animated Treatment (Fixed C = 1) -------------------

v2S2 <- 4
w1S2 <- 4
v1S2 <- 6
intervalW2S2 <- seq(0,10,0.01)




#         + Numeric Control (NC)  ------------------------------------------------------

#Numeric Control
parametersLRNCFixedCS2 <- list()
parametersLRNCFixedCS2[["bw"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRNCFixedCS2[["bv"]] = DCMResultsNCFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRNCFixedCS2[["aw"]] = 1
parametersLRNCFixedCS2[["av"]] = 1
parametersLRNCFixedCS2[["c"]] = 1
LRNCFixedCS2 <- LogitResponseFunction(parameters = parametersLRNCFixedCS2, interval_w2 = intervalW2S2, v2 = v2S2, w1 = w1S2, v1 = v1S2)
#         + Numeric Treatment (NC)  ------------------------------------------------------

#Numeric Control
parametersLRNTFixedCS2 <- list()
parametersLRNTFixedCS2[["bw"]] = DCMResultsNTFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRNTFixedCS2[["bv"]] = DCMResultsNTFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRNTFixedCS2[["aw"]] = 1
parametersLRNTFixedCS2[["av"]] = 1
parametersLRNTFixedCS2[["c"]] = 1
LRNTFixedCS2 <- LogitResponseFunction(parameters = parametersLRNTFixedCS2, interval_w2 = intervalW2S2, v2 = v2S2, w1 = w1S2, v1 = v1S2)
#         + Animated Control (AC)  ------------------------------------------------------


#Animated Control
parametersLRACFixedCS2 <- list()
parametersLRACFixedCS2[["bw"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedCS2[["bv"]] = DCMResultsACFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedCS2[["aw"]] = 1
parametersLRACFixedCS2[["av"]] = 1
parametersLRACFixedCS2[["c"]] = 1 #DCMResultsACFixedCS2[[1]][,"estimate"][["thetaP"]]
LRACFixedCS2 <- LogitResponseFunction(parameters = parametersLRACFixedCS2, interval_w2 = intervalW2S2, v2 = v2S2, w1 = w1S2, v1 = v1S2)

#         + Animated Treatment (AT)  ------------------------------------------------------

parametersLRATFixedCS2 <- list()
parametersLRATFixedCS2[["bw"]] = DCMResultsATFixedC[[1]][,"estimate"][["thetaW"]]
parametersLRATFixedCS2[["bv"]] = DCMResultsATFixedC[[1]][,"estimate"][["thetaV"]]
parametersLRATFixedCS2[["aw"]] = 1
parametersLRATFixedCS2[["av"]] = 1
parametersLRATFixedCS2[["c"]] = 1 #DCMResultsATFixedCS2[[1]][,"estimate"][["thetaP"]]
LRATFixedCS2 <- LogitResponseFunction(parameters = parametersLRATFixedCS2, interval_w2 = intervalW2S2, v2 = v2S2, w1 = w1S2, v1 = v1S2)

#         + Combined Plot (x4) -----------------------------------------------------------

estimatesPlotsLRS2 <- rbind(data.frame(deltaW = LRNCFixedCS2$deltaW2W1, p1 = LRNCFixedCS2$p1, type = "NC")
                            ,data.frame(deltaW = LRNTFixedCS2$deltaW2W1, p1 = LRNTFixedCS2$p1, type = "NT")
                            ,data.frame(deltaW = LRACFixedCS2$deltaW2W1, p1 = LRACFixedCS2$p1, type = "AC")
                            , data.frame(deltaW = LRATFixedCS2$deltaW2W1, p1 = LRATFixedCS2$p1, type = "AT")
)

ggplot(estimatesPlotsLRS2, aes(x = deltaW, y = p1, color = interaction(type), linetype = interaction(type))) +
  geom_line(aes(group = type))+
  scale_color_manual(name = "", values=c("black","black","gray","gray")
                     , labels = c("NC","NT","AC","AT"))+ 
  scale_linetype_manual(name = "", values=c("solid","dashed","solid","dashed")
                        ,labels = c("NC","NT","AC","AT"))+ 
  # geom_hline(yintercept = with(simulationParametersNumeric,bw),color = "red", linetype="dashed")+
  # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
  # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
  # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsLRS2$p1),1)) +
  labs(x = expression(paste(Delta,w==w[2]-bar(w)[1])), y = expression(paste(P[1],"=[",1+exp(V[2]-V[1]),"]",""^{-1})))+
  themePlotsLRPaper1

ggsave(str_c("export","/figures/Paper1/LogitResponse/LR2.pdf")
       , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#     iii) S3: Santiago vs London in Animated and Numeric Control (Fixed C = 1) ------------------------------------------------------
v2S3 <- 4
w1S3 <- 4
v1S3 <- 6
intervalW2S3 <- seq(0,10,0.01)


#         + Santiago (AC & NC)  ------------------------------------------------------

#Numeric Control
parametersLRNCFixedCSantiagoS3 <- list()
parametersLRNCFixedCSantiagoS3[["bw"]] = DCMResultsNCFixedCSantiago[[1]][,"estimate"][["thetaW"]]
parametersLRNCFixedCSantiagoS3[["bv"]] = DCMResultsNCFixedCSantiago[[1]][,"estimate"][["thetaV"]]
parametersLRNCFixedCSantiagoS3[["aw"]] = 1
parametersLRNCFixedCSantiagoS3[["av"]] = 1
parametersLRNCFixedCSantiagoS3[["c"]] = 1
LRNCFixedCSantiagoS3 <- LogitResponseFunction(parameters = parametersLRNCFixedCSantiagoS3, interval_w2 = intervalW2S3, v2 = v2S3, w1 = w1S3, v1 = v1S3)
# plot(LRNCFixedCS1)

#Animated Control
parametersLRACFixedCSantiagoS3 <- list()
parametersLRACFixedCSantiagoS3[["bw"]] = DCMResultsACFixedCSantiago[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedCSantiagoS3[["bv"]] = DCMResultsACFixedCSantiago[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedCSantiagoS3[["aw"]] = 1
parametersLRACFixedCSantiagoS3[["av"]] = 1
parametersLRACFixedCSantiagoS3[["c"]] = 1 #DCMResultsACFixedCSantiagoS3[[1]][,"estimate"][["thetaP"]]
LRACFixedCSantiagoS3 <- LogitResponseFunction(parameters = parametersLRACFixedCSantiagoS3, interval_w2 = intervalW2S3, v2 = v2S3, w1 = w1S3, v1 = v1S3)
# plot(LRNCFreeCS1)


#         + London (AC & NC)  ------------------------------------------------------

#Numeric Control
parametersLRNCFixedCLondonS3 <- list()
parametersLRNCFixedCLondonS3[["bw"]] = DCMResultsNCFixedCLondon[[1]][,"estimate"][["thetaW"]]
parametersLRNCFixedCLondonS3[["bv"]] = DCMResultsNCFixedCLondon[[1]][,"estimate"][["thetaV"]]
parametersLRNCFixedCLondonS3[["aw"]] = 1
parametersLRNCFixedCLondonS3[["av"]] = 1
parametersLRNCFixedCLondonS3[["c"]] = 1
LRNCFixedCLondonS3 <- LogitResponseFunction(parameters = parametersLRNCFixedCLondonS3, interval_w2 = intervalW2S3, v2 = v2S3, w1 = w1S3, v1 = v1S3)
# plot(LRNCFixedCS1)

#Animated Control
parametersLRACFixedCLondonS3 <- list()
parametersLRACFixedCLondonS3[["bw"]] = DCMResultsACFixedCLondon[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedCLondonS3[["bv"]] = DCMResultsACFixedCLondon[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedCLondonS3[["aw"]] = 1
parametersLRACFixedCLondonS3[["av"]] = 1
parametersLRACFixedCLondonS3[["c"]] = 1 #DCMResultsACFixedCLondonS3[[1]][,"estimate"][["thetaP"]]
LRACFixedCLondonS3 <- LogitResponseFunction(parameters = parametersLRACFixedCLondonS3, interval_w2 = intervalW2S3, v2 = v2S3, w1 = w1S3, v1 = v1S3)

#         + Combined Plot (x4) -----------------------------------------------------------

estimatesPlotsLRS3 <- rbind(data.frame(deltaW = LRNCFixedCSantiagoS3$deltaW2W1, p1 = LRNCFixedCSantiagoS3$p1, type = "NC Santiago")
                            , data.frame(deltaW = LRNCFixedCLondonS3$deltaW2W1, p1 = LRNCFixedCLondonS3$p1, type = "NC London")
                            ,data.frame(deltaW = LRACFixedCSantiagoS3$deltaW2W1, p1 = LRACFixedCSantiagoS3$p1, type = "AC  Santiago")
                            ,data.frame(deltaW = LRACFixedCLondonS3$deltaW2W1, p1 = LRACFixedCLondonS3$p1, type = "AC  London")
)

ggplot(estimatesPlotsLRS3, aes(x = deltaW, y = p1, color = interaction(type), linetype = interaction(type))) +
  geom_line(aes(group = type))+
  scale_color_manual(name = "", values=c("red", "blue","red", "blue")
                     , labels = c("NC Santiago","NC London","AC Santiago","AC London"))+ 
  scale_linetype_manual(name = "", values=c("solid","solid","dashed","dashed")
                        , labels = c("NC Santiago","NC London","AC Santiago","AC London"))+ 
  # geom_hline(yintercept = with(simulationParametersNumeric,bw),color = "red", linetype="dashed")+
  # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
  # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
  # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsLRS3$p1),1)) +
  # labs(x = expression(paste(Delta,w)), y = expression(P[1]))+
  labs(x = expression(paste(Delta,w==w[2]-bar(w)[1])), y = expression(paste(P[1],"=[",1+exp(V[2]-V[1]),"]",""^{-1})))+
  themePlotsLRPaper1

ggsave(str_c("export","/figures/Paper1/LogitResponse/LR3.pdf")
       , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#     iv) S4: Santiago vs London in Animated Control vs Animated Treatment (Fixed C = 1) ------------------------------------------------------
v2S4 <- 4
w1S4 <- 4
v1S4 <- 6
intervalW2S4 <- seq(0,10,0.01)


#         + Santiago (AC & AT)  ------------------------------------------------------

#Animated Control
parametersLRACFixedCSantiagoS4 <- list()
parametersLRACFixedCSantiagoS4[["bw"]] = DCMResultsACFixedCSantiago[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedCSantiagoS4[["bv"]] = DCMResultsACFixedCSantiago[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedCSantiagoS4[["aw"]] = 1
parametersLRACFixedCSantiagoS4[["av"]] = 1
parametersLRACFixedCSantiagoS4[["c"]] = 1 #DCMResultsACFixedCSantiagoS4[[1]][,"estimate"][["thetaP"]]
LRACFixedCSantiagoS4 <- LogitResponseFunction(parameters = parametersLRACFixedCSantiagoS4, interval_w2 = intervalW2S4, v2 = v2S4, w1 = w1S4, v1 = v1S4)
# plot(LRNCFreeCS1)

#Animated Treatment
parametersLRATFixedCSantiagoS4 <- list()
parametersLRATFixedCSantiagoS4[["bw"]] = DCMResultsATFixedCSantiago[[1]][,"estimate"][["thetaW"]]
parametersLRATFixedCSantiagoS4[["bv"]] = DCMResultsATFixedCSantiago[[1]][,"estimate"][["thetaV"]]
parametersLRATFixedCSantiagoS4[["aw"]] = 1
parametersLRATFixedCSantiagoS4[["av"]] = 1
parametersLRATFixedCSantiagoS4[["c"]] = 1 #DCMResultsATFixedCSantiagoS4[[1]][,"estimate"][["thetaP"]]
LRATFixedCSantiagoS4 <- LogitResponseFunction(parameters = parametersLRATFixedCSantiagoS4, interval_w2 = intervalW2S4, v2 = v2S4, w1 = w1S4, v1 = v1S4)



#         + London (AC & AT)  ------------------------------------------------------

#Animated Control
parametersLRACFixedCLondonS4 <- list()
parametersLRACFixedCLondonS4[["bw"]] = DCMResultsACFixedCLondon[[1]][,"estimate"][["thetaW"]]
parametersLRACFixedCLondonS4[["bv"]] = DCMResultsACFixedCLondon[[1]][,"estimate"][["thetaV"]]
parametersLRACFixedCLondonS4[["aw"]] = 1
parametersLRACFixedCLondonS4[["av"]] = 1
parametersLRACFixedCLondonS4[["c"]] = 1 #DCMResultsACFixedCLondonS4[[1]][,"estimate"][["thetaP"]]
LRACFixedCLondonS4 <- LogitResponseFunction(parameters = parametersLRACFixedCLondonS4, interval_w2 = intervalW2S4, v2 = v2S4, w1 = w1S4, v1 = v1S4)

#Animated Treatment
parametersLRATFixedCLondonS4 <- list()
parametersLRATFixedCLondonS4[["bw"]] = DCMResultsATFixedCLondon[[1]][,"estimate"][["thetaW"]]
parametersLRATFixedCLondonS4[["bv"]] = DCMResultsATFixedCLondon[[1]][,"estimate"][["thetaV"]]
parametersLRATFixedCLondonS4[["aw"]] = 1
parametersLRATFixedCLondonS4[["av"]] = 1
parametersLRATFixedCLondonS4[["c"]] = 1 #DCMResultsATFixedCLondonS4[[1]][,"estimate"][["thetaP"]]
LRATFixedCLondonS4 <- LogitResponseFunction(parameters = parametersLRATFixedCLondonS4, interval_w2 = intervalW2S4, v2 = v2S4, w1 = w1S4, v1 = v1S4)

#         + Combined Plot (x4) -----------------------------------------------------------

estimatesPlotsLRS4 <- rbind(data.frame(deltaW = LRACFixedCSantiagoS4$deltaW2W1, p1 = LRACFixedCSantiagoS4$p1, type = "AC Santiago")
                            ,data.frame(deltaW = LRATFixedCSantiagoS4$deltaW2W1, p1 = LRATFixedCSantiagoS4$p1, type = "AT Santiago")
                            ,data.frame(deltaW = LRACFixedCLondonS4$deltaW2W1, p1 = LRACFixedCLondonS4$p1, type = "AC  London")
                            , data.frame(deltaW = LRATFixedCLondonS4$deltaW2W1, p1 = LRATFixedCLondonS4$p1, type = "AT London")
)

ggplot(estimatesPlotsLRS4, aes(x = deltaW, y = p1, color = interaction(type), linetype = interaction(type))) +
  geom_line(aes(group = type))+
  scale_color_manual(name = "", values=c("red", "red","blue", "blue")
                     , labels = c("AC Santiago","AT Santiago","AC London","AT London"))+ 
  scale_linetype_manual(name = "", values=c("dashed","dotted","dashed","dotted")
                        , labels = c("AC Santiago","AT Santiago","AC London","AT London"))+ 
  # geom_hline(yintercept = with(simulationParametersNumeric,bw),color = "red", linetype="dashed")+
  # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
  # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
  # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsLRS4$p1),1)) +
  # labs(x = expression(paste(Delta,w)), y = expression(P[1]))+
  labs(x = expression(paste(Delta,w==w[2]-bar(w)[1])), y = expression(paste(P[1],"=[",1+exp(V[2]-V[1]),"]",""^{-1})))+
  themePlotsLRPaper1

ggsave(str_c("export","/figures/Paper1/LogitResponse/LR4.pdf")
       , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)







# 6) Joint Model Estimation  ---------------------------------------------------------------------
#   a) Data formatting ---------------------------------------------------------

DCMData <- realChoiceData

cols <- c("averageWaitingTimeR","averageTravelTimeR","variabilityWaitingTimeR","variabilityTravelTimeR"
          ,"averageWaitingTimeI","averageTravelTimeI","variabilityWaitingTimeI","variabilityTravelTimeI"
          ,"choice", "realChoice"
          ,"experimentType","experimentalCondition","scenario","participantId", "city"
)

DCMData <- DCMData[,cols]

DCMData$choice <- DCMData$realChoice

DCMData$choice <- ifelse(realChoiceData$choice == 1,"rational","irrational")

#   b) Data curation -----------------------------------------------------------

# DCMData <- subset(DCMData, participantId %in% manipulationChecksParticipantsS1S2Rational$participantId & scenario %in% c("01","02","03","04","05","06","07","08") )

#   c) Model Estimation --------------------------------------------------------

logitProbability <- function(V1,V2){
  
  return(exp(V1)/(exp(V1)+exp(V2)))
  
}

LogitEstimation <- function(DCMData, LogLik, logitParameters, constraints){
  
  LogLik <- function(parameters,data = DCMData){
    
    browse()
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- parameters[["thetaW"]]
    thetaV <-  parameters[["thetaV"]]
    thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    # # 
    
    VR = constant + thetaW*data$averageWaitingTimeR^(thetaPW) + thetaV*data$averageTravelTimeR^(thetaPV) +thetaDW*data$variabilityWaitingTimeR+ thetaDV*data$variabilityTravelTimeR
    VI = thetaW*data$averageWaitingTimeI^(thetaPW) + thetaV*data$averageTravelTimeI^(thetaPV) + thetaDW*data$variabilityWaitingTimeI+ thetaDV*data$variabilityTravelTimeI
    
    SUMR <- sum(ifelse(data$choice == "rational",1,0)*log(logitProbability(V1 = VR, V2 = VI)))
    SUMI <- sum(ifelse(data$choice == "irrational",1,0)*log(logitProbability(V1 = VI, V2 = VR)))
    
    return(sum(SUMR+SUMI))
  }
  
  # logitParameters <- c(constant = 0, thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0,thetaPW = 0.5,thetaPV = 0.5)
  # logitParameters <- c(thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0)
  
  # logitParameters <- c(constant = 0, thetaW = -1.724, thetaV = -1.412, thetaDW = -0.326, thetaDV = -0.317)
  
  # LogLik(parameters = logitParameters)
  
  # Among the different features of maxLik, it allows constraint optimization by using the argument constraints. 
  # This argument can be either NULL for unconstrained optimization (the default in gmnl) or a list with two components. 
  # The components may be either eqA and eqB for equality-constrained optimization Aθ+B=0; or ineqA and ineqB for inequality constraints Aθ+B>0. 
  # More than one row in ineqA and ineqB corresponds to more than one linear constraint, in that case all these must be zero (equality) or positive 
  # (inequality constraints).
  
  
  # 
  # A <- matrix(c(0,0,0,0,0,1), nrow = 1 , ncol = 6)
  # B =  matrix(c(0), nrow = 1 , ncol = 1)
  
  MLParameters <- maxLik(logLik = LogLik,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

LogitEstimation1 <- function(DCMData, LogLik, logitParameters, constraints){
  
  LogLikMNL <- function(parameters,data = DCMData){
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- parameters[["thetaW"]]
    thetaV <-  parameters[["thetaV"]]
    thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    # # 
    
    VR = constant + thetaW*data$averageWaitingTimeR^(thetaPW*ifelse(data$experimentalCondition == "control",1,0)) + thetaV*data$averageTravelTimeR^(thetaPV*ifelse(data$experimentalCondition == "control",1,0))
    VI = + thetaW*data$averageWaitingTimeR^(thetaPW*ifelse(data$experimentalCondition == "control",1,0)) + thetaV*data$averageTravelTimeI^(thetaPV*ifelse(data$experimentalCondition == "control",1,0))
    
    SUMR <- sum(ifelse(data$choice == "rational",1,0)*log(logitProbability(V1 = VR, V2 = VI)))
    SUMI <- sum(ifelse(data$choice == "irrational",1,0)*log(logitProbability(V1 = VI, V2 = VR)))
    
    return(sum(SUMR+SUMI))
  }
  
  # logitParameters <- c(constant = 0, thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0,thetaPW = 0.5,thetaPV = 0.5)
  # logitParameters <- c(thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0)
  
  # logitParameters <- c(constant = 0, thetaW = -1.724, thetaV = -1.412, thetaDW = -0.326, thetaDV = -0.317)
  
  # LogLik(parameters = logitParameters)
  
  # Among the different features of maxLik, it allows constraint optimization by using the argument constraints. 
  # This argument can be either NULL for unconstrained optimization (the default in gmnl) or a list with two components. 
  # The components may be either eqA and eqB for equality-constrained optimization Aθ+B=0; or ineqA and ineqB for inequality constraints Aθ+B>0. 
  # More than one row in ineqA and ineqB corresponds to more than one linear constraint, in that case all these must be zero (equality) or positive 
  # (inequality constraints).
  
  
  # 
  # A <- matrix(c(0,0,0,0,0,1), nrow = 1 , ncol = 6)
  # B =  matrix(c(0), nrow = 1 , ncol = 1)
  
  MLParameters <- maxLik(logLik = LogLik,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

LogitEstimationBoxCox <- function(DCMData, LogLik, logitParameters, constraints){
  
  LogLikBoxCox <- function(parameters,data = DCMData){
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- parameters[["thetaW"]]
    thetaV <-  parameters[["thetaV"]]
    thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    
    thetaT <-  ifelse(is.na(parameters["thetaT"]),1,parameters[["thetaT"]]) #Scaling parameter
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
      VR2 <-  thetaT*thetaV*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
      VI2 <- thetaT*thetaV*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW!=0 & thetaPV==0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
      VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
      VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    }
    
    if(thetaPW==0 & thetaPV!=0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW==0 & thetaPV==0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
      VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    }
    
    
    
    VR = constant + VR1+VR2
    VI = VI1+VI2
    
    SUMR <- sum(ifelse(data$choice == "rational",1,0)*log(logitProbability(V1 = VR, V2 = VI)))
    SUMI <- sum(ifelse(data$choice == "irrational",1,0)*log(logitProbability(V1 = VI, V2 = VR)))
    
    return(sum(SUMR+SUMI))
  }
  
  # logitParameters <- c(constant = 0, thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0,thetaPW = 0.5,thetaPV = 0.5)
  # logitParameters <- c(thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0)
  
  # logitParameters <- c(constant = 0, thetaW = -1.724, thetaV = -1.412, thetaDW = -0.326, thetaDV = -0.317)
  
  # LogLik(parameters = logitParameters)
  
  # Among the different features of maxLik, it allows constraint optimization by using the argument constraints. 
  # This argument can be either NULL for unconstrained optimization (the default in gmnl) or a list with two components. 
  # The components may be either eqA and eqB for equality-constrained optimization Aθ+B=0; or ineqA and ineqB for inequality constraints Aθ+B>0. 
  # More than one row in ineqA and ineqB corresponds to more than one linear constraint, in that case all these must be zero (equality) or positive 
  # (inequality constraints).
  
  
  # 
  # A <- matrix(c(0,0,0,0,0,1), nrow = 1 , ncol = 6)
  # B =  matrix(c(0), nrow = 1 , ncol = 1)
  
  MLParameters <- maxLik(logLik = LogLikBoxCox,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

LogitEstimationBoxCoxDescriptive <- function(DCMData, LogLik, logitParameters, constraints){
  
  LogLikBoxCoxDescriptive <- function(parameters,data = DCMData){
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- parameters[["thetaW"]]
    thetaV <-  parameters[["thetaV"]]
    thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    
    thetaT <-  ifelse(is.na(parameters["thetaT"]),1,parameters[["thetaT"]]) #Scaling parameter
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),thetaPW,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      VR1  <- thetaT*thetaW*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2 <-  thetaT*thetaV*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaT*thetaW*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2 <- thetaT*thetaV*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW!=0 & thetaPV==0){
      VR1  <- thetaT*thetaW*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
      VI1 <- thetaT*thetaW*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    }
    
    if(thetaPW==0 & thetaPV!=0){
      VR1  <- thetaW*thetaT*log(data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaW*thetaT*log(data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW==0 & thetaPV==0){
      VR1  <- thetaW*thetaT*log(data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
      VI1 <- thetaW*thetaT*log(data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    }
    
    
    
    VR = constant + VR1+VR2
    VI = VI1+VI2
    
    SUMR <- sum(ifelse(data$choice == "rational",1,0)*log(logitProbability(V1 = VR, V2 = VI)))
    SUMI <- sum(ifelse(data$choice == "irrational",1,0)*log(logitProbability(V1 = VI, V2 = VR)))
    
    return(sum(SUMR+SUMI))
  }
  
  # logitParameters <- c(constant = 0, thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0,thetaPW = 0.5,thetaPV = 0.5)
  # logitParameters <- c(thetaW = 0, thetaV = 0, thetaDW = 0, thetaDV = 0)
  
  # logitParameters <- c(constant = 0, thetaW = -1.724, thetaV = -1.412, thetaDW = -0.326, thetaDV = -0.317)
  
  # LogLik(parameters = logitParameters)
  
  # Among the different features of maxLik, it allows constraint optimization by using the argument constraints. 
  # This argument can be either NULL for unconstrained optimization (the default in gmnl) or a list with two components. 
  # The components may be either eqA and eqB for equality-constrained optimization Aθ+B=0; or ineqA and ineqB for inequality constraints Aθ+B>0. 
  # More than one row in ineqA and ineqB corresponds to more than one linear constraint, in that case all these must be zero (equality) or positive 
  # (inequality constraints).
  
  
  # 
  # A <- matrix(c(0,0,0,0,0,1), nrow = 1 , ncol = 6)
  # B =  matrix(c(0), nrow = 1 , ncol = 1)
  
  MLParameters <- maxLik(logLik = LogLikBoxCoxDescriptive,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

LogitEstimationBoxCoxSameSensitivity <- function(DCMData, LogLik, logitParameters, constraints){
  
  # browser()
  
  # ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
  
  LogLikBoxCoxSameSensitivity <- function(parameters,data = DCMData){
    
    # browser()
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- -2.5 #parameters[["thetaW"]]
    thetaV <-  -1.5#parameters[["thetaV"]]
    # thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    # thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    # 
    thetaT <-  ifelse(is.na(parameters["thetaT"]),1,parameters[["thetaT"]]) #Scaling parameter
    
    # thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    # thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    thetaP <-  ifelse(is.na(parameters["thetaP"]),1,parameters[["thetaP"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
      VR2 <-  thetaT*thetaV*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
      VI2 <- thetaT*thetaV*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW!=0 & thetaPV==0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
      VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
      VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    }
    
    if(thetaPW==0 & thetaPV!=0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
      VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    }
    
    if(thetaPW==0 & thetaPV==0){
      VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
      VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
      VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
      VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    }
    
    
    
    VR = constant + VR1+VR2
    VI = VI1+VI2
    
    SUMR <- sum(ifelse(data$choice == "rational",1,0)*log(logitProbability(V1 = VR, V2 = VI)))
    SUMI <- sum(ifelse(data$choice == "irrational",1,0)*log(logitProbability(V1 = VI, V2 = VR)))
    
    return(sum(SUMR+SUMI))
  }
  
  # browser()
  MLParameters <- maxLik(logLik = LogLikBoxCoxSameSensitivity,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}



#   d) Estimation Results -------------------------------------------------------



#   e) Model 1 (Numeric and animated choices in control condition) -----------------------------------------------------------------

LogitEstimationControlModel <- function(DCMData, LogLik, logitParameters, constraints){
  
  # browser()
  
  # ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
  
  LogLikControlModel<- function(parameters,data = DCMData){
    
    # browser()
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- ifelse(is.na(parameters["thetaW"]),0,parameters["thetaW"]) #parameters[["thetaW"]]
    thetaV <-  ifelse(is.na(parameters["thetaV"]),0,parameters["thetaV"])#parameters[["thetaV"]]
    # thetaDW <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
    # thetaDV <-  ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDV"]])
    # 
    mu_A <-  ifelse(is.na(parameters["mu_A"]),1,parameters[["mu_A"]]) #Scaling parameter applied to animated choices
    mu_N <-  ifelse(is.na(parameters["mu_N"]),1,parameters[["mu_N"]]) #Scaling parameter applied to numeric choices
    
    # thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    # thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    thetaPN <-  ifelse(is.na(parameters["thetaPN"]),1,parameters[["thetaPN"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      # VR1_A  <- thetaW*thetaT*(subset(data, experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      # VR2_A <-  thetaV*thetaT*(subset(data, experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      # 
      # VR1_N  <- thetaW*thetaN*(subset(data, experimentType == 'numeric')$averageWaitingTimeR^(thetaPN)-1)/thetaPN
      # VR2_N <-  thetaV*thetaN*(subset(data, experimentType == 'numeric')$averageTravelTimeR^(thetaPN)-1)/thetaPN
      
      # print(thetaN)
      # browser()
      
      # Animated choices
      
      # - Rational alternatives
      VR1_A  <- thetaW*mu_A*(subset(data, experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_A <-  thetaV*mu_A*(subset(data, experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_A <- VR1_A+VR2_A
      
      # Irrational alternatives
      VI1_A  <- thetaW*mu_A*(subset(data, experimentType == 'animated')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_A <-  thetaV*mu_A*(subset(data, experimentType == 'animated')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_A = VI1_A+VI2_A
      
      # Likelihood sum
      
      SUMR_A <- sum(ifelse(subset(data, experimentType == 'animated')$choice == "rational",1,0)*log(logitProbability(V1 = VR_A, V2 = VI_A)))
      
      SUMI_A <- sum(ifelse(subset(data, experimentType == 'animated')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_A, V2 = VR_A)))
      
      SUM_A <- sum(SUMR_A+SUMI_A)
      
      #Numeric choices
      
      # - Rational alternatives
      VR1_N  <- thetaW*mu_N*(subset(data, experimentType == 'numeric')$averageWaitingTimeR^(thetaPN)-1)/thetaPN
      VR2_N <-  thetaV*mu_N*(subset(data, experimentType == 'numeric')$averageTravelTimeR^(thetaPN)-1)/thetaPN
      
      VR_N <- VR1_N+VR2_N
      
      # Irrational alternatives
      VI1_N  <- thetaW*mu_N*(subset(data, experimentType == 'numeric')$averageWaitingTimeI^(thetaPN)-1)/thetaPN
      VI2_N <-  thetaV*mu_N*(subset(data, experimentType == 'numeric')$averageTravelTimeI^(thetaPN)-1)/thetaPN
      
      VI_N = VI1_N+VI2_N
      
      SUMR_N <- sum(ifelse(subset(data, experimentType == 'numeric')$choice == "rational",1,0)*log(logitProbability(V1 = VR_N, V2 = VI_N)))
      
      SUMI_N <- sum(ifelse(subset(data, experimentType == 'numeric')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_N, V2 = VR_N)))
      
      
      SUM_N <- sum(SUMR_N+SUMI_N)
      
      
    }
    
    # if(thetaPW!=0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
    #   VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
    #   VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    # }
    # 
    # if(thetaPW==0 & thetaPV!=0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    # }
    # 
    # if(thetaPW==0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    # }
    
    # browser()
    
    
    
    # browser()
    return(sum(SUM_A+SUM_N))
    # return(sum(SUMR+SUMI))
  }
  
  # browser()
  MLParameters <- maxLik(logLik =  LogLikControlModel,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

constraints2 = NULL
DCMData1 <- subset(DCMData, experimentalCondition == 'control' & scenario %in% c("01","02","03","04","05","06","07","08") )
# DCMData1 <- subset(DCMData,  scenario %in% c("01","02","03","04","05","06","07","08") )

# Fixing curvatures to one

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaN=NULL, mu_A=NULL,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  NULL)

results1 <- LogitEstimationControlModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

# Letting all curvature in animated choices to be different than 1

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = 0.5, thetaPW=  0.5,thetaPV =  0.5)

results1 <- LogitEstimationControlModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

# Letting curvature of waiting time in animated choices to be different than 1

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  0.5)

results1 <- LogitEstimationControlModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

#   f) Model 2 (Animated choices in control and treatment conditions) -----------------------------------------------------------------

LogitEstimationAnimatedModel <- function(DCMData, LogLik, logitParameters, constraints){
  
  # browser()
  
  # ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
  
  LogLikAnimatedModel<- function(parameters,data = DCMData){
    
    # browser()
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- ifelse(is.na(parameters["thetaW"]),0,parameters["thetaW"]) #parameters[["thetaW"]]
    thetaV <-  ifelse(is.na(parameters["thetaV"]),0,parameters["thetaV"])#parameters[["thetaV"]]


    thetaWT <-  ifelse(is.na(parameters["thetaWT"]),1,parameters[["thetaWT"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      # VR1_A  <- thetaW*thetaT*(subset(data, experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      # VR2_A <-  thetaV*thetaT*(subset(data, experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      # 
      # VR1_N  <- thetaW*thetaN*(subset(data, experimentType == 'numeric')$averageWaitingTimeR^(thetaPN)-1)/thetaPN
      # VR2_N <-  thetaV*thetaN*(subset(data, experimentType == 'numeric')$averageTravelTimeR^(thetaPN)-1)/thetaPN
      
      # print(thetaN)
      # browser()
      
      # Animated choices (control)
      
      # - Rational alternatives
      VR1_AC  <- thetaW*(subset(data, experimentalCondition == 'control')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AC <-  thetaV*(subset(data, experimentalCondition == 'control')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_AC <- VR1_AC+VR2_AC
      
      # Irrational alternatives
      VI1_AC  <- thetaW*(subset(data, experimentalCondition == 'control')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AC <-  thetaV*(subset(data, experimentalCondition == 'control')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_AC = VI1_AC+VI2_AC
      
      # Likelihood sum
      
      SUMR_AC <- sum(ifelse(subset(data, experimentalCondition == 'control')$choice == "rational",1,0)*log(logitProbability(V1 = VR_AC, V2 = VI_AC)))
      
      SUMI_AC <- sum(ifelse(subset(data, experimentalCondition == 'control')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AC, V2 = VR_AC)))
      
      SUM_AC <- sum(SUMR_AC+SUMI_AC)
      
      #Animated choices (treatment)
      
      # - Rational alternatives
      VR1_AT  <- thetaWT*(subset(data, experimentalCondition == 'treatment')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AT <-  thetaV*(subset(data, experimentalCondition == 'treatment')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_AT <- VR1_AT+VR2_AT
      
      # Irrational alternatives
      VI1_AT  <- thetaWT*(subset(data, experimentalCondition == 'treatment')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AT <-  thetaV*(subset(data, experimentalCondition == 'treatment')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_AT = VI1_AT+VI2_AT
      
      # Likelihood sum
      
      SUMR_AT <- sum(ifelse(subset(data, experimentalCondition == 'treatment')$choice == "rational",1,0)*log(logitProbability(V1 = VR_AT, V2 = VI_AT)))
      
      SUMI_AT <- sum(ifelse(subset(data, experimentalCondition == 'treatment')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AT, V2 = VR_AT)))
      
      SUM_AT <- sum(SUMR_AT+SUMI_AT)
      

      
      
    }
    
    # if(thetaPW!=0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
    #   VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
    #   VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    # }
    # 
    # if(thetaPW==0 & thetaPV!=0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    # }
    # 
    # if(thetaPW==0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    # }
    
    # browser()
    
    
    
    # browser()
    return(sum(SUM_AC+SUM_AT))
    # return(sum(SUMR+SUMI))
  }
  
  # browser()
  MLParameters <- maxLik(logLik =  LogLikAnimatedModel,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

constraints2 = NULL
DCMData1 <- subset(DCMData, experimentType == 'animated'  & scenario %in% c("01","02","03","04","05","06","07","08") )
# DCMData1 <- subset(DCMData,  scenario %in% c("01","02","03","04","05","06","07","08") )

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL,constant = NULL, thetaPW=  1,thetaPV =  1)

results1 <- LogitEstimationAnimatedModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

#   g) Model 3 (All) -----------------------------------------------------------------

LogitEstimationFullModel <- function(DCMData, LogLik, logitParameters, constraints){
  
  # browser()
  
  # ifelse(is.na(parameters["thetaDW"]),0,parameters[["thetaDW"]])
  
  LogLikFullModel<- function(parameters,data = DCMData){
    
    # browser()
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- ifelse(is.na(parameters["thetaW"]),0,parameters["thetaW"]) #parameters[["thetaW"]]
    thetaV <-  ifelse(is.na(parameters["thetaV"]),0,parameters["thetaV"])#parameters[["thetaV"]]
    
    mu_A <-  ifelse(is.na(parameters["mu_A"]),1,parameters[["mu_A"]]) #Scaling parameter applied to animated choices
    mu_N <-  ifelse(is.na(parameters["mu_N"]),1,parameters[["mu_N"]]) #Scaling parameter applied to numeric choices
    
    # thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    # thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    thetaPN <-  ifelse(is.na(parameters["thetaPN"]),1,parameters[["thetaPN"]])
    
    
    thetaWT <-  ifelse(is.na(parameters["thetaWT"]),0,parameters[["thetaWT"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),1,parameters[["thetaPW"]])
    
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),1,parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
      # VR1_A  <- thetaW*thetaT*(subset(data, experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      # VR2_A <-  thetaV*thetaT*(subset(data, experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      # 
      # VR1_N  <- thetaW*thetaN*(subset(data, experimentType == 'numeric')$averageWaitingTimeR^(thetaPN)-1)/thetaPN
      # VR2_N <-  thetaV*thetaN*(subset(data, experimentType == 'numeric')$averageTravelTimeR^(thetaPN)-1)/thetaPN
      
      # print(thetaN)
      # browser()
      
      # Animated choices (control)
      
      # - Rational alternatives
      VR1_AC  <- thetaW*mu_A*(subset(data, experimentalCondition == 'control' & experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AC <-  thetaV*mu_A*(subset(data, experimentalCondition == 'control' & experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_AC <- VR1_AC+VR2_AC
      
      # Irrational alternatives
      VI1_AC  <- thetaW*mu_A*(subset(data, experimentalCondition == 'control'& experimentType == 'animated')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AC <-  thetaV*mu_A*(subset(data, experimentalCondition == 'control'& experimentType == 'animated')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_AC = VI1_AC+VI2_AC
      
      # Likelihood sum
      
      SUMR_AC <- sum(ifelse(subset(data, experimentalCondition == 'control'& experimentType == 'animated')$choice == "rational",1,0)*log(logitProbability(V1 = VR_AC, V2 = VI_AC)))
      
      SUMI_AC <- sum(ifelse(subset(data, experimentalCondition == 'control'& experimentType == 'animated')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AC, V2 = VR_AC)))
      
      SUM_AC <- sum(SUMR_AC+SUMI_AC)
      
      #Animated choices (treatment)
      
      # - Rational alternatives
      VR1_AT  <- (thetaW+thetaWT)*mu_A*(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AT <-  thetaV*mu_A*(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_AT <- VR1_AT+VR2_AT
      
      # Irrational alternatives
      VI1_AT  <- (thetaW+thetaWT)*mu_A*(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AT <-  thetaV*mu_A*(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_AT = VI1_AT+VI2_AT
      
      # Likelihood sum
      
      SUMR_AT <- sum(ifelse(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$choice == "rational",1,0)*log(logitProbability(V1 = VR_AT, V2 = VI_AT)))
      
      SUMI_AT <- sum(ifelse(subset(data, experimentalCondition == 'treatment'& experimentType == 'animated')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AT, V2 = VR_AT)))
      
      SUM_AT <- sum(SUMR_AT+SUMI_AT)
      
      
      #Numeric choices
      
      # - Rational alternatives
      VR1_N  <- thetaW*mu_N*(subset(data, experimentType == 'numeric')$averageWaitingTimeR^(thetaPN)-1)/thetaPN
      VR2_N <-  thetaV*mu_N*(subset(data, experimentType == 'numeric')$averageTravelTimeR^(thetaPN)-1)/thetaPN
      
      VR_N <- VR1_N+VR2_N
      
      # Irrational alternatives
      VI1_N  <- thetaW*mu_N*(subset(data, experimentType == 'numeric')$averageWaitingTimeI^(thetaPN)-1)/thetaPN
      VI2_N <-  thetaV*mu_N*(subset(data, experimentType == 'numeric')$averageTravelTimeI^(thetaPN)-1)/thetaPN
      
      VI_N = VI1_N+VI2_N
      
      SUMR_N <- sum(ifelse(subset(data, experimentType == 'numeric')$choice == "rational",1,0)*log(logitProbability(V1 = VR_N, V2 = VI_N)))
      
      SUMI_N <- sum(ifelse(subset(data, experimentType == 'numeric')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_N, V2 = VR_N)))
      
      
      SUM_N <- sum(SUMR_N+SUMI_N)
      
      
      
      
    }
    
    # if(thetaPW!=0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
    #   VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
    #   VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    # }
    # 
    # if(thetaPW==0 & thetaPV!=0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    # }
    # 
    # if(thetaPW==0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    # }
    
    # browser()
    
    
    
    # browser()
    return(sum(SUM_AC+SUM_AT + SUM_N))
    # return(sum(SUMR+SUMI))
  }
  
  # browser()
  MLParameters <- maxLik(logLik =  LogLikFullModel,start = logitParameters,method = "BFGS",constraints = constraints) #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

constraints2 = NULL
# DCMData1 <- subset(DCMData,  scenario %in% c("01","02","03","04","05","06","07","08") )
# DCMData1 <- subset(DCMData, city == 'Santiago' & scenario %in% c("01","02","03","04","05","06","07","08") )
DCMData1 <- subset(DCMData, city == 'London' & scenario %in% c("01","02","03","04","05","06","07","08") )

# 1) only waiting and in-vehicle time  
logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL, thetaN=NULL, mu_A=NULL,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  NULL)

results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]
AIC(results1[[2]])

# 2) Relative scale factor to predict jointly
logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  NULL)

results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]
AIC(results1[[2]])

# 3) Add curvatures

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = 1, thetaPW=  1,thetaPV =  1)

results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]
AIC(results1[[2]])

# 4) Only travel time curvature set to be different than 1 (assume waiting time curvature is 1)
logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  1)

results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]
AIC(results1[[2]])

# # Only waiting time curvature set to be different than 1
# logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=NULL, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = NULL, thetaPW=  1,thetaPV =  NULL)
# 
# results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
# results1
# 
# results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

# 5) Model with treatment effect of waiting time
logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=1, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = NULL, thetaPW=  NULL,thetaPV =  1)
results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
results1

results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

AIC(results1[[2]])

# # 6) Full model
# logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=1, thetaN=NULL, mu_A=1,constant = NULL, thetaPN = 1, thetaPW=  1,thetaPV = 1)
# results1 <- LogitEstimationFullModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints2)
# results1
# 
# results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

#   h) Model 4 (fixing time perception exponents to recover true time preferences animated experiment)  -------------------------------

LogitEstimationAnimatedFixedTimePerceptionModel <- function(DCMData, LogLik, logitParameters, constraints){

  
  LogLikAnimatedFixedTimePerceptionModel<- function(parameters,data = DCMData){
    
    # browser()
    
    constant <- ifelse(is.na(parameters["constant"]),0,parameters["constant"])
    thetaW <- ifelse(is.na(parameters["thetaW"]),0,parameters["thetaW"]) 
    thetaV <-  ifelse(is.na(parameters["thetaV"]),0,parameters["thetaV"])
    
    
    thetaWT <-  ifelse(is.na(parameters["thetaWT"]),constraints[["thetaWT"]],parameters[["thetaWT"]])
    
    thetaPW <-  ifelse(is.na(parameters["thetaPW"]),constraints[["thetaPW"]],parameters[["thetaPW"]])
    
    thetaPV <-  ifelse(is.na(parameters["thetaPV"]),constraints[["thetaPV"]],parameters[["thetaPV"]])
    
    # # 
    
    if(thetaPW !=0 & thetaPV!=0){
  
      # Animated choices (control)
      
      # - Rational alternatives
      VR1_AC  <- thetaW*(subset(data, experimentType == 'animated' & experimentalCondition == 'control')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AC <-  thetaV*(subset(data, experimentType == 'animated' & experimentalCondition == 'control')$averageTravelTimeR^(thetaPV)-1)/thetaPV
      
      VR_AC <- VR1_AC+VR2_AC
      
      # Irrational alternatives
      VI1_AC  <- thetaW*(subset(data, experimentType == 'animated' & experimentalCondition == 'control')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AC <-  thetaV*(subset(data, experimentType == 'animated' & experimentalCondition == 'control')$averageTravelTimeI^(thetaPV)-1)/thetaPV
      
      VI_AC = VI1_AC+VI2_AC
      
      # Likelihood sum
      
      SUMR_AC <- sum(ifelse(subset(data,)$choice == "rational",1,0)*log(logitProbability(V1 = VR_AC, V2 = VI_AC)))
      
      SUMI_AC <- sum(ifelse(subset(data)$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AC, V2 = VR_AC)))
      
      SUM_AC <- sum(SUMR_AC+SUMI_AC)
      
      #Animated choices (treatment)

      # - Rational alternatives
      VR1_AT  <- (thetaW+thetaWT)*(subset(data, experimentType == 'animated' & experimentalCondition == 'treatment')$averageWaitingTimeR^(thetaPW)-1)/thetaPW
      VR2_AT <-  thetaV*(subset(data, experimentType == 'animated' &experimentalCondition == 'treatment')$averageTravelTimeR^(thetaPV)-1)/thetaPV

      VR_AT <- VR1_AT+VR2_AT

      # Irrational alternatives
      VI1_AT  <- (thetaW+thetaWT)*(subset(data, experimentType == 'animated' &experimentalCondition == 'treatment')$averageWaitingTimeI^(thetaPW)-1)/thetaPW
      VI2_AT <-  thetaV*(subset(data, experimentType == 'animated' &experimentalCondition == 'treatment')$averageTravelTimeI^(thetaPV)-1)/thetaPV

      VI_AT = VI1_AT+VI2_AT

      # Likelihood sum

      SUMR_AT <- sum(ifelse(subset(data, experimentType == 'animated' &experimentalCondition == 'treatment')$choice == "rational",1,0)*log(logitProbability(V1 = VR_AT, V2 = VI_AT)))

      SUMI_AT <- sum(ifelse(subset(data, experimentType == 'animated' &experimentalCondition == 'treatment')$choice == "irrational",1,0)*log(logitProbability(V1 = VI_AT, V2 = VR_AT)))

      SUM_AT <- sum(SUMR_AT+SUMI_AT)
      
      
      
      
    }
    
    # if(thetaPW!=0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeR^(thetaPW)-1)/thetaPW,data$averageWaitingTimeR)
    #   VR2 <-  thetaT*thetaV*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*(data$averageWaitingTimeI^(thetaPW)-1)/thetaPW,data$averageWaitingTimeI)
    #   VI2 <- thetaT*thetaV*log(data$averageTravelTimeI)
    # }
    # 
    # if(thetaPW==0 & thetaPV!=0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*(data$averageTravelTimeR^(thetaPV)-1)/thetaPV
    #   VI1 <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*(data$averageTravelTimeI^(thetaPV)-1)/thetaPV
    # }
    # 
    # if(thetaPW==0 & thetaPV==0){
    #   VR1  <- thetaW*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeR),data$averageWaitingTimeR)
    #   VR2 <-  thetaV*thetaT*log(data$averageTravelTimeR)
    #   VI1 <- thetaW*thetaT*ifelse(data$experimentalCondition == "control",thetaT*log(data$averageWaitingTimeI),data$averageWaitingTimeI)
    #   VI2 <- thetaV*thetaT*log(data$averageTravelTimeI)
    # }
    
    # browser()
    
    
    
    # browser()
    return(sum(SUM_AC+SUM_AT))
    # return(sum(SUMR+SUMI))
  }
  
  # browser()
  MLParameters <- maxLik(logLik =  LogLikAnimatedFixedTimePerceptionModel,start = logitParameters,method = "BFGS") #,constraints = list(ineqA = A, ineqB = B)
  MLParameters
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  return(list(resultsML,MLParameters))
  # return(list(MLParameters))
}

logitParameters1 <- c(thetaW =0, thetaV = 0, thetaWT=0,constant = NULL, thetaPW=  NULL,thetaPV =  NULL)

#Santiago
DCMData1 <- subset(DCMData, experimentType == 'animated' & city == 'Santiago' & scenario %in% c("01","02","03","04","05","06","07","08") )


# 1) Santiago with exponents equal to 1
constraints1 = c(thetaW =0, thetaV = 0, thetaWT=NULL,constant = NULL, thetaPW=  1,thetaPV =  1)
results1 <- LogitEstimationAnimatedFixedTimePerceptionModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints1)
results1
results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

# 2) Santiago with fixed perception
constraints1 = c(thetaW =0, thetaV = 0, thetaWT=NULL,constant = NULL, thetaPW=  1,thetaPV =  1.117)
results1 <- LogitEstimationAnimatedFixedTimePerceptionModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints1)
results1
results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

#London
DCMData1 <- subset(DCMData, experimentType == 'animated' & city == 'London' & scenario %in% c("01","02","03","04","05","06","07","08") )

# 3) London with exponents equal to 1
constraints1 = c(thetaW =0, thetaV = 0, thetaWT=NULL,constant = NULL, thetaPW=  1,thetaPV =  1)
results1 <- LogitEstimationAnimatedFixedTimePerceptionModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints1)
results1
results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]

# 4) London with fixed perception
constraints1 = c(thetaW =0, thetaV = 0, thetaWT=NULL,constant = NULL, thetaPW=  1,thetaPV =  1.188)
results1 <- LogitEstimationAnimatedFixedTimePerceptionModel(DCMData = DCMData1,LogLik = LogLikBoxCoxSameSensitivity, logitParameters = logitParameters1, constraints = constraints1)
results1
results1[[1]]["thetaW","estimate"]/results1[[1]]["thetaV","estimate"]





