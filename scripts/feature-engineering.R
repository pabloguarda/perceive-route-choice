
# 1) PARTICIPANTS INFORMATION ------------------------------------------------

#   a) Information from Google Form  -------------------------------------------
# 
#Participants per Subject
table(googleFormsParticipantsChile$eduSubject)

#     - List of participants names and ids ------------------------------------

unique(data.frame(experiencedLearningResponses$participantId))

#     - Session Duration ------------------------------------------------------

mean(participantsChile$sessionDuration)

#     - Append Relevant Participant Information to Other Datasets -------------

# varListParticipants = c("participantId","gender","age","eduLevel","eduSubject","publicTransportUse","country")
varListParticipants = c("participantId","gender","age","eduLevel","country","city"
                        ,"realWalkingTime","realWaitingTime","realTravelTime","realJourneyTime")

experiencedLearningResponses <- merge(experiencedLearningResponses, participants[varListParticipants],by = c("participantId"))
experiencedChoiceResponses <- merge(experiencedChoiceResponses, participants[varListParticipants],by = c("participantId"))
experiencedChoiceResponsesDecisionStep <- merge(experiencedChoiceResponsesDecisionStep, participants[varListParticipants],by = c("participantId"))
experiencedChoiceResponsesConfirmationStep <- merge(experiencedChoiceResponsesConfirmationStep, participants[varListParticipants],by = c("participantId"))


descriptiveLearningResponses <- merge(descriptiveLearningResponses, participants[varListParticipants],by = c("participantId"))
descriptiveChoiceResponses <- merge(descriptiveChoiceResponses, participants[varListParticipants],by = c("participantId"))

#   b) Travel Behaviour Responses (only UK) ---------------------------------



# 2) EXPERIENCED EXPERIMENTS -------------------------------------------------

#   a) Experienced Learning Responses --------------------------------------------
#   b) Experienced Choice Responses --------------------------------------------
#   c) Split dataset by Experimental Condition -----------------------------------------------

experiencedChoiceResponsesControlCondition <- subset(experiencedChoiceResponses,experiencedChoiceResponses$experimentalCondition == "control")
experiencedChoiceResponsesTreatmentCondition <- subset(experiencedChoiceResponses,experiencedChoiceResponses$experimentalCondition == "treatment")
#   d) Missing choice (only for the dataset collected in Chile) -----------------------

#dd a column indicating the missing scenario in he experienced experiment conducted in Chile
experiencedChoiceResponses$missingExperiencedScenario <- NA
experiencedChoiceResponses$imputation <- NA
nAddedRows <- 0

chileanParticipantsIds <- unique(subset(participants,country == "Chile")$participantId)
id <- 2
for(id in 1:length(chileanParticipantsIds)){
  # participant_Id = chileanParticipantsIds[id]
  participant_Id = chileanParticipantsIds[id]

  #Decision Choice Set
  choicesParticipantDecisionStepExperimentalBlock3 = subset(experiencedChoiceResponses,experimentalStep == "decision" & participantId == participant_Id & experimentalBlock == "03")
  choicesParticipantDecisionStepExperimentalBlock3 = choicesParticipantDecisionStepExperimentalBlock3[order(choicesParticipantDecisionStepExperimentalBlock3$scenario),]#Order observations based on the scenario absolute id

  #Confirmation Choice Set
  choicesParticipantConfirmationStepExperimentalBlock3 = subset(experiencedChoiceResponses,experimentalStep == "confirmation"& participantId == participant_Id & experimentalBlock == "03")
  choicesParticipantConfirmationStepExperimentalBlock3 = choicesParticipantConfirmationStepExperimentalBlock3[order(choicesParticipantConfirmationStepExperimentalBlock3$scenario),]#Order observations based on the scenario absolute id

  #There should be 14 scenarios but there is only 13. There is one missing in the experimental block 3.
  firstScenarioExperimentalBlock3 = min(as.integer(subset(choicesParticipantDecisionStepExperimentalBlock3,choicesParticipantDecisionStepExperimentalBlock3$experimentalBlock == "03")$scenario))
  # nScenarioExperimentalBlock3 = min(as.integer(subset(choicesParticipantDecisionStepExperimentalBlock3,choicesParticipantDecisionStepExperimentalBlock3$experimentalBlock == "03")$scenario))
  nScenariosExperiment = 14

  counterScenarioExperimentalBlock3 = 9 #Counter to check whether all scenarios were written in the file.


  scenario14missing = TRUE
  nRowMissingExperiencedScenarioDecisionStep = NA
  nRowMissingExperiencedScenarioConfirmationStep = NA #1 row after the previous one

  for(i in 1:dim(choicesParticipantDecisionStepExperimentalBlock3)[1]){

    counterScenarioExperimentalBlock3 <- scenario.id(counterScenarioExperimentalBlock3) #The first scenario in block 3 is "09"

    nScenarioExperimentalBlock3 <- choicesParticipantDecisionStepExperimentalBlock3[i,]$scenario

    if(nScenarioExperimentalBlock3 != counterScenarioExperimentalBlock3){
      experiencedChoiceResponses$missingExperiencedScenario[experiencedChoiceResponses$participantId == participant_Id] <- counterScenarioExperimentalBlock3
      counterScenarioExperimentalBlock3 <- as.integer(counterScenarioExperimentalBlock3) + 1
      scenario14missing = FALSE
      nRowMissingExperiencedScenarioDecisionStep <- choicesParticipantDecisionStepExperimentalBlock3[i,]$nObservation
      nRowMissingExperiencedScenarioConfirmationStep <- nRowMissingExperiencedScenarioDecisionStep + 1
    }

    if(scenario14missing == TRUE & i == dim(choicesParticipantDecisionStepExperimentalBlock3)[1]){

      counterScenarioExperimentalBlock3 <- as.integer(counterScenarioExperimentalBlock3) + 1
      counterScenarioExperimentalBlock3 <- scenario.id(counterScenarioExperimentalBlock3)
      experiencedChoiceResponses$missingExperiencedScenario[experiencedChoiceResponses$participantId == participant_Id] <- counterScenarioExperimentalBlock3
      # nRowMissingExperiencedScenarioDecisionStep = choicesParticipantDecisionStepExperimentalBlock3[i+1,]$nObservation
      # nRowMissingExperiencedScenarioConfirmationStep <- nRowMissingExperiencedScenarioDecisionStep + 1
    }

    counterScenarioExperimentalBlock3 <-as.integer(counterScenarioExperimentalBlock3) + 1

    # i <- i+1
  }

}

  #I need to implement this part
  # #Add a row temporarilly with NA values
  # rowPosition = nRowMissingExperiencedScenarioDecisionStep+nAddedRows
  # experiencedChoiceResponses = insertRow(existingDF = experiencedChoiceResponses,newrow = NA,r=rowPosition)
  # # experiencedChoiceResponses = insertRow(existingDF = experiencedChoiceResponses,newrow = rep("NA",dim(experiencedChoiceResponses)[2]),r=nRowMissingExperiencedScenarioDecisionStep+nAddedRows)
  # nAddedRows <- nAddedRows + 1
  
#Add the imputating choices to another dataset
# experiencedChoiceResponsesDecisionStep <- subset(experiencedChoiceResponses,experimentalStep == "decision")



# 3) DESCRIPTIVE EXPERIMENTS -----------------------------------------------

#   a) DescabsoluteOrderScenarioriptive Learning Responses --------------------------------------------
#   b) Descriptive Choice Responses --------------------------------------------

# 4) CHOICE RESPONSES (Both Experiments) ----------------------------------------------
#   a) Creation of dataset -----------------------------------------------------
#Common var list between experienced and descriptive experiment
# commonVarList = c("participantId","gender","publicTransportUse","eduSubject"
#                   ,"scenario","experimentType","experimentalCondition","experimentalBlock","  8reactionTime","choice")

commonVarList = c("participantId","gender","city","country"
                  ,"scenario","absoluteOrderScenario","experimentType","experimentalCondition"
                  ,"experimentalBlock","reactionTime","choice")

choiceResponses = rbind(experiencedChoiceResponsesDecisionStep[commonVarList],descriptiveChoiceResponses[commonVarList])

#Merge certainty level for the experienced experiment
choiceResponses = merge(choiceResponses,experiencedChoiceResponsesConfirmationStep[c("participantId","scenario","experimentType","certaintyLevel")], by = c("participantId","scenario","experimentType"),all.x = TRUE)

#Rational route chosen 
choiceResponses$rationalRouteChosen <- 0
choiceResponses$rationalRouteChosen[choiceResponses$choice == "rational"] <- 1

#Irrational route chosen 
choiceResponses$irrationalRouteChosen <- 0
choiceResponses$irrationalRouteChosen[choiceResponses$choice != "rational"] <- 1

# View(choiceResponses)
# descriptiveDCMDataset = experiencedChoiceResponsesDecisionStep[]

varListExperimentalScenarios = c("scenario","route","alternative"
                                 ,"averageWaitingTime","averageTravelTime","averageJourneyTime"
                                 ,"variabilityWaitingTime","variabilityTravelTime","variabilityJourneyTime"
                                 
                                 
)

# - Add participant Information -------------------------------------------

#Merge travel behaviour responses using the participant id 

variablesParticipantsToMerge <- c("participantId","waitingTimeImportance","travelTimeImportance","journeyTimeImportance"
                                  ,"waitingTimeReliabilityImportance","travelTimeReliabilityImportance","timeCounting")

#If I add ,"realWaitingTime","realTravelTime" then I cannot estimate the DCM model

choiceResponses <- merge(choiceResponses,participants[,variablesParticipantsToMerge],by = "participantId")

#   b) Imputation -------------------------------------------------------------

#     i) Experienced Experiment (Only for Chile) -----------------------------
#       - Replication of Choice from Descriptive Experiment in the same scenario----------------------------------------------------------------

# Imputation of the missing response of the experienced experiment using the descriptive choice in the same scenario. Note
# that this will increase the consistency between the responses of the experiment and I should account for that in the analyses
# In addition, it may change the results of the regresion models. 

# View(experiencedChoiceResponses)
# View(choiceResponses)

missingScenariosParticipants <- unique(experiencedChoiceResponses[c("participantId","missingExperiencedScenario")])
missingScenariosParticipants <- subset(missingScenariosParticipants,is.na(missingScenariosParticipants$missingExperiencedScenario)==FALSE)

choiceResponses <- merge(choiceResponses,missingScenariosParticipants,by =c("participantId"),all.x = TRUE)
choiceResponses$missingChoice <- 0 #Taking the value 1 if the choice was added in the dataset as a copy of the descriptive response.

#Descriptive rows of the choice responses dataset
temp <- subset(choiceResponses,experimentType == "descriptive")

for(nParticipant in 1:dim(missingScenariosParticipants)[1]){
  nMissingScenario <- missingScenariosParticipants[nParticipant,]$missingExperiencedScenario
  nParticipantId <- missingScenariosParticipants[nParticipant,]$participantId
  
  descriptiveChoiceImputation <- subset(temp,temp$scenario == nMissingScenario & temp$participantId == nParticipantId)
  descriptiveChoiceImputation$reactionTime <- NA
  descriptiveChoiceImputation$experimentType <- "experienced"
  descriptiveChoiceImputation$missingChoice <- 1
  
  choiceResponses <- rbind(choiceResponses,descriptiveChoiceImputation)
  
}

#The new row set the reaction in NA and will not have a value in the certainty level (because the descriptive response do not have this value)

#       - Certainty Level  ------------------------------------------------------
##I imputted the average certainty level of the participant in block 3 of the experienced experiment
temp <- subset(choiceResponses,experimentalBlock == "03" & experimentType == "experienced")
temp1 <-tapply(temp$certaintyLevel,temp$participantId,mean, na.rm = TRUE)
participantsMissingCertaintyLevel <- subset(temp,is.na(certaintyLevel) == TRUE)

for(nRow in 1:dim(participantsMissingCertaintyLevel)[1]){
  nParticipantId <- participantsMissingCertaintyLevel[nRow,]$participantId
  
  choiceResponses$certaintyLevel[with(choiceResponses,missingExperiencedScenario == scenario & participantId == nParticipantId & experimentType == "experienced")] <- round(as.numeric(temp1[nParticipantId]),0)
  # choiceResponses[row.names(subset(choiceResponses,is.na(certaintyLevel)==TRUE)),]$certaintyLevel[nRow] <- 
}





#       - Reaction Time ---------------------------------------------------------
##I imputted the average reaction time of the participant in block 3 of the experienced experiment
temp <- subset(choiceResponses,experimentalBlock == "03" & experimentType == "experienced")
temp1 <-tapply(temp$reactionTime,temp$participantId,mean, na.rm = TRUE)
participantsMissingReactionTime <- subset(temp,is.na(reactionTime) == TRUE)

for(nRow in 1:dim(participantsMissingReactionTime)[1]){
  nParticipantId <- participantsMissingReactionTime[nRow,]$participantId
  
  choiceResponses$reactionTime[with(choiceResponses,missingExperiencedScenario == scenario & participantId == nParticipantId & experimentType == "experienced")] <- round(as.numeric(temp1[nParticipantId]),0)
  # choiceResponses[row.names(subset(choiceResponses,is.na(reactionTime)==TRUE)),]$reactionTime[nRow] <- 
}






#     ii) Descriptive Experiment (Chile and UK) ----------------------------------------------
#       - Reaction Time (only in control condition)  ---------------------------------------------------------------------
#I imputted the average reaction time of the participants in the descriptive experiment
temp <- subset(choiceResponses,experimentType == "descriptive")
temp1 <-tapply(temp$reactionTime,temp$participantId,mean, na.rm = TRUE)
participantsMissingReactionTime <- subset(temp,is.na(reactionTime) == TRUE)
#Note that only in the control condition is not possible to calculate the reaction time of the first choice of the participant.


nRow <- 1
for(nRow in 1:dim(participantsMissingReactionTime)[1]){
  nParticipantId <- participantsMissingReactionTime[nRow,]$participantId
  nScenario <- participantsMissingReactionTime[nRow,]$scenario
  
  choiceResponses$reactionTime[with(choiceResponses,scenario == nScenario & participantId == nParticipantId & experimentType == "descriptive")] <- round(as.numeric(temp1[nParticipantId]),0)
  # choiceResponses[row.names(subset(choiceResponses,is.na(reactionTime)==TRUE)),]$reactionTime[nRow] <- 
}



#       - Add Time Attributes of Experimental Scenarios ---------------------------

#choiceResponses1 do not have the information of the time attributes of the alternatives
choiceResponses1 <- choiceResponses
choiceResponses <- merge(choiceResponses,experimentalScenarios[varListExperimentalScenarios],by.x = c("scenario","choice"),by.y=c("scenario","alternative"))

# 5) DISCRETE CHOICE MODELS -----------------------------------------------
#   - Creation of Dataset --------------------------------------------------

DCMLongDataset <- merge(choiceResponses1,experimentalScenarios[varListExperimentalScenarios],by=c("scenario"))
# DCMLongDataset <- choiceScenario
DCMLongDataset$choiceId <- NA
DCMLongDataset$choiceId[with(DCMLongDataset,choice==alternative)] <- TRUE
DCMLongDataset$choiceId[with(DCMLongDataset,choice!=alternative)] <- FALSE


#   - Dependent Measures ------------------------------------------------------


#Dependent variable coided with 0 and 1

#Alternative Chosen (1) and not chosen (0)
DCMLongDataset$alternativeChosen <- NA
DCMLongDataset$alternativeChosen[DCMLongDataset$choice == DCMLongDataset$alternative] <- 1
DCMLongDataset$alternativeChosen[DCMLongDataset$choice != DCMLongDataset$alternative] <- 0

#Create new dependent variable that adjust for certainty level
DCMLongDataset$alternativeChosenCertaintyLevel <- with(DCMLongDataset,alternativeChosen*certaintyLevel/100)
#   - New Independent Measures ------------------------------------------------

#Adjusted Waiting Time
DCMLongDataset$averageWaitingTimeAdj <- with(DCMLongDataset,averageWaitingTime-averageTravelTime*1/8)
DCMLongDataset$averageWaitingTimeAdj[DCMLongDataset$averageWaitingTimeAdj < 0] <- 0
DCMLongDataset$busArrivingTime <- with(DCMLongDataset,averageWaitingTime-averageWaitingTimeAdj)



#   - Subset  -----------------------------------------------------------------
# DCMLongDataset<- subset(DCMLongDataset,DCMLongDataset$scenario!="01" & DCMLongDataset$scenario != "02")
# DCMLongDataset<- subset(DCMLongDataset,(experimentalBlock == "02" & scenario != "07") | scenario == "11" | scenario == "14" )


#   - Deterministic Scenarios (Block1 and Block2) -----------------------------


# View(DCMLongDatasetDeterministicScenarios)

