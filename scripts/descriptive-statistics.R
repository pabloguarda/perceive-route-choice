# DESCRIPTIVE STATISTICS --------------------------------------------------------------



# 1) Plot themes ---------------------------------------------------------------

themeBarsPlots <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                        ,panel.background = element_rect(fill = 'white', colour = 'black')
                        
                        ,axis.text.x=element_text(color = "black", size=20, hjust = 0.5, vjust = 0.4) #Text for experimental conditions
                        # ,scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10))
                        ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                        # ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                        ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                        ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                        ,panel.margin = unit(0, units="mm")
                        # ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamaño de la imagen en que está contenido el gráfico
                        ,plot.margin = (unit(c(1, 1, 1, 1), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                        ,axis.ticks.margin=unit(1, units="mm")
                        ,axis.ticks.length=unit(3, units="mm")
                        ,axis.line = element_line(color = 'black')
                        ,legend.position="bottom"
                        ,legend.title=element_blank()
                        ,legend.key = element_rect(colour="black")
                        ,legend.key.width=unit(0.6,"cm")
                        ,legend.text=element_text(size=18)
                        # ,legend.margin=unit(0, units="cm")
                        # ,legend.key = element_rect(size = 5),legend.key.width=unit(1,"cm"),legend.key.size=unit(1,"cm")
                        ,legend.background = element_rect(colour = 'black',size = 0.3, fill = "white")
) 


# 2) Choice Proportions -------------------------------------------------------------

# - Tutorials --------------------------------------------------------------


#Pivot tables
#http://marcoghislanzoni.com/blog/2014/09/01/pivot-tables-r-dplyr/


# - Calculations ------------------------------------------------------------

choiceProportions <- DCMLongDataset

choiceProportions$choice[choiceProportions$choice=="rational"] <- 1
choiceProportions$choice[choiceProportions$choice=="irrational"] <- 0

choiceProportions$choice <- as.numeric(choiceProportions$choice)

choiceProportionsByWSBS <- choiceProportions %>%
  # na.omit() %>%
  group_by(experimentalCondition,experimentType,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsByWS <- choiceProportions %>%
  # na.omit() %>%
  group_by(experimentType,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsByBSExperienced <- subset(choiceProportions,experimentType == "experienced") %>%
  # na.omit() %>%
  group_by(experimentalCondition,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsByBSDescriptive <- subset(choiceProportions,experimentType == "descriptive") %>%
  # na.omit() %>%
  group_by(experimentalCondition,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))


# - Plots -----------------------------------------------------------------

# choiceProportionsByBSPlot <- subset(choiceProportionsByBS,experimentalCondition == "control")

#a) Two B-S s in experienced experiment

choiceProportionsByBSExperiencedPlot <- choiceProportionsByBSExperienced

choiceProportionsByBSExperiencedPlot$xIrrational <- round(100-choiceProportionsByBSExperiencedPlot$xRational,1)
choiceProportionsByBSExperiencedPlot <- melt(choiceProportionsByBSExperiencedPlot
                                           , vars = c("xRational","xIrrational")
                                           , variable.name = "choiceType", value.name = "xChoices")

# choiceProportionsByBSExperiencedPlot$scenarioType <- NA
# 
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("01","02")] <- "Type 1"
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("03","04")] <- "Type 2"
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("05","06","07","08")] <- "Type 3"
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("09","12")] <- "Type 4"
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("10","13")] <- "Type 5"
# choiceProportionsByBSExperiencedPlot$scenarioType[choiceProportionsByBSExperiencedPlot$scenario %in% c("11","14")] <- "Type 6"

choiceProportionsByBSExperiencedPlot$scenario <- factor(choiceProportionsByBSExperiencedPlot$scenario
                                                      ,levels = sort(unique(choiceProportionsByBSExperiencedPlot$scenario),decreasing = TRUE)
                                                      ,labels = sort(unique(choiceProportionsByBSExperiencedPlot$scenario),decreasing = TRUE)
                                                      )

ggplot(choiceProportionsByBSExperiencedPlot,
        aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
        scale_y_continuous() +
        geom_bar(stat='identity',colour="black") +
        geom_text(size = 3, position = position_stack(vjust = 0.5))+
        facet_wrap(~experimentalCondition,ncol=2)+
        geom_hline(yintercept=50, linetype = 'dashed') +
        coord_flip() +
        theme_bw() +
        ylab('Rational/Irrational choices') +
        xlab('Scenario') +
        scale_fill_manual(values=c("gray", "white")) +
        theme(legend.position='none') +
        ggtitle('Choice Proportion in ')

#b) Two B-S s in descriptive experiment

choiceProportionsByBSDescriptivePlot <- choiceProportionsByBSDescriptive

choiceProportionsByBSDescriptivePlot$xIrrational <- round(100-choiceProportionsByBSDescriptivePlot$xRational,1)
choiceProportionsByBSDescriptivePlot <- melt(choiceProportionsByBSDescriptivePlot
                                             , vars = c("xRational","xIrrational")
                                             , variable.name = "choiceType", value.name = "xChoices")

# choiceProportionsByBSDescriptivePlot$scenarioType <- NA
# 
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("01","02")] <- "Type 1"
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("03","04")] <- "Type 2"
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("05","06","07","08")] <- "Type 3"
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("09","12")] <- "Type 4"
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("10","13")] <- "Type 5"
# choiceProportionsByBSDescriptivePlot$scenarioType[choiceProportionsByBSDescriptivePlot$scenario %in% c("11","14")] <- "Type 6"

choiceProportionsByBSDescriptivePlot$scenario <- factor(choiceProportionsByBSDescriptivePlot$scenario
                                                        ,levels = sort(unique(choiceProportionsByBSDescriptivePlot$scenario),decreasing = TRUE)
                                                        ,labels = sort(unique(choiceProportionsByBSDescriptivePlot$scenario),decreasing = TRUE)
)

ggplot(choiceProportionsByBSDescriptivePlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentalCondition,ncol=2)+
  geom_hline(yintercept=50, linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Rational/Irrational choices') +
  xlab('Scenario') +
  scale_fill_manual(values=c("gray", "white")) +
  theme(legend.position='none') +
  ggtitle('Choice Proportion in ')

#c) Two W-S s in descriptive experiment

choiceProportionsByWSPlot <- choiceProportionsByWS

choiceProportionsByWSPlot$xIrrational <- round(100-choiceProportionsByWSPlot$xRational,1)
choiceProportionsByWSPlot <- melt(choiceProportionsByWSPlot
                                           , vars = c("xRational","xIrrational")
                                           , variable.name = "choiceType", value.name = "xChoices")

# choiceProportionsByWSPlot$scenarioType <- NA
# 
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("01","02")] <- "Type 1"
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("03","04")] <- "Type 2"
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("05","06","07","08")] <- "Type 3"
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("09","12")] <- "Type 4"
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("10","13")] <- "Type 5"
# choiceProportionsByWSPlot$scenarioType[choiceProportionsByWSPlot$scenario %in% c("11","14")] <- "Type 6"

choiceProportionsByWSPlot$scenario <- factor(choiceProportionsByWSPlot$scenario
                                                      ,levels = sort(unique(choiceProportionsByWSPlot$scenario),decreasing = TRUE)
                                                      ,labels = sort(unique(choiceProportionsByWSPlot$scenario),decreasing = TRUE)
)

ggplot(choiceProportionsByWSPlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentType,ncol=2)+
  geom_hline(yintercept=50, linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Rational/Irrational choices') +
  xlab('Scenario') +
  scale_fill_manual(values=c("gray", "white")) +
  theme(legend.position='none') +
  ggtitle('Choice Proportion in ')


#d) Four experimentalCondition conditions

choiceProportionsByWSBSPlot <- choiceProportionsByWSBS

choiceProportionsByWSBSPlot$xIrrational <- round(100-choiceProportionsByWSBSPlot$xRational,1)
choiceProportionsByWSBSPlot <- melt(choiceProportionsByWSBSPlot
                                           , vars = c("xRational","xIrrational")
                                           , variable.name = "choiceType", value.name = "xChoices")

choiceProportionsByWSBSPlot$scenarioType <- NA

choiceProportionsByWSBSPlot$scenario <- factor(choiceProportionsByWSBSPlot$scenario
                                                      ,levels = sort(unique(choiceProportionsByWSBSPlot$scenario),decreasing = TRUE)
                                                      ,labels = sort(unique(choiceProportionsByWSBSPlot$scenario),decreasing = TRUE)
)

 
ggplot(choiceProportionsByWSBSPlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentType*experimentalCondition,ncol=4)+
  geom_hline(yintercept=50, linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Rational/Irrational choices') +
  xlab('Scenario') +
  scale_fill_manual(values=c("gray", "white")) +
  theme(legend.position='none') +
  ggtitle('Choice Proportion in ')





# 3) Participants Information ---------------------------------------------


#   i) Comparison between Chilean and London’s participants -----------------
#     - Experiment duration  ----------------------------------------------------
tapply(participants$sessionDuration,participants$country,mean)

summarise(group_by(participants, country), meanSessionDuration=mean(sessionDuration), maxSessionDuration=max(sessionDuration))





#   ii) Comparison between Experimental Conditions  -----------------------------

#     - Time Attributes Experiment ---------------------------------------------------------

#Whether the participant counted or not
tapply(participants$timeCounting,participants$experimentalCondition,mean,na.rm = TRUE)

#Experienced Walking Time
tapply(participants$experiencedWalkingTime,participants$experimentalCondition,mean,na.rm = TRUE)
#Experienced Waiting Time
tapply(participants$experiencedWaitingTime,participants$experimentalCondition,mean,na.rm = TRUE)
#Experienced Travel Time
tapply(participants$experiencedTravelTime,participants$experimentalCondition,mean,na.rm = TRUE)

summarise(group_by(participants, experimentalCondition)
          , xTimeCountingParticipants = 100*mean(timeCounting,na.rm = TRUE)
          , meanExperiencedWalkingTime=mean(experiencedWalkingTime,na.rm = TRUE)
          , meanExperiencedWaitingTime=mean(experiencedWaitingTime,na.rm = TRUE)
          , meanExperiencedTravelTime=mean(experiencedTravelTime,na.rm = TRUE)
          )





#     - Time Atttributes in Commuting ---------------------------------------------------------
#Walking
tapply(participants$timeCounting,participants$experimentalCondition,mean,na.rm = TRUE)
#Waiting
tapply(participants$realWaitingTime,participants$experimentalCondition,mean,na.rm = TRUE)
#Travel
tapply(participants$realTravelTime,participants$experimentalCondition,mean,na.rm = TRUE)
#Journey Time
tapply(participants$realJourneyTime,participants$experimentalCondition,mean,na.rm = TRUE)
#All together 

summarise(group_by(participants, experimentalCondition), meanTimeCounting=mean(timeCounting,na.rm = TRUE)
          ,  meanRealWaitingTime=mean(realWaitingTime,na.rm = TRUE), meanRealTravelTime=mean(realTravelTime,na.rm = TRUE)
          ,  meanRealJourneyTime=mean(realJourneyTime,na.rm = TRUE))


#     - Time Valuation ----------------------------------------------------------
#Waiting Time Average
tapply(participants$waitingTimeImportance,participants$experimentalCondition,mean,na.rm = TRUE)
#Waiting Time Variability
tapply(participants$waitingTimeReliabilityImportance,participants$experimentalCondition,mean,na.rm = TRUE)
#Travel Time Average
tapply(participants$travelTimeImportance,participants$experimentalCondition,mean,na.rm = TRUE)
#Travel Time Variability
tapply(participants$travelTimeReliabilityImportance,participants$experimentalCondition,mean,na.rm = TRUE)
#Journey Time
tapply(participants$journeyTimeImportance,participants$experimentalCondition,mean,na.rm = TRUE)

temp <- summarise(group_by(participants, experimentalCondition)
          , meanWaitingTimeImportance=mean(waitingTimeImportance,na.rm = TRUE)
          , meanWaitingTimeReliabilityImportance=mean(waitingTimeReliabilityImportance,na.rm = TRUE)
          , meanTravelTimeImportance=mean(travelTimeImportance,na.rm = TRUE)
          , meanTravelTimeReliabilityImportance=mean(travelTimeReliabilityImportance,na.rm = TRUE)
          , meanJourneyTimeImportance=mean(journeyTimeImportance,na.rm = TRUE))

# View(temp)

temp <- participants %>%
  na.omit() %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(mean,var),waitingTimeImportance,waitingTimeReliabilityImportance,travelTimeImportance
          ,  travelTimeReliabilityImportance,journeyTimeImportance)

# View(temp)

#   iii) Other analyses for London -----------------------------------------------------
#     - Use of real-time information -----------------------------------------------------------------------
#     - Experiment Identification ---------------------------------------------
mean(participants$experiencedExperimentIdentification,na.rm=TRUE)
mean(participants$descriptiveExperimentIdentification,na.rm=TRUE)

#   iv) Analysis for Both UK and Chilean Participants -----------------------
#     - Age ---------------------------------------------------------------------
round(mean(participants$age),2)
#     - Gender ----------------------------------------------------------------
round(table(participants$gender)/dim(participants)[1],2)

#     - Education -------------------------------------------------------------

round(table(participants$eduLevel)/dim(participants)[1],2)







#     - Experiment Duration ---------------------------------------------------
round(mean(participants$sessionDuration),2)
round(sd(participants$sessionDuration),2)


#   v) Contingency tables ------------------------------------------------------
#     - City and Gender -------------------------------------------------------

temp <- aggregate(participants$participantId,
                    by = list(gender = participants$gender, city = participants$city),
                    FUN = function(x) c(n = length(x)))



# 4) CERTAINTY LEVEL ------------------------------------------------------
#   a) By Scenario -----------------------------------------------------------

#     ii) Table -----------------------------------------------------------------
temp <- choiceResponses %>% 
  group_by(experimentalCondition,scenario) %>%
  summarise_at(vars(certaintyLevel),funs(mean(.,na.rm = TRUE), sd(.,na.rm = TRUE)))

# View(temp)

#   b) By Block -------------------------------------------------------------
#     i) Table -----------------------------------------------------------------
temp <- choiceResponses %>%
  group_by(experimentalCondition,experimentalBlock) %>%
  summarise_at(vars(certaintyLevel),funs(mean)) 

# View(temp)



# 5) RESPONSE TIMES -------------------------------------------------------
#   a) Comparison By Scenario ----------------------------------------------------------
#     - i) tables -----------------------------------------------------------------

temp <- choiceResponses %>%
  group_by(experimentalCondition,scenario) %>%
  summarise_each(funs(mean(.,na.rm = TRUE), mean(.,na.rm = TRUE)),reactionTime)

#   b) Comparison By Block ----------------------------------------------------------
#     - i) tables -----------------------------------------------------------------

temp <- choiceResponses %>%
  group_by(experimentalCondition,experimentalBlock) %>%
  summarise_each(funs(mean(.,na.rm = TRUE), mean(.,na.rm = TRUE)),reactionTime)

#   b) Experienced Experiment ----------------------------------------------------------
#     - Average Decision and Confirmation Steps ---------------------------------

# Both Conditions
round(tapply(experiencedChoiceResponses$durationExperimentalStep,experiencedChoiceResponses$scenario,mean, na.rm = TRUE),1)

# Control Condition
round(tapply(subset(experiencedChoiceResponses,experimentalCondition == "control")$durationExperimentalStep,subset(experiencedChoiceResponses,experimentalCondition == "control")$scenario,mean, na.rm = TRUE),1)

# Treatment Condition
round(tapply(subset(experiencedChoiceResponses,experimentalCondition == "treatment")$durationExperimentalStep,subset(experiencedChoiceResponses,experimentalCondition == "treatment")$scenario,mean, na.rm = TRUE),1)

#     - Decision Step -------------------------------------------------------

# Both Conditions
round(tapply(experiencedChoiceResponsesDecisionStep$durationExperimentalStep,experiencedChoiceResponsesDecisionStep$scenario,mean, na.rm = TRUE),1)

# Control Condition
round(tapply(subset(experiencedChoiceResponsesDecisionStep,experimentalCondition == "control")$durationExperimentalStep,subset(experiencedChoiceResponsesDecisionStep,experimentalCondition == "control")$scenario,mean, na.rm = TRUE),1)

# Treatment Condition
round(tapply(subset(experiencedChoiceResponsesDecisionStep,experimentalCondition == "treatment")$durationExperimentalStep,subset(experiencedChoiceResponsesDecisionStep,experimentalCondition == "treatment")$scenario,mean, na.rm = TRUE),1)


#     - Confirmation Step -------------------------------------------------------

# Both Conditions
round(tapply(experiencedChoiceResponsesConfirmationStep$durationExperimentalStep,experiencedChoiceResponsesConfirmationStep$scenario,mean, na.rm = TRUE),1)

#Control Condition
round(tapply(subset(experiencedChoiceResponsesConfirmationStep,experimentalCondition == "control")$durationExperimentalStep,subset(experiencedChoiceResponsesConfirmationStep,experimentalCondition == "control")$scenario,mean, na.rm = TRUE),1)

#Treatment Condition
round(tapply(subset(experiencedChoiceResponsesConfirmationStep,experimentalCondition == "treatment")$durationExperimentalStep,subset(experiencedChoiceResponsesConfirmationStep,experimentalCondition == "treatment")$scenario,mean, na.rm = TRUE),1)






#   c) Descriptive Experiment ----------------------------------------------------------
#     - Average Both Experimental Conditions -------------------------------------------------------
round(tapply(descriptiveChoiceResponses$scenarioDuration,descriptiveChoiceResponses$scenario,mean, na.rm = TRUE),1)
#     - Control Condition -------------------------------------------------------

round(tapply(subset(descriptiveChoiceResponses,experimentalCondition == "control")$scenarioDuration,subset(descriptiveChoiceResponses,experimentalCondition == "control")$scenario,mean, na.rm = TRUE),1)

#     - Treatment Condition -----------------------------------------------------
round(tapply(subset(descriptiveChoiceResponses,experimentalCondition == "treatment")$scenarioDuration,subset(descriptiveChoiceResponses,experimentalCondition == "treatment")$scenario,mean, na.rm = TRUE),1)


#   c) Comparison Reaction Times Across Experimental Conditions and  --------



# 6) CHOICE CONSISTENCY ---------------------------------------------------


#   a) Create Dataset ----------------------------------------------------------
temp1 <- experiencedChoiceResponsesDecisionStep[c("participantId","scenario","choice","experimentalBlock")]
temp1 <- plyr::rename(temp1,c(choice="experiencedChoice"))

temp2 <-descriptiveChoiceResponses[c("participantId","scenario","choice","experimentalBlock")]
temp2 <-plyr::rename(temp2,c(choice="descriptiveChoice"))

choicePerScenarioParticipants <- merge(temp2,temp1,by=c("participantId","scenario","experimentalBlock"),all.x = TRUE)

choicePerScenarioParticipants$consistentChoice <- 0
choicePerScenarioParticipants$consistentChoice[is.na(choicePerScenarioParticipants$experiencedChoice)] <- NA
choicePerScenarioParticipants$consistentChoice[with(choicePerScenarioParticipants,descriptiveChoice==experiencedChoice)] <- 1 

mean(choicePerScenarioParticipants$consistentChoice)

#Adding Infromation from Participant Dataset
choicePerScenarioParticipants <- merge(choicePerScenarioParticipants,participants[c("participantId","experimentalCondition","city","timeCounting")])

choicePerScenarioParticipantsConsistency <- subset(choicePerScenarioParticipants,is.na(consistentChoice) == FALSE)

#Create a datasets where each 


#   b) Comparison By Scenario ----------------------------------------------------
#     i) Table -----------------------------------------------------------------
tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$scenario,mean)

#   - Analysis by Experimental Condition --------------------------------------

tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$experimentalCondition,mean)

#   - Analysis by City -----------------------------------------------------

tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$city,mean)

#   - Analysis by Participant -------------------------------------------------

temp <- data.frame(participantId= row.names(tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$participantId,mean))
                   ,participantConsistency=as.numeric(tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$participantId,mean)))


participantAnalysis <- data.frame(participantId = participants$participantId)
participantAnalysis <-merge(participantAnalysis,temp,by="participantId")

#Distribution


 



# View(participantAnalysis)

# - Analysis by Block -----------------------------------------------------

tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$experimentalBlock,mean)

# - Analysis by Scenario -----------------------------------------------------

tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$scenario,mean)
tapply(subset(choicePerScenarioParticipantsConsistency,city=="London")$consistent,subset(choicePerScenarioParticipantsConsistency,city=="London")$scenario,mean)
tapply(subset(choicePerScenarioParticipantsConsistency,city=="Santiago")$consistent,subset(choicePerScenarioParticipantsConsistency,city=="Santiago")$scenario,mean)

tapply(subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$consistent,subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$scenario,mean)


tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$scenario,mean)

tapply(subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$consistent,subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$experimentalBlock,mean)
tapply(subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="treatment")$consistent,subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="treatment")$experimentalBlock,mean)

tapply(subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$consistent,subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="control")$scenario,mean)
tapply(subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="treatment")$consistent,subset(choicePerScenarioParticipantsConsistency,experimentalCondition=="treatment")$scenario,mean)

#   - Effect of Time Counting -------------------------------------------------

# View(data.frame(tapply(choicePerScenarioParticipantsConsistency$consistent,choicePerScenarioParticipantsConsistency$participantId,mean)
# ,tapply(choicePerScenarioParticipantsConsistency$timeCounting,choicePerScenarioParticipantsConsistency$participantId,mean)))

participantAnalysis <- merge(participantAnalysis,participants[c("participantId","timeCounting")],by="participantId")

tapply(participantAnalysis$participantConsistency,participantAnalysis$timeCounting,mean)








# 7) MANIPULATION CHECKS --------------------------------------------------

#   a) Analysis by participant ----------------------------------------------
#   - Experienced Experiment ------------------------------------------------


manipulationCheckParticipants <- choicePerScenarioParticipantsConsistency

manipulationCheckParticipantsS1Rational <- subset(manipulationCheckParticipants,scenario == "01" & experiencedChoice == "rational")
manipulationCheckParticipantsS2Rational <- subset(manipulationCheckParticipants,scenario == "02" & experiencedChoice == "rational")

manipulationCheckParticipantsS1 <- subset(manipulationCheckParticipants,scenario == "01")[,c("participantId","experiencedChoice")]
colnames(manipulationCheckParticipantsS1) <- c("participantId","EC1")

manipulationCheckParticipantsS2 <- subset(manipulationCheckParticipants,scenario == "02")[,c("participantId","experiencedChoice")]
colnames(manipulationCheckParticipantsS2) <- c("participantId","EC2")

manipulationChecksParticipantsS1S2 <- merge(manipulationCheckParticipantsS1,manipulationCheckParticipantsS2,by="participantId")

manipulationChecksParticipantsS1S2Rational <- subset(manipulationChecksParticipantsS1S2, EC1 == "rational" & EC2 == "rational")

manipulationChecksParticipantsS1S2Rational <- merge(manipulationChecksParticipantsS1S2Rational,unique(manipulationCheckParticipants[,c("participantId","experimentalCondition")]),by ="participantId")


manipulationChecksParticipantsS1Rational <- subset(manipulationChecksParticipantsS1S2, EC1 == "rational")
manipulationChecksParticipantsS2Rational <- subset(manipulationChecksParticipantsS1S2, EC2 == "rational")

table(manipulationChecksParticipantsS1S2Rational$experimentalCondition)

#List of participants that passed the manipulation check

listRationalParticipantsExperienced <-  str_sub(manipulationChecksParticipantsS1S2Rational$participantId,2,3)
listRationalParticipantsS1Experienced <-  str_sub(manipulationChecksParticipantsS1Rational$participantId,2,3)
listRationalParticipantsS2Experienced <-  str_sub(manipulationChecksParticipantsS2Rational$participantId,2,3)

# listRationalParticipantsExperienced <- listRationalParticipants
# listRationalParticipantsS1Experienced <- listRationalParticipantsS1
# listRationalParticipantsS2Experienced <- listRationalParticipantsS2


# #Only participants who pass the test on time perception 
# 
# temp <- str_c("P",listRationalParticipants)
# 
# choicePerScenarioParticipantsConsistencyRational <- subset(choicePerScenarioParticipantsConsistency,participantId %in% temp)
# 
# tapply(choicePerScenarioParticipantsConsistencyRational$consistent,choicePerScenarioParticipantsConsistencyRational$scenario,mean)

#   - Descriptive Experiment ------------------------------------------------

manipulationCheckParticipants <- choicePerScenarioParticipantsConsistency

manipulationCheckParticipantsS1RationalDescriptive <- subset(manipulationCheckParticipants,scenario == "01" & descriptiveChoice == "rational")
manipulationCheckParticipantsS2RationalDescriptive <- subset(manipulationCheckParticipants,scenario == "02" & descriptiveChoice == "rational")

manipulationCheckParticipantsS1Descriptive <- subset(manipulationCheckParticipants,scenario == "01")[,c("participantId","descriptiveChoice")]
colnames(manipulationCheckParticipantsS1Descriptive) <- c("participantId","DC1")

manipulationCheckParticipantsS2Descriptive <- subset(manipulationCheckParticipants,scenario == "02")[,c("participantId","descriptiveChoice")]
colnames(manipulationCheckParticipantsS2Descriptive) <- c("participantId","DC2")

manipulationChecksParticipantsS1S2Descriptive <- merge(manipulationCheckParticipantsS1Descriptive,manipulationCheckParticipantsS2Descriptive,by="participantId")

manipulationChecksParticipantsS1S2RationalDescriptive <- subset(manipulationChecksParticipantsS1S2Descriptive, DC1 == "rational" & DC2 == "rational")

manipulationChecksParticipantsS1S2RationalDescriptive <- merge(manipulationChecksParticipantsS1S2Rational,unique(manipulationCheckParticipants[,c("participantId","experimentalCondition")]),by ="participantId")

manipulationChecksParticipantsS1RationalDescriptive <- subset(manipulationChecksParticipantsS1S2Descriptive, DC1 == "rational")
manipulationChecksParticipantsS2RationalDescriptive <- subset(manipulationChecksParticipantsS1S2Descriptive, DC2 == "rational")


listRationalParticipantsDescriptive <-  str_sub(manipulationChecksParticipantsS1S2RationalDescriptive$participantId,2,3)
listRationalParticipantsS1Descriptive <-  str_sub(manipulationChecksParticipantsS1RationalDescriptive$participantId,2,3)
listRationalParticipantsS2Descriptive <-  str_sub(manipulationChecksParticipantsS2RationalDescriptive$participantId,2,3)


# 7) RISK ATTITUDES -------------------------------------------------------

"To examine whether individuals had similar risk preferences across outcome
types and frames we collapsed across choices within frame and outcome type creating 
a variable indicating a participant’s average rate of selecting the riskier option"

# 8) EXPERIMENT DEBRIEF QUESTIONS (London only) -----------------------------------------
#   a) Level of Identification ----------------------------------------------
#     - Data formatting -------------------------------------------------------

participantsExperimentIdentification <- subset(participants,is.na(descriptiveExperimentIdentification) ==FALSE)
# participantsTimeValuation <- participantsTimeValuation[c("participantId","waitingTimeImportance","travelTimeImportance","journeyTimeImportance","experimentalCondition")]

#     - Averages --------------------------------------------------------------

averageExperimentIdentificationByCondition <- participantsExperimentIdentification %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)),experiencedExperimentIdentification,descriptiveExperimentIdentification)

averageExperimentIdentificationByCondition 

averageExperimentIdentification <- participantsExperimentIdentification %>%
  summarise_each(funs(mean(.,na.rm = TRUE)),experiencedExperimentIdentification,descriptiveExperimentIdentification)

averageExperimentIdentification 

sum(with(participantsExperimentIdentification,experiencedExperimentIdentification+descriptiveExperimentIdentification),na.rm = TRUE)/72

sum(with(subset(participantsExperimentIdentification,experimentalCondition == "control"),experiencedExperimentIdentification+descriptiveExperimentIdentification),na.rm = TRUE)/36
sum(with(subset(participantsExperimentIdentification,experimentalCondition == "treatment"),experiencedExperimentIdentification+descriptiveExperimentIdentification),na.rm = TRUE)/36


#     - Discrete  ---------------------------------------------------------------

participantsExperimentIdentification$preferenceED <- ifelse(with(participantsExperimentIdentification,experiencedExperimentIdentification>descriptiveExperimentIdentification),"e","d")
participantsExperimentIdentification$preferenceE <- ifelse(participantsExperimentIdentification$preferenceED == "e",1,0)
participantsExperimentIdentification$preferenceD <- ifelse(participantsExperimentIdentification$preferenceED == "d",1,0)


experimentIdentificationByCondition <- participantsExperimentIdentification %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(sum(.,na.rm = TRUE)),preferenceE,preferenceD)

experimentIdentificationByCondition

# table(participantsTimeValuation$preferenceWT)


# b) Stated valuation of time attributes ----------------------------------

#     - Data formatting -------------------------------------------------------

participantsTimeValuation <- subset(participants,is.na(waitingTimeImportance) ==FALSE)
participantsTimeValuation <- participantsTimeValuation[c("participantId","waitingTimeImportance","travelTimeImportance","journeyTimeImportance","experimentalCondition")]


#     - Averages --------------------------------------------------------------
  
averageTimeValuationByCondition <- participantsTimeValuation %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)),waitingTimeImportance,travelTimeImportance,journeyTimeImportance)

averageTimeValuationByCondition 

averageTimeValuation <- participantsTimeValuation %>%
  summarise_each(funs(mean(.,na.rm = TRUE)),waitingTimeImportance,travelTimeImportance)

averageTimeValuation 

sum(with(participants,waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/72

sum(with(subset(participantsTimeValuation,experimentalCondition == "control"),waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/36
sum(with(subset(participantsTimeValuation,experimentalCondition == "treatment"),waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/36
#     - Discrete  ---------------------------------------------------------------

participantsTimeValuation$preferenceWT <- ifelse(with(participantsTimeValuation,waitingTimeImportance>travelTimeImportance),"w","t")
participantsTimeValuation$preferenceW <- ifelse(participantsTimeValuation$preferenceWT == "w",1,0)
participantsTimeValuation$preferenceT <- ifelse(participantsTimeValuation$preferenceWT == "t",1,0)


timePreferencesByCondition <- participantsTimeValuation %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(sum(.,na.rm = TRUE)),preferenceW,preferenceT)

timePreferencesByCondition

table(participantsTimeValuation$preferenceWT)





# 9) EXTRA LEARNING QUERIES -----------------------------------------------


# Average Extra Learning by Decision Scenario ----------------------------

extraLearningTrials <- subset(experiencedLearningResponses,experimentalStep=="extra-learning")
extraLearningTrials$ones <- 1

learningTrials <- subset(experiencedLearningResponses,experimentalStep=="learning")
learningTrials$ones <- 1

tapply(extraLearningTrials$ones,extraLearningTrials$scenario,sum)
tapply(learningTrials$ones,learningTrials$scenario,sum)
