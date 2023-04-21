
# # SETUP ----------------------------------------------------------------
# 
# closeAllConnections() #Remove all the data
# rm(list=ls()) #remove all variables

# P1)  Choice proportions (S01-S08) and manipulation checks (S01 & S02) --------------------------------------------------------------------
#This is only made to show that the animated experiment works 

#   a) Chi squared calculations ------------------------------------------------

#     + Irrational Lines -----------------------------------------------------
#       - Functions -------------------------------------------------------------

chiSquaredDistributionBinaryProportion <- function(N,digitsP,digitsChi, digitsProportions){
  
  #Plot X2 for n =72
  expectedRouteChoices <- c(0.5,0.5)
  chiSquaredValuesN <- data.frame(n=NA,p=NA,chi = NA, choiceProportion1 = NA,choiceProportion2 = NA, n1 = NA, n2 = NA)
  for(n in 0:(N)){
    test <- chisq.test(x = c(n,N-n),p = expectedRouteChoices)
    temp <- data.frame(n=n,p=as.numeric(test$p.value),chi = as.numeric(test$statistic)
                       ,choiceProportion1 = round(100*n/N,digitsProportions),choiceProportion2 = round(100*(N-n)/N,digitsProportions)
                       ,n1 = n, n2=N-n
    )
    chiSquaredValuesN <- rbind(chiSquaredValuesN,temp)
    
  }
  chiSquaredValuesN <- chiSquaredValuesN[2:(dim(chiSquaredValuesN)[1]),]
  chiSquaredValuesN$p <- round(chiSquaredValuesN$p,digitsP)
  chiSquaredValuesN$chi <- round(chiSquaredValuesN$chi,digitsChi)
  
  return(chiSquaredValuesN)
  
}
#       - 36 participants  --------------------------------------------------------


expectedRouteChoices <- c(0.5,0.5)

#To calculate the lines if the comparisons of choice proportion are between Chilean or London's participants.

#Here the test reject at the 99% Level of Confidence (26,10) -> Proportion 72 and 28
chisq.test(x = c(26,10),
           p = expectedRouteChoices)
c(26,10)[1]/sum(c(26,10))

#Here the test reject at the 97.5% Level of Confidence (25,11)  -> Proportion 70 and 30
chisq.test(x = c(25,11),
           p = expectedRouteChoices)
c(25,11)[1]/sum(c(25,11))

#Here the test reject at the 95% Level of Confidence (24,12) -> Proportion 67 and 33
chisq.test(x = c(24,12),
           p = expectedRouteChoices)
c(24,12)[1]/sum(c(24,12))

#Here the test reject at the 90% Level of Confidence (23,13) -> Proportion 64 and 36
chisq.test(x = c(23,13),
           p = expectedRouteChoices)
c(23,13)[1]/sum(c(23,13))

#Plot Chi2 for n =36
chiSquaredValuesN36 <- chiSquaredDistributionBinaryProportion(N=36,digitsP=3,digitsChi= 2,digitsProportions = 2)
# View(chiSquaredValuesN36)

#Here the test reject at the 95% Level of Confidence (24,12) -> Proportion 67 and 33
chisq.test(x = c(24,12),
           p = expectedRouteChoices)

#Percentage units
UBChi2N36 <- 100*c(24,12)[1]/sum(c(24,12))
LBChi2N36 <- 100-UBChi2N36

# Using 2.5% of confidence

chisq.test(x = c(25,11),
           p = expectedRouteChoices)

c(25,11)[1]/sum(c(25,11))

# Using 1.25% of confidence
chisq.test(x = c(26,10),
           p = expectedRouteChoices)

c(26,10)[1]/sum(c(26,10))


#       - 72 participants  --------------------------------------------------------

expectedRouteChoices <- c(0.5,0.5)

chisq.test(x = c(48,24),
           p = expectedRouteChoices)

#Here the test reject at the 99% Level of Confidence (47,25) -> Proportion 66 and 34
chisq.test(x = c(47,25),
           p = expectedRouteChoices)

c(47,25)[1]/sum(c(47,25))

#Here the test reject at the 97.5% Level of Confidence (46,26)  -> Proportion 64 and 36
chisq.test(x = c(46,26),
           p = expectedRouteChoices)

c(46,26)[1]/sum(c(46,26))

#Here the test reject at the 95% Level of Confidence (45,27) -> Proportion 62.5 and 37.5
chisq.test(x = c(45,27),
           p = expectedRouteChoices)

c(45,27)[1]/sum(c(45,27))

chisq.test(x = c(44,28),
           p = expectedRouteChoices)

#Here the test reject at the 90% Level of Confidence (43,29) -> Proportion 60 and 40
chisq.test(x = c(43,29),
           p = expectedRouteChoices)
c(43,29)[1]/sum(c(43,29))

#Plot Chi2 for n =72
chiSquaredValuesN72 <- chiSquaredDistributionBinaryProportion(N=72,digitsP=3,digitsChi= 2, digitsProportions = 2)
# View(chiSquaredValuesN72)



#   b) Plots choice proportions (Paper-plot) ------------------------------------------------

#     - Plot themes and formatting ---------------------------------------------------------------

themeChoiceProportionsPlotsPaper1 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                        ,panel.background = element_rect(fill = 'white', colour = 'black')
                        
                        ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4) #Text for experimental conditions
                        # ,scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10))
                        ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                        ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                        ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                        ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                        ,panel.margin = unit(0, units="mm")
                        # ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamaño de la imagen en que está contenido el gráfico
                        ,plot.margin = (unit(c(0.3, 0.3, 0.3, 0.3), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                        ,axis.ticks.margin=unit(1, units="mm")
                        ,axis.ticks.length=unit(3, units="mm")
                        ,axis.line = element_line(color = 'black')
                        ,legend.position="bottom"
                        ,legend.title=element_blank()
                        ,legend.key = element_rect(colour="black")
                        ,legend.key.width=unit(0.6,"cm")
                        ,legend.text=element_text(size=18)
                        ,legend.spacing.x = unit(0.1, 'cm')
                        # ,legend.margin=unit(2, units="cm")
                        # ,legend.key = element_rect(size = 5),legend.key.width=unit(1,"cm"),legend.key.size=unit(1,"cm")
                        ,legend.background = element_rect(colour = 'black',size = 0.3, fill = "white")
                        , strip.text = element_text(size=25) #Size of columns' titles
                        ,strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles
                        # ,legend.margin=unit(0, units="cm")
                        # ,legend.key = element_rect(size = 5),legend.key.width=unit(1,"cm"),legend.key.size=unit(1,"cm")
) 


widthChoiceProportionsPlots <- 20 #To consider space for legend
heightChoiceProportionsPlots <- 20 #Originally 20
dpiChoiceProportionsPlots <- 300

#     - Animated Control & Treatment --------------------------------------------

choiceProportionsAnimated <- subset(DCMLongDataset,experimentType == "experienced" & scenario %in% str_c("0",seq(1,8)))

choiceProportionsAnimated$choice[choiceProportionsAnimated$choice=="rational"] <- 1
choiceProportionsAnimated$choice[choiceProportionsAnimated$choice=="irrational"] <- 0

choiceProportionsAnimated$choice <- as.numeric(choiceProportionsAnimated$choice)

#Boundary lines chi squared test (N = 36)


choiceProportionsByBSAnimated <- subset(choiceProportionsAnimated) %>%
  # na.omit() %>%
  group_by(experimentalCondition,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsByBSAnimatedPlot <- choiceProportionsByBSAnimated

choiceProportionsByBSAnimatedPlot$xIrrational <- round(100-choiceProportionsByBSAnimatedPlot$xRational,1)
choiceProportionsByBSAnimatedPlot <- melt(choiceProportionsByBSAnimatedPlot
                                             , vars = c("xRational","xIrrational")
                                             , variable.name = "choiceType", value.name = "xChoices")


choiceProportionsByBSAnimatedPlot$scenario <- factor(choiceProportionsByBSAnimatedPlot$scenario
                                                        ,levels = sort(unique(choiceProportionsByBSAnimatedPlot$scenario),decreasing = TRUE)
                                                        ,labels = sort(unique(choiceProportionsByBSAnimatedPlot$scenario),decreasing = TRUE)
)


choiceProportionsByBSAnimatedPlot$experimentalCondition <- factor(choiceProportionsByBSAnimatedPlot$experimentalCondition
                                                                     , levels = c("control","treatment")
                                                                     , labels = c("Control","Treatment")
)

#Legend components 
# choiceProportionsByBSAnimatedPlot$choiceType <- factor(choiceProportionsByBSAnimatedPlot$choiceType
#                                                                      , levels = c("xRational", "xIrrational")
#                                                                      , labels = c("Route 1","Route 2"))

choiceProportionsByBSAnimatedPlot$choiceType <- factor(choiceProportionsByBSAnimatedPlot$choiceType
                                                       , levels = c("xIrrational","xRational")
                                                       , labels = c("Route 2","Route 1"))

ggplot(choiceProportionsByBSAnimatedPlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType,order = choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentalCondition,ncol=2)+
  geom_hline(yintercept=UBChi2N36, linetype = 'dashed') + #Upper bound chi2
  geom_hline(yintercept=LBChi2N36, linetype = 'dashed') + #Lower bound chi2
  coord_flip() +
  themeChoiceProportionsPlotsPaper1+
  ylab('Choice proportion [%]') +
  xlab('Scenario') +
  scale_fill_manual(values=c("Route 1"="gray", "Route 2" = "white"),labels=c("Route 1  ","Route 2"),breaks=c('Route 1',"Route 2")) #+
  # theme(legend.position='none')#+
  #ggtitle('Choice Proportion in ')

ggsave(str_c("export","/figures/Paper1/ChoiceProportions/ChoiceProportionsAnimatedConditions.pdf")
       , width = 1.3*widthChoiceProportionsPlots, height = heightChoiceProportionsPlots , unit="cm", dpi=dpiChoiceProportionsPlots)


#     - Numeric (control & treatment) -------------------------------------

choiceProportionsByBSNumeric <- subset(DCMLongDataset,experimentType == "descriptive" & scenario %in% str_c("0",seq(1,8)))


choiceProportionsByBSNumeric$choice[choiceProportionsByBSNumeric$choice=="rational"] <- 1
choiceProportionsByBSNumeric$choice[choiceProportionsByBSNumeric$choice=="irrational"] <- 0

choiceProportionsByBSNumeric$choice <- as.numeric(choiceProportionsByBSNumeric$choice)

choiceProportionsByBSNumeric <- subset(choiceProportionsByBSNumeric) %>%
  # na.omit() %>%
  group_by(experimentalCondition,scenario) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsByBSNumericPlot <- choiceProportionsByBSNumeric

choiceProportionsByBSNumericPlot$xIrrational <- round(100-choiceProportionsByBSNumericPlot$xRational,1)
choiceProportionsByBSNumericPlot <- melt(choiceProportionsByBSNumericPlot
                                             , vars = c("xRational","xIrrational")
                                             , variable.name = "choiceType", value.name = "xChoices")


choiceProportionsByBSNumericPlot$scenario <- factor(choiceProportionsByBSNumericPlot$scenario
                                                        ,levels = sort(unique(choiceProportionsByBSNumericPlot$scenario),decreasing = TRUE)
                                                        ,labels = sort(unique(choiceProportionsByBSNumericPlot$scenario),decreasing = TRUE)
)


choiceProportionsByBSNumericPlot$experimentalCondition <- factor(choiceProportionsByBSNumericPlot$experimentalCondition
                                                                     , levels = c("control","treatment")
                                                                     , labels = c("Control","Treatment")
)

#Legend components 
choiceProportionsByBSNumericPlot$choiceType <- factor(choiceProportionsByBSNumericPlot$choiceType
                                                      , levels = c("xIrrational","xRational")
                                                      , labels = c("Route 2","Route 1"))

ggplot(choiceProportionsByBSNumericPlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentalCondition,ncol=2)+
  geom_hline(yintercept=UBChi2N36, linetype = 'dashed') + #Upper bound chi2
  geom_hline(yintercept=LBChi2N36, linetype = 'dashed') + #Lower bound chi2
  coord_flip() +
  themeChoiceProportionsPlotsPaper1+
  ylab('Choice proportion [%]') +
  xlab('Scenario') +
  scale_fill_manual(values=c("Route 1"="gray", "Route 2" = "white"),labels=c("Route 1  ","Route 2"),breaks=c('Route 1',"Route 2")) #+
# theme(legend.position='none')#+
#ggtitle('Choice Proportion in ')

ggsave(str_c("export","/figures/Paper1/ChoiceProportions/ChoiceProportionsNumericConditions.pdf")
       , width = 1.3*widthChoiceProportionsPlots, height = heightChoiceProportionsPlots , unit="cm", dpi=dpiChoiceProportionsPlots)


#     - Animated and Numeric -------------------------------------

choiceProportionsAnimatedAndNumeric <- subset(DCMLongDataset, scenario %in% str_c("0",seq(1,8)))

choiceProportionsAnimatedAndNumeric$choice[choiceProportionsAnimatedAndNumeric$choice=="rational"] <- 1
choiceProportionsAnimatedAndNumeric$choice[choiceProportionsAnimatedAndNumeric$choice=="irrational"] <- 0

choiceProportionsAnimatedAndNumeric$choice <- as.numeric(choiceProportionsAnimatedAndNumeric$choice)

choiceProportionsAnimatedAndNumeric <- subset(choiceProportionsAnimatedAndNumeric) %>%
  # na.omit() %>%
  group_by(scenario,experimentType) %>%
  summarise(xRational = round(100*mean(choice),1))

choiceProportionsAnimatedAndNumericPlot <- choiceProportionsAnimatedAndNumeric

choiceProportionsAnimatedAndNumericPlot$xIrrational <- round(100-choiceProportionsAnimatedAndNumericPlot$xRational,1)
choiceProportionsAnimatedAndNumericPlot <- melt(choiceProportionsAnimatedAndNumericPlot
                                         , vars = c("xRational","xIrrational")
                                         , variable.name = "choiceType", value.name = "xChoices")


choiceProportionsAnimatedAndNumericPlot$scenario <- factor(choiceProportionsAnimatedAndNumericPlot$scenario
                                                    ,levels = sort(unique(choiceProportionsAnimatedAndNumericPlot$scenario),decreasing = TRUE)
                                                    ,labels = sort(unique(choiceProportionsAnimatedAndNumericPlot$scenario),decreasing = TRUE)
)


choiceProportionsAnimatedAndNumericPlot$experimentType <- factor(choiceProportionsAnimatedAndNumericPlot$experimentType
                                                                 , levels = c("experienced","descriptive")
                                                                 , labels = c("Animated","Numerical")
)

#Legend components 
choiceProportionsAnimatedAndNumericPlot$choiceType <- factor(choiceProportionsAnimatedAndNumericPlot$choiceType
                                                      , levels = c("xIrrational","xRational")
                                                      , labels = c("Route 2","Route 1"))

ggplot(choiceProportionsAnimatedAndNumericPlot,
       aes(x= interaction(scenario), y=xChoices, fill=choiceType, label = xChoices)) +
  scale_y_continuous() +
  geom_bar(stat='identity',colour="black") +
  geom_text(size = 6, position = position_stack(vjust = 0.5))+
  facet_wrap(~experimentType,ncol=2)+
  geom_hline(yintercept=UBChi2N36, linetype = 'dashed') + #Upper bound chi2
  geom_hline(yintercept=LBChi2N36, linetype = 'dashed') + #Lower bound chi2
  coord_flip() +
  themeChoiceProportionsPlotsPaper1+
  ylab('Choice proportion [%]') +
  xlab('Scenario') +
  scale_fill_manual(values=c("Route 1"="gray", "Route 2" = "white"),labels=c("Route 1  ","Route 2"),breaks=c('Route 1',"Route 2")) #+
# theme(legend.position='none')#+
#ggtitle('Choice Proportion in ')

ggsave(str_c("export","/figures/Paper1/ChoiceProportions/ChoiceProportionsAnimatedAndNumericConditions.pdf")
       , width = 1.3*widthChoiceProportionsPlots, height = heightChoiceProportionsPlots , unit="cm", dpi=dpiChoiceProportionsPlots)




# P2) Manipulation checks (TBA) -------------------------------------------------
#   a) Comparison between scenario "01" and "02" ----------------------------------------------------------------
#     - Data ------------------------------------------------------------------

temp1 <- experiencedChoiceResponsesDecisionStep[c("participantId","scenario","choice","experimentalBlock")]
setnames(temp1,old = "choice", new = "experiencedChoice")

temp2 <-descriptiveChoiceResponses[c("participantId","scenario","choice","experimentalBlock")]
setnames(temp2,old = "choice", new = "descriptiveChoice")


choicePerScenarioParticipants <- merge(temp2,temp1,by=c("participantId","scenario","experimentalBlock"),all.x = TRUE)

# choicePerScenarioParticipantsConsistency <- subset(choicePerScenarioParticipants,is.na(consistentChoice) == FALSE)

#Adding Infromation from Participant Dataset
choicePerScenarioParticipants <- merge(choicePerScenarioParticipants,participants[c("participantId","experimentalCondition","city","timeCounting")])

#     - Logistic Regression -------------------------------------------
#       + Control (Paper-report) ----------------------------------------------------------------


#We want to test whether a higher preference for waiting times makes participant to better differentiate a difference of two seconds
#Since the exponent c is equal accross attributes, and we already know that av>aw, then it should be easier to see difference between travel times
#However, since the preference for waiting time is higher, then this should help participants to better detect difference in waiting time. 
#All in all, the only way to detect diffference in perception is to divide participants according to their preferences or time estimations, and then run these tests.
#         * All (Paper) -------------------------------------------------------------------


dataManipulationChecks <- choicePerScenarioParticipants

dataManipulationChecks <- subset(dataManipulationChecks,scenario == c("01","02"))

dataManipulationChecks <- dataManipulationChecks[,c("participantId","scenario","experiencedChoice","experimentalCondition","city")]

dataManipulationChecks$experiencedChoice <- ifelse(dataManipulationChecks$experiencedChoice == "rational",1,0)

MixedEffectModel_ManipulationChecks_Control <- glmer(experiencedChoice ~ factor(scenario)
                                             + (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                             ,data=subset(dataManipulationChecks,experimentalCondition == "control"))


summary(MixedEffectModel_ManipulationChecks_Control)

# #See whether participants with stronger preference for waiting pass more eaisly the test
# 
# rationalChoiceS3 <- subset(choicePerScenarioParticipantsConsistency,descriptiveChoice == "rational" & scenario == "03")
# rationalChoiceS4 <- subset(choicePerScenarioParticipantsConsistency,descriptiveChoice == "rational" & scenario == "04")
# 
# participantsPreferWaitingList <- (intersect(rationalChoiceS3$participantId,rationalChoiceS4$participantId))
# 
# 
# dataManipulationChecks$participantPreferWaiting <- 0
# dataManipulationChecks[dataManipulationChecks$participantId %in% participantsPreferWaitingList,]$participantPreferWaiting <- 1
# 
# MixedEffectModel_ManipulationChecks2 <- glmer(experiencedChoice ~ factor(participantPreferWaiting)
#                                              + (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
#                                              ,data=subset(dataManipulationChecks,experimentalCondition == "control" & scenario == "01"))
# 
# 
# summary(MixedEffectModel_ManipulationChecks2)

#         * London (X) -------------------------------------------------------------------

MixedEffectModel_ManipulationChecks_Control_London <- glmer(experiencedChoice ~ factor(scenario)
                                                     + (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                     ,data=subset(dataManipulationChecks,experimentalCondition == "control" & city == "London"))


summary(MixedEffectModel_ManipulationChecks_Control_London)

#         * Santiago (X)-------------------------------------------------------------------

MixedEffectModel_ManipulationChecks_Control_Santiago <- glmer(experiencedChoice ~ factor(scenario)
                                                              + (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                              ,data=subset(dataManipulationChecks,experimentalCondition == "control" & city == "Santiago"))


summary(MixedEffectModel_ManipulationChecks_Control_Santiago )

#     + Extra -----------------------------------------------------------------

#       * Descriptive statistics ------------------------------------------------

manipulationCheckParticipants <- choicePerScenarioParticipants

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

manipulationChecksParticipantsS1Irrational <- subset(manipulationChecksParticipantsS1S2, EC1 == "irrational")
manipulationChecksParticipantsS2Irrational <- subset(manipulationChecksParticipantsS1S2, EC2 == "irrational")
manipulationChecksParticipantsS1S2Irrational <- subset(manipulationChecksParticipantsS1S2, EC1 == "irrational" & EC2 == "irrational")
manipulationChecksParticipantsS1OrS2Irrational <- subset(manipulationChecksParticipantsS1S2, EC1 == "irrational" | EC2 == "irrational")

table(manipulationChecksParticipantsS1S2Rational$experimentalCondition)

#List of participants that passed the manipulation check

listRationalParticipantsExperienced <-  str_sub(manipulationChecksParticipantsS1S2Rational$participantId,2,3)
listRationalParticipantsS1Experienced <-  str_sub(manipulationChecksParticipantsS1Rational$participantId,2,3)
listRationalParticipantsS2Experienced <-  str_sub(manipulationChecksParticipantsS2Rational$participantId,2,3)


listIrrationalParticipantsExperienced <-  str_sub(manipulationChecksParticipantsS1OrS2Irrational$participantId,2,3)

#       * Logistic regression Treatment ----------------------------------------------------------------

MixedEffectModel_ManipulationChecks <- glmer(experiencedChoice ~ 1+factor(scenario)
                                             + (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                             ,data=subset(dataManipulationChecks,experimentalCondition == "treatment"))


MixedEffectModel_ManipulationChecks <- glm(experiencedChoice ~ 1+factor(scenario), family = binomial
                                           ,data=subset(dataManipulationChecks,experimentalCondition == "treatment"))

summary(MixedEffectModel_ManipulationChecks)



# P3) Effect of within-subjects conditions on impact of waiting time (Paper-tables) -----------------------------------

#   a) Numeric versus animated choices in control condition -----------------------------------------

#Report three models in three different columns. 
#I report results for London because the analysis of time estimation was made only for them. 

#     Data preparation --------------------------------------------------------

dataAnimatedNumericControl <- subset(choicePerScenarioParticipants,experimentalCondition == "control")

# dataAnimatedNumericControl <- subset(dataAnimatedNumericControl,scenario %in% c("03","04","05","06","07","08")) #"01","02",
dataAnimatedNumericControl <- subset(dataAnimatedNumericControl,scenario %in% c("03","04","05","06","07","08")) #,
# dataAnimatedNumericControl <- subset(dataAnimatedNumericControl,scenario %in% c("03","04")) #,

dataAnimatedNumericControl <- dataAnimatedNumericControl[,c("participantId","scenario","experiencedChoice","descriptiveChoice","city")]

dataAnimatedNumericControl <- melt(dataAnimatedNumericControl ,measure.vars = c("experiencedChoice","descriptiveChoice"))

colnames(dataAnimatedNumericControl) <- c("participantId","scenario","city","experiment","choice")

dataAnimatedNumericControl$choice <- ifelse(dataAnimatedNumericControl$choice == "rational",1,0)


#     + Analysis by experiment (Animated vs Numeric)  --------------------------------------------------------------------
#       * All -------------------------------------------------------------------


#Lower effect of waiting because the scale parameter of the time perception associated to in-vehicle times is higher

MixedEffectModel_NumericVSAnimated <- glmer(choice ~ factor(experiment == "experiencedChoice")+
                                                  (1|scenario)+(1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                ,data=dataAnimatedNumericControl)

summary(MixedEffectModel_NumericVSAnimated)

MixedEffectModel_NumericVSAnimatedInteractions <- glmer(choice ~ factor(experiment == "experiencedChoice")*factor(scenario)+
                                                          (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                        ,data=dataAnimatedNumericControl)

summary(MixedEffectModel_NumericVSAnimatedInteractions)

#       * Numeric -------------------------------------------------------------------


MixedEffectModel_Numeric<- glmer(choice ~ factor(scenario)+
                                              (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                            ,data=subset(dataAnimatedNumericControl,experiment = "descriptiveChoice"))

summary(MixedEffectModel_Numeric)

#       * Animated -------------------------------------------------------------------

MixedEffectModel_Animated<- glmer(choice ~ factor(scenario)+
                                   (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                 ,data=subset(dataAnimatedNumericControl,experiment = "animatedChoice"))

summary(MixedEffectModel_Animated)

#     + Analysis by city (Paper)  --------------------------------------------------------------------
#       * All -------------------------------------------------------------

MixedEffectModel_NumericVSAnimated <- glmer(choice ~ factor(experiment == "experiencedChoice")
                                                            +(1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                            ,data=subset(dataAnimatedNumericControl))

summary(MixedEffectModel_NumericVSAnimated)

MixedEffectModel_NumericVSAnimatedByCity <- glmer(choice ~ factor(experiment == "experiencedChoice")+factor(city == "London")
                                                  +(1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataAnimatedNumericControl))

summary(MixedEffectModel_NumericVSAnimatedByCity)

MixedEffectModel_NumericVSAnimatedInteractionsCity <- glmer(choice ~ factor(experiment == "experiencedChoice")*factor(city== "London")
                                                  +(1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataAnimatedNumericControl))

summary(MixedEffectModel_NumericVSAnimatedInteractionsCity)

#       * Santiago -----------------------------------------------------------

MixedEffectModel_NumericVSAnimatedSantiago <- glmer(choice ~ factor(experiment == "experiencedChoice")+factor(scenario)+(1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                        ,data=subset(dataAnimatedNumericControl,city == "Santiago"))

summary(MixedEffectModel_NumericVSAnimatedSantiago)







#       * London -------------------------------------------------------------

MixedEffectModel_NumericVSAnimatedLondon <- glmer(choice ~ factor(experiment == "experiencedChoice")+factor(scenario)+(1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataAnimatedNumericControl,city == "London"))

summary(MixedEffectModel_NumericVSAnimatedLondon)
#   b) Control vs treatment condition in animated choices  -----------------------------------------
#      Data preparation --------------------------------------------------------
dataAnimated <- choicePerScenarioParticipants

dataAnimated <- dataAnimated[,c("participantId","scenario","experiencedChoice","experimentalCondition","city")]

dataAnimated <- subset(dataAnimated,scenario %in% c("03","04","05","06","07","08")) #"01","02",
# dataAnimated <- subset(dataAnimated,scenario %in% c("03","04","05")) #"01","02",

dataAnimated$experiencedChoice <- ifelse(dataAnimated$experiencedChoice == "rational",1,0)

# dataAnimated$experimentalCondition <- ifelse(dataAnimated$experimentalCondition == "treatment",1,-1)
# dataAnimated$city <- ifelse(dataAnimated$city == "London",1,-1)


#Higher effect of waiting


#      + Analysis by city (Paper)  --------------------------------------------------------------------
#        * All ---------------------------------------------------------------------
MixedEffectModel_TreatmentEffectAnimated <- glmer(experiencedChoice ~ factor(experimentalCondition)+
                                               (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                             ,data=subset(dataAnimated))

summary(MixedEffectModel_TreatmentEffectAnimated)

MixedEffectModel_TreatmentEffectAnimatedByCity <- glmer(experiencedChoice ~ factor(experimentalCondition)+factor(city == "London")+
                                                          (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                        ,data=subset(dataAnimated))

summary(MixedEffectModel_TreatmentEffectAnimatedByCity)

MixedEffectModel_TreatmentEffectAnimatedInteractionsCity <- glmer(experiencedChoice ~ factor(experimentalCondition)*factor(city)+
                                                    (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataAnimated))

summary(MixedEffectModel_TreatmentEffectAnimatedInteractionsCity)



#        * Santiago --------------------------------------------------------------

MixedEffectModel_TreatmentEffectAnimated_Santiago <- glmer(experiencedChoice ~ factor(experimentalCondition)+factor(scenario)+
                                                             (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                           ,data=subset(dataAnimated,city == "Santiago"))

summary(MixedEffectModel_TreatmentEffectAnimated_Santiago)


#        * London ----------------------------------------------------------------

MixedEffectModel_TreatmentEffectAnimated_London <- glmer(experiencedChoice ~ factor(experimentalCondition)+factor(scenario)+
                                                    (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataAnimated,city == "London"))

summary(MixedEffectModel_TreatmentEffectAnimated_London)

#   c) Control vs treatment condition in numeric choices  -----------------------------------------
#      Data preparation --------------------------------------------------------
dataNumeric <- choicePerScenarioParticipants

dataNumeric <- dataNumeric[,c("participantId","scenario","descriptiveChoice","experimentalCondition","city")]

dataNumeric <- subset(dataNumeric,scenario %in% c("03","04","05","06","07","08")) #"01","02",
# dataNumeric <- subset(dataNumeric,scenario %in% c("03","04")) #"01","02",

dataNumeric$descriptiveChoice <- ifelse(dataNumeric$descriptiveChoice == "rational",1,0)

#Higher effect of waiting


#      + Analysis by city (Paper)  --------------------------------------------------------------------
#        * All ---------------------------------------------------------------------
MixedEffectModel_TreatmentEffectNumeric <- glmer(descriptiveChoice ~ factor(experimentalCondition)+
                                                    (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                  ,data=subset(dataNumeric))

summary(MixedEffectModel_TreatmentEffectNumeric)

MixedEffectModel_TreatmentEffectNumericByCity <- glmer(descriptiveChoice ~ +factor(experimentalCondition)+factor(city == "London")+
                                                         (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                       ,data=subset(dataNumeric))

summary(MixedEffectModel_TreatmentEffectNumericByCity)

MixedEffectModel_TreatmentEffectNumericInteractionsCity <- glmer(descriptiveChoice ~ +factor(experimentalCondition)*factor(city == "London")+
                                                                    (1|participantId)+(1|scenario), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                                  ,data=subset(dataNumeric))

summary(MixedEffectModel_TreatmentEffectNumericInteractionsCity)




#        * Santiago --------------------------------------------------------------

MixedEffectModel_TreatmentEffectNumeric_Santiago <- glmer(descriptiveChoice ~ factor(experimentalCondition)+factor(scenario)+
                                                             (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                           ,data=subset(dataNumeric,city == "Santiago"))

summary(MixedEffectModel_TreatmentEffectNumeric_Santiago)


#        * London ----------------------------------------------------------------

MixedEffectModel_TreatmentEffectNumeric_London <- glmer(descriptiveChoice ~ factor(experimentalCondition)+factor(scenario)+
                                                           (1|participantId), family = binomial, control = glmerControl(optimizer = "bobyqa")
                                                         ,data=subset(dataNumeric,city == "London"))

summary(MixedEffectModel_TreatmentEffectNumeric_London)


#   e) export tables --------------------------------------------------------
#     - BLR tables (format according to Ashby and Rakow, 2017) ------------------------------------------------------------
roundToDigits <- function(data,digits){
  #if the number is rounded, it still keep two numbers
  
  for(i in 1:length(data)){
    
    lengthNumRounded <- str_length(as.character(round(as.numeric(data[i]),2)))-2-1
    
    if(lengthNumRounded>3| data[i]=="Inf"){ #If the number is higher than 10 it does not make sense to showing it.
      data[i] <- "*"
    }
    
    if(data[i]!="*"){
      
      
      data[i] <- round(as.numeric(data[i]),digits)
      count = 0
      
      if(as.numeric(data[i])%%1!=0){  
        if(data[i]<0){
          count = 1
        }
        
        
        data[i] <- as.character(data[i])
        
        
        
        while(str_length(data[i])<2+digits+count){
          data[i] <- str_c(data[i],"0")
        }
      }
      
      
      else{ #Integer number
        data[i] <- as.character(data[i])
        data[i] <- str_c(data[i],".")
        
        while(str_length(data[i])<lengthNumRounded+4+digits){
          data[i] <- str_c(data[i],"0")
        }
        
      }
    }
  }
  return(data)
}

generateTableExperimentalModel <- function(model,digits){
  model.df <- tidy(model)
  
  model.df <- model.df[-which(str_detect(model.df$term,"sd")),] #No fixed effects
  
  
  # model.df <- model.df[1:(dim(model.df)[1]-1),] #No fixed effects
  
  model.df <- model.df %>% 
    mutate(or = exp(estimate),  # Odds ratio/gradient
           var.diag = diag(vcov(model)),  # Variance of each coefficient
           or.se = sqrt(or^2 * var.diag))  # Odds-ratio adjusted 
  
  # Odd-ratios
  #From this discussion: https://stackoverflow.com/questions/26417005/odds-ratio-and-confidence-intervals-from-glmer-output
  # temp <- as.data.frame(confint(model,parm="beta_",method="Wald"))
  
  # cc <- confint(model,parm="beta_")  ## slow (~ 11 seconds)
  # ctab <- cbind(est=fixef(gm1),cc)
  se <- sqrt(diag(vcov(model)))
  tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 * se)
  tab <- exp(tab)
  tab <- as.data.frame(tab)
  tab$term <- row.names(tab)
  row.names(tab) <- NULL
  colnames(tab) <- c("or","CILower","CIUpper","term")
  
  #Create a unique string for confidence interval
  tab$CI <- str_c("[",roundToDigits(data=tab$CILower,digits),", ",roundToDigits(data=tab$CIUpper,digits),"]")
  
  #Merge with existing output
  model.df <- merge(model.df,tab[c("CILower","CIUpper","CI","term")],by=c("term"))
  
  #Replace parenthesis by "." from predictors names (needs to be before the replacement of the levels of the predictors)
  model.df <- as.data.frame(sapply(model.df,function(x) gsub("[()]", ".", x)))
  
  #Replace : by x
  model.df <- as.data.frame(sapply(model.df,function(x) gsub(":", " x ", x)))
  
  #Relabel predictors
  
  #The number define order of variables (create a dataset with the variable names in the ordered desired and merge with the regression table)
  #The first term is the original value (parenthesis replaced by dot before)
  #The second term is the new label that will be shown in the table (parenthesis replaced by dot before)
  labels <- list(
    list(label="factor.dominatingInWaitingTime.1",newLabel="Time dominance",order=1) #1#
    ,list(label="factor.experimentType.experienced",newLabel="Degree of experience",order=2) #2#
    ,list(label="factor.experimentalCondition.treatment",newLabel="Degree of information",order=3) #3#
    ,list(label="factor.city.Santiago",newLabel="City",order=4) #4#
    ,list(label="factor.largerAdvantageInTradeOffWT.1",newLabel="Difference in Proportion of Waiting Time",order=5) #6# Same journey time
    ,list(label="factor.largerAdvantageInTradeOffWTButHigherJourneyTime.1",newLabel="EU Gain in Longer Journey Option",order=6) #6# Different journey times
    
    ,list(label="factor.largeAdvantageInTradeOffWTAndSameJourneyTime.1",newLabel="Same journey time - Expected Utility Gain",order=7) #H2C
    ,list(label="factor.moderateAdvantageInTradeOffWTButHigherJourneyTime.1",newLabel="Longer Journey Time - Expected Utility Gain",order=8) #
    ,list(label="factor.largeAdvantageInTradeOffWTButHigherJourneyTime.1",newLabel="Journey Time - Expected Utility Gain",order=9)
    ,list(label="factor.moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime.1",newLabel="Journey time",order=10)
    
    # ,list(label="factor.largerDisadvantageInTradeOffWT.1",newLabel="Trade-Off Disadvantage",order=6) #6# Different journey times
    ,list(label="factor.highVariability.1",newLabel="Level of time variability",order=11) #4#
    ,list(label="factor.highVariabilityInWaitingTime.1",newLabel="Level of waiting time variability ",order=12) ##
    ,list(label="factor.highVariabilityInTravelTime.1",newLabel="Level of in in-vehicle time variability",order=13) ##
    ,list(label="factor.variabilityInWaitingTime.1",newLabel="Allocation of time variability",order=14) ##
    ,list(label=".Intercept.",newLabel="Constant",order=15) #1#
    ,list(label=".Intercept.",newLabel="Constant",order=15) #1#
    ,list(label=".Intercept.",newLabel="Constant",order=15) #1#
    ,list(label="factor.scenario.04",newLabel="Scenario S4",order=16) #1#
    ,list(label="factor.scenario.05",newLabel="Scenario S5",order=17) #1#
    ,list(label="factor.scenario.06",newLabel="Scenario S6",order=18) #1#
    ,list(label="factor.scenario.07",newLabel="Scenario S7",order=19) #1#
    ,list(label="factor.scenario.08",newLabel="Scenario S8",order=20) #1#
  )
  
  model.df$term <- as.character(model.df$term)
  
  #The main effects add an additional description between parenthesis about the level of the variable
  model.df$term<- ifelse(model.df$term==labels[[1]][["label"]],str_c(labels[[1]][["label"]]," (Travelling vs. Waiting)"),model.df$term) #Experiment Type
  model.df$term<- ifelse(model.df$term==labels[[2]][["label"]],str_c(labels[[2]][["label"]]," (Descriptive vs. Experiential)"),model.df$term) #Experiment Type
  model.df$term<- ifelse(model.df$term==labels[[3]][["label"]],str_c(labels[[3]][["label"]]," (Lower vs. Higher)"),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[4]][["label"]],str_c(labels[[4]][["label"]]," (London vs. Santiago)"),model.df$term) #City
  model.df$term<- ifelse(model.df$term==labels[[5]][["label"]],str_c(labels[[5]][["label"]]," (Low vs. High)             "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[6]][["label"]],str_c(labels[[6]][["label"]]," (Moderate vs. High)        "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[7]][["label"]],str_c(labels[[7]][["label"]]," (Low vs. High)        "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[8]][["label"]],str_c(labels[[8]][["label"]]," (Low vs. Moderate)        "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[9]][["label"]],str_c(labels[[9]][["label"]]," (Low vs. High)        "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[10]][["label"]],str_c(labels[[10]][["label"]]," (Equal vs. Longer)        "),model.df$term) #Experimental Condition
  model.df$term<- ifelse(model.df$term==labels[[11]][["label"]],str_c(labels[[11]][["label"]]," (Low vs. High)"),model.df$term)#Level of variability 
  model.df$term<- ifelse(model.df$term==labels[[12]][["label"]],str_c(labels[[12]][["label"]]," (Low vs. High)"),model.df$term)#Level of variability in waiting time
  model.df$term<- ifelse(model.df$term==labels[[13]][["label"]],str_c(labels[[13]][["label"]]," (Low vs. High)"),model.df$term)#Level of variability in travel time 
  model.df$term<- ifelse(model.df$term==labels[[14]][["label"]],str_c(labels[[14]][["label"]]," (Travelling vs. Waiting)"),model.df$term) #Allocation of Time sVariability
  
  
  # model.df$term == ''"(Intercept)"''
  for(element in 1:length(labels)){
    model.df$term <- str_replace(model.df$term,labels[[element]][["label"]],labels[[element]][["newLabel"]])
  }
  
  model.df$term <- as.character(model.df$term)
  
  #Rename columns
  model.df <- plyr::rename(model.df,c(term="Predictor"))
  model.df <- plyr::rename(model.df,c(or="OR"))
  model.df <- plyr::rename(model.df,c(statistic="z"))
  model.df <- plyr::rename(model.df,c(p.value="p"))
  
  
  #Reorder labels
  labels.df <- data.frame(order=labels[[1]][["order"]],predictor=labels[[1]][["newLabel"]])
  
  for(element in 2:length(labels)){
    labels.df <- rbind(labels.df,data.frame(order=labels[[element]][["order"]],predictor=labels[[element]][["newLabel"]]))  
  }
  
  #Subset of columns and rounding decimals
  model.df <- model.df[c("Predictor","OR","z","p","CI")]
  
  model.df$OR <- as.numeric(as.character(model.df$OR))
  model.df$OR <- roundToDigits(data = model.df$OR,digits = digits)
  
  model.df$z <-  as.numeric(as.character(model.df$z))
  model.df$z <- roundToDigits(model.df$z,digits)
  
  model.df$p <-  as.numeric(as.character(model.df$p))
  model.df$p <- roundToDigits(model.df$p,digits)
  
  
  return(model.df)
}

exporttablesExperimentalModel <- function(models,digits,path,format,folder){
  
  
  for(element in 1:length(models)){
    model.df <- generateTableExperimentalModel(model = models[[element]][["model"]],digits = digits)
    
    
    if(format=="doc"){
      # write.csv(model.df,file = str_c(filePath,".csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")  
      xTable <- xtable(model.df)
      filePath <- str_c(path,"Word/",folder,"/",models[[element]][["filename"]])
      print.xtable(xTable, type="html", file=str_c(filePath,".doc"), include.rownames=FALSE)
    }
    if(format == "tex"){
      xTable <- xtable(model.df)
      filePath <- str_c(path,"Latex/",folder,"/",models[[element]][["filename"]])
      print.xtable(xTable, type="latex", file=str_c(filePath,".tex"), include.rownames=FALSE,floating=FALSE,latex.environments=NULL,booktabs=TRUE)
    }
  }
}


#     - Parameters ------------------------------------------------------------
digits <- 2
format <- c("tex")
# format <- c("doc")
#     - Output ----------------------------------------------------------------

models <- list(
  list(model = MixedEffectModel_NumericVSAnimatedSantiago, filename = "MixedEffectModel_NumericVSAnimatedSantiago")
  ,list(model = MixedEffectModel_NumericVSAnimatedLondon, filename = "MixedEffectModel_NumericVSAnimatedLondon")
  ,list(model = MixedEffectModel_TreatmentEffectAnimated_Santiago, filename = "MixedEffectModel_TreatmentEffectAnimated_Santiago")
  ,list(model = MixedEffectModel_TreatmentEffectAnimated_London, filename = "MixedEffectModel_TreatmentEffectAnimated_London")
)    

exporttablesExperimentalModel(format=format, models = models,digits = digits, path = str_c("export/tables/Paper1/"),folder="LogisticRegression")

models1 <- list(
  list(model = MixedEffectModel_NumericVSAnimatedInteractionsCity, filename = "MixedEffectModel_NumericVSAnimatedInteractionsCity")
, list(model = MixedEffectModel_TreatmentEffectAnimatedInteractionsCity, filename = "MixedEffectModel_TreatmentEffectAnimatedInteractionsCity")
)

exporttablesExperimentalModel(format=format, models = models1,digits = digits, path = str_c("export/tables/Paper1/"),folder="LogisticRegression")


modelsPaper <- list(
  list(model = MixedEffectModel_NumericVSAnimated, filename = "MixedEffectModel_NumericVSAnimated")
  ,list(model = MixedEffectModel_TreatmentEffectNumeric, filename = "MixedEffectModel_TreatmentEffectNumeric")
  ,list(model = MixedEffectModel_TreatmentEffectAnimated, filename = "MixedEffectModel_TreatmentEffectAnimated")
)

exporttablesExperimentalModel(format=format, models = modelsPaper,digits = digits, path = str_c("export/tables/Paper1/"),folder="LogisticRegression")



# P4) Analysis of certainty level (X) -----------------------------------------

certaintyLevelResponses <- choiceResponses[,c("participantId","scenario","certaintyLevel","experimentalCondition","experimentType")]

certaintyLevelResponses <- subset(certaintyLevelResponses,!is.na(certaintyLevel))

certaintyLevelResponses <- subset(certaintyLevelResponses, scenario %in% str_c("0",seq(1,8)))

certaintyLevelResponses$passTest <- NA

certaintyLevelResponses$passTest = ifelse(certaintyLevelResponses$participantId %in% manipulationChecksParticipantsS1S2Rational$participantId,1,0)


averageCertaintlyLevelByCondition <- certaintyLevelResponses %>%
  group_by(experimentalCondition,passTest) %>%
  summarise_at(vars(certaintyLevel),funs(mean(.,na.rm = TRUE)))

averageCertaintlyLevelByCondition


# P5) Analysis of Defrief Questions ---------------------------------- --------
#   1) TIME ESTIMATION ---------------------------------------------------------
#     a) Create Dataset ----------------------------------------------------------
#      - Choice scenarios ------------------------------------------------------
choiceScenario1 <- c(w1 = 4, v1 = 6, w2 = 6, v2 = 6) 
choiceScenario2 <- c(w1 = 6, v1 = 4, w2 = 6, v2 = 6) 
choiceScenario3 <- c(w1 = 4, v1 = 6, w2 = 6, v2 = 4) 
choiceScenario4 <- c(w1 = 1, v1 = 9, w2 = 9, v2 = 1) 
choiceScenario5 <- c(w1 = 2, v1 = 9, w2 = 8, v2 = 2) 
choiceScenario6 <- c(w1 = 3, v1 = 8, w2 = 8, v2 = 2) 
choiceScenario7 <- c(w1 = 8, v1 = 1, w2 = 2, v2 = 8) 
choiceScenario8 <- c(w1 = 7, v1 = 2, w2 = 2, v2 = 8) 
choiceScenario9 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6) 
choiceScenario10 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6) 
choiceScenario11 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6) 
choiceScenario12 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6) 
choiceScenario13 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6) 
choiceScenario14 <- c(w1 = 4, v1 = 6, w2 = 4, v2 = 6)

choiceScenarios <- rbind(s1 = choiceScenario1,s2 = choiceScenario2, s3 = choiceScenario3,s4= choiceScenario4
                         , s5 = choiceScenario5, s6 = choiceScenario6, s7= choiceScenario7, s8 = choiceScenario8
                         , s9 = choiceScenario9, s10 = choiceScenario10, s11= choiceScenario11, s12 = choiceScenario12
                         , s13 = choiceScenario13, s14 = choiceScenario14
)

choiceScenarios <- data.frame(cbind(scenario = row.names(choiceScenarios), choiceScenarios),row.names = NULL)

choiceScenarios$scenario <- as.character(choiceScenarios$scenario)
choiceScenarios$w1 <- as.numeric(as.character(choiceScenarios$w1))
choiceScenarios$w2 <- as.numeric(as.character(choiceScenarios$w2))
choiceScenarios$v1 <- as.numeric(as.character(choiceScenarios$v1))
choiceScenarios$v2 <- as.numeric(as.character(choiceScenarios$v2))

#       - Participant information -------------------------------------------------


temp1 <- experiencedChoiceResponsesDecisionStep[c("participantId","scenario","choice","experimentalBlock")]
setnames(temp1,old = "choice", new = "experiencedChoice")

temp2 <-descriptiveChoiceResponses[c("participantId","scenario","choice","experimentalBlock")]
setnames(temp2,old = "choice", new = "descriptiveChoice")

choicePerScenarioParticipants <- merge(temp2,temp1,by=c("participantId","scenario","experimentalBlock"),all.x = TRUE)

choicePerScenarioParticipants$consistentChoice <- 0
choicePerScenarioParticipants$consistentChoice[is.na(choicePerScenarioParticipants$experiencedChoice)] <- NA
choicePerScenarioParticipants$consistentChoice[with(choicePerScenarioParticipants,descriptiveChoice==experiencedChoice)] <- 1 

mean(choicePerScenarioParticipants$consistentChoice, na.rm = FALSE)

#Adding Infromation from Participant Dataset
choicePerScenarioParticipants <- merge(choicePerScenarioParticipants,participants[c("participantId","experimentalCondition","city","timeCounting")])

choicePerScenarioParticipantsConsistency <- subset(choicePerScenarioParticipants,is.na(consistentChoice) == FALSE)

#     b) Differences between estimations of average waiting and in-vehicles times  ---------------------------------


#       i) Plot of distribution of time estimates  --------------------------------------------------------------------

#         Themes and formatting ---------------------------------------------------

themePlotsDistributionTimeEstimationPaper1 <-  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                                     ,panel.background = element_blank(), axis.line = element_line(colour = "black")
                                                     ,panel.margin = unit(0, units="mm")
                                                     ,plot.margin = (unit(c(0.3, 0.3, 0.3, 0.3), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                                                     ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
                                                     ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                                                     ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                                                     ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                                                     ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                                                     ,legend.text.align = 0
                                                     # ,legend.title.align = 0
                                                     ,legend.title = element_text(size=18)
                                                     ,legend.background = element_rect(colour = 'black', fill = "transparent" )
                                                     ,legend.box.background = element_blank()
                                                     ,legend.key = element_rect(colour="black", fill = "transparent" )
                                                     ,legend.key.size= unit(1.3,"cm")
                                                     ,legend.text=element_text(size=18)
                                                     ,legend.position="bottom"
)

widthTimeEstimationPlots <- 20 #To consider space for legend
heightTimeEstimationPlots <- 20 #Originally 20
dpiTimeEstimationPlots <- 300


#         + Control ---------------------------------------------------------------

participantsControl <-subset(participants,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime) & experimentalCondition == "control") 

#Plot
plot(x=participantsControl$experiencedTravelTime, y= participantsControl$experiencedWaitingTime)

#Correlation between participants estimates of waiting and in-vehicle time 
cor(participantsControl$experiencedTravelTime, participantsControl$experiencedWaitingTime)

# Analysis without outliers
participantsControlWithoutOutliers <- subset(participantsControl,experiencedTravelTime < 15 & experiencedWaitingTime < 15)
dim(participantsControlWithoutOutliers)
plot(x= participantsControlWithoutOutliers$experiencedTravelTime, y= participantsControlWithoutOutliers$experiencedWaitingTime)
cor(participantsControlWithoutOutliers$experiencedTravelTime, participantsControlWithoutOutliers$experiencedWaitingTime)

#GGplot

#Average waiting and in-vehicle times
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeEstimationControl <- participantsControlWithoutOutliers[,c("experiencedTravelTime","experiencedWaitingTime")]

ggplot(dataPlotTimeEstimationControl, aes(x = experiencedTravelTime, y = experiencedWaitingTime)) +
  geom_point()+
  
  #Average waiting
  geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  
  #Average travel
  geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  
  # scale_color_manual(name = "Condition", values=c("lightgray", "black"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 14)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 14)) +
  labs(x = "Estimated in-vehicle time", y = "Estimated waiting time")+
  themePlotsDistributionTimeEstimationPaper1

ggsave(str_c("export","/figures/Paper1/DefriefQuestions/DistributionTimeEstimationControl.pdf")
       , width = 1.1*widthTimeEstimationPlots, height = heightTimeEstimationPlots , unit="cm", dpi=dpiTimeEstimationPlots)

#         + Treatment -------------------------------------------------------------

participantsTreatment <-subset(participants,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime) & experimentalCondition == "treatment") 
dim(participantsTreatment)
#Plot
plot(x=participantsTreatment$experiencedTravelTime, y= participantsTreatment$experiencedWaitingTime)

#Correlation between participants estimates of waiting and in-vehicle time 
cor(participantsTreatment$experiencedTravelTime, participantsTreatment$experiencedWaitingTime)

# Analysis without outliers
participantsTreatmentWithoutOutliers <- subset(participantsTreatment,experiencedTravelTime<15 & experiencedWaitingTime<15)
dim(participantsTreatmentWithoutOutliers)
plot(x= participantsTreatmentWithoutOutliers$experiencedTravelTime, y= participantsTreatmentWithoutOutliers$experiencedWaitingTime)
cor(participantsTreatmentWithoutOutliers$experiencedTravelTime, participantsTreatmentWithoutOutliers$experiencedWaitingTime)

# GGplot

#Average waiting and in-vehicle times
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeEstimationTreatment <- participantsTreatmentWithoutOutliers[,c("experiencedTravelTime","experiencedWaitingTime")]

ggplot(dataPlotTimeEstimationTreatment, aes(x = experiencedTravelTime, y = experiencedWaitingTime)) +
  geom_point()+
  
  #Average waiting
  geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  
  #Average travel
  geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  
  # scale_color_manual(name = "Condition", values=c("lightgray", "black"))+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimated in-vehicle time", y = "Estimated waiting time")+
  themePlotsDistributionTimeEstimationPaper1

ggsave(str_c("export","/figures/Paper1/DefriefQuestions/DistributionTimeEstimationTreatment.pdf")
       , width = 1.1*widthTimeEstimationPlots, height = heightTimeEstimationPlots , unit="cm", dpi=dpiTimeEstimationPlots)



#         + Control and treatment (Paper-plot) -------------------------------------------------

#Average waiting and in-vehicle times
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeEstimation <- rbind(cbind(dataPlotTimeEstimationControl,type = "Control"),cbind(dataPlotTimeEstimationTreatment,type = "Treatment"))

dim(dataPlotTimeEstimation)



ggplot(dataPlotTimeEstimation, aes(x = experiencedTravelTime, y = experiencedWaitingTime, color = type)) +
  geom_point(position = "jitter")+ #There are many points that overlap so I use the option "jitter"
  
  #Average waiting
  geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  
  #Average travel
  geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  # geom_text(aes(x=averageWaitingExperiment, label="w = 4.2", y=20), colour="blue", angle=90, text=element_text(size=11))+
  annotate("text", label = expression(bar(v)==5.5), x = averageTravelExperiment, y = 0, size = 6, colour = "black", vjust = -0.2,hjust = -0.1)+
  annotate("text", label = expression(bar(w)==4.6), x = 0, y = averageWaitingExperiment, size = 6, colour = "black", vjust = -0.2,hjust = -0.1)+
  scale_color_manual(name = "", values=c("gray", "black"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 12),breaks=seq(0,12,1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 12),breaks=seq(0,12,1)) +
  labs(x = "Estimated in-vehicle time [s]", y = "Estimated waiting time [s]")+
  themePlotsDistributionTimeEstimationPaper1

ggsave(str_c("export","/figures/Paper1/DefriefQuestions/DistributionTimeEstimation.pdf")
       , width = 1.1*widthTimeEstimationPlots, height = heightTimeEstimationPlots , unit="cm", dpi=dpiTimeEstimationPlots)

#Table with all cases

#Overestimation travel time (19/33 = 57.5%)
dim(subset(dataPlotTimeEstimation,experiencedTravelTime > averageTravelExperiment & experiencedWaitingTime>averageWaitingExperiment))
dim(subset(dataPlotTimeEstimation,experiencedTravelTime > averageTravelExperiment & experiencedWaitingTime<averageWaitingExperiment))

#Overestimation waiting time  (14/33 = 42.4%)
dim(subset(dataPlotTimeEstimation,experiencedTravelTime < averageTravelExperiment & experiencedWaitingTime>averageWaitingExperiment))
dim(subset(dataPlotTimeEstimation,experiencedTravelTime < averageTravelExperiment & experiencedWaitingTime<averageWaitingExperiment))




#       ii) Plot of Average estimates ----------------------------------------------------
#         Themes and formatting in plots ---------------------------------------------------

themePlotsAverageTimeEstimationPaper1 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                               ,panel.background = element_rect(fill = 'white', colour = 'black')
                                               
                                               ,axis.text.x=element_text(color = "black", size=18, hjust = 0.5, vjust = 0.4) #Text for experimental conditions
                                               # ,scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10))
                                               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                                               ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                                               ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                                               ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                                               ,panel.margin = unit(0, units="mm")
                                               # ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamaño de la imagen en que está contenido el gráfico
                                               ,plot.margin = (unit(c(0.3, 0.3, 0.3, 0.3), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                                               ,axis.ticks.margin=unit(1, units="mm")
                                               ,axis.ticks.length=unit(3, units="mm")
                                               ,axis.line = element_line(color = 'black')
                                               ,legend.position="bottom"
                                               # ,legend.margin=unit(-.05,"cm")
                                               ,legend.box.margin=margin(-22,0,0,0)
                                               ,legend.title=element_blank()
                                               ,legend.key = element_rect(colour="black")
                                               ,legend.key.size= unit(1.3,"cm")
                                               # ,legend.key.width=unit(0.6,"cm")
                                               ,legend.text=element_text(size=18)
                                               ,legend.spacing.x = unit(0.1, 'cm')
                                               # ,legend.margin=unit(2, units="cm")
                                               # ,legend.key = element_rect(size = 5),legend.key.width=unit(1,"cm"),legend.key.size=unit(1,"cm")
                                               ,legend.background = element_rect(colour = 'black',size = 0.3, fill = "white")
) 


#         - Control vs Treatment (Paper-plot) -------------------------------------------------

#Only participnats in London as they were the only that were asked to make time estimation
myData <- subset(participants,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime) & !is.na(experiencedWalkingTime)) 

#Select only relevant information
myData <- myData[,c("experiencedWaitingTime","experiencedTravelTime","experiencedWalkingTime","experimentalCondition")]

#Remove outliers
myData <- subset(myData, experiencedWaitingTime <= 10 & experiencedTravelTime<=10 )

#Rearrange information for plotting
myData <- melt(myData,measure.vars = c("experiencedWaitingTime","experiencedTravelTime","experiencedWalkingTime"))

#Rename variable associated to time attribute 
myData$variable <- factor(myData$variable, levels = c("experiencedWalkingTime","experiencedWaitingTime","experiencedTravelTime"),
                          labels = c("walking","waiting","travel")
)

#Preparing data for plotting
myData <- aggregate(myData$value,
                    by = list(experimentalCondition = myData$experimentalCondition,variable = myData$variable),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

#After this, we’ll need to do a little manipulation since the previous function returned matrices instead of vectors
myData <- do.call(data.frame, myData)

#And now let's compute the standard error for each group. We can then rename the columns just for ease of use.
myData$se <- myData$x.sd / sqrt(myData$x.n)

#Rename data for plotting
colnames(myData) <- c("experimentalCondition","timeAttribute","mean", "sd", "n", "se")

myData$timeAttribute <- factor(myData$timeAttribute, labels = c("Walking time","Waiting time","In-vehicle time"), levels = c("walking","waiting", "travel"))
myData$experimentalCondition <- factor(myData$experimentalCondition, labels = c("Control","Treatment"), levels = c("control","treatment"))

#Add bars with real means of waiting and in-vehicle times
nRoutesWalkingExperiment <- length(c(choiceScenarios$w1,choiceScenarios$w2))
nRoutesWaitingExperiment <- length(c(choiceScenarios$w1,choiceScenarios$w2))
nRoutesTravelExperiment <- length((c(choiceScenarios$v1,choiceScenarios$v2)))


#Average waiting and in-vehicle times
averageWalkingExperiment <- mean(experiencedLearningResponses$walkingTime, na.rm = TRUE)
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))


#SD waiting and in-vehicle times
sdWalkingExperiment <- sd(experiencedLearningResponses$walkingTime, na.rm = TRUE)
sdWaitingExperiment <- sd(c(choiceScenarios$w1,choiceScenarios$w2))
sdTravelExperiment <- sd(c(choiceScenarios$v1,choiceScenarios$v2))


#SE waiting and in-vehicle times
seWalkingExperiment <- sdWalkingExperiment/sqrt(nRoutesWalkingExperiment)
seWaitingExperiment <- sdWaitingExperiment/sqrt(nRoutesWaitingExperiment)
seTravelExperiment <- sdTravelExperiment/sqrt(nRoutesTravelExperiment)


myData <- rbind(myData
      ,data.frame(experimentalCondition = "Real", timeAttribute = "Walking time"
                  , mean = averageWalkingExperiment, sd = sdWalkingExperiment, n = 0, se = seWalkingExperiment)
      ,data.frame(experimentalCondition = "Real", timeAttribute = "Waiting time"
                  , mean = averageWaitingExperiment, sd = sdWaitingExperiment, n = 0, se = seWaitingExperiment)
      ,data.frame(experimentalCondition = "Real", timeAttribute = "In-vehicle time"
                  , mean = averageTravelExperiment, sd = sdTravelExperiment , n = 0, se = seTravelExperiment)
      )

#Plotting


myData$timeAttribute <- factor(myData$timeAttribute, labels = c("Walking Time  ","Waiting time  ","In-vehicle time")
                               , levels = c("Walking time", "Waiting time", "In-vehicle time"))
myData$experimentalCondition <- factor(myData$experimentalCondition, labels = c("Real","Control","Treatment"), levels = c("Real","Control","Treatment"))


limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)


interval_y <- 1
# max_y <- round((max(myData$mean)+interval_y)/10,0)*10
max_y <- 12


ggplot(data = myData, aes(x = factor(experimentalCondition), y = mean,fill = factor(timeAttribute), colour = "black"))+
  scale_y_continuous(limits=c(0,max_y),breaks=seq(0,max_y,interval_y),expand = c(0, 0))+
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),width = 0.25) +
  labs(x = "", y = "Average time estimate [s]")+ 
  scale_fill_manual(values=c("white","red","blue"))+ #Fill
  scale_color_manual(values=c("black","black","black")) +#Borders
  guides(colour=FALSE)+
  themePlotsAverageTimeEstimationPaper1

ggsave(str_c("export/figures/Paper1/DefriefQuestions/TimeEstimationControlTreatment.pdf")
       , width = 1.1*widthTimeEstimationPlots, height = heightTimeEstimationPlots, unit="cm", dpi=dpiTimeEstimationPlots)



#       iii) Regressions ----------------------------------------------------------------

#See Marteen class on controlling for the correlation between observations corresponding to the same participants
#At the end, we regress the differences in the estimates of waiting and in-vehicle times of each participant 
#         a) Difference between average waiting and travel time estimates ----------
#           - Data processing -------------------------------------------------------

dataANOVATimeEstimation <- participants[,c("participantId","experiencedWaitingTime","experiencedTravelTime","experimentalCondition")]
dataANOVATimeEstimation <- subset(dataANOVATimeEstimation,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime))
dataANOVATimeEstimation <- subset(dataANOVATimeEstimation,experiencedTravelTime < 15 & experiencedWaitingTime < 15)

#           - Estimation --------------------------------------------------------------
#             + Control ----------------------------------------------------------------

dataANOVATimeEstimationControl <- subset(dataANOVATimeEstimation,experimentalCondition == "control")

#Contrasts: -1 (waiting), +1 (travel)

#Capture the impact of the difference between the time estimation tests

# compute composite variable
dataANOVATimeEstimationControl$W2 <- with(dataANOVATimeEstimationControl,experiencedTravelTime-experiencedWaitingTime)/sqrt(2) # estimate model A
mod_A <- lm(W2~1,data=dataANOVATimeEstimationControl)
summary(mod_A)

# "estimate" model C
mod_C <- lm(W2~-1,data=dataANOVATimeEstimationControl) # -1 means no intercept 
summary(mod_C)
# compare to model C
anova(mod_C,mod_A)


#Test whether there are significant differences

mod <- lm(cbind(experiencedWaitingTime, experiencedTravelTime) ~ 1,data=dataANOVATimeEstimationControl)
#Set all combinations between the categorical variables (this allow to include interactions later). idata is the matrix with the contrast codes basically
idata <- data.frame(attribute=factor(c(1,2),labels=c("Waiting","Travel")))

contrasts(idata$attribute) <- contr.helmert(2) #Create contrast code for colour
# contrasts(idata$shape) <- contr.helmert(2) #Create contrast code for shape
timeEstimation_aov <- Anova(mod,idata=idata,idesign=~attribute,type=3) #Model estimation ANOVA repeated measures. 
summary(timeEstimation_aov,multivariate=FALSE) #Results are the same same as were shown in part a),b) and c)


#             + Treatment -------------------------------------------------------------

dataANOVATimeEstimationTreatment <- subset(dataANOVATimeEstimation,experimentalCondition == "treatment")

#Contrasts: -1 (waiting), +1 (travel)

#Capture the impact of the difference between the time estimation tests

# compute composite variable
dataANOVATimeEstimationTreatment$W2 <- with(dataANOVATimeEstimationTreatment,experiencedTravelTime-experiencedWaitingTime)/sqrt(2) # estimate model A
mod_A <- lm(W2~1,data=dataANOVATimeEstimationTreatment)
summary(mod_A)

# "estimate" model C
mod_C <- lm(W2~-1,data=dataANOVATimeEstimationTreatment) # -1 means no intercept 
summary(mod_C)
# compare to model C
anova(mod_C,mod_A)


#Test whether there are significant differences

mod <- lm(cbind(experiencedWaitingTime, experiencedTravelTime) ~ 1,data=dataANOVATimeEstimationTreatment)
#Set all combinations between the categorical variables (this allow to include interactions later). idata is the matrix with the contrast codes basically
idata <- data.frame(attribute=factor(c(1,2),labels=c("Waiting","Travel")))

contrasts(idata$attribute) <- contr.helmert(2) #Create contrast code for colour
# contrasts(idata$shape) <- contr.helmert(2) #Create contrast code for shape
timeEstimation_aov <- Anova(mod,idata=idata,idesign=~attribute,type=3) #Model estimation ANOVA repeated measures. 
summary(timeEstimation_aov,multivariate=FALSE) #Results are the same same as were shown in part a),b) and c)

#             + Control & Treatment  ---------------------------------------------------

#Using package AOV

dataAOVTimeEstimation <- participants[,c("participantId","experiencedWaitingTime","experiencedTravelTime","experimentalCondition")]
dataAOVTimeEstimation <- subset(dataAOVTimeEstimation,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime)) #Keep only observations with no NAs
dataAOVTimeEstimation <- subset(dataAOVTimeEstimation,experiencedTravelTime < 15 & experiencedWaitingTime < 15) #Remove outliners

dataAOVTimeEstimation <- melt(dataAOVTimeEstimation ,measure.vars = c("experiencedWaitingTime","experiencedTravelTime"))

#               * Mixed effect linear regression ------------------------------------------

#Mixed effect linear regression (structurally similar to repeated measure anova)

#Tutorial: https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf
#Documentation: https://cran.r-project.org/web/packages/lme4/lme4.pdf

library(lme4)

mod <- lmer(value~1+factor(variable)+factor(experimentalCondition) +(1|participantId),data= dataAOVTimeEstimation)
summary(mod)

coef.tbl <- function(fm){
  ## check that fm is an object of the "mer" class
  # stopifnot(is(fm, "glmer"))
  cc <- fixef(fm)
  ss <- sqrt(diag(vcov(fm)))
  return(data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc)))
}

modResults<- coef.tbl(fm=mod)

# mod$fixef


#export table 



#               * Repeated measures ANOVA (X) ------------------------------------------


#Tutorial: https://stats.idre.ucla.edu/r/seminars/repeated-measures-analysis-with-r/

factor(dataAOVTimeEstimation$variable)

demo3.aov <- aov(value ~ experimentalCondition +  factor(variable)+ Error(participantId), data = dataAOVTimeEstimation)
summary(demo3.aov)

#Regression

summary(lm(value~1+factor(variable)+factor(experimentalCondition), data = dataAOVTimeEstimation))



#a) ANOVA

#dataANOVATimeEstimation

mod <- lm(cbind(experiencedWaitingTime, experiencedTravelTime) ~ experimentalCondition,data=dataANOVATimeEstimation)
# idata <- data.frame(time = ordered(1:5))
idata <- data.frame(attribute=factor(c(1,2),labels=c("Waiting","Travel")))

#Contrasts: -1 (waiting), +1 (travel)
contrasts(idata$attribute)<- contr.helmert(2) 

head_aov <- Anova(mod,idata=idata,idesign=~attribute,data=dataANOVATimeEstimation,type=3)
summary(head_aov,multivariate=FALSE)

#b) Effect sizes

#i) Difference between the type of time estimation tests (waiting or travel) (within subjects effect)

dataANOVATimeEstimation$W2 <- with(dataANOVATimeEstimation,experiencedTravelTime-experiencedWaitingTime)/sqrt(2) # estimate model A
mod_A <- lm(W2~1,data=dataANOVATimeEstimation)
summary(mod_A)
#We have to rescale it as the regression was made with the composite variables(divide by square root of 2)
2*mod_A$coefficients/sqrt(2) 
#Because the contrast coding, the meaninf of beta is the difference between the mean of the experimental condition and the average of all participants
#Then, the coefficient needs to be multiplied by 2 in order to get the difference between the experimental conditions (1--1 = 2).

#ii) Difference between experimental conditions (between subjects effect)

dataANOVATimeEstimation$averageExperiencedTime <- with(dataANOVATimeEstimation,(experiencedWaitingTime + experiencedTravelTime)/2)
dataANOVATimeEstimation$experimentalConditionCCoding <- ifelse(dataANOVATimeEstimation$experimentalCondition == "control", -1, 1)

# summary(lm(averageExperiencedTime~1, data = dataANOVATimeEstimation))
summary(lm(averageExperiencedTime~1+experimentalConditionCCoding, data = dataANOVATimeEstimation))



#c) Analysis only for the effect of the time attribute estimated


# 
# # compute composite variable
# dataANOVATimeEstimation$W2 <- with(dataANOVATimeEstimation,experiencedTravelTime-experiencedWaitingTime)/sqrt(2) # estimate model A
# mod_A <- lm(W2~1,data=dataANOVATimeEstimation)
# summary(mod_A)
# 
# # "estimate" model C
# mod_C <- lm(W2~-1,data=dataANOVATimeEstimation) # -1 means no intercept 
# summary(mod_C)
# # compare to model C
# anova(mod_C,mod_A)


#Test whether there are significant differences

mod <- lm(cbind(experiencedWaitingTime, experiencedTravelTime) ~ 1,data=dataANOVATimeEstimation)
#Set all combinations between the categorical variables (this allow to include interactions later). idata is the matrix with the contrast codes basically
idata <- data.frame(attribute=factor(c(1,2),labels=c("Waiting","Travel")))

contrasts(idata$attribute) <- contr.helmert(2) #Create contrast code for colour
# contrasts(idata$shape) <- contr.helmert(2) #Create contrast code for shape
timeEstimation_aov <- Anova(mod,idata=idata,idesign=~attribute,type=3) #Model estimation ANOVA repeated measures. 
summary(timeEstimation_aov,multivariate=FALSE) #Results are the same same as were shown in part a),b) and c)

#         b) Difference between real vs estimated time (test difference in constrast to 0)  ----------------------------------------

#           - Data processing -------------------------------------------------------

dataANOVATimeEstimation <- participants[,c("participantId","experiencedWalkingTime","experiencedWaitingTime","experiencedTravelTime","experimentalCondition")]
dataANOVATimeEstimation <- subset(dataANOVATimeEstimation,!is.na(experiencedWaitingTime) & !is.na(experiencedTravelTime)) #Keep only observations with no NAs
dataANOVATimeEstimation <- subset(dataANOVATimeEstimation,experiencedTravelTime < 15 & experiencedWaitingTime < 15) #Remove outliners

dataANOVATimeEstimation$realWalkingTime <-  mean(experiencedLearningResponses$walkingTime, na.rm = TRUE) #averageWaitingExperiment
dataANOVATimeEstimation$realWaitingTime <- mean(c(choiceScenarios$w1,choiceScenarios$w2)) #averageWaitingExperiment
dataANOVATimeEstimation$realTravelTime <- mean(c(choiceScenarios$v1,choiceScenarios$v2)) #averageTravelExperiment
 
dataANOVATimeEstimation$differenceExperiencedVSRealWalkingTime <- with(dataANOVATimeEstimation,experiencedWalkingTime-realWalkingTime)
dataANOVATimeEstimation$differenceExperiencedVSRealWaitingTime <- with(dataANOVATimeEstimation,experiencedWaitingTime-realWaitingTime)
dataANOVATimeEstimation$differenceExperiencedVSRealTravelTime <- with(dataANOVATimeEstimation,experiencedTravelTime-realTravelTime)


dataAOVTimeEstimation <- melt(dataANOVATimeEstimation 
                              ,measure.vars = c("differenceExperiencedVSRealWalkingTime","differenceExperiencedVSRealWaitingTime","differenceExperiencedVSRealTravelTime"))

#           - Estimation (PAPER-TABLE) --------------------------------------------------------------

#Intercept refer to waiting time. It is expected to be non-significant.

mod <- lmer(value~1+factor(variable)+factor(experimentalCondition) +(1|participantId)
            ,data= subset(dataAOVTimeEstimation,variable %in% c("differenceExperiencedVSRealWaitingTime","differenceExperiencedVSRealTravelTime"))
)
summary(mod)

coef.tbl <- function(fm){
  ## check that fm is an object of the "mer" class
  # stopifnot(is(fm, "glmer"))
  cc <- fixef(fm)
  ss <- sqrt(diag(vcov(fm)))
  return(data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc)))
}

modResults<- coef.tbl(fm=mod)

# View(round(modResults,3))

#         c) Extra ------------------------------------------------------------------
#           - Logistic Regression (X) --------------------------------------------------
#           - Regressions -------------------------------------------------------------

#Information can be combined because of the result obtained with the time estimation bar plots 
# -> there are no difference in estimations across experimental conditions 

#             + Data preparation --------------------------------------------------------


participantsList <- subset(participants,experimentalCondition == "control")

timePerceptionData <- subset(participantsList,experiencedTravelTime<=10 & experiencedWaitingTime<=10 )

timeDistortedAverage <- choiceScenarios[,c("w1","v1","w2","v2")]

product <- function(vec){
  out <- 1
  for(i in 1:length(vec)){
    out <- out*vec[i]
  }
  out
}

constantRegressionWaitingTime <- 0.9*log(product(timeDistortedAverage$w1)*product(timeDistortedAverage$w2))
constantRegressionTravelTime <- 0.9*log(product(with(timeDistortedAverage,v1))*product(with(timeDistortedAverage,v2)))

timeRegressionWaitingTime <- data.frame(subjectiveTime = timePerceptionData$experiencedWaitingTime, participantId =  timePerceptionData$participantId)
timeRegressionWaitingTime$attribute <- "Waiting"
timeRegressionWaitingTime$objectiveTimeConstant <- constantRegressionWaitingTime

timeRegressionTravelTime <- data.frame(subjectiveTime = timePerceptionData$experiencedTravelTime, participantId =  timePerceptionData$participantId)
timeRegressionTravelTime$attribute <- "Travel"
timeRegressionTravelTime$objectiveTimeConstant <- constantRegressionTravelTime

dataRegressionTime<- rbind(timeRegressionWaitingTime,timeRegressionTravelTime)

dataRegressionTime$WaitingConstant <- ifelse(dataRegressionTime$attribute == "Waiting",1,0)
dataRegressionTime$TravelConstant <- ifelse(dataRegressionTime$attribute == "Travel",1,0)

dataRegressionTime$ObjectiveTimeConstant <- dataRegressionTime$objectiveTimeConstant/0.9 #To apply then the offset equal to 1



#             + Results with data from travel and waiting time estimations ----------------------------------------------------


summary(lm(log(experiencedTravelTime/experiencedWaitingTime)~1, data = subset(timePerceptionData,experimentalCondition == "control")))

diffavaw <- exp(0.44979-(constantRegressionTravelTime-constantRegressionWaitingTime))*16; diffavaw

#Improved regression


# summary(lm(log(subjectiveTime)~1+TravelConstant+objectiveTimeConstant, data = dataRegressionTime))#+WalkingConstant

# dataRegressionTime$subjectiveTimeAdj <- log(dataRegressionTime$subjectiveTime)-dataRegressionTime$objectiveTimeConstant
# summary(lm(subjectiveTimeAdj~1+TravelConstant+WaitingConstant, data = dataRegressionTime))#+WalkingConstant

#Two different regressions for waiting and travel or combined

#Waiting
modelWaitingTime <- lm(log(subjectiveTime)~1+offset(objectiveTimeConstant), data = subset(dataRegressionTime, attribute == "Waiting"))#+WalkingConstant

#Travel
modelTravelTime <- lm(log(subjectiveTime)~1+offset(objectiveTimeConstant), data = subset(dataRegressionTime, attribute == "Travel"))#+WalkingConstant

exp(as.numeric(modelWaitingTime$coefficients))/exp(as.numeric(modelTravelTime$coefficients))


#Combined regression
summary(lm(log(subjectiveTime)~1+TravelConstant+offset(objectiveTimeConstant), data = dataRegressionTime))#+WalkingConstant

exp(-3.17)*16 #16 is the number of scenarios (N)

exp(0)*16 #16 is the number of scenarios (N)

exp(-3.17)/exp(0) #The constant for travel time is 24 times greater

# #           + Travel, Waiting and Walking -------------------------------------------
# 
# #Regression with 3 different intercepts
# 
# constantRegressionWalkingTime<- 0.9*log((4.28)^(dim(timeDistortedAverage)[1])) #4.28 corresponde al tiempo promedio que lo
# constantRegressionWaitingTime<- 0.9*log(product(with(timeDistortedAverage,w1))*product(with(timeDistortedAverage,w2)))
# constantRegressionTravelTime <- 0.9*log(product(with(timeDistortedAverage,v1))*product(with(timeDistortedAverage,v2)))
# 
# 
# timeRegressionWalkingTime <- data.frame(subjectiveTime = timePerceptionData$experiencedWalkingTime, participantId =  timePerceptionData$participantId)
# timeRegressionWalkingTime$attribute <- "Walking"
# timeRegressionWalkingTime$objectiveTimeConstant <- constantRegressionWalkingTime
# 
# timeRegressionWaitingTime <- data.frame(subjectiveTime = timePerceptionData$experiencedWaitingTime, participantId =  timePerceptionData$participantId)
# timeRegressionWaitingTime$attribute <- "Waiting"
# timeRegressionWaitingTime$objectiveTimeConstant <- constantRegressionWaitingTime
# 
# timeRegressionTravelTime <- data.frame(subjectiveTime = timePerceptionData$experiencedTravelTime, participantId =  timePerceptionData$participantId)
# timeRegressionTravelTime$attribute <- "Travel"
# timeRegressionTravelTime$objectiveTimeConstant <- constantRegressionTravelTime
# 
# 
# # dataRegressionTime<- rbind(timeRegressionWalkingTime,timeRegressionWaitingTime,timeRegressionTravelTime)
# dataRegressionTime<- rbind(timeRegressionWaitingTime,timeRegressionTravelTime)
# 
# dataRegressionTime$WalkingConstant <- ifelse(dataRegressionTime$attribute == "Walking",1,0)
# dataRegressionTime$WaitingConstant <- ifelse(dataRegressionTime$attribute == "Waiting",1,0)
# dataRegressionTime$TravelConstant <- ifelse(dataRegressionTime$attribute == "Travel",1,0)
# 
# dataRegressionTime$ObjectiveTimeConstant <- dataRegressionTime$objectiveTimeConstant/0.9
# 
# summary(lm(log(subjectiveTime)~1+TravelConstant+offset(objectiveTimeConstant), data = dataRegressionTime))#+WalkingConstant
# 
# dataRegressionTime$subjectiveTimeAdj <- log(dataRegressionTime$subjectiveTime)-dataRegressionTime$objectiveTimeConstant
# 
# summary(lm(subjectiveTimeAdj~1+TravelConstant+WaitingConstant, data = dataRegressionTime))#+WalkingConstant
# 
# 
# exp(-3.17)*16
# 
# exp(-19.542)*16
# exp(-18.545)*16
# 
# #Now we capture correlation between the same participant responses. 
# library(lme4)
# 
# dataRegressionTime$participantId <- as.character(dataRegressionTime$participantId)
# 
# summary(lmer(subjectiveTimeAdj~1+TravelConstant+(1|participantId),data = dataRegressionTime))
# 
# summary(lmer(subjectiveTimeAdj~1+WaitingConstant+TravelConstant+(1|participantId),data = dataRegressionTime))

#           - Analysis Real Times and Perception Accuracy -----------------------------
#             + Control -----------------------------------------------------------------

participantsList <- subset(participants,experimentalCondition == "control")

#Waiting Time in experiment
mean(choiceResponses$averageWaitingTime)

#All
subset(participantsList)$experiencedWaitingTime
mean(subset(participantsList)$experiencedWaitingTime,na.rm = TRUE)
mean(subset(participantsList, experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)

#People passing the manipulation check in waiting time
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$realWaitingTime,na.rm = TRUE)
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced))& experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)
hist(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$experiencedWaitingTime)

#People not passing the manipulation check in waiting time
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$realWaitingTime,na.rm = TRUE)
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS1Experienced))& experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)
hist(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$experiencedWaitingTime)

#Travel Time in experiment
mean(choiceResponses$averageTravelTime)

#All
mean(subset(participantsList)$experiencedTravelTime,na.rm = TRUE)
mean(subset(participantsList, experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)

#People passing the manipulation check in travel time
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime,na.rm = TRUE)
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced))&experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)
hist(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime)

#People not passing the manipulation check in travel time
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$realTravelTime,na.rm = TRUE)
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced))& experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)
hist(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime)


#Average waiting and travel time 

mean(participantsList$realJourneyTime,na.rm = TRUE)
mean(participantsList$realTravelTime,na.rm = TRUE)
mean(participantsList$realWaitingTime,na.rm = TRUE)


hist(participantsList$realTravelTime)
hist(participantsList$realWaitingTime)


#             + Treatment -----------------------------------------------------------------

participantsList <- subset(participants,experimentalCondition == "treatment")

#Waiting Time in experiment
mean(choiceResponses$averageWaitingTime)

#All
subset(participantsList)$experiencedWaitingTime
mean(subset(participantsList)$experiencedWaitingTime,na.rm = TRUE)
mean(subset(participantsList, experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)

#People passing the manipulation check in waiting time
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$realWaitingTime,na.rm = TRUE)
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced))& experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)
hist(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$experiencedWaitingTime)

#People not passing the manipulation check in waiting time
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS1Experienced)))$realWaitingTime,na.rm = TRUE)
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS1Experienced))& experiencedWaitingTime<=10 )$experiencedWaitingTime,na.rm = TRUE)

'%!in%' <- function(x,y)!('%in%'(x,y))
# hist(subset(participantsList,(participantId %!in% str_c("P",listRationalParticipantsS1Experienced)))$experiencedWaitingTime)

#Travel Time in experiment
mean(choiceResponses$averageTravelTime)

#All
mean(subset(participantsList)$experiencedTravelTime,na.rm = TRUE)
mean(subset(participantsList, experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)

#People passing the manipulation check in travel time
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime,na.rm = TRUE)
mean(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced))&experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)
hist(subset(participantsList,(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime)

#People not passing the manipulation check in travel time
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$realTravelTime,na.rm = TRUE)
mean(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced))& experiencedTravelTime<=10 )$experiencedTravelTime,na.rm = TRUE)
hist(subset(participantsList,!(participantId %in% str_c("P",listRationalParticipantsS2Experienced)))$experiencedTravelTime)


participantsList <- subset(participants)

TimePerceptionData <- subset(participantsList,experiencedTravelTime<=10 & experiencedWaitingTime<=10 )




timeDistortedAverage <- unique(choiceScenarios[,c("w1","v1","w2","v2")])

# timeDistortedAverage <- timeDistortedAverage[1:4,]

product <- function(vec){
  out <- 1
  for(i in 1:length(vec)){
    out <- out*vec[i]
  }
  out
}

# constant<- 0.9*log(mean(choiceResponses$averageTravelTime)/mean(choiceResponses$averageWaitingTime));constant

constantRegressionWalkingTime<- 0.9*log((4.28)^(dim(timeDistortedAverage)[1]))
constantRegressionWaitingTime<- 0.9*log(product(with(timeDistortedAverage,w1))*product(with(timeDistortedAverage,w2)))
constantRegressionTravelTime <- 0.9*log(product(with(timeDistortedAverage,v1))*product(with(timeDistortedAverage,v2)))

summary(lm(log(experiencedTravelTime/experiencedWaitingTime)~1, data = subset(TimePerceptionData,experimentalCondition == "control")))

diffavaw <- exp(0.44979-(constantRegressionTravelTime-constantRegressionWaitingTime))*16; diffavaw

#Regression with 3 different intercepts

TimePerceptionData$participantId
TimePerceptionData$participantId

timeRegressionWalkingTime <- data.frame(subjectiveTime = TimePerceptionData$experiencedWalkingTime, participantId =  TimePerceptionData$participantId)
timeRegressionWalkingTime$attribute <- "Walking"
timeRegressionWalkingTime$objectiveTimeConstant <- constantRegressionWalkingTime

timeRegressionWaitingTime <- data.frame(subjectiveTime = TimePerceptionData$experiencedWaitingTime, participantId =  TimePerceptionData$participantId)
timeRegressionWaitingTime$attribute <- "Waiting"
timeRegressionWaitingTime$objectiveTimeConstant <- constantRegressionWaitingTime

timeRegressionTravelTime <- data.frame(subjectiveTime = TimePerceptionData$experiencedTravelTime, participantId =  TimePerceptionData$participantId)
timeRegressionTravelTime$attribute <- "Travel"
timeRegressionTravelTime$objectiveTimeConstant <- constantRegressionTravelTime


# dataRegressionTime<- rbind(timeRegressionWalkingTime,timeRegressionWaitingTime,timeRegressionTravelTime)
dataRegressionTime<- rbind(timeRegressionWaitingTime,timeRegressionTravelTime)

dataRegressionTime$WalkingConstant <- ifelse(dataRegressionTime$attribute == "Walking",1,0)
dataRegressionTime$WaitingConstant <- ifelse(dataRegressionTime$attribute == "Waiting",1,0)
dataRegressionTime$TravelConstant <- ifelse(dataRegressionTime$attribute == "Travel",1,0)

dataRegressionTime$ObjectiveTimeConstant <- dataRegressionTime$objectiveTimeConstant/0.9

summary(lm(log(subjectiveTime)~1+TravelConstant+offset(objectiveTimeConstant), data = dataRegressionTime))#+WalkingConstant

dataRegressionTime$subjectiveTimeAdj <- log(dataRegressionTime$subjectiveTime)-dataRegressionTime$objectiveTimeConstant

summary(lm(subjectiveTimeAdj~1+TravelConstant+WaitingConstant, data = dataRegressionTime))#+WalkingConstant

exp(-19.542)*16
exp(-18.545)*16

#Now we capture correlation between the same participant responses. 
library(lme4)

dataRegressionTime$participantId <- as.character(dataRegressionTime$participantId)

summary(lmer(subjectiveTimeAdj~1+TravelConstant+(1|participantId),data = dataRegressionTime))

# summary(lmer(subjectiveTimeAdj~1+WaitingConstant+TravelConstant+(1|participantId),data = dataRegressionTime))








#   2) STATED VALUATION ---------------------------------------------------------
#     a) Data formatting -------------------------------------------------------

participantsTimeValuation <- subset(participants,is.na(waitingTimeImportance) ==FALSE)
participantsTimeValuation <- participantsTimeValuation[c("participantId","waitingTimeImportance","travelTimeImportance","journeyTimeImportance","experimentalCondition")]

#     b) Descriptive  statistics ----------------------------------------------

#       - Averages --------------------------------------------------------------

averageTimeValuationByCondition <- participantsTimeValuation %>%
  group_by(experimentalCondition) %>%
  summarise_each(funs(mean(.,na.rm = TRUE)),waitingTimeImportance,travelTimeImportance,journeyTimeImportance)

averageTimeValuationByCondition 

averageTimeValuation <- participantsTimeValuation %>%
  group_by(experimentalCondition) %>%
  summarise_at(vars(waitingTimeImportance,travelTimeImportance),funs(mean(.,na.rm = TRUE)))

averageTimeValuation 

sum(with(participants,waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/72

sum(with(subset(participantsTimeValuation,experimentalCondition == "control"),waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/36
sum(with(subset(participantsTimeValuation,experimentalCondition == "treatment"),waitingTimeImportance+travelTimeImportance),na.rm = TRUE)/36
#       - Discrete  ---------------------------------------------------------------

participantsTimeValuation$preferenceWT <- ifelse(with(participantsTimeValuation,waitingTimeImportance>travelTimeImportance),"w","t")
participantsTimeValuation$preferenceW <- ifelse(participantsTimeValuation$preferenceWT == "w",1,0)
participantsTimeValuation$preferenceT <- ifelse(participantsTimeValuation$preferenceWT == "t",1,0)


timePreferencesByCondition <- participantsTimeValuation %>%
  group_by(experimentalCondition) %>%
  summarise_at(vars(preferenceW,preferenceT),funs(sum(.,na.rm = TRUE)))

timePreferencesByCondition

table(participantsTimeValuation$preferenceWT)






#     c) Plot of distribution of time valuations  --------------------------------------------------------------------

#       Themes and formatting ---------------------------------------------------

themePlotsDistributionTimeValuationPaper1 <-  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                                     ,panel.background = element_blank(), axis.line = element_line(colour = "black")
                                                     ,panel.margin = unit(0, units="mm")
                                                     ,plot.margin = (unit(c(0.3, 0.3, 0.3, 0.3), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                                                     ,axis.text.x=element_text(color = "black", size=16, hjust = 0.5, vjust = 0.4)
                                                     ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                                                     ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                                                     ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                                                     ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                                                     ,legend.text.align = 0
                                                     # ,legend.title.align = 0
                                                     ,legend.title = element_text(size=18)
                                                     ,legend.background = element_rect(colour = 'black', fill = "transparent" )
                                                     ,legend.box.background = element_blank()
                                                     ,legend.key = element_rect(colour="black", fill = "transparent" )
                                                     ,legend.key.size= unit(1.3,"cm")
                                                     ,legend.text=element_text(size=18)
                                                     ,legend.position="bottom"
)

widthTimeValuationPlots <- 20 #To consider space for legend
heightTimeValuationPlots <- 20 #Originally 20
dpiTimeValuationPlots <- 300


#       + Control ---------------------------------------------------------------

participantsControl <-subset(participants,experimentalCondition == "control") 

#Plot
plot(x=participantsControl$waitingTimeImportance, y= participantsControl$travelTimeImportance)

#Correlation between participants estimates of waiting and in-vehicle time 
cor(participantsControl$travelTimeImportance, participantsControl$waitingTimeImportance)

# Analysis without outliers
participantsControlWithoutOutliers <- subset(participantsControl)
dim(participantsControlWithoutOutliers)
plot(x= participantsControlWithoutOutliers$travelTimeImportance, y= participantsControlWithoutOutliers$waitingTimeImportance)
cor(participantsControlWithoutOutliers$travelTimeImportance, participantsControlWithoutOutliers$waitingTimeImportance)

#GGplot

#Average waiting and in-vehicle times
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeValuationControl <- participantsControlWithoutOutliers[,c("travelTimeImportance","waitingTimeImportance")]

ggplot(dataPlotTimeValuationControl, aes(x = travelTimeImportance, y = waitingTimeImportance)) +
  geom_point()+
  
  geom_abline(slope =1, intercept = 0,color = "black", linetype="dashed")+
  # #Average waiting
  # geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  # 
  # #Average travel
  # geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  
  # scale_color_manual(name = "Condition", values=c("lightgray", "black"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  labs(x = "Estimated in-vehicle time", y = "Estimated waiting time")+
  themePlotsDistributionTimeValuationPaper1

# ggsave(str_c("export","/figures/Paper1/TimeValuation/DistributionTimeValuationControl.pdf")
#        , width = 1.1*widthTimeValuationPlots, height = heightTimeValuationPlots , unit="cm", dpi=dpiTimeValuationPlots)

#       + Treatment -------------------------------------------------------------

participantsTreatment <-subset(participants,experimentalCondition == "treatment") 

#Plot
plot(x=participantsTreatment$waitingTimeImportance, y= participantsTreatment$travelTimeImportance)

#Correlation between participants estimates of waiting and in-vehicle time 
cor(participantsTreatment$travelTimeImportance, participantsTreatment$waitingTimeImportance)

# Analysis without outliers
participantsTreatmentWithoutOutliers <- subset(participantsTreatment)
dim(participantsTreatmentWithoutOutliers)
plot(x= participantsTreatmentWithoutOutliers$travelTimeImportance, y= participantsTreatmentWithoutOutliers$waitingTimeImportance)
cor(participantsTreatmentWithoutOutliers$travelTimeImportance, participantsTreatmentWithoutOutliers$waitingTimeImportance)

#GGplot

#Average waiting and in-vehicle times
averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeValuationTreatment <- participantsTreatmentWithoutOutliers[,c("travelTimeImportance","waitingTimeImportance")]

ggplot(dataPlotTimeValuationTreatment, aes(x = travelTimeImportance, y = waitingTimeImportance)) +
  geom_point()+
  
  geom_abline(slope =1, intercept = 0,color = "black", linetype="dashed")+
  # #Average waiting
  # geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  # 
  # #Average travel
  # geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  
  # scale_color_manual(name = "Condition", values=c("lightgray", "black"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  labs(x = "Estimated in-vehicle time", y = "Estimated waiting time")+
  themePlotsDistributionTimeValuationPaper1


#       + Control and treatment (Paper-plot) -------------------------------------------------

#Average waiting and in-vehicle times
# averageWaitingExperiment <- mean(c(choiceScenarios$w1,choiceScenarios$w2))
# averageTravelExperiment <- mean(c(choiceScenarios$v1,choiceScenarios$v2))

dataPlotTimeValuation <- rbind(cbind(dataPlotTimeValuationControl,type = "Control"),cbind(dataPlotTimeValuationTreatment,type = "Treatment"))

dim(dataPlotTimeValuation)



ggplot(dataPlotTimeValuation, aes(x = travelTimeImportance, y = waitingTimeImportance, color = type)) +
  geom_point(position = "jitter")+ #There are many points that overlap so I use the option "jitter"
  
  # #Average waiting
  # geom_hline(yintercept = averageWaitingExperiment,color = "black", linetype="dashed")+
  # 
  # #Average travel
  # geom_vline(xintercept = averageTravelExperiment,color = "black", linetype="dashed")+
  # geom_text(aes(x=averageWaitingExperiment, label="w = 4.2", y=20), colour="blue", angle=90, text=element_text(size=11))+
  # annotate("text", label = expression(bar(v)==5.5), x = averageTravelExperiment, y = 0, size = 6, colour = "black", vjust = -0.2,hjust = -0.1)+
  # annotate("text", label = expression(bar(w)==4.6), x = 0, y = averageWaitingExperiment, size = 6, colour = "black", vjust = -0.2,hjust = -0.1)+
  scale_color_manual(name = "", values=c("gray", "black"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  labs(x = "Estimated in-vehicle time [s]", y = "Estimated waiting time [s]")+
  themePlotsDistributionTimeValuationPaper1

ggsave(str_c("export","/figures/Paper1/TimeValuation/DistributionTimeValuation.pdf")
       , width = 1.1*widthTimeValuationPlots, height = heightTimeValuationPlots , unit="cm", dpi=dpiTimeValuationPlots)

#Table with all cases

#Overestimation travel time (19/33 = 57.5%)
dim(subset(dataPlotTimeEstimation,experiencedTravelTime > averageTravelExperiment & experiencedWaitingTime>averageWaitingExperiment))
dim(subset(dataPlotTimeEstimation,experiencedTravelTime > averageTravelExperiment & experiencedWaitingTime<averageWaitingExperiment))

#Overestimation waiting time  (14/33 = 42.4%)
dim(subset(dataPlotTimeEstimation,experiencedTravelTime < averageTravelExperiment & experiencedWaitingTime>averageWaitingExperiment))
dim(subset(dataPlotTimeEstimation,experiencedTravelTime < averageTravelExperiment & experiencedWaitingTime<averageWaitingExperiment))




#     d) Plot of average valuation ----------------------------------------------------
#       Themes and formatting in plots ---------------------------------------------------

themePlotsAverageTimeValuationPaper1 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                               ,panel.background = element_rect(fill = 'white', colour = 'black')
                                               
                                               ,axis.text.x=element_text(color = "black", size=18, hjust = 0.5, vjust = 0.4) #Text for experimental conditions
                                               # ,scale_x_continuous(limits=c(0,100),breaks=seq(0,100,10))
                                               ,axis.text.y=element_text(color = "black", size=16,hjust = 0.5, vjust = 0.4, angle = 0) 
                                               ,axis.title.x=element_text(size=20,vjust = -0.5,hjust = 0.5)
                                               ,axis.title.y=element_text(size=20,vjust = 2,hjust = 0.5)
                                               ,plot.title = element_text(size = 22,vjust = 4,hjust = 0.5) 
                                               ,panel.margin = unit(0, units="mm")
                                               # ,plot.margin = (unit(c(.5, .5, 2, 2), "cm")) #Tamaño de la imagen en que está contenido el gráfico
                                               ,plot.margin = (unit(c(0.3, 0.3, 0.3, 0.3), "cm")) #Tamaño de la imagen en que está contenido el gráfico            
                                               ,axis.ticks.margin=unit(1, units="mm")
                                               ,axis.ticks.length=unit(3, units="mm")
                                               ,axis.line = element_line(color = 'black')
                                               ,legend.position="bottom"
                                               # ,legend.margin=unit(-.05,"cm")
                                               ,legend.box.margin=margin(-22,0,0,0)
                                               ,legend.title=element_blank()
                                               ,legend.key = element_rect(colour="black")
                                               ,legend.key.size= unit(1.3,"cm")
                                               # ,legend.key.width=unit(0.6,"cm")
                                               ,legend.text=element_text(size=18)
                                               ,legend.spacing.x = unit(0.1, 'cm')
                                               # ,legend.margin=unit(2, units="cm")
                                               # ,legend.key = element_rect(size = 5),legend.key.width=unit(1,"cm"),legend.key.size=unit(1,"cm")
                                               ,legend.background = element_rect(colour = 'black',size = 0.3, fill = "white")
) 


#       - Control vs Treatment (Paper-plot) -------------------------------------------------

#Only participnats in London as they were the only that were asked to make time estimation
myData <- subset(participants,!is.na(waitingTimeImportance) & !is.na(travelTimeImportance) ) 

#Select only relevant information
myData <- myData[,c("waitingTimeImportance","travelTimeImportance","journeyTimeImportance","experimentalCondition")]

# #Remove outliers
# myData <- subset(myData, waitingTimeImportance <= 10 & travelTimeImportance<=10 )

#Rearrange information for plotting
myData <- melt(myData,measure.vars = c("waitingTimeImportance","travelTimeImportance","journeyTimeImportance"))

#Rename variable associated to time attribute 
myData$variable <- factor(myData$variable, levels = c("journeyTimeImportance","waitingTimeImportance","travelTimeImportance"),
                          labels = c("journey","waiting","travel")
)

#Preparing data for plotting
myData1 <- aggregate(myData$value,
                    by = list(experimentalCondition = myData$experimentalCondition,variable = myData$variable),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

#After this, we’ll need to do a little manipulation since the previous function returned matrices instead of vectors
myData1 <- do.call(data.frame, myData1)

#And now let's compute the standard error for each group. We can then rename the columns just for ease of use.
myData1$se <- myData1$x.sd / sqrt(myData1$x.n)

#Rename data for plotting
colnames(myData1) <- c("experimentalCondition","timeAttribute","mean", "sd", "n", "se")

myData1$timeAttribute <- factor(myData1$timeAttribute, labels = c("Journey time", "Waiting time","In-vehicle time"), levels = c("journey","waiting", "travel"))
myData1$experimentalCondition <- factor(myData1$experimentalCondition, labels = c("Control","Treatment"), levels = c("control","treatment"))

#- Without aggregating by experimental condition 

#Preparing data for plotting
myData2 <- aggregate(myData$value,
                    by = list(variable = myData$variable),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

#After this, we’ll need to do a little manipulation since the previous function returned matrices instead of vectors
myData2 <- do.call(data.frame, myData2)

#And now let's compute the standard error for each group. We can then rename the columns just for ease of use.
myData2$se <- myData2$x.sd / sqrt(myData2$x.n)

#Rename data for plotting
colnames(myData2) <- c("timeAttribute","mean", "sd", "n", "se")

myData2 <- cbind(experimentalCondition = "Both",myData2)

myData2$timeAttribute <- factor(myData2$timeAttribute, labels = c("Journey time", "Waiting time","In-vehicle time"), levels = c("journey","waiting", "travel"))
#- Combine both dataframes

myData <- rbind(myData1, myData2)

#Plotting

myData$timeAttribute <- factor(myData$timeAttribute, labels = c("Journey time  ", "Waiting time  ","In-vehicle time")
                               , levels = c("Journey time","Waiting time", "In-vehicle time"))
myData$experimentalCondition <- factor(myData$experimentalCondition, labels = c("Both Conditions","Control","Treatment"), levels = c("Both","Control","Treatment"))


limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)


interval_y <- 10
# max_y <- round((max(myData$mean)+interval_y)/10,0)*10
max_y <- 105


ggplot(data = myData, aes(x = factor(experimentalCondition), y = mean,fill = factor(timeAttribute), colour = "black"))+
  scale_y_continuous(limits=c(0,max_y),breaks=seq(0,max_y,interval_y),expand = c(0, 0))+
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),width = 0.25) +
  labs(x = "", y = "Average importance of time attribute [0-100]")+ 
  scale_fill_manual(values=c("gray","red","blue"))+ #Fill
  scale_color_manual(values=c("black","black","black")) +#Borders
  guides(colour=FALSE)+
  themePlotsAverageTimeEstimationPaper1

ggsave(str_c("export/figures/Paper1/DefriefQuestions/TimeValuationControlTreatment.pdf")
       , width = 1.1*widthTimeEstimationPlots, height = heightTimeEstimationPlots, unit="cm", dpi=dpiTimeEstimationPlots)






#     e) Regression -----------------------------------------------------------

#       - Data Processing -------------------------------------------------------

dataAOVTimeValuation <- participants[,c("participantId","waitingTimeImportance","travelTimeImportance","experimentalCondition")]
dataAOVTimeValuation <- subset(dataAOVTimeValuation,!is.na(waitingTimeImportance) & !is.na(travelTimeImportance)) #Keep only observations with no NAs

tempWaiting <- cbind(dataAOVTimeValuation[,c("participantId","waitingTimeImportance","experimentalCondition")],attribute = "waiting")

setnames(tempWaiting, old = c("participantId","waitingTimeImportance","experimentalCondition","attribute")
         , new = c("participantId","importance","experimentalCondition","attribute"))

tempTravel <- cbind(dataAOVTimeValuation[,c("participantId","travelTimeImportance","experimentalCondition")],attribute = "travel")

setnames(tempTravel, old = c("participantId","travelTimeImportance","experimentalCondition","attribute")
         , new = c("participantId","importance","experimentalCondition","attribute"))


dataAOVTimeValuation <- rbind(tempWaiting,tempTravel)




#       - Estimation (PAPER-TABLE) --------------------------------------------------------------

#Intercept refer to in-vehicle time. 
# 
# mod <- lmer(importance~1+factor(attribute == "waiting")+factor(experimentalCondition)*factor(attribute == "waiting") + (1|participantId)
#             ,data= dataAOVTimeValuation)
# 
# summary(mod)

mod <- lmer(importance~1+factor(attribute == "waiting") +factor(experimentalCondition)+ (1|participantId)
            ,data= dataAOVTimeValuation)

summary(mod)

coef.tbl <- function(fm){
  ## check that fm is an object of the "mer" class
  # stopifnot(is(fm, "glmer"))
  cc <- fixef(fm)
  ss <- sqrt(diag(vcov(fm)))
  return(data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc)))
}

modResults<- coef.tbl(fm=mod)

modResults
# View(round(modResults,3))

#       - Contrast coding ---------------------------------------------

dataAOVTimeValuation$attribute_contrast = ifelse(dataAOVTimeValuation$attribute == "waiting",-1,1)
dataAOVTimeValuation$condition_contrast = ifelse(dataAOVTimeValuation$experimentalCondition == "control",-1,1)

mod <- lmer(importance~1+attribute_contrast+ condition_contrast + (1|participantId)
            ,data= dataAOVTimeValuation)

mod <- lmer(importance~1+factor(attribute == "waiting")+ (1|participantId)
            ,data= dataAOVTimeValuation, contrasts = list(attribute = contr.Helmert))

summary(mod)

coef.tbl <- function(fm){
  ## check that fm is an object of the "mer" class
  # stopifnot(is(fm, "glmer"))
  cc <- fixef(fm)
  ss <- sqrt(diag(vcov(fm)))
  return(data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc)))
}

modResults<- coef.tbl(fm=mod)

modResults
# View(round(modResults,3))




