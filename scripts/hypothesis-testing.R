# 1) Choice Proportions Tests ----------------------------------------------

#   a) Chi-Square test (Intra-scenario comparisons) ------------------------------------------
#     + Tutorials ------------------------------------------------------------
#Commands R Studio: https://rcompanion.org/rcompanion/b_03.html
#Theorethical Insights: http://www.biostathandbook.com/chigof.html

#Null hypohtesis: The expected proportion is equal to what was obtained.

#chisq Function pchisq(q = 6,df = 2) 

#inverse Function: qchisq(.95, df=7)

qchisq(.95, df=1) #Our case
qchisq(.975, df=2)
sqrt(qchisq(.99, df=2))


#     + Functions ------------------------------------------------------------


chisqValueTerm <- function(observedFrequency,expectedFrequency){
  
  return ((observedProportion-expectedProportion)^2/expectedProportion)
  
}

#Df is set at 1 as I will be always comparing propotion between a binary outcome
chisqValue <- function(observedProportion,expectedProportion,df = 1){
  
  df <- df
  observedProportionOtherBinaryOutcome = 1-observedProportion
  
  return(chisqValueTerm(observedProportion=observedProportion, expectedProportion=0.5) + chisqValueTerm(observedProportion=observedProportionOtherBinaryOutcome, expectedProportion=0.5))
  
}

multipleChiSquareTest <- function(data,experimentType=list("NA"), experimentalCondition=list("NA"), country=list("NA"), scenarios =list("NA")){
  
  data <- data
  experimentType <- experimentType
  experimentalCondition <- experimentalCondition
  country <- country
  scenarios <- scenarios
  
  #The analysis is not dissagregated by scenario
  if(length(scenarios) == 0){ 
    scenarios <- NA
  }
  
  #If scenarios is "all" (i.e. not indiviudal scenarios are specified by the user), apply the function over each of the 14 scenarios
  if(scenarios == "all"){ 
    scenarios <- as.list(scenario.id(seq(1,14)))
  }
  
  #If experiment type is not specified by the user, the function is applied on the two types of experiment
  if(length(experimentType) == 0){ 
    experimentType <- NA
  }
  
  #If experimentType is "all" (i.e. not indiviudal expTypes are specified by the user), apply the function over the two experiment types
  if(experimentType == "all"){ 
    experimentType <- list("descriptive","experienced")
  }
  
  #If experimental condition is not specified by the user, apply the function over all the experimental conditions
  if(length(experimentalCondition) == 0){ 
    experimentalCondition <- NA
  }
  
  #If experimental condition is "all" (i.e. not indiviudal experimentalCondition are specified by the user), apply the function over the two experimental conditions
  if(experimentalCondition == "all"){ 
    experimentalCondition <- list("control","treatment")
  }
  
  #If country is not specified by the user, apply the function over all the experimental conditions
  if(length(country) == 0){ 
    country <- NA
  }
  
  #If country is "all" (i.e. not individual countries are specified by the user), apply the function over the two countries
  if(country == "all"){ 
    country <- list("Chile","UK")
  }
  
  chiSquaresTestsScenarios <- data.frame(scenario = NA,chisqValue = NA, pvalue = NA)
  # chiSquaresTestsScenarios <- list()
  
  for(nScenario in 1:length(scenarios)){
    
    scenarioId <- scenario.id(nScenario)
    myData <- subset(data,scenario == scenarioId)
    observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
    expectedRouteChoices = c(0.5,0.5)
    
    chisqTestValue <- chisq.test(x = observedRouteChoices,p = expectedRouteChoices)
    chiSquaresTestsScenariosTemp<- data.frame(scenario = NA,chisqValue = NA, pvalue = NA)
    chiSquaresTestsScenariosTemp$scenario <- scenarioId
    chiSquaresTestsScenariosTemp$pvalue <- as.numeric(round(chisqTestValue$p.value,2))
    chiSquaresTestsScenariosTemp$chisqValue <- as.numeric(chisqTestValue$statistic)
    chiSquaresTestsScenarios <- rbind(chiSquaresTestsScenarios,chiSquaresTestsScenariosTemp)
    
    # chiSquaresTestsScenarios[nScenario] <- list(pvalue = chisqTestValue$p.value, chiValue = chisqTestValue$statistic)
  }
  chiSquaresTestsScenarios <- chiSquaresTestsScenarios[2:dim(chiSquaresTestsScenarios)[1],]
  row.names(chiSquaresTestsScenarios) <- seq(1,dim(chiSquaresTestsScenarios)[1])
  return(chiSquaresTestsScenarios)
  
}

multipleChiSquareTest(data = subset(choiceResponses,experimentType == "experienced"), scenarios = "all")



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

#     + Manipulation Checks -----------------------------------------------
#       -S01: Dominated Alternative Travel Time, Scenario 01 (Reject)  ---------------------------------------------------------------------


#       -S02: Dominated Alternative Waiting Time, Scenario 02  (Reject)  ---------------------------------------------------------------------

myData <- subset(choiceResponses,scenario %in% c("02") & experimentType == "experienced")
observedRouteChoices <-  c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
# observedRouteChoices <- observedRouteChoices/sum(observedRouteChoices)
expectedRouteChoices = c(0.5,0.5)


chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#   b) McNemar test ----------------------------------------------------------

#     + Tutorials -------------------------------------------------------------
#To compare the choice propotions of aggregated samples. 

#Referencia: Tesis Mitzu

#Commands in R: https://cran.r-project.org/web/packages/exact2x2/vignettes/exactMcNemar.pdf

#     + Functions -------------------------------------------------------------

# data = dataH1B
# idScenarioControl = "03"
# idScenarioTreatment = "04"
McNemarTest<- function(data,idScenarioControl,idScenarioTreatment){
  
  data$choice <- factor(data$choice,levels = c("rational","irrational"))

  dataControl <- subset(data,scenario %in% c(idScenarioControl))
  dataControl <- dataControl[c("participantId","choice")]
  dataControl <- plyr::rename(dataControl,c(choice="choiceControl"))
  
  
  dataTreatment <- subset(data,scenario %in% c(idScenarioTreatment))
  dataTreatment <- dataTreatment[c("participantId","choice")]
  dataTreatment <- plyr::rename(dataTreatment,c(choice="choiceTreatment"))
  
  dataMcNemar <- merge(dataControl,dataTreatment, by = "participantId")
  rm(dataControl); rm(dataTreatment)
  
  dataMcNemar$choiceCode <- NA
  dataMcNemar$choiceCode[with(dataMcNemar,choiceControl== "rational" & choiceTreatment== "rational")] <- "RR"
  dataMcNemar$choiceCode[with(dataMcNemar,choiceControl== "rational" & choiceTreatment== "irrational")] <- "RI"
  dataMcNemar$choiceCode[with(dataMcNemar,choiceControl== "irrational" & choiceTreatment== "rational")] <- "IR"
  dataMcNemar$choiceCode[with(dataMcNemar,choiceControl== "irrational" & choiceTreatment== "irrational")] <- "II"
  
  contingencyTableMcNemar <- table(dataMcNemar$choiceCode)
  matrixMcNemar <- matrix(c(contingencyTableMcNemar["RR"],contingencyTableMcNemar["IR"],contingencyTableMcNemar["RI"],contingencyTableMcNemar["II"]),2,2)
  matrixMcNemarLatex <- matrix(c("Control","Rational","Irrational","Rational",contingencyTableMcNemar["RR"],contingencyTableMcNemar["IR"],"Irrational",contingencyTableMcNemar["RI"],contingencyTableMcNemar["II"]),3,3)
  tableMcNemarTest<- mcnemar.test(matrixMcNemar)
  
  
  return(list(contingencyTableMcNemar = contingencyTableMcNemar, matrixMcNemar= matrixMcNemarLatex,tableMcNemarTest = tableMcNemarTest, data = dataMcNemar))
  
}


# #   c) Cochran's Q test ----------------------------------------------------------
# # A generalization of the McNemar test when there are more than two groups of related samples
# #https://en.wikipedia.org/wiki/Cochran%27s_Q_test
# #Documentation: https://cran.r-project.org/web/packages/RVAideMemoire/RVAideMemoire.pdf
# 
# #Example video with R studio:  https://www.r-bloggers.com/cochran-q-test-for-k-related-samples-in-r/
# #Doocument of the example: http://capone.mtsu.edu/dwalsh/6604/6604COCH.pdf
# # Examples:
# response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1) #Equivalent to the choice (rational: 1 or irrational:0)
# fact <- gl(3,1,30,labels=LETTERS[1:3]) #Equivalent to scenario
# block <- gl(10,3,labels=letters[1:10]) #Equivalent to participant
# cochran.qtest(response~fact|block)
# 
# #     + Experienced Experiment -----------------------------------------------
# 
# #       - H4A: Aversion to Waiting - Same Journey Time (S5-S8)  ----------------------------------------------------
# 
# dataH4A <- subset(choiceResponses,experimentType == "experienced")
# dataH4A <- subset(dataH4A,scenario %in% c("03","04"))
# dataH4A <- dataH4A[order(dataH4A$participantId),]
# 
# dataH4A$h4AChosen <- NA
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 0] <- 0
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 1] <- 1
# # View(dataH4A)
# 
# response <- dataH4A$rationalRouteChosen #Equivalent to the choice (rational: 1 or irrational:0)
# fact <- gl(2,1,144,labels=LETTERS[1:2]) #Equivalent to scenario
# block <- factor(dataH4A$participantId) #Equivalent to participant
# cochran.qtest(response~fact|block)
# 
# 
# 
# #       - H4A: Aversion to Waiting - Different Journey Time (S5-S8)  ----------------------------------------------------
# dataH4A <- subset(choiceResponses,experimentType == "experienced")
# dataH4A <- subset(dataH4A,scenario %in% c("05","06","07","08"))
# dataH4A <- dataH4A[order(dataH4A$participantId),]
# 
# dataH4A$h4AChosen <- NA
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 0] <- 0
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 1] <- 1
# # View(dataH4A)
# 
# response <- dataH4A$rationalRouteChosen #Equivalent to the choice (rational: 1 or irrational:0)
# fact <- gl(4,1,288,labels=LETTERS[1:4]) #Equivalent to scenario
# block <- factor(dataH4A$participantId) #Equivalent to participant
# cochran.qtest(response~fact|block)
# 
# #     + Descriptive Experiment -----------------------------------------------
# 
# 
# #       - H4A: Aversion to Waiting - Same Journey Time (S5-S8)  ----------------------------------------------------
# 
# dataH4A <- subset(choiceResponses,experimentType == "descriptive")
# dataH4A <- subset(dataH4A,scenario %in% c("03","04"))
# dataH4A <- dataH4A[order(dataH4A$participantId),]
# 
# dataH4A$h4AChosen <- NA
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 0] <- 0
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 1] <- 1
# # View(dataH4A)
# 
# response <- dataH4A$rationalRouteChosen #Equivalent to the choice (rational: 1 or irrational:0)
# fact <- gl(2,1,144,labels=LETTERS[1:2]) #Equivalent to scenario
# block <- factor(dataH4A$participantId) #Equivalent to participant
# cochran.qtest(response~fact|block)
# 
# 
# #       - H4A: Aversion to Waiting - Different Journey Time (S5-S8)  ----------------------------------------------------
# 
# dataH4A <- subset(choiceResponses,experimentType == "descriptive")
# dataH4A <- subset(dataH4A,scenario %in% c("05","06","07","08"))
# dataH4A <- dataH4A[order(dataH4A$participantId),]
# 
# dataH4A$h4AChosen <- NA
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 0] <- 0
# dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 1] <- 1
# # View(dataH4A)
# 
# response <- dataH4A$rationalRouteChosen #Equivalent to the choice (rational: 1 or irrational:0)
# fact <- gl(6,1,432,labels=LETTERS[1:6]) #Equivalent to scenario
# block <- factor(dataH4A$participantId) #Equivalent to participant
# cochran.qtest(response~fact|block)
# 

# 2) TESTING -----------------------------------------------------
# + H0: Manipulation check ("01","02") ----------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH0 <- choiceResponses

dataH0 <- subset(dataH0,scenario %in% c("01","02"))

# View(dataH0)
dataH0$h0Chosen <- NA
dataH0$h0Chosen[dataH0$rationalRouteChosen == 0] <- 0
dataH0$h0Chosen[dataH0$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH0$tradeOffWT <- with(dataH0,averageTravelTime/averageWaitingTime)
dataH0$dominatingInWaitingTime <- ifelse(dataH0$scenario=="01",1,0)


#     - Estimation ------------------------------------------------


BLRmodelH0ExperiencedExperiment <- glmer(h0Chosen ~ (1|participantId)+factor(dominatingInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
                                         ,data=subset(dataH0,experimentType == "experienced"))
summary(BLRmodelH0ExperiencedExperiment)

BLRmodelH0ExperiencedExperimentPooled1 <- glmer(h0Chosen ~ (1|participantId)+factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                ,data=subset(dataH0,experimentType == "experienced"))
summary(BLRmodelH0ExperiencedExperimentPooled1)

BLRmodelH0ExperiencedExperimentPooled2 <- glmer(h0Chosen ~ (1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH0,experimentType == "experienced"))
summary(BLRmodelH0ExperiencedExperimentPooled2)

BLRmodelH0ExperiencedExperimentCity <- glmer(h0Chosen ~ (1|participantId)+factor(dominatingInWaitingTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                             ,data=subset(dataH0,experimentType == "experienced"))
summary(BLRmodelH0ExperiencedExperimentCity)

BLRmodelH0DescriptiveExperiment <- glmer(h0Chosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(experimentalCondition)
                                         ,data=subset(dataH0,experimentType == "descriptive")
                                         , family = binomial(link='logit'))
summary(BLRmodelH0DescriptiveExperiment)

BLRmodelH0DescriptiveExperimentPooled1 <- glmer(h0Chosen ~(1|participantId)+factor(dominatingInWaitingTime)
                                                ,data=subset(dataH0,experimentType == "descriptive")
                                                , family = binomial(link='logit'))
summary(BLRmodelH0DescriptiveExperimentPooled1)

BLRmodelH0DescriptiveExperimentPooled2 <- glmer(h0Chosen ~(1|participantId)+factor(experimentalCondition)
                                                ,data=subset(dataH0,experimentType == "descriptive")
                                                , family = binomial(link='logit'))
summary(BLRmodelH0DescriptiveExperimentPooled2)


BLRmodelH0DescriptiveExperimentCity <- glmer(h0Chosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(experimentalCondition)*factor(city)
                                             ,data=subset(dataH0,experimentType == "descriptive")
                                             , family = binomial(link='logit'))
summary(BLRmodelH0DescriptiveExperimentCity)

#Both
BLRmodelH0ExperiencedAndDescriptiveExperiments <-  glmer(h0Chosen ~(1|participantId)+factor(experimentType)*factor(dominatingInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                         ,data=subset(dataH0))
summary(BLRmodelH0ExperiencedAndDescriptiveExperiments)

BLRmodelH0ExperiencedAndDescriptiveExperimentsPooled1 <-  glmer(h0Chosen ~(1|participantId)+factor(experimentType)*factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                                ,data=subset(dataH0))
summary(BLRmodelH0ExperiencedAndDescriptiveExperimentsPooled1)


# #Do not converge
# BLRmodelH0ExperiencedAndDescriptiveExperimentCity <-  glmer(h0Chosen ~(1|participantId)+factor(experimentType)*factor(dominatingInWaitingTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
#                                                          ,data=subset(dataH0))
# summary(BLRmodelH0ExperiencedAndDescriptiveExperimentsCity)


#     - Conclusion -----------------------------------------------------------
#Time distorsion reduced efficacy of participants to answer correctly in the experience part of the experiment.
#However, people are able to distinguish on what routes you wait or travel longer. 


# a) H1 (AVERSION TO WAITING) ----------------------------------------------------------------------
#   i) H1A: Travellers prefer routes with shorter than longer waiting time as long as the journey times of the available routes are the same (S3,S4) --------

#     - Dataset ---------------------------------------------------------------
dataH1A <- choiceResponses

dataH1A <- subset(dataH1A,scenario %in% c("03","04"))
dataH1A$h1AChosen <- NA
dataH1A$h1AChosen[dataH1A$rationalRouteChosen == 0] <- 0
dataH1A$h1AChosen[dataH1A$rationalRouteChosen == 1] <- 1

#     - Estimation ------------------------------------------------
#       + Chi square ------------------------------------------------------------
# i) Experienced Experiment

# S03: Marginal Substitution Waiting and Travel Times (Low Ratio)

myData <- subset(dataH1A,scenario %in% c("03")& experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S04: Marginal Substitution Waiting and Travel Times (High Ratio) 

myData <- subset(dataH1A,scenario %in% c("04") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#ii) Descriptive

# S03: Marginal Substitution Waiting and Travel Times (Low Ratio)

myData <- subset(dataH1A,scenario %in% c("03")& experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S04: Marginal Substitution Waiting and Travel Times (High Ratio) 

myData <- subset(dataH1A,scenario %in% c("04") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)



#   ii) H1B: Preferences for routes with shorter waiting time increases with the difference in the proportion of waiting time between the routes (S3,S4) ----------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH1B <- choiceResponses

dataH1B <- subset(dataH1B,scenario %in% c("03","04"))

# View(dataH1B)
dataH1B$h1BChosen <- NA
dataH1B$h1BChosen[dataH1B$rationalRouteChosen == 0] <- 0
dataH1B$h1BChosen[dataH1B$rationalRouteChosen == 1] <- 1


#     - Covariates --------------------------------------------------------------
dataH1B$tradeOffWT <- with(dataH1B,averageTravelTime/averageWaitingTime)
dataH1B$largerAdvantageInTradeOffWT <- ifelse(dataH1B$scenario=="04",1,0)

#     - Estimation ------------------------------------------------
#       + BLR ---------------------------------------------------------------------
#         * Experienced Experiment -----------------------------------------------------------
BLRmodelH1BExperiencedExperiment <- glmer(h1BChosen ~ (1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentalCondition)
                                          ,data=subset(dataH1B,experimentType == "experienced")
                                          , family = binomial(link='logit'))

summary(BLRmodelH1BExperiencedExperiment)

BLRmodelH1BExperiencedExperimentPooled1 <- glmer(h1BChosen ~ (1|participantId)+factor(largerAdvantageInTradeOffWT)
                                                 ,data=subset(dataH1B,experimentType == "experienced")
                                                 , family = binomial(link='logit'))

summary(BLRmodelH1BExperiencedExperimentPooled1)

BLRmodelH1BExperiencedExperimentPooled2 <- glmer(h1BChosen ~ (1|participantId)+factor(experimentalCondition)
                                                 ,data=subset(dataH1B,experimentType == "experienced")
                                                 , family = binomial(link='logit'))

summary(BLRmodelH1BExperiencedExperimentPooled2)

#         * Descriptive Experiment -----------------------------------------------------------
#
BLRmodelH1BDescriptiveExperiment <- glmer(h1BChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentalCondition),family=binomial(link='logit')
                                          ,data=subset(dataH1B,experimentType == "descriptive"))
summary(BLRmodelH1BDescriptiveExperiment)

BLRmodelH1BDescriptiveExperimentPooled1 <- glmer(h1BChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT),family=binomial(link='logit')
                                                 ,data=subset(dataH1B,experimentType == "descriptive"))
summary(BLRmodelH1BDescriptiveExperimentPooled1)

BLRmodelH1BDescriptiveExperimentPooled2 <- glmer(h1BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH1B,experimentType == "descriptive"))
summary(BLRmodelH1BDescriptiveExperimentPooled2)

#         * City-----------------------------------------------------------

#Experienced
BLRmodelH1BExperiencedExperimentCity <- glmer(h1BChosen ~ (1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentalCondition)*factor(city)
                                              ,data=subset(dataH1B,experimentType == "experienced")
                                              , family = binomial(link='logit'))

summary(BLRmodelH1BExperiencedExperimentCity)

BLRmodelH1BExperiencedExperimentCityPooled1 <- glmer(h1BChosen ~ (1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(city)
                                                     ,data=subset(dataH1B,experimentType == "experienced")
                                                     , family = binomial(link='logit'))

summary(BLRmodelH1BExperiencedExperimentCityPooled1)

#Descriptive
BLRmodelH1BDescriptiveExperimentCity <- glmer(h1BChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                              ,data=subset(dataH1B,experimentType == "descriptive"))
summary(BLRmodelH1BDescriptiveExperimentCity)

BLRmodelH1BDescriptiveExperimentCityPooled <- glmer(h1BChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(city),family=binomial(link='logit')
                                                    ,data=subset(dataH1B,experimentType == "descriptive"))
summary(BLRmodelH1BDescriptiveExperimentCityPooled)

#       + McNemar ---------------------------------------------------------------

# i) Control Scenario: S3; Treatment Scenario: S4

# Experienced Experiment
McNemarTestH1BExperiencedExperiment <- McNemarTest(data = subset(dataH1B,experimentType == "experienced"), idScenarioControl= "03",idScenarioTreatment = "04")
# Descriptive Experiment
McNemarTestH1BDescriptiveExperiment <- McNemarTest(data = subset(dataH1B,experimentType == "descriptive"), idScenarioControl= "03",idScenarioTreatment = "04")

#     - Conclusion -----------------------------------------------------------
#Both experiments shows a significant preferences for better trade-off of waiting and travel times. However, higher trade-off does not increase preferences for that alternative.
#In addition, in the experienced experiment there is a decrease.




#   iii) H1C: Travellers prefer routes with shorter than longer waiting time as long as the journey times of the available routes are the same (S3,S4) --------

#     - Dataset ---------------------------------------------------------------
dataH1C <- choiceResponses

dataH1C <- subset(dataH1C,scenario %in% c("05","06","07","08"))
dataH1C$h1CChosen <- NA
dataH1C$h1CChosen[dataH1C$rationalRouteChosen == 0] <- 0
dataH1C$h1CChosen[dataH1C$rationalRouteChosen == 1] <- 1

#     - Estimation ------------------------------------------------
#       + Chi square ------------------------------------------------------------
# i) Experienced Experiment

#S05: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("05") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S06: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("06") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S07: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("07") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S08: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("08") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#ii) Descriptive

# S03: Marginal Substitution Waiting and Travel Times (Low Ratio)

#S05: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("05") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S06: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("06") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S07: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("07") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#S08: EUT versus TTB

myData <- subset(dataH1C,scenario %in% c("08") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)



#   iv) H1D: Preferences for routes with shorter waiting time increases with the difference in the proportion of waiting time between the routes (S5,S6,S7,S8)  --------
#     - Dataset ---------------------------------------------------------------
dataH1D <- choiceResponses
dataH1D <- subset(dataH1D,scenario %in% c("05","06","07","08"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH1D$h1DChosen <- NA
dataH1D$h1DChosen[dataH1D$rationalRouteChosen == 0] <- 0
dataH1D$h1DChosen[dataH1D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH1D$tradeOffWT <- with(dataH1D,averageWaitingTime/averageTravelTime)
dataH1D$xWaitingTime <- with(dataH1D,averageWaitingTime/(averageJourneyTime))
#The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
dataH1D$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH1D$scenario %in% c("05","08"),1,0) 

#     - Estimation ------------------------------------------------
#       + BLR -------------------------------------------------------------------


#Experienced
BLRmodelH1DExperiencedExperiment <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                       ,data=subset(dataH1D,experimentType == "experienced"))
summary(BLRmodelH1DExperiencedExperiment)

BLRmodelH1DExperiencedExperimentPooled1 <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
                                         ,data=subset(dataH1D,experimentType == "experienced"))
summary(BLRmodelH1DExperiencedExperimentPooled1)

BLRmodelH1DExperiencedExperimentPooled2 <- glmer(h1DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH1D,experimentType == "experienced"))
summary(BLRmodelH1DExperiencedExperimentPooled2)

#Descriptive

BLRmodelH1DDescriptiveExperiment <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                       ,data=subset(dataH1D,experimentType == "descriptive"))
summary(BLRmodelH1DDescriptiveExperiment)

BLRmodelH1DDescriptiveExperimentPooled1 <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
                                         ,data=subset(dataH1D,experimentType == "descriptive"))
summary(BLRmodelH1DDescriptiveExperimentPooled1)

BLRmodelH1DDescriptiveExperimentPooled2 <- glmer(h1DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH1D,experimentType == "descriptive"))
summary(BLRmodelH1DDescriptiveExperimentPooled2)

BLRmodelH1DDescriptiveExperimentCity <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                         ,data=subset(dataH1D,experimentType == "descriptive"))
summary(BLRmodelH1DDescriptiveExperimentCity)

BLRmodelH1DDescriptiveExperimentCityPooled1 <- glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                             ,data=subset(dataH1D,experimentType == "descriptive"))
summary(BLRmodelH1DDescriptiveExperimentCityPooled1)


#Both
BLRmodelH1DExperiencedAndDescriptiveExperiments <-  glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType)*factor(experimentalCondition),family=binomial(link='logit')
                                                       ,data=subset(dataH1D))
summary(BLRmodelH1DExperiencedAndDescriptiveExperiments)

BLRmodelH1DExperiencedAndDescriptiveExperimentsPooled1 <-  glmer(h1DChosen ~(1|participantId)+factor(experimentType)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
                                                          ,data=subset(dataH1D))
summary(BLRmodelH1DExperiencedAndDescriptiveExperimentsPooled1)

BLRmodelH1DExperiencedAndDescriptiveExperimentsCity <-  glmer(h1DChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                          ,data=subset(dataH1D))
summary(BLRmodelH1DExperiencedAndDescriptiveExperimentsCity)


#       + McNemar ---------------------------------------------------------------

# i) Control Scenario: S6; Treatment Scenario: S5

# Experienced Experiment 
McNemarTestH1DS6S5ExperiencedExperiment <- McNemarTest(data = subset(dataH1D,experimentType == "experienced"), idScenarioControl= "06",idScenarioTreatment = "05")
# Descriptive Experiment (Control: S6, Treatment: S5)
McNemarTestH1DS6S5DescriptiveExperiment <- McNemarTest(data = subset(dataH1D,experimentType == "descriptive"), idScenarioControl= "06",idScenarioTreatment = "05")

# ii) Control Scenario: S7; Treatment Scenario: S8

# Experienced Experiment 
McNemarTestH1DS7S8ExperiencedExperiment <- McNemarTest(data = subset(dataH1D,experimentType == "experienced"), idScenarioControl= "07",idScenarioTreatment = "08")
# Descriptive Experiment
McNemarTestH1DS7S8DescriptiveExperiment <- McNemarTest(data = subset(dataH1D,experimentType == "descriptive"), idScenarioControl= "07",idScenarioTreatment = "08")

#     - Conclusion -----------------------------------------------------------

#- Participants in the experienced experiment have a higher preference for shorter journeys than in the descriptive experiment. 
# It is not clear that participants have a preference in the descriptive experiment
# It does not make sense to group scenarios for this hypothesis.


# b) H2 (AVERSION TO TIME VARIABILITY) ----------------------------------------------------------------------
#   i) H2A: Travellers prefer routes with no variability in waiting time (S9,S12) --------

#     - Dataset ---------------------------------------------------------------
dataH2A <- choiceResponses

dataH2A <- subset(dataH2A,scenario %in% c("09","12"))
dataH2A$h2AChosen <- NA
dataH2A$h2AChosen[dataH2A$rationalRouteChosen == 0] <- 0
dataH2A$h2AChosen[dataH2A$rationalRouteChosen == 1] <- 1

#     - Estimation ------------------------------------------------
#       + Chi square ------------------------------------------------------------
# i) Experienced Experiment

# S09: Low waiting time variability

myData <- subset(dataH2A,scenario %in% c("09")& experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S12: High waiting time variability

myData <- subset(dataH2A,scenario %in% c("12") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#ii) Descriptive

# S09: Low waiting time variability

myData <- subset(dataH2A,scenario %in% c("09")& experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S12: High waiting time variability

myData <- subset(dataH2A,scenario %in% c("12") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#   ii) H2B: Preferences for routes with no variability in waiting time increase with the level of variability (S9,S12) ---------------------

#     - Dataset ---------------------------------------------------------------
dataH2B <- choiceResponses
dataH2B <- subset(dataH2B,scenario %in% c("09","12"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH2B$h2BChosen <- NA
dataH2B$h2BChosen[dataH2B$rationalRouteChosen == 0] <- 0
dataH2B$h2BChosen[dataH2B$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH2B$tradeOffWT <- with(dataH2B,averageWaitingTime/averageTravelTime)
dataH2B$xWaitingTime <- with(dataH2B,averageWaitingTime/(averageJourneyTime))
dataH2B$highVariabilityInWaitingTime <- ifelse(with(dataH2B,scenario == "12"),1,0)

#     - Estimation ------------------------------------------------

#       + BLR -------------------------------------------------------------------


#Experienced
BLRmodelH2BExperiencedExperiment <- glmer(h2BChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInWaitingTime),family=binomial(link='logit')
                                       ,data=subset(dataH2B,experimentType == "experienced"))
summary(BLRmodelH2BExperiencedExperiment)

BLRmodelH2BExperiencedExperimentPooled1 <- glmer(h2BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime),family=binomial(link='logit')
                                          ,data=subset(dataH2B,experimentType == "experienced"))
summary(BLRmodelH2BExperiencedExperimentPooled1)

BLRmodelH2BExperiencedExperimentPooled2 <- glmer(h2BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH2B,experimentType == "experienced"))
summary(BLRmodelH2BExperiencedExperimentPooled2)

# BLRmodelH2BExperiencedExperiment <- BLRmodelH2BExperiencedExperimentPooled #We show the pooled results without mediation of the experiment

#Descriptive
BLRmodelH2BDescriptiveExperiment <- glmer(h2BChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInWaitingTime),family=binomial(link='logit')
                                       ,data=subset(dataH2B,experimentType == "descriptive"))
summary(BLRmodelH2BDescriptiveExperiment)

BLRmodelH2BDescriptiveExperimentPooled1 <- glmer(h2BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime),family=binomial(link='logit')
                                          ,data=subset(dataH2B,experimentType == "descriptive"))
summary(BLRmodelH2BDescriptiveExperimentPooled1)

BLRmodelH2BDescriptiveExperimentPooled2 <- glmer(h2BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH2B,experimentType == "descriptive"))
summary(BLRmodelH2BDescriptiveExperimentPooled2)

#Analysis by city
BLRmodelH2BDescriptiveExperimentCity <- glmer(h2BChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
                                              ,data=subset(dataH2B,experimentType == "descriptive"))
summary(BLRmodelH2BDescriptiveExperimentCity)

BLRmodelH2BDescriptiveExperimentCityPooled1 <- glmer(h2BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
                                                     ,data=subset(dataH2B,experimentType == "descriptive"))
summary(BLRmodelH2BDescriptiveExperimentCityPooled1)


#       + McNemar ---------------------------------------------------------------

# i) Control Scenario: S9; Treatment Scenario: S12

# Experienced Experiment 
McNemarTestH2BExperiencedExperiment <- McNemarTest(data = subset(dataH2B,experimentType == "experienced"), idScenarioControl= "09",idScenarioTreatment = "12")
# Descriptive Experiment
McNemarTestH2BDescriptiveExperiment <- McNemarTest(data = subset(dataH2B,experimentType == "descriptive"), idScenarioControl= "09",idScenarioTreatment = "12")
#     - Conclusion -----------------------------------------------------------

#People prefer less variability in waiting time. No statisticaly differences between the two types of experiments and experimental condtions.
#As higher is the variability in waiting times, higher is the preference for the deterministic option.

#   iii) H2C: Travellers prefer routes with no variability in in-vehicle time (S10,S13) --------

#     - Dataset ---------------------------------------------------------------
dataH2C <- choiceResponses

dataH2C <- subset(dataH2C,scenario %in% c("10","13"))
dataH2C$h2CChosen <- NA
dataH2C$h2CChosen[dataH2C$rationalRouteChosen == 0] <- 0
dataH2C$h2CChosen[dataH2C$rationalRouteChosen == 1] <- 1

#     - Estimation ------------------------------------------------
#       + Chi square ------------------------------------------------------------
# i) Experienced Experiment

# S10: Low in-vehicle time variability

myData <- subset(dataH2C,scenario %in% c("10")& experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S13: High in-vehicle time variability

myData <- subset(dataH2C,scenario %in% c("13") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#ii) Descriptive

# S10: Low in-vehicle time variability

myData <- subset(dataH2C,scenario %in% c("10")& experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S13: High in-vehicle time variability

myData <- subset(dataH2C,scenario %in% c("13") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#   iv) H2D: Preferences for routes with no variability in in-vehicle time increase with the level of variability (S10,S13)---------------------

#     - Dataset ---------------------------------------------------------------
dataH2D <- choiceResponses
dataH2D <- subset(dataH2D,scenario %in% c("10","13"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH2D$h2DChosen <- NA
dataH2D$h2DChosen[dataH2D$rationalRouteChosen == 0] <- 0
dataH2D$h2DChosen[dataH2D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH2D$tradeOffWT <- with(dataH2D,averageWaitingTime/averageTravelTime)
dataH2D$xWaitingTime <- with(dataH2D,averageWaitingTime/(averageJourneyTime))
dataH2D$highVariabilityInTravelTime <- ifelse(with(dataH2D,scenario == "13"),1,0)

#     - Estimation ------------------------------------------------
#       + BLR --------------------------------------------------------------------


#Experienced
BLRmodelH2DExperiencedExperiment <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                       ,data=subset(dataH2D,experimentType == "experienced"))
summary(BLRmodelH2DExperiencedExperiment)

BLRmodelH2DExperiencedExperimentPooled1 <- glmer(h2DChosen ~(1|participantId)+factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                          ,data=subset(dataH2D,experimentType == "experienced"))
summary(BLRmodelH2DExperiencedExperimentPooled1)

BLRmodelH2DExperiencedExperimentPooled2 <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH2D,experimentType == "experienced"))
summary(BLRmodelH2DExperiencedExperimentPooled2)

BLRmodelH2DExperiencedExperimentCity <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInTravelTime)+factor(city),family=binomial(link='logit')
                                          ,data=subset(dataH2D,experimentType == "experienced"))
summary(BLRmodelH2DExperiencedExperimentCity)

#Descriptive
BLRmodelH2DDescriptiveExperiment <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                       ,data=subset(dataH2D,experimentType == "descriptive"))
summary(BLRmodelH2DDescriptiveExperiment)

BLRmodelH2DDescriptiveExperimentPooled1 <- glmer(h2DChosen ~(1|participantId)+factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                          ,data=subset(dataH2D,experimentType == "descriptive"))
summary(BLRmodelH2DDescriptiveExperimentPooled1)

BLRmodelH2DDescriptiveExperimentPooled2 <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH2D,experimentType == "descriptive"))
summary(BLRmodelH2DDescriptiveExperimentPooled2)

# BLRmodelH2DDescriptiveExperimentPooled2 <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                 ,data=subset(dataH2D,experimentType == "descriptive"))
# summary(BLRmodelH2DDescriptiveExperimentPooled2)

#Analysis by city
BLRmodelH2DDescriptiveExperimentCity <- glmer(h2DChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariabilityInTravelTime)*factor(city),family=binomial(link='logit')
                                          ,data=subset(dataH2D,experimentType == "descriptive"))
summary(BLRmodelH2DDescriptiveExperimentCity)

BLRmodelH2DDescriptiveExperimentCityPooled1 <- glmer(h2DChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(city),family=binomial(link='logit')
                                              ,data=subset(dataH2D,experimentType == "descriptive"))
summary(BLRmodelH2DDescriptiveExperimentCityPooled1)

#       + McNemar ---------------------------------------------------------------

# i) Control Scenario: 10; Treatment Scenario: S13

# Experienced Experiment 
McNemarTestH2DExperiencedExperiment <- McNemarTest(data = subset(dataH2D,experimentType == "experienced"), idScenarioControl= "10",idScenarioTreatment = "13")
# Descriptive Experiment
McNemarTestH2DDescriptiveExperiment <- McNemarTest(data = subset(dataH2D,experimentType == "descriptive"), idScenarioControl= "10",idScenarioTreatment = "13")

#     - Conclusion -----------------------------------------------------------

#People prefer less variability in travel times in the two types of experiments. In the treatment condition the negative impact of variability is much higher.

# c) H3 (AVERSION TO WAITING VARIABILITY) -------------------------------------------------------------------

#   i) H3A: Travellers prefer routes with variability allocated in in-vehicle times than in waiting times (S11,S14) --------
#     - Dataset ---------------------------------------------------------------
dataH3A <- choiceResponses

dataH3A <- subset(dataH3A,scenario %in% c("11","14"))
dataH3A$h3AChosen <- NA
dataH3A$h3AChosen[dataH3A$rationalRouteChosen == 0] <- 0
dataH3A$h3AChosen[dataH3A$rationalRouteChosen == 1] <- 1

#     - Estimation ------------------------------------------------
#       + Chi square ------------------------------------------------------------
# i) Experienced Experiment

# S11: Low variability

myData <- subset(dataH3A,scenario %in% c("11")& experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S14: High variability

myData <- subset(dataH3A,scenario %in% c("14") & experimentType == "experienced")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

#ii) Descriptive

# S11: Low variability

myData <- subset(dataH3A,scenario %in% c("11")& experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)

# S14: High variability
myData <- subset(dataH3A,scenario %in% c("14") & experimentType == "descriptive")
observedRouteChoices = c(sum(myData$rationalRouteChosen),sum(myData$irrationalRouteChosen))
expectedRouteChoices = c(0.5,0.5)

chisq.test(x = observedRouteChoices,
           p = expectedRouteChoices)


#   ii) H3B: Preferences for routes with variable in-vehicle times increase with the level of variability (S11,S14) ---------------------
#     - Dataset ---------------------------------------------------------------
dataH3B <- choiceResponses
dataH3B <- subset(dataH3B,scenario %in% c("11","14"))

# View(dataH1)
#Rational here means picking the route with the variability applied in travel times
dataH3B$h3BChosen <- NA
dataH3B$h3BChosen[dataH3B$rationalRouteChosen == 0] <- 0
dataH3B$h3BChosen[dataH3B$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH3B$tradeOffWT <- with(dataH3B,averageWaitingTime/averageTravelTime)
dataH3B$xWaitingTime <- with(dataH3B,averageWaitingTime/(averageJourneyTime))
dataH3B$highVariability <- ifelse(with(dataH3B,scenario == "14"),1,0)


#     - Estimation ------------------------------------------------
#       + BLR -------------------------------------------------------------------


#Experienced
BLRmodelH3BExperiencedExperiment <- glmer(h3BChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariability),family=binomial(link='logit')
                                         ,data=subset(dataH3B,experimentType == "experienced"))
summary(BLRmodelH3BExperiencedExperiment)

BLRmodelH3BExperiencedExperimentPooled1 <- glmer(h3BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                         ,data=subset(dataH3B,experimentType == "experienced"))
summary(BLRmodelH3BExperiencedExperimentPooled1)

BLRmodelH3BExperiencedExperimentPooled2 <- glmer(h3BChosen ~(1|participantId)+factor(highVariability), family=binomial(link='logit')
                                                ,data=subset(dataH3B,experimentType == "experienced"))
summary(BLRmodelH3BExperiencedExperimentPooled2)

#Descriptive
BLRmodelH3BDescriptiveExperiment <- glmer(h3BChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariability),family=binomial(link='logit')
                                         ,data=subset(dataH3B,experimentType == "descriptive"))
summary(BLRmodelH3BDescriptiveExperiment)

BLRmodelH3BDescriptiveExperimentPooled1 <- glmer(h3BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                         ,data=subset(dataH3B,experimentType == "descriptive"))
summary(BLRmodelH3BDescriptiveExperimentPooled1)

BLRmodelH3BDescriptiveExperimentPooled2 <- glmer(h3BChosen ~(1|participantId)+factor(highVariability),family=binomial(link='logit')
                                                ,data=subset(dataH3B,experimentType == "descriptive"))
summary(BLRmodelH3BDescriptiveExperimentPooled2)

#       + McNemar ---------------------------------------------------------------

# i) Control Scenario: 11; Treatment Scenario: S14

# Experienced Experiment 
McNemarTestH3BExperiencedExperiment <- McNemarTest(data = subset(dataH3B,experimentType == "experienced"), idScenarioControl= "11",idScenarioTreatment = "14")
# Descriptive Experiment
McNemarTestH3BDescriptiveExperiment <- McNemarTest(data = subset(dataH3B,experimentType == "descriptive"), idScenarioControl= "11",idScenarioTreatment = "14")

#       - Conclusion -----------------------------------------------------------

#Participants have a stronger preferences for using alternatives where the variability is included in the travel times (but only at the 10% significance level).

# d) H4  (DEGREE OF EXPERIENCE) -------------------------------------------------------------------

# #   i) Manipulation Check ------------------------------------------------------
# #     - Dataset ---------------------------------------------------------------
# dataH4S1S2 <- subset(choiceResponses,scenario %in% c("01","02"))
# # dataH4S1S2 <- subset(dataH4S1S2,experimentalCondition == "control")
# 
# 
# # View(dataH1)
# #Rational here means picking the shorter route. We actually think people will take the longer one
# dataH4S1S2$h4AChosen <- NA
# dataH4S1S2$h4AChosen[dataH4S1S2$rationalRouteChosen == 0] <- 0
# dataH4S1S2$h4AChosen[dataH4S1S2$rationalRouteChosen == 1] <- 1
# 
# 
# #     - Covariates --------------------------------------------------------------
# 
# dataH4S1S2$dominatingInWaitingTime <- ifelse(dataH4S1S2$scenario=="01",1,0)
# 
# #     - Estimation ------------------------------------------------
# #       + Control Condition ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(experimentType),family=binomial(link='logit')
#                                                                               ,data=subset(dataH4S1S2,experimentalCondition == "control"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlCondition)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
#                                                                                      ,data=subset(dataH4S1S2,experimentalCondition == "control"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionPooled1)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH4S1S2,experimentalCondition == "control"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCity)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                          ,data=subset(dataH4S1S2,experimentalCondition == "control"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)
# 
# #       + Treatment Condition ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(experimentType),family=binomial(link='logit')
#                                                                                 ,data=subset(dataH4S1S2,experimentalCondition == "treatment"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentCondition)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
#                                                                                        ,data=subset(dataH4S1S2,experimentalCondition == "treatment"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH4S1S2,experimentalCondition == "treatment"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionCity)
# 
# BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                            ,data=subset(dataH4S1S2,experimentalCondition == "treatment"))
# summary(BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)
# 

#   ii) H4A: The aversion to waiting increases with the degree of experience (S3-S8) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH4A <- choiceResponses
dataH4A <- subset(dataH4A,scenario %in% c("03","04","05","06","07","08"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH4A$h4AChosen <- NA
dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 0] <- 0
dataH4A$h4AChosen[dataH4A$rationalRouteChosen == 1] <- 1

dataH4AS3S4 <- subset(dataH4A,scenario %in% c("03","04"))
dataH4AS5S8 <- subset(dataH4A,scenario %in% c("05","06","07","08"))
dataH4A <- subset(dataH4A,scenario %in% c("03","04","05","06","07","08"))


#     - Covariates --------------------------------------------------------------

#S3 & S4
#S4 generates a higher difference in utility
dataH4AS3S4$largerAdvantageInTradeOffWT <- ifelse(dataH4AS3S4$scenario=="04",1,0)

#S5-S8
#- The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#- The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#- Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
dataH4AS5S8$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH4AS5S8$scenario %in% c("05","08"),1,0)

#S3-S8
#Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
#Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
#as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
dataH4A$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH4A$scenario %in% c("06","08"),1,0) 
dataH4A$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH4A$scenario %in% c("05","07"),1,0) 
dataH4A$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH4A$scenario %in% c("05","07","06","08"),1,0) 
dataH4A$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH4A$scenario %in%"04",1,0)

#     - Estimation ------------------------------------------------
#       + Control Condition ------------------------------------------------------------
#         * S3 & S4 -----------------------------------------------------------------

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentType),family=binomial(link='logit')
                                                                                ,data=subset(dataH4AS3S4,experimentalCondition == "control"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4AS3S4,experimentalCondition == "control"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(largerAdvantageInTradeOffWT)*factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4AS3S4,experimentalCondition == "control"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionCity)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                           ,data=subset(dataH4AS3S4,experimentalCondition == "control"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)

#         * S5 - S8 -----------------------------------------------------------------

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType),family=binomial(link='logit')
                                                                                ,data=subset(dataH4AS5S8,experimentalCondition == "control"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4AS5S8,experimentalCondition == "control"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4AS5S8,experimentalCondition == "control"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionCity)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                           ,data=subset(dataH4AS5S8,experimentalCondition == "control"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)

#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4AChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                                ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled2 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                 ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled2)

#City

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCity)

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                           ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled2 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                                     ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled2)

BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled3 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                     ,data=subset(dataH4A,experimentalCondition == "control"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionCityPooled3)

#       + Treatment Condition ------------------------------------------------------------
#         * S3 & S4 -----------------------------------------------------------------

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentType),family=binomial(link='logit')
                                                              ,data=subset(dataH4AS3S4,experimentalCondition == "treatment"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                     ,data=subset(dataH4AS3S4,experimentalCondition == "treatment"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(largerAdvantageInTradeOffWT)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH4AS3S4,experimentalCondition == "treatment"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH4AS3S4,experimentalCondition == "treatment"))
summary(BLRmodelH4AS3S4ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)

#         * S5 - S8 -----------------------------------------------------------------

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType),family=binomial(link='logit')
                                                              ,data=subset(dataH4AS5S8,experimentalCondition == "treatment"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                     ,data=subset(dataH4AS5S8,experimentalCondition == "treatment"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH4AS5S8,experimentalCondition == "treatment"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH4AS5S8,experimentalCondition == "treatment"))
summary(BLRmodelH4AS5S8ExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)

#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4AChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                              ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType),family=binomial(link='logit')
                                                                     ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2)

#City
BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCity<-  glmer(h4AChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2 <-  glmer(h4AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2)

BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4A,experimentalCondition == "treatment"))
summary(BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3)



#     - Conclusion -----------------------------------------------------------

#- Participants in the experienced experiment have a higher preference for shorter journeys than in the descriptive experiment. 
# It is not clear that participants have a preference in the descriptive experiment
# It does not make sense to group scenarios for this hypothesis.




#   iii) H4B: The aversion to time variability increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH4B <- choiceResponses
dataH4B <- subset(dataH4B,scenario %in% c("09","12"))

# View(dataH1)
#Rational here means picking the route with no variability
dataH4B$h4BChosen <- NA
dataH4B$h4BChosen[dataH4B$rationalRouteChosen == 0] <- 0
dataH4B$h4BChosen[dataH4B$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH4BS9S12 <- subset(dataH4B,scenario %in% c("09","12"))
# dataH4BS10S13 <- subset(dataH4B,scenario %in% c("10","13"))
# dataH4B <- subset(dataH4B,scenario %in% c("09","12","10","13"))

#     - Covariates ------------------------------------------------------------

# S9 & S12
dataH4B$tradeOffWT <- with(dataH4B,averageWaitingTime/averageTravelTime)
dataH4B$xWaitingTime <- with(dataH4B,averageWaitingTime/(averageJourneyTime))
dataH4B$highVariabilityInWaitingTime <- ifelse(with(dataH4B,scenario == "12"),1,0)

# # S10 & S13
# dataH4BS10S13$tradeOffWT <- with(dataH4BS10S13,averageWaitingTime/averageTravelTime)
# dataH4BS10S13$xWaitingTime <- with(dataH4BS10S13,averageWaitingTime/(averageJourneyTime))
# dataH4BS10S13$highVariabilityInTravelTime <- ifelse(with(dataH4BS10S13,scenario == "13"),1,0)
# 
# # S09 & S12 vs. S10 & S13
# dataH4B$variabilityInWaitingTime <- ifelse(with(dataH4B,scenario == "09"|scenario =="12"),1,0)
# dataH4B$highVariability <- ifelse(with(dataH4B,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Control Condition ------------------------------------------------------------
#         * S9 & S12 ----------------------------------------------------------------
#Both

BLRmodelH4BExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                          ,data=subset(dataH4B,experimentalCondition == "control"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(experimentType),family=binomial(link='logit')
                                                                 ,data=subset(dataH4B,experimentalCondition == "control"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                 ,data=subset(dataH4B,experimentalCondition == "control"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled2)

# #City
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCity <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                               ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCity)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                           ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
#                                                        ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlCondition)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentType),family=binomial(link='logit')
#                                                               ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled1)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled2)
# 
# 
# #City
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCity<-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                           ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCity)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                   ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                      ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled2)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled3 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                      ,data=subset(dataH4B,experimentalCondition == "control"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionCityPooled3)

#       + Treatment Condition ------------------------------------------------------------
#         * S9 & S12 ----------------------------------------------------------------
#Both

BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                               ,data=subset(dataH4B,experimentalCondition == "treatment"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(experimentType),family=binomial(link='logit')
                                                                                      ,data=subset(dataH4B,experimentalCondition == "treatment"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4B,experimentalCondition == "treatment"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2)

#City
BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCity <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4B,experimentalCondition == "treatment"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                          ,data=subset(dataH4B,experimentalCondition == "treatment"))
summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
#                                                                                       ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentCondition)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentType),family=binomial(link='logit')
#                                                                                              ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2)
# 
# #City
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCity<-  glmer(h4BChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                          ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCity)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                                  ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2 <-  glmer(h4BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                        ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2)
# 
# BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3 <-  glmer(h4BChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                        ,data=subset(dataH4B,experimentalCondition == "treatment"))
# summary(BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3)

#   iii) H4C: The aversion to time variability increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH4C <- choiceResponses
dataH4C <- subset(dataH4C,scenario %in% c("10","13"))

# View(dataH1)
#Rational here means picking the route with no variability
dataH4C$h4CChosen <- NA
dataH4C$h4CChosen[dataH4C$rationalRouteChosen == 0] <- 0
dataH4C$h4CChosen[dataH4C$rationalRouteChosen == 1] <- 1

# Subdatasets
# dataH4C <- subset(dataH4C,scenario %in% c("09","12","10","13"))

#     - Covariates ------------------------------------------------------------
# S10 & S13
dataH4C$tradeOffWT <- with(dataH4C,averageWaitingTime/averageTravelTime)
dataH4C$xWaitingTime <- with(dataH4C,averageWaitingTime/(averageJourneyTime))
dataH4C$highVariabilityInTravelTime <- ifelse(with(dataH4C,scenario == "13"),1,0)

# # S09 & S12 vs. S10 & S13
# dataH4C$variabilityInWaitingTime <- ifelse(with(dataH4C,scenario == "09"|scenario =="12"),1,0)
# dataH4C$highVariability <- ifelse(with(dataH4C,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Control Condition ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------
#Both

BLRmodelH4CExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4CChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                                ,data=subset(dataH4C,experimentalCondition == "control"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(experimentType),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4C,experimentalCondition == "control"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled2 <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                 ,data=subset(dataH4C,experimentalCondition == "control"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled2)

# #City
# BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionCity <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType)*factor(city)*factor(highVariabilityInTravelTime),family=binomial(link='logit')
#                                                                                     ,data=subset(dataH4C,experimentalCondition == "control"))
# summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionCity)
# 
# BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
#                                                                                            ,data=subset(dataH4C,experimentalCondition == "control"))
# summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)



#       + Treatment Condition ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------
#Both

BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4CChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                                  ,data=subset(dataH4C,experimentalCondition == "treatment"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(experimentType),family=binomial(link='logit')
                                                                                         ,data=subset(dataH4C,experimentalCondition == "treatment"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2 <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                         ,data=subset(dataH4C,experimentalCondition == "treatment"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2)

#City
BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionCity <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType)*factor(city)*factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                                                                      ,data=subset(dataH4C,experimentalCondition == "treatment"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4CChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                             ,data=subset(dataH4C,experimentalCondition == "treatment"))
summary(BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)


#   iv) H4D: The aversion to waiting variability increases with the degree of experience (S11,S14) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH4D <- choiceResponses
dataH4D <- subset(dataH4D,scenario %in% c("11","14"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH4D$h4DS11S14Dhosen <- NA
dataH4D$h4DChosen[dataH4D$rationalRouteChosen == 0] <- 0
dataH4D$h4DChosen[dataH4D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH4D$highVariability <- ifelse(with(dataH4D,scenario == "14"),1,0)

#     - Estimation ------------------------------------------------
#       + Control Condition ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
#Both

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlCondition <-  glmer(h4DChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                                  ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlCondition)

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled1 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(experimentType),family=binomial(link='logit')
                                                                                         ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled1)

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled2 <-  glmer(h4DChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                 ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled2)

#City
BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCity <-  glmer(h4DChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                      ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCity)

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled1 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                             ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled1)

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled2 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(city),family=binomial(link='logit')
                                                                                     ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled2)

BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled3 <-  glmer(h4DChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                     ,data=subset(dataH4D,experimentalCondition == "control"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionCityPooled3)



#       + Treatment Condition ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
#Both

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentCondition <-  glmer(h4DChosen ~(1|participantId)+factor(scenario)*factor(experimentType),family=binomial(link='logit')
                                                                                 ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentCondition)

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(experimentType),family=binomial(link='logit')
                                                                                        ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1)

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2 <-  glmer(h4DChosen ~(1|participantId)+factor(experimentType),family=binomial(link='logit')
                                                                                   ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2)

#City
BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCity <-  glmer(h4DChosen ~(1|participantId)+factor(scenario)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                     ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCity)

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                            ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled1)

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2 <-  glmer(h4DChosen ~(1|participantId)+factor(highVariability)*factor(city),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled2)

BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3 <-  glmer(h4DChosen ~(1|participantId)+factor(experimentType)*factor(city),family=binomial(link='logit')
                                                                                       ,data=subset(dataH4D,experimentalCondition == "treatment"))
summary(BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionCityPooled3)

# e) H5 (DEGREE OF INFORMATION IN EXPERIENTIAL CHOICES) -------------------------------------------------------------------

#   i) Manipulation Check ------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH5S1S2 <- subset(choiceResponses,scenario %in% c("01","02"))
dataH5S1S2 <- subset(dataH5S1S2,experimentType == "experienced")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH5S1S2$h4AChosen <- NA
dataH5S1S2$h4AChosen[dataH5S1S2$rationalRouteChosen == 0] <- 0
dataH5S1S2$h4AChosen[dataH5S1S2$rationalRouteChosen == 1] <- 1


#     - Covariates --------------------------------------------------------------

dataH5S1S2$dominatingInWaitingTime <- ifelse(dataH5S1S2$scenario=="01",1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S1 & S2 -----------------------------------------------------------------

BLRmodelH5S1S2ExperiencedExperiment <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                             ,data=subset(dataH5S1S2))
summary(BLRmodelH5S1S2ExperiencedExperiment)

BLRmodelH5S1S2ExperiencedExperimentPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                                    ,data=subset(dataH5S1S2))
summary(BLRmodelH5S1S2ExperiencedExperimentPooled1)

BLRmodelH5S1S2ExperiencedExperimentCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city)*factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                                ,data=subset(dataH5S1S2))
summary(BLRmodelH5S1S2ExperiencedExperimentCity)

BLRmodelH5S1S2ExperiencedExperimentCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                        ,data=subset(dataH5S1S2))
summary(BLRmodelH5S1S2ExperiencedExperimentCityPooled1)

#   ii) H5A: The degree of information on waiting times mediates the impact of the aversion to waiting on travellers choices (S3-S8) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH5A <- choiceResponses
dataH5A <- subset(dataH5A,scenario %in% c("03","04","05","06","07","08"))
dataH5A <- subset(dataH5A,experimentType == "experienced")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH5A$h5AChosen <- NA
dataH5A$h5AChosen[dataH5A$rationalRouteChosen == 0] <- 0
dataH5A$h5AChosen[dataH5A$rationalRouteChosen == 1] <- 1

#Subssets
dataH5AS3S4 <- subset(dataH5A,scenario %in% c("03","04"))
dataH5AS5S8 <- subset(dataH5A,scenario %in% c("05","06","07","08"))
dataH5A <- subset(dataH5A,scenario %in% c("03","04","05","06","07","08"))


#     - Covariates --------------------------------------------------------------

#S3 & S4
#S4 generates a higher difference in utility
dataH5AS3S4$largerAdvantageInTradeOffWT <- ifelse(dataH5AS3S4$scenario=="04",1,0)

#S5-S8
#- The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#- The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#- Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
dataH5AS5S8$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH5AS5S8$scenario %in% c("05","08"),1,0)

#S3-S8
#Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
#Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
#as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
dataH5A$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH5A$scenario %in% c("06","08"),1,0) 
dataH5A$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH5A$scenario %in% c("05","07"),1,0) 
dataH5A$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH5A$scenario %in% c("05","07","06","08"),1,0) 
dataH5A$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH5A$scenario %in%"04",1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S3 & S4 -----------------------------------------------------------------

BLRmodelH5AS3S4ExperiencedExperiment <-  glmer(h5AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWT)*factor(experimentalCondition),family=binomial(link='logit')
                                                              ,data=subset(dataH5AS3S4))
summary(BLRmodelH5AS3S4ExperiencedExperiment)

BLRmodelH5AS3S4ExperiencedExperimentPooled1 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                                     ,data=subset(dataH5AS3S4))
summary(BLRmodelH5AS3S4ExperiencedExperimentPooled1)

BLRmodelH5AS3S4ExperiencedExperimentCity<-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition)*factor(largerAdvantageInTradeOffWT)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH5AS3S4))
summary(BLRmodelH5AS3S4ExperiencedExperimentCity)

BLRmodelH5AS3S4ExperiencedExperimentCityPooled1 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH5AS3S4))
summary(BLRmodelH5AS3S4ExperiencedExperimentCityPooled1)

#         * S5 - S8 -----------------------------------------------------------------

BLRmodelH5AS5S8ExperiencedExperiment <-  glmer(h5AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                              ,data=subset(dataH5AS5S8))
summary(BLRmodelH5AS5S8ExperiencedExperiment)

BLRmodelH5AS5S8ExperiencedExperimentPooled1 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                                     ,data=subset(dataH5AS5S8))
summary(BLRmodelH5AS5S8ExperiencedExperimentPooled1)

BLRmodelH5AS5S8ExperiencedExperimentCity<-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH5AS5S8))
summary(BLRmodelH5AS5S8ExperiencedExperimentCity)

BLRmodelH5AS5S8ExperiencedExperimentCityPooled1 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH5AS5S8))
summary(BLRmodelH5AS5S8ExperiencedExperimentCityPooled1)

#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH5AExperiencedExperiment <-  glmer(h5AChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperiment)

BLRmodelH5AExperiencedExperimentPooled1 <-  glmer(h5AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                              ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperimentPooled1)

BLRmodelH5AExperiencedExperimentPooled2 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                                     ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperimentPooled2)

BLRmodelH5AExperiencedExperimentCity<-  glmer(h5AChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                              ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperimentCity)

BLRmodelH5AExperiencedExperimentCityPooled1<-  glmer(h5AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                 ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperimentCityPooled1)

BLRmodelH5AExperiencedExperimentCityPooled2 <-  glmer(h5AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                         ,data=subset(dataH5A))
summary(BLRmodelH5AExperiencedExperimentCityPooled2)


#   iii) H5B: The aversion to time variability in waiting times will decrease with the degree of information (S9,S12) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH5B <- choiceResponses
# dataH5B <- subset(dataH5B,scenario %in% c("09","10","12","13"))
dataH5B <- subset(dataH5B,scenario %in% c("09","12"))
dataH5B <- subset(dataH5B,experimentType == "experienced")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH5B$h5BChosen <- NA
dataH5B$h5BChosen[dataH5B$rationalRouteChosen == 0] <- 0
dataH5B$h5BChosen[dataH5B$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH5BS9S12 <- subset(dataH5B,scenario %in% c("09","12"))
# dataH5BS10S13 <- subset(dataH5B,scenario %in% c("10","13"))

#     - Covariates ------------------------------------------------------------

# # S9 & S12
# dataH5BS9S12$highVariabilityInWaitingTime <- ifelse(with(dataH5BS9S12,scenario == "12"),1,0)

dataH5B$highVariabilityInWaitingTime <- ifelse(with(dataH5B,scenario == "12"),1,0)

# # S10 & S13
# dataH5B$highVariabilityInTravelTime <- ifelse(with(dataH5B,scenario == "13"),1,0)
# 
# # S09 & S12 vs. S10 & S13
# dataH5B$variabilityInWaitingTime <- ifelse(with(dataH5B,scenario == "09"|scenario =="12"),1,0)
# dataH5B$highVariability <- ifelse(with(dataH5B,scenario == "12"|scenario =="13"),1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------

#         * S9 & S12 ----------------------------------------------------------------
BLRmodelH5BExperiencedExperiment <-  glmer(h5BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperiment)

BLRmodelH5BExperiencedExperimentPooled1 <-  glmer(h5BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperimentPooled1)

BLRmodelH5BExperiencedExperimentPooled2 <-  glmer(h5BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                       ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperimentPooled2)

#City
BLRmodelH5BExperiencedExperimentCity <-  glmer(h5BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperimentCity)

BLRmodelH5BExperiencedExperimentCityPooled1<-  glmer(h5BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                              ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperimentCityPooled1)

BLRmodelH5BExperiencedExperimentCityPooled2<-  glmer(h5BChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                          ,data=subset(dataH5B))
summary(BLRmodelH5BExperiencedExperimentCityPooled2)
# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH5BDescriptiveExperiment <-  glmer(h5BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
#                                            ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperiment)
# 
# BLRmodelH5BDescriptiveExperimentPooled1 <-  glmer(h5BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
#                                                   ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperimentPooled1)
# 
# BLRmodelH5BDescriptiveExperimentPooled2 <-  glmer(h5BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                   ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperimentPooled2)
# 
# #City
# BLRmodelH5BDescriptiveExperimentCity<-  glmer(h5BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
#                                               ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperimentCity)
# 
# BLRmodelH5BDescriptiveExperimentCityPooled1 <-  glmer(h5BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
#                                                       ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperimentCityPooled1)
# 
# BLRmodelH5BDescriptiveExperimentCityPooled2 <-  glmer(h5BChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
#                                                       ,data=subset(dataH5B))
# summary(BLRmodelH5BDescriptiveExperimentCityPooled2)

#   iii) H5C: The aversion to time variability in in-vehicle times will not change with the degree of information (S10,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH5C <- choiceResponses
# dataH5C <- subset(dataH5C,scenario %in% c("09","10","12","13"))
dataH5C <- subset(dataH5C,scenario %in% c("10","13"))
dataH5C <- subset(dataH5C,experimentType == "experienced")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH5C$h5CChosen <- NA
dataH5C$h5CChosen[dataH5C$rationalRouteChosen == 0] <- 0
dataH5C$h5CChosen[dataH5C$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH5CS9S12 <- subset(dataH5C,scenario %in% c("09","12"))
# dataH5CS10S13 <- subset(dataH5C,scenario %in% c("10","13"))

#     - Covariates ------------------------------------------------------------

# # S9 & S12
# dataH5CS9S12$highVariabilityInWaitingTime <- ifelse(with(dataH5CS9S12,scenario == "12"),1,0)

# S10 & S13
dataH5C$highVariabilityInTravelTime <- ifelse(with(dataH5C,scenario == "13"),1,0)

# # S09 & S12 vs. S10 & S13
# dataH5C$variabilityInWaitingTime <- ifelse(with(dataH5C,scenario == "09"|scenario =="12"),1,0)
# dataH5C$highVariability <- ifelse(with(dataH5C,scenario == "12"|scenario =="13"),1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------
BLRmodelH5CExperiencedExperiment <-  glmer(h5CChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperiment)

BLRmodelH5CExperiencedExperimentPooled1 <-  glmer(h5CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                        ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperimentPooled1)

BLRmodelH5CExperiencedExperimentPooled2 <-  glmer(h5CChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                        ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperimentPooled2)

#City
BLRmodelH5CExperiencedExperimentCity<-  glmer(h5CChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                    ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperimentCity)

BLRmodelH5CExperiencedExperimentCityPooled1 <-  glmer(h5CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                            ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperimentCityPooled1)

BLRmodelH5CExperiencedExperimentCityPooled2 <-  glmer(h5CChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                            ,data=subset(dataH5C))
summary(BLRmodelH5CExperiencedExperimentCityPooled2)


#   iv) H5D: The degree of information on waiting and in-vehicle times mediates the influence of aversion to waiting variability (S11,S14)-------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH5D <- choiceResponses
# dataH5D <- subset(dataH5D,scenario %in% c("09","11","12","14"))
dataH5D <- subset(dataH5D,scenario %in% c("11","14"))
dataH5D <- subset(dataH5D,experimentType == "experienced")

# View(dataH1)
#Rational here means picking the route with the variability applied in travel times
dataH5D$h5DChosen <- NA
dataH5D$h5DChosen[dataH5D$rationalRouteChosen == 0] <- 0
dataH5D$h5DChosen[dataH5D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------
dataH5D$highVariability <- ifelse(with(dataH5D,scenario == "14"),1,0)
# dataH5D$variabilityTravelTime <- ifelse(with(dataH5D,scenario %in%  c("11","14")),1,0)
#     - Estimation ------------------------------------------------
#         * S11 & S14----------------------------------------------------------------
#Both
BLRmodelH5DExperiencedExperiment <-  glmer(h5DChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                                          ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperiment)

BLRmodelH5DExperiencedExperimentPooled1 <-  glmer(h5DChosen ~(1|participantId)+factor(highVariability)*factor(experimentalCondition),family=binomial(link='logit')
                                                                 ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperimentPooled1)

BLRmodelH5DExperiencedExperimentPooled2 <-  glmer(h5DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperimentPooled2)


#City
BLRmodelH5DExperiencedExperimentCity <-  glmer(h5DChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                              ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperimentCity)

#Certainty effect: Becauuse you know the variability on waiting, you prefer it over something that has variation but it is unknown.
BLRmodelH5DExperiencedExperimentCityPooled1 <-  glmer(h5DChosen ~(1|participantId)+factor(highVariability)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                              ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperimentCityPooled1)

BLRmodelH5DExperiencedExperimentCityPooled2 <-  glmer(h5DChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                      ,data=subset(dataH5D))
summary(BLRmodelH5DExperiencedExperimentCityPooled2)


# f) H6 (DEGREE OF INFORMATION IN DESCRIPTIVE CHOICES) -------------------------------------------------------------------

#   i) Manipulation Check ------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH6S1S2 <- subset(choiceResponses,scenario %in% c("01","02"))
dataH6S1S2 <- subset(dataH6S1S2,experimentType == "descriptive")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH6S1S2$h4AChosen <- NA
dataH6S1S2$h4AChosen[dataH6S1S2$rationalRouteChosen == 0] <- 0
dataH6S1S2$h4AChosen[dataH6S1S2$rationalRouteChosen == 1] <- 1


#     - Covariates --------------------------------------------------------------

dataH6S1S2$dominatingInWaitingTime <- ifelse(dataH6S1S2$scenario=="01",1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S1 & S2 -----------------------------------------------------------------

BLRmodelH6S1S2DescriptiveExperiment <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                             ,data=subset(dataH6S1S2))
summary(BLRmodelH6S1S2DescriptiveExperiment)

BLRmodelH6S1S2DescriptiveExperimentPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                                    ,data=subset(dataH6S1S2))
summary(BLRmodelH6S1S2DescriptiveExperimentPooled1)

BLRmodelH6S1S2DescriptiveExperimentCity<-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city)*factor(dominatingInWaitingTime),family=binomial(link='logit')
                                                                ,data=subset(dataH6S1S2))
summary(BLRmodelH6S1S2DescriptiveExperimentCity)

BLRmodelH6S1S2DescriptiveExperimentCityPooled1 <-  glmer(h4AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                                        ,data=subset(dataH6S1S2))
summary(BLRmodelH6S1S2DescriptiveExperimentCityPooled1)

#   ii) H6A: The aversion to waiting will not change with the degree of information -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH6A <- choiceResponses
dataH6A <- subset(dataH6A,scenario %in% c("03","04","05","06","07","08"))
dataH6A <- subset(dataH6A,experimentType == "descriptive")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH6A$h6AChosen <- NA
dataH6A$h6AChosen[dataH6A$rationalRouteChosen == 0] <- 0
dataH6A$h6AChosen[dataH6A$rationalRouteChosen == 1] <- 1

#Subssets
dataH6AS3S4 <- subset(dataH6A,scenario %in% c("03","04"))
dataH6AS5S8 <- subset(dataH6A,scenario %in% c("05","06","07","08"))
dataH6A <- subset(dataH6A,scenario %in% c("03","04","05","06","07","08"))
#     - Covariates --------------------------------------------------------------

#S3 & S4
#S4 generates a higher difference in utility
dataH6AS3S4$largerAdvantageInTradeOffWT <- ifelse(dataH6AS3S4$scenario=="04",1,0)

#S5-S8
#- The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#- The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#- Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
dataH6AS5S8$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH6AS5S8$scenario %in% c("05","08"),1,0)

#S3-S8
#Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
#Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
#as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
dataH6A$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH6A$scenario %in% c("06","08"),1,0) 
dataH6A$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH6A$scenario %in% c("05","07"),1,0) 
dataH6A$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH6A$scenario %in% c("05","07","06","08"),1,0) 
dataH6A$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH6A$scenario %in%"04",1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S3 & S4 -----------------------------------------------------------------

BLRmodelH6AS3S4DescriptiveExperiment <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(largerAdvantageInTradeOffWT),family=binomial(link='logit')
                                               ,data=subset(dataH6AS3S4))
summary(BLRmodelH6AS3S4DescriptiveExperiment)

BLRmodelH6AS3S4DescriptiveExperimentPooled1 <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                               ,data=subset(dataH6AS3S4))
summary(BLRmodelH6AS3S4DescriptiveExperimentPooled1)

BLRmodelH6AS3S4DescriptiveExperimentCity<-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city)*factor(largerAdvantageInTradeOffWT),family=binomial(link='logit')
                                                  ,data=subset(dataH6AS3S4))
summary(BLRmodelH6AS3S4DescriptiveExperimentCity)

BLRmodelH6AS3S4DescriptiveExperimentCityPooled1<-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH6AS3S4))
summary(BLRmodelH6AS3S4DescriptiveExperimentCityPooled1)

#         * S5 - S8 -----------------------------------------------------------------

BLRmodelH6AS5S8DescriptiveExperiment <-  glmer(h6AChosen ~(1|participantId)+factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH6AS5S8))
summary(BLRmodelH6AS5S8DescriptiveExperiment)

BLRmodelH6AS5S8DescriptiveExperimentPooled1 <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                       ,data=subset(dataH6AS5S8))
summary(BLRmodelH6AS5S8DescriptiveExperimentPooled1)

BLRmodelH6AS5S8DescriptiveExperimentCity<-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                   ,data=subset(dataH6AS5S8))
summary(BLRmodelH6AS5S8DescriptiveExperimentCity)

BLRmodelH6AS5S8DescriptiveExperimentCityPooled1 <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                           ,data=subset(dataH6AS5S8))
summary(BLRmodelH6AS5S8DescriptiveExperimentCityPooled1)

#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH6ADescriptiveExperiment <-  glmer(h6AChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperiment)

BLRmodelH6ADescriptiveExperimentPooled1 <-  glmer(h6AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                       ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperimentPooled1)

BLRmodelH6ADescriptiveExperimentPooled2 <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperimentPooled2)

#Analysis by city

BLRmodelH6ADescriptiveExperimentCity<-  glmer(h6AChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                   ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperimentCity)

BLRmodelH6ADescriptiveExperimentCityPooled1 <-  glmer(h6AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                           ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperimentCityPooled1)

BLRmodelH6ADescriptiveExperimentCityPooled2 <-  glmer(h6AChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                      ,data=subset(dataH6A))
summary(BLRmodelH6ADescriptiveExperimentCityPooled2)


#   iii) H6B: The aversion to time variability in waiting will increase with the degree of information, respectivel (S9,S12)-------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH6B <- choiceResponses
# dataH6B <- subset(dataH6B,scenario %in% c("09","12","10","13"))
dataH6B <- subset(dataH6B,scenario %in% c("09","12"))
dataH6B <- subset(dataH6B,experimentType == "descriptive")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH6B$h6BChosen <- NA
dataH6B$h6BChosen[dataH6B$rationalRouteChosen == 0] <- 0
dataH6B$h6BChosen[dataH6B$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH6BS9S12 <- subset(dataH6B,scenario %in% c("09","12"))
# dataH6BS10S13 <- subset(dataH6B,scenario %in% c("10","13"))
#     - Covariates ------------------------------------------------------------

# # S9 & S12
dataH6B$highVariabilityInWaitingTime <- ifelse(with(dataH6B,scenario == "12"),1,0)

# # S10 & S13
# dataH6BS10S13$highVariabilityInTravelTime <- ifelse(with(dataH6BS10S13,scenario == "13"),1,0)
# 
# # S09 & S12 vs. S10 & S13
# dataH6B$variabilityInWaitingTime <- ifelse(with(dataH6B,scenario == "09"|scenario =="12"),1,0)
# dataH6B$highVariability <- ifelse(with(dataH6B,scenario == "12"|scenario =="13"),1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S9 & S12 ----------------------------------------------------------------
BLRmodelH6BDescriptiveExperiment <-  glmer(h6BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH6B))
summary(BLRmodelH6BDescriptiveExperiment)

BLRmodelH6BDescriptiveExperimentPooled1 <-  glmer(h6BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                ,data=subset(dataH6B))
summary(BLRmodelH6BDescriptiveExperimentPooled1)

BLRmodelH6BDescriptiveExperimentPooled2 <-  glmer(h6BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH6B))
summary(BLRmodelH6BDescriptiveExperimentPooled2)

#City
BLRmodelH6BDescriptiveExperimentCity<-  glmer(h6BChosen ~(1|participantId)+factor(experimentalCondition)*factor(city)*factor(highVariabilityInWaitingTime),family=binomial(link='logit')
                                                   ,data=subset(dataH6B))
summary(BLRmodelH6BDescriptiveExperimentCity)

BLRmodelH6BDescriptiveExperimentCityPooled1<-  glmer(h6BChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                   ,data=subset(dataH6B))
summary(BLRmodelH6BDescriptiveExperimentCityPooled1)

# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH6BDescriptiveExperiment <-  glmer(h6BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
#                                                  ,data=subset(dataH6B))
# summary(BLRmodelH6BDescriptiveExperiment)
# 
# BLRmodelH6BDescriptiveExperimentPooled1 <-  glmer(h6BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentalCondition),family=binomial(link='logit')
#                                                         ,data=subset(dataH6B))
# summary(BLRmodelH6BDescriptiveExperimentPooled1)
# 
# BLRmodelH6BDescriptiveExperimentPooled2 <-  glmer(h6BChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                   ,data=subset(dataH6B))
# summary(BLRmodelH6BDescriptiveExperimentPooled2)
# 
# # #City
# # BLRmodelH6BDescriptiveExperimentCity<-  glmer(h6BChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
# #                                                     ,data=subset(dataH6B))
# # summary(BLRmodelH6BDescriptiveExperimentCity)
# # 
# # BLRmodelH6BDescriptiveExperimentCityPooled1 <-  glmer(h6BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
# #                                                             ,data=subset(dataH6B))
# # summary(BLRmodelH6BDescriptiveExperimentCityPooled1)
# # 
# # BLRmodelH6BDescriptiveExperimentCityPooled2 <-  glmer(h6BChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
# #                                                       ,data=subset(dataH6B))
# # summary(BLRmodelH6BDescriptiveExperimentCityPooled2)

#   iii) H6C: The aversion to time variability in-vehicle times will decrease with the degree of information, respectivel (S10,S13)-------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH6C <- choiceResponses
dataH6C <- subset(dataH6C,scenario %in% c("10","13"))
dataH6C <- subset(dataH6C,experimentType == "descriptive")

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH6C$h6CChosen <- NA
dataH6C$h6CChosen[dataH6C$rationalRouteChosen == 0] <- 0
dataH6C$h6CChosen[dataH6C$rationalRouteChosen == 1] <- 1
#     - Covariates ------------------------------------------------------------

# S10 & S13
dataH6C$highVariabilityInTravelTime <- ifelse(with(dataH6C,scenario == "13"),1,0)

# # S09 & S12 vs. S10 & S13
# dataH6C$variabilityInWaitingTime <- ifelse(with(dataH6C,scenario == "09"|scenario =="12"),1,0)
# dataH6C$highVariability <- ifelse(with(dataH6C,scenario == "12"|scenario =="13"),1,0)

#     - Estimation ------------------------------------------------
#       + BLR ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------
BLRmodelH6CDescriptiveExperiment <-  glmer(h6CChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                                 ,data=subset(dataH6C))
summary(BLRmodelH6CDescriptiveExperiment)

BLRmodelH6CDescriptiveExperimentPooled1 <-  glmer(h6CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(experimentalCondition),family=binomial(link='logit')
                                                        ,data=subset(dataH6C))
summary(BLRmodelH6CDescriptiveExperimentPooled1)

BLRmodelH6CDescriptiveExperimentPooled2 <-  glmer(h6CChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH6C))
summary(BLRmodelH6CDescriptiveExperimentPooled2)

#City
BLRmodelH6CDescriptiveExperimentCity<-  glmer(h6CChosen ~(1|participantId)+factor(experimentalCondition)*factor(city)*factor(highVariabilityInTravelTime),family=binomial(link='logit')
                                                    ,data=subset(dataH6C))
summary(BLRmodelH6CDescriptiveExperimentCity)

BLRmodelH6CDescriptiveExperimentCityPooled1 <-  glmer(h6CChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                            ,data=subset(dataH6C))
summary(BLRmodelH6CDescriptiveExperimentCityPooled1)


#   iv) H6D: The degree of information on waiting and in-vehicle times mediates the influence of aversion to waiting variability (S11,S14)-------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH6D <- choiceResponses
dataH6D <- subset(dataH6D,scenario %in% c("11","14"))
dataH6D <- subset(dataH6D,experimentType == "descriptive")

# View(dataH1)
#Rational here means picking the route with the variability applied in travel times
dataH6D$h6DChosen <- NA
dataH6D$h6DChosen[dataH6D$rationalRouteChosen == 0] <- 0
dataH6D$h6DChosen[dataH6D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH6D$tradeOffWT <- with(dataH6D,averageWaitingTime/averageTravelTime)
dataH6D$xWaitingTime <- with(dataH6D,averageWaitingTime/(averageJourneyTime))
dataH6D$highVariability <- ifelse(with(dataH6D,scenario == "14"),1,0)


#     - Estimation ------------------------------------------------

#Both
BLRmodelH6DDescriptiveExperiment <-  glmer(h6DChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperiment)

BLRmodelH6DDescriptiveExperimentPooled1 <-  glmer(h6DChosen ~(1|participantId)+factor(highVariability)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperimentPooled1)

BLRmodelH6DDescriptiveExperimentPooled2 <-  glmer(h6DChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperimentPooled2)


#City
BLRmodelH6DDescriptiveExperimentCity <-  glmer(h6DChosen ~(1|participantId)+factor(scenario)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                               ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperimentCity)

BLRmodelH6DDescriptiveExperimentCityPooled1 <-  glmer(h6DChosen ~(1|participantId)+factor(highVariability)*factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                      ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperimentCityPooled1)

BLRmodelH6DDescriptiveExperimentCityPooled2 <-  glmer(h6DChosen ~(1|participantId)+factor(experimentalCondition)*factor(city),family=binomial(link='logit')
                                                      ,data=subset(dataH6D))
summary(BLRmodelH6DDescriptiveExperimentCityPooled2)


# g) H7 (TRAVEL EXPERIENCE) -------------------------------------------------------------------

# #   i) Manipulation Check ------------------------------------------------------
# #     - Dataset ---------------------------------------------------------------
# dataH7S1S2 <- subset(choiceResponses,scenario %in% c("01","02"))
# 
# # View(dataH1)
# #Rational here means picking the shorter route. We actually think people will take the longer one
# dataH7S1S2$h7AChosen <- NA
# dataH7S1S2$h7AChosen[dataH7S1S2$rationalRouteChosen == 0] <- 0
# dataH7S1S2$h7AChosen[dataH7S1S2$rationalRouteChosen == 1] <- 1
# 
# 
# #     - Covariates --------------------------------------------------------------
# 
# dataH7S1S2$dominatingInWaitingTime <- ifelse(dataH7S1S2$scenario=="01",1,0)
# 
# #     - Estimation ------------------------------------------------
# #       + Experienced experiment ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH7S1S2ExperiencedExperiment <-  glmer(h7AChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                              ,data=subset(dataH7S1S2,experimentType == "experienced"))
# summary(BLRmodelH7S1S2ExperiencedExperiment)
# 
# BLRmodelH7S1S2ExperiencedExperimentPooled1 <-  glmer(h7AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                     ,data=subset(dataH7S1S2,experimentType == "experienced"))
# summary(BLRmodelH7S1S2ExperiencedExperimentPooled1)
# 
# BLRmodelH7S1S2ExperiencedExperimentPooled2 <-  glmer(h7AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH7S1S2,experimentType == "experienced"))
# summary(BLRmodelH7S1S2ExperiencedExperimentPooled2)
# 
# BLRmodelH7S1S2ExperiencedExperimentPooled3 <-  glmer(h7AChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH7S1S2,experimentType == "experienced"))
# summary(BLRmodelH7S1S2ExperiencedExperimentPooled3)
# 
# #       + Descriptive experiment ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH7S1S2DescriptiveExperiment <-  glmer(h7AChosen ~(1|participantId)+factor(city)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                ,data=subset(dataH7S1S2,experimentType == "descriptive"))
# summary(BLRmodelH7S1S2DescriptiveExperiment)
# 
# BLRmodelH7S1S2DescriptiveExperimentPooled1 <-  glmer(h7AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                       ,data=subset(dataH7S1S2,experimentType == "descriptive"))
# summary(BLRmodelH7S1S2DescriptiveExperimentPooled1)
# 
# BLRmodelH7S1S2DescriptiveExperimentPooled2 <-  glmer(h7AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH7S1S2,experimentType == "descriptive"))
# summary(BLRmodelH7S1S2DescriptiveExperimentPooled2)
# 
# BLRmodelH7S1S2DescriptiveExperimentPooled3 <-  glmer(h7AChosen ~(1|participantId)+factor(dominatingInWaitingTime),family=binomial(link='logit')
#                                                      ,data=subset(dataH7S1S2,experimentType == "descriptive"))
# summary(BLRmodelH7S1S2DescriptiveExperimentPooled3)

#   ii) H7A: The aversion to waiting increases with the degree of experience (S3-S8) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH7A <- choiceResponses
dataH7A <- subset(dataH7A,scenario %in% c("03","04","05","06","07","08"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH7A$h7AChosen <- NA
dataH7A$h7AChosen[dataH7A$rationalRouteChosen == 0] <- 0
dataH7A$h7AChosen[dataH7A$rationalRouteChosen == 1] <- 1

dataH7AS3S4 <- subset(dataH7A,scenario %in% c("03","04"))
dataH7AS5S8 <- subset(dataH7A,scenario %in% c("05","06","07","08"))
dataH7A <- subset(dataH7A,scenario %in% c("03","04","05","06","07","08"))


#     - Covariates --------------------------------------------------------------

#S3 & S4
#S4 generates a higher difference in utility
dataH7AS3S4$largerAdvantageInTradeOffWT <- ifelse(dataH7AS3S4$scenario=="04",1,0)

#S5-S8
#- The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#- The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#- Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
dataH7AS5S8$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH7AS5S8$scenario %in% c("05","08"),1,0)

#S3-S8
#Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
#Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
#as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
dataH7A$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH7A$scenario %in% c("06","08"),1,0) 
dataH7A$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH7A$scenario %in% c("05","07"),1,0) 
dataH7A$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH7A$scenario %in% c("05","07","06","08"),1,0) 
dataH7A$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH7A$scenario %in%"04",1,0)

#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH7AExperiencedExperiment <-  glmer(h7AChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                                          ,data=subset(dataH7A,experimentType == "experienced"))
summary(BLRmodelH7AExperiencedExperiment)

BLRmodelH7AExperiencedExperimentPooled1 <-  glmer(h7AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                                                 ,data=subset(dataH7A,experimentType == "experienced"))
summary(BLRmodelH7AExperiencedExperimentPooled1)

BLRmodelH7AExperiencedExperimentPooled2 <-  glmer(h7AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7A,experimentType == "experienced"))
summary(BLRmodelH7AExperiencedExperimentPooled2)

BLRmodelH7AExperiencedExperimentPooled3 <-  glmer(h7AChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7A,experimentType == "experienced"))
summary(BLRmodelH7AExperiencedExperimentPooled3)


#       + Descriptive Choices ------------------------------------------------------------
#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH7ADescriptiveExperiment <-  glmer(h7AChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                                            ,data=subset(dataH7A,experimentType == "descriptive"))
summary(BLRmodelH7ADescriptiveExperiment)

BLRmodelH7ADescriptiveExperimentPooled1 <-  glmer(h7AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                                                   ,data=subset(dataH7A,experimentType == "descriptive"))
summary(BLRmodelH7ADescriptiveExperimentPooled1)

BLRmodelH7ADescriptiveExperimentPooled2 <-  glmer(h7AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH7A,experimentType == "descriptive"))
summary(BLRmodelH7ADescriptiveExperimentPooled2)

BLRmodelH7ADescriptiveExperimentPooled3 <-  glmer(h7AChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                                                   ,data=subset(dataH7A,experimentType == "descriptive"))
summary(BLRmodelH7ADescriptiveExperimentPooled3)

#     - Conclusion -----------------------------------------------------------

#   iii) H7B: The aversion to variability while waiting increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH7B <- choiceResponses
# dataH7B <- subset(dataH7B,scenario %in% c("09","10","12","13"))
dataH7B <- subset(dataH7B,scenario %in% c("09","12"))

# View(dataH1)
#Rational here means picking the route with no variability
dataH7B$h7BChosen <- NA
dataH7B$h7BChosen[dataH7B$rationalRouteChosen == 0] <- 0
dataH7B$h7BChosen[dataH7B$rationalRouteChosen == 1] <- 1

#     - Covariates ------------------------------------------------------------

# S9 & S12
dataH7B$tradeOffWT <- with(dataH7B,averageWaitingTime/averageTravelTime)
dataH7B$xWaitingTime <- with(dataH7B,averageWaitingTime/(averageJourneyTime))
dataH7B$highVariabilityInWaitingTime <- ifelse(with(dataH7B,scenario == "12"),1,0)

# # S10 & S13
# dataH7BS10S13$tradeOffWT <- with(dataH7BS10S13,averageWaitingTime/averageTravelTime)
# dataH7BS10S13$xWaitingTime <- with(dataH7BS10S13,averageWaitingTime/(averageJourneyTime))
# dataH7BS10S13$highVariabilityInTravelTime <- ifelse(with(dataH7BS10S13,scenario == "13"),1,0)
# 
# # S09 & S12 vs. S10 & S13
# dataH7B$variabilityInWaitingTime <- ifelse(with(dataH7B,scenario == "09"|scenario =="12"),1,0)
# dataH7B$highVariability <- ifelse(with(dataH7B,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S09 & S12----------------------------------------------------------------

BLRmodelH7BExperiencedExperiment <-  glmer(h7BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7B,experimentType == "experienced"))
summary(BLRmodelH7BExperiencedExperiment)

BLRmodelH7BExperiencedExperimentPooled1 <-  glmer(h7BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "experienced"))
summary(BLRmodelH7BExperiencedExperimentPooled1)

BLRmodelH7BExperiencedExperimentPooled2 <-  glmer(h7BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "experienced"))
summary(BLRmodelH7BExperiencedExperimentPooled2)

BLRmodelH7BExperiencedExperimentPooled3 <-  glmer(h7BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "experienced"))
summary(BLRmodelH7BExperiencedExperimentPooled3)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH7BExperiencedExperiment <-  glmer(h7BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                           ,data=subset(dataH7B,experimentType == "experienced"))
# summary(BLRmodelH7BExperiencedExperiment)
# 
# BLRmodelH7BExperiencedExperimentPooled1 <-  glmer(h7BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH7B,experimentType == "experienced"))
# summary(BLRmodelH7BExperiencedExperimentPooled1)
# 
# BLRmodelH7BExperiencedExperimentPooled2 <-  glmer(h7BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH7B,experimentType == "experienced"))
# summary(BLRmodelH7BExperiencedExperimentPooled2)
# 
# BLRmodelH7BExperiencedExperimentPooled3 <-  glmer(h7BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                   ,data=subset(dataH7B,experimentType == "experienced"))
# summary(BLRmodelH7BExperiencedExperimentPooled3)

#       + Descriptive Choices ------------------------------------------------------------
#         * S09 & S12 ----------------------------------------------------------------

BLRmodelH7BDescriptiveExperiment <-  glmer(h7BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7B,experimentType == "descriptive"))
summary(BLRmodelH7BDescriptiveExperiment)

BLRmodelH7BDescriptiveExperimentPooled1 <-  glmer(h7BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "descriptive"))
summary(BLRmodelH7BDescriptiveExperimentPooled1)

BLRmodelH7BDescriptiveExperimentPooled2 <-  glmer(h7BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "descriptive"))
summary(BLRmodelH7BDescriptiveExperimentPooled2)

BLRmodelH7BDescriptiveExperimentPooled3 <-  glmer(h7BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7B,experimentType == "descriptive"))
summary(BLRmodelH7BDescriptiveExperimentPooled3)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH7BDescriptiveExperiment <-  glmer(h7BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                             ,data=subset(dataH7B,experimentType == "descriptive"))
# summary(BLRmodelH7BDescriptiveExperiment)
# 
# BLRmodelH7BDescriptiveExperimentPooled1 <-  glmer(h7BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH7B,experimentType == "descriptive"))
# summary(BLRmodelH7BDescriptiveExperimentPooled1)
# 
# BLRmodelH7BDescriptiveExperimentPooled2 <-  glmer(h7BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH7B,experimentType == "descriptive"))
# summary(BLRmodelH7BDescriptiveExperimentPooled2)
# 
# BLRmodelH7BDescriptiveExperimentPooled3 <-  glmer(h7BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH7B,experimentType == "descriptive"))
# summary(BLRmodelH7BDescriptiveExperimentPooled3)
# 

#   iii) H7C: The aversion to variability while travelling increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH7C <- choiceResponses
dataH7C <- subset(dataH7C,scenario %in% c("10","13"))

# View(dataH1)
#Rational here means picking the route with no variability
dataH7C$h7CChosen <- NA
dataH7C$h7CChosen[dataH7C$rationalRouteChosen == 0] <- 0
dataH7C$h7CChosen[dataH7C$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH7CS9S12 <- subset(dataH7C,scenario %in% c("09","12"))
# dataH7CS10S13 <- subset(dataH7C,scenario %in% c("10","13"))
# dataH7C <- subset(dataH7C,scenario %in% c("09","12","10","13"))

#     - Covariates ------------------------------------------------------------

# # S9 & S12
# dataH7CS9S12$tradeOffWT <- with(dataH7CS9S12,averageWaitingTime/averageTravelTime)
# dataH7CS9S12$xWaitingTime <- with(dataH7CS9S12,averageWaitingTime/(averageJourneyTime))
# dataH7CS9S12$highVariabilityInWaitingTime <- ifelse(with(dataH7CS9S12,scenario == "12"),1,0)
# 
# # S10 & S13
# dataH7CS10S13$tradeOffWT <- with(dataH7CS10S13,averageWaitingTime/averageTravelTime)
# dataH7CS10S13$xWaitingTime <- with(dataH7CS10S13,averageWaitingTime/(averageJourneyTime))
dataH7C$highVariabilityInTravelTime <- ifelse(with(dataH7C,scenario == "13"),1,0)

# S09 & S12 vs. S10 & S13
# dataH7C$variabilityInWaitingTime <- ifelse(with(dataH7C,scenario == "09"|scenario =="12"),1,0)
# dataH7C$highVariability <- ifelse(with(dataH7C,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S10 & S13----------------------------------------------------------------

BLRmodelH7CExperiencedExperiment <-  glmer(h7CChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7C,experimentType == "experienced"))
summary(BLRmodelH7CExperiencedExperiment)

BLRmodelH7CExperiencedExperimentPooled1 <-  glmer(h7CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "experienced"))
summary(BLRmodelH7CExperiencedExperimentPooled1)

BLRmodelH7CExperiencedExperimentPooled2 <-  glmer(h7CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "experienced"))
summary(BLRmodelH7CExperiencedExperimentPooled2)

BLRmodelH7CExperiencedExperimentPooled3 <-  glmer(h7CChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "experienced"))
summary(BLRmodelH7CExperiencedExperimentPooled3)


#       + Descriptive Choices ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------

BLRmodelH7CDescriptiveExperiment <-  glmer(h7CChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7C,experimentType == "descriptive"))
summary(BLRmodelH7CDescriptiveExperiment)

BLRmodelH7CDescriptiveExperimentPooled1 <-  glmer(h7CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "descriptive"))
summary(BLRmodelH7CDescriptiveExperimentPooled1)

BLRmodelH7CDescriptiveExperimentPooled2 <-  glmer(h7CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "descriptive"))
summary(BLRmodelH7CDescriptiveExperimentPooled2)

BLRmodelH7CDescriptiveExperimentPooled3 <-  glmer(h7CChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7C,experimentType == "descriptive"))
summary(BLRmodelH7CDescriptiveExperimentPooled3)


#   iv) H7D: The aversion to waiting variability increases with the degree of experience (S11,S14) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH7D <- choiceResponses
dataH7D <- subset(dataH7D,scenario %in% c("11","14"))

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH7D$h7DS11S14Chosen <- NA
dataH7D$h7DChosen[dataH7D$rationalRouteChosen == 0] <- 0
dataH7D$h7DChosen[dataH7D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH7D$highVariability <- ifelse(with(dataH7D,scenario == "14"),1,0)

#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
BLRmodelH7DExperiencedExperiment <-  glmer(h7DChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7D,experimentType == "experienced"))
summary(BLRmodelH7DExperiencedExperiment)

BLRmodelH7DExperiencedExperimentPooled1 <-  glmer(h7DChosen ~(1|participantId)+factor(highVariability)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "experienced"))
summary(BLRmodelH7DExperiencedExperimentPooled1)

BLRmodelH7DExperiencedExperimentPooled2 <-  glmer(h7DChosen ~(1|participantId)+factor(highVariability)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "experienced"))
summary(BLRmodelH7DExperiencedExperimentPooled2)

BLRmodelH7DExperiencedExperimentPooled3 <-  glmer(h7DChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "experienced"))
summary(BLRmodelH7DExperiencedExperimentPooled3)

#       + Descriptive Choices ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
BLRmodelH7DDescriptiveExperiment <-  glmer(h7DChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH7D,experimentType == "descriptive"))
summary(BLRmodelH7DDescriptiveExperiment)

BLRmodelH7DDescriptiveExperimentPooled1 <-  glmer(h7DChosen ~(1|participantId)+factor(highVariability)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "descriptive"))
summary(BLRmodelH7DDescriptiveExperimentPooled1)

BLRmodelH7DDescriptiveExperimentPooled2 <-  glmer(h7DChosen ~(1|participantId)+factor(highVariability)*factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "descriptive"))
summary(BLRmodelH7DDescriptiveExperimentPooled2)

BLRmodelH7DDescriptiveExperimentPooled3 <-  glmer(h7DChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
                                                  ,data=subset(dataH7D,experimentType == "descriptive"))
summary(BLRmodelH7DDescriptiveExperimentPooled3)

# g) H8 (GENDER) -------------------------------------------------------------------

# #   i) Manipulation Check ------------------------------------------------------
# #     - Dataset ---------------------------------------------------------------
# dataH8S1S2 <- subset(choiceResponses,scenario %in% c("01","02"))
# 
# # View(dataH1)
# #Rational here means picking the shorter route. We actually think people will take the longer one
# dataH8S1S2$H8AChosen <- NA
# dataH8S1S2$H8AChosen[dataH8S1S2$rationalRouteChosen == 0] <- 0
# dataH8S1S2$H8AChosen[dataH8S1S2$rationalRouteChosen == 1] <- 1
# 
# 
# #     - Covariates --------------------------------------------------------------
# 
# dataH8S1S2$dominatingInWaitingTime <- ifelse(dataH8S1S2$scenario=="01",1,0)
# 
# #     - Estimation ------------------------------------------------
# #       + Experienced experiment ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH8S1S2ExperiencedExperiment <-  glmer(H8AChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                               ,data=subset(dataH8S1S2,experimentType == "experienced"))
# summary(BLRmodelH8S1S2ExperiencedExperiment)
# 
# BLRmodelH8S1S2ExperiencedExperimentPooled1 <-  glmer(H8AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "experienced"))
# summary(BLRmodelH8S1S2ExperiencedExperimentPooled1)
# 
# BLRmodelH8S1S2ExperiencedExperimentPooled2 <-  glmer(H8AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "experienced"))
# summary(BLRmodelH8S1S2ExperiencedExperimentPooled2)
# 
# BLRmodelH8S1S2ExperiencedExperimentPooled3 <-  glmer(H8AChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "experienced"))
# summary(BLRmodelH8S1S2ExperiencedExperimentPooled3)
# 
# #       + Descriptive experiment ------------------------------------------------------------
# #         * S1 & S2 -----------------------------------------------------------------
# 
# BLRmodelH8S1S2DescriptiveExperiment <-  glmer(H8AChosen ~(1|participantId)+factor(city)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                               ,data=subset(dataH8S1S2,experimentType == "descriptive"))
# summary(BLRmodelH8S1S2DescriptiveExperiment)
# 
# BLRmodelH8S1S2DescriptiveExperimentPooled1 <-  glmer(H8AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "descriptive"))
# summary(BLRmodelH8S1S2DescriptiveExperimentPooled1)
# 
# BLRmodelH8S1S2DescriptiveExperimentPooled2 <-  glmer(H8AChosen ~(1|participantId)+factor(dominatingInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "descriptive"))
# summary(BLRmodelH8S1S2DescriptiveExperimentPooled2)
# 
# BLRmodelH8S1S2DescriptiveExperimentPooled3 <-  glmer(H8AChosen ~(1|participantId)+factor(dominatingInWaitingTime),family=binomial(link='logit')
#                                                      ,data=subset(dataH8S1S2,experimentType == "descriptive"))
# summary(BLRmodelH8S1S2DescriptiveExperimentPooled3)

#   ii) H8A: The aversion to waiting increases with the degree of experience (S3-S8) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH8A <- choiceResponses
dataH8A <- subset(dataH8A,scenario %in% c("03","04","05","06","07","08"))
dataH8AOtherGender <- subset(dataH8A,gender == "other")
dataH8A <- subset(dataH8A, gender == "male" | gender == "female")
nObsOtherGender <- dim(dataH8AOtherGender)[1] #Number of choice responses from participants who declare "other" gender
nOtherGender <-  length(unique(dataH8AOtherGender$participantId)) #Number of participants who declares "other" gender

# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH8A$H8AChosen <- NA
dataH8A$H8AChosen[dataH8A$rationalRouteChosen == 0] <- 0
dataH8A$H8AChosen[dataH8A$rationalRouteChosen == 1] <- 1

# dataH8AS3S4 <- subset(dataH8A,scenario %in% c("03","04"))
# dataH8AS5S8 <- subset(dataH8A,scenario %in% c("05","06","07","08"))
dataH8A <- subset(dataH8A,scenario %in% c("03","04","05","06","07","08"))


#     - Covariates --------------------------------------------------------------

#S3 & S4
#S4 generates a higher difference in utility
# dataH8AS3S4$largerAdvantageInTradeOffWT <- ifelse(dataH8AS3S4$scenario=="04",1,0)

#S5-S8
#- The longer journey options in Scenarios 5 and 8 gives a similar gain in U than the shorter option
#- The longer journey options in Scenarios 6 and 7 gives a similar gain in U but lower than Scenarios 5 and 8
#- Then we would expect the average between 5-8 increase preferences for longer journey, marginally at least.
# dataH8AS5S8$largerAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH8AS5S8$scenario %in% c("05","08"),1,0)

#S3-S8
#Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
#Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
#as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
dataH8A$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH8A$scenario %in% c("06","08"),1,0) 
dataH8A$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH8A$scenario %in% c("05","07"),1,0) 
dataH8A$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH8A$scenario %in% c("05","07","06","08"),1,0) 
dataH8A$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH8A$scenario %in%"04",1,0)

#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH8AExperiencedExperiment <-  glmer(H8AChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8A,experimentType == "experienced"))
summary(BLRmodelH8AExperiencedExperiment)

BLRmodelH8AExperiencedExperimentPooled1 <-  glmer(H8AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "experienced"))
summary(BLRmodelH8AExperiencedExperimentPooled1)

BLRmodelH8AExperiencedExperimentPooled2 <-  glmer(H8AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "experienced"))
summary(BLRmodelH8AExperiencedExperimentPooled2)

BLRmodelH8AExperiencedExperimentPooled3 <-  glmer(H8AChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "experienced"))
summary(BLRmodelH8AExperiencedExperimentPooled3)


#       + Descriptive Choices ------------------------------------------------------------
#         * S3 - S8 -----------------------------------------------------------------

BLRmodelH8ADescriptiveExperiment <-  glmer(H8AChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8A,experimentType == "descriptive"))
summary(BLRmodelH8ADescriptiveExperiment)

BLRmodelH8ADescriptiveExperimentPooled1 <-  glmer(H8AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "descriptive"))
summary(BLRmodelH8ADescriptiveExperimentPooled1)

BLRmodelH8ADescriptiveExperimentPooled2 <-  glmer(H8AChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "descriptive"))
summary(BLRmodelH8ADescriptiveExperimentPooled2)

BLRmodelH8ADescriptiveExperimentPooled3 <-  glmer(H8AChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8A,experimentType == "descriptive"))
summary(BLRmodelH8ADescriptiveExperimentPooled3)

#     - Conclusion -----------------------------------------------------------

#   iii) H8B: The aversion to variability while waiting increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH8B <- choiceResponses
# dataH8B <- subset(dataH8B,scenario %in% c("09","10","12","13"))
dataH8B <- subset(dataH8B,scenario %in% c("09","12"))
dataH8BOtherGender <- subset(dataH8B,gender == "other")
dataH8B <- subset(dataH8B, gender == "male" | gender == "female")
nObsOtherGender <- dim(dataH8BOtherGender)[1] #Number of choice responses from participants who declare "other" gender
nOtherGender <-  length(unique(dataH8BOtherGender$participantId)) #Number of participants who declares "other" gender

# View(dataH1)
#Rational here means picking the route with no variability
dataH8B$H8BChosen <- NA
dataH8B$H8BChosen[dataH8B$rationalRouteChosen == 0] <- 0
dataH8B$H8BChosen[dataH8B$rationalRouteChosen == 1] <- 1

#     - Covariates ------------------------------------------------------------

# S9 & S12
dataH8B$tradeOffWT <- with(dataH8B,averageWaitingTime/averageTravelTime)
dataH8B$xWaitingTime <- with(dataH8B,averageWaitingTime/(averageJourneyTime))
dataH8B$highVariabilityInWaitingTime <- ifelse(with(dataH8B,scenario == "12"),1,0)

# # S10 & S13
# dataH8BS10S13$tradeOffWT <- with(dataH8BS10S13,averageWaitingTime/averageTravelTime)
# dataH8BS10S13$xWaitingTime <- with(dataH8BS10S13,averageWaitingTime/(averageJourneyTime))
# dataH8BS10S13$highVariabilityInTravelTime <- ifelse(with(dataH8BS10S13,scenario == "13"),1,0)
# 
# # S09 & S12 vs. S10 & S13
# dataH8B$variabilityInWaitingTime <- ifelse(with(dataH8B,scenario == "09"|scenario =="12"),1,0)
# dataH8B$highVariability <- ifelse(with(dataH8B,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S09 & S12----------------------------------------------------------------

BLRmodelH8BExperiencedExperiment <-  glmer(H8BChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8B,experimentType == "experienced"))
summary(BLRmodelH8BExperiencedExperiment)

BLRmodelH8BExperiencedExperimentPooled1 <-  glmer(H8BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "experienced"))
summary(BLRmodelH8BExperiencedExperimentPooled1)

BLRmodelH8BExperiencedExperimentPooled2 <-  glmer(H8BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "experienced"))
summary(BLRmodelH8BExperiencedExperimentPooled2)

BLRmodelH8BExperiencedExperimentPooled3 <-  glmer(H8BChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "experienced"))
summary(BLRmodelH8BExperiencedExperimentPooled3)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH8BExperiencedExperiment <-  glmer(H8BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                           ,data=subset(dataH8B,experimentType == "experienced"))
# summary(BLRmodelH8BExperiencedExperiment)
# 
# BLRmodelH8BExperiencedExperimentPooled1 <-  glmer(H8BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH8B,experimentType == "experienced"))
# summary(BLRmodelH8BExperiencedExperimentPooled1)
# 
# BLRmodelH8BExperiencedExperimentPooled2 <-  glmer(H8BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                  ,data=subset(dataH8B,experimentType == "experienced"))
# summary(BLRmodelH8BExperiencedExperimentPooled2)
# 
# BLRmodelH8BExperiencedExperimentPooled3 <-  glmer(H8BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                   ,data=subset(dataH8B,experimentType == "experienced"))
# summary(BLRmodelH8BExperiencedExperimentPooled3)

#       + Descriptive Choices ------------------------------------------------------------
#         * S09 & S12 ----------------------------------------------------------------

BLRmodelH8BDescriptiveExperiment <-  glmer(H8BChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8B,experimentType == "descriptive"))
summary(BLRmodelH8BDescriptiveExperiment)

BLRmodelH8BDescriptiveExperimentPooled1 <-  glmer(H8BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "descriptive"))
summary(BLRmodelH8BDescriptiveExperimentPooled1)

BLRmodelH8BDescriptiveExperimentPooled2 <-  glmer(H8BChosen ~(1|participantId)+factor(highVariabilityInWaitingTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "descriptive"))
summary(BLRmodelH8BDescriptiveExperimentPooled2)

BLRmodelH8BDescriptiveExperimentPooled3 <-  glmer(H8BChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8B,experimentType == "descriptive"))
summary(BLRmodelH8BDescriptiveExperimentPooled3)


# #         * S09 & S12 vs. S10 & S13----------------------------------------------------------------
# 
# BLRmodelH8BDescriptiveExperiment <-  glmer(H8BChosen ~(1|participantId)+factor(scenario)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                             ,data=subset(dataH8B,experimentType == "descriptive"))
# summary(BLRmodelH8BDescriptiveExperiment)
# 
# BLRmodelH8BDescriptiveExperimentPooled1 <-  glmer(H8BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city)*factor(experimentalCondition),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH8B,experimentType == "descriptive"))
# summary(BLRmodelH8BDescriptiveExperimentPooled1)
# 
# BLRmodelH8BDescriptiveExperimentPooled2 <-  glmer(H8BChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH8B,experimentType == "descriptive"))
# summary(BLRmodelH8BDescriptiveExperimentPooled2)
# 
# BLRmodelH8BDescriptiveExperimentPooled3 <-  glmer(H8BChosen ~(1|participantId)+factor(city),family=binomial(link='logit')
#                                                                                    ,data=subset(dataH8B,experimentType == "descriptive"))
# summary(BLRmodelH8BDescriptiveExperimentPooled3)
# 

#   iii) H8C: The aversion to variability while travelling increases with the degree of experience (S9,S10,S12,S13) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH8C <- choiceResponses
dataH8C <- subset(dataH8C,scenario %in% c("10","13"))

dataH8COtherGender <- subset(dataH8C,gender == "other")
dataH8C <- subset(dataH8C, gender == "male" | gender == "female")
nObsOtherGender <- dim(dataH8COtherGender)[1] #Number of choice responses from participants who declare "other" gender
nOtherGender <-  length(unique(dataH8COtherGender$participantId)) #Number of participants who declares "other" gender


# View(dataH1)
#Rational here means picking the route with no variability
dataH8C$H8CChosen <- NA
dataH8C$H8CChosen[dataH8C$rationalRouteChosen == 0] <- 0
dataH8C$H8CChosen[dataH8C$rationalRouteChosen == 1] <- 1

# # Subdatasets
# dataH8CS9S12 <- subset(dataH8C,scenario %in% c("09","12"))
# dataH8CS10S13 <- subset(dataH8C,scenario %in% c("10","13"))
# dataH8C <- subset(dataH8C,scenario %in% c("09","12","10","13"))

#     - Covariates ------------------------------------------------------------

# # S9 & S12
# dataH8CS9S12$tradeOffWT <- with(dataH8CS9S12,averageWaitingTime/averageTravelTime)
# dataH8CS9S12$xWaitingTime <- with(dataH8CS9S12,averageWaitingTime/(averageJourneyTime))
# dataH8CS9S12$highVariabilityInWaitingTime <- ifelse(with(dataH8CS9S12,scenario == "12"),1,0)
# 
# # S10 & S13
# dataH8CS10S13$tradeOffWT <- with(dataH8CS10S13,averageWaitingTime/averageTravelTime)
# dataH8CS10S13$xWaitingTime <- with(dataH8CS10S13,averageWaitingTime/(averageJourneyTime))
dataH8C$highVariabilityInTravelTime <- ifelse(with(dataH8C,scenario == "13"),1,0)

# S09 & S12 vs. S10 & S13
# dataH8C$variabilityInWaitingTime <- ifelse(with(dataH8C,scenario == "09"|scenario =="12"),1,0)
# dataH8C$highVariability <- ifelse(with(dataH8C,scenario == "12"|scenario =="13"),1,0)


#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S10 & S13----------------------------------------------------------------

BLRmodelH8CExperiencedExperiment <-  glmer(H8CChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8C,experimentType == "experienced"))
summary(BLRmodelH8CExperiencedExperiment)

BLRmodelH8CExperiencedExperimentPooled1 <-  glmer(H8CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "experienced"))
summary(BLRmodelH8CExperiencedExperimentPooled1)

BLRmodelH8CExperiencedExperimentPooled2 <-  glmer(H8CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "experienced"))
summary(BLRmodelH8CExperiencedExperimentPooled2)

BLRmodelH8CExperiencedExperimentPooled3 <-  glmer(H8CChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "experienced"))
summary(BLRmodelH8CExperiencedExperimentPooled3)


#       + Descriptive Choices ------------------------------------------------------------
#         * S10 & S13 ----------------------------------------------------------------

BLRmodelH8CDescriptiveExperiment <-  glmer(H8CChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8C,experimentType == "descriptive"))
summary(BLRmodelH8CDescriptiveExperiment)

BLRmodelH8CDescriptiveExperimentPooled1 <-  glmer(H8CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "descriptive"))
summary(BLRmodelH8CDescriptiveExperimentPooled1)

BLRmodelH8CDescriptiveExperimentPooled2 <-  glmer(H8CChosen ~(1|participantId)+factor(highVariabilityInTravelTime)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "descriptive"))
summary(BLRmodelH8CDescriptiveExperimentPooled2)

BLRmodelH8CDescriptiveExperimentPooled3 <-  glmer(H8CChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8C,experimentType == "descriptive"))
summary(BLRmodelH8CDescriptiveExperimentPooled3)


#   iv) H8D: The aversion to waiting variability increases with the degree of experience (S11,S14) -------------------------------------------------------------------
#     - Dataset ---------------------------------------------------------------
dataH8D <- choiceResponses
dataH8D <- subset(dataH8D,scenario %in% c("11","14"))

dataH8DOtherGender <- subset(dataH8D,gender == "other")
dataH8D <- subset(dataH8D, gender == "male" | gender == "female")
nObsOtherGender <- dim(dataH8DOtherGender)[1] #Number of choice responses from participants who declare "other" gender
nOtherGender <-  length(unique(dataH8DOtherGender$participantId)) #Number of participants who declares "other" gender


# View(dataH1)
#Rational here means picking the shorter route. We actually think people will take the longer one
dataH8D$H8DS11S14Chosen <- NA
dataH8D$H8DChosen[dataH8D$rationalRouteChosen == 0] <- 0
dataH8D$H8DChosen[dataH8D$rationalRouteChosen == 1] <- 1

#     - Covariates --------------------------------------------------------------

dataH8D$highVariability <- ifelse(with(dataH8D,scenario == "14"),1,0)

#     - Estimation ------------------------------------------------
#       + Experiential Choices ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
BLRmodelH8DExperiencedExperiment <-  glmer(H8DChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8D,experimentType == "experienced"))
summary(BLRmodelH8DExperiencedExperiment)

BLRmodelH8DExperiencedExperimentPooled1 <-  glmer(H8DChosen ~(1|participantId)+factor(highVariability)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "experienced"))
summary(BLRmodelH8DExperiencedExperimentPooled1)

BLRmodelH8DExperiencedExperimentPooled2 <-  glmer(H8DChosen ~(1|participantId)+factor(highVariability)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "experienced"))
summary(BLRmodelH8DExperiencedExperimentPooled2)

BLRmodelH8DExperiencedExperimentPooled3 <-  glmer(H8DChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "experienced"))
summary(BLRmodelH8DExperiencedExperimentPooled3)

#       + Descriptive Choices ------------------------------------------------------------
#         * S11 & S14 ----------------------------------------------------------------
BLRmodelH8DDescriptiveExperiment <-  glmer(H8DChosen ~(1|participantId)+factor(scenario)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                           ,data=subset(dataH8D,experimentType == "descriptive"))
summary(BLRmodelH8DDescriptiveExperiment)

BLRmodelH8DDescriptiveExperimentPooled1 <-  glmer(H8DChosen ~(1|participantId)+factor(highVariability)*factor(gender)*factor(experimentalCondition),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "descriptive"))
summary(BLRmodelH8DDescriptiveExperimentPooled1)

BLRmodelH8DDescriptiveExperimentPooled2 <-  glmer(H8DChosen ~(1|participantId)+factor(highVariability)*factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "descriptive"))
summary(BLRmodelH8DDescriptiveExperimentPooled2)

BLRmodelH8DDescriptiveExperimentPooled3 <-  glmer(H8DChosen ~(1|participantId)+factor(gender),family=binomial(link='logit')
                                                  ,data=subset(dataH8D,experimentType == "descriptive"))
summary(BLRmodelH8DDescriptiveExperimentPooled3)




# 2) Extras ---------------------------------------------------------------

# # + H1': People prefer shorter waiting times, but dislike to have longer journey times for waiting less ("05","06","07","08","09","10")-----
# 
# #People with bad trade-off of waiting and travel times (long waiting & low travel) do not will to have longer journeys in order to have a better trade-off of waiting and travel (lower waiting but longer travel time) ("05","06") 
# 
# #     - Dataset ---------------------------------------------------------------
# dataH2C <- choiceResponses
# dataH2C <- subset(dataH2C,scenario %in% c("03","04","05","06","07","08"))
# 
# # View(dataH1)
# #Rational here means picking the shorter route.
# dataH2C$h2CChosen <- NA
# dataH2C$h2CChosen[dataH2C$rationalRouteChosen == 0] <- 0
# dataH2C$h2CChosen[dataH2C$rationalRouteChosen == 1] <- 1
# 
# #     - Covariates --------------------------------------------------------------
# 
# dataH2C$tradeOffWT <- with(dataH2C,averageWaitingTime/averageTravelTime)
# dataH2C$xWaitingTime <- with(dataH2C,averageWaitingTime/(averageJourneyTime))
# 
# #Scenario 5 offers a better trade-off than scenario 6. Both with the same journey time. 
# #Lower gain in trade-off (Scenario 6) choose should increase likelihood to choose lower journey time with worst trade-off
# #as we are focus on the journey time, we can test whether a larger difference in trade-off (Scenario 5)
# dataH2C$moderateAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH2C$scenario %in% c("06","08"),1,0) 
# dataH2C$largeAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH2C$scenario %in% c("05","07"),1,0) 
# dataH2C$moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime <- ifelse(dataH2C$scenario %in% c("05","07","06","08"),1,0) 
# dataH2C$largeAdvantageInTradeOffWTAndSameJourneyTime <- ifelse(dataH2C$scenario %in%"04",1,0)
# 
# # dataH2C$AdvantageInTradeOffWTButHigherJourneyTime <- NA
# # dataH2C$AdvantageInTradeOffWTButHigherJourneyTime[dataH2C$scenario %in% c("05","07")] <- 2
# # dataH2C$AdvantageInTradeOffWTButHigherJourneyTime[dataH2C$scenario %in% c("06","08")] <- 1
# # dataH2C$AdvantageInTradeOffWTButHigherJourneyTime[dataH2C$scenario %in% c("03","04")] <- 0
# 
# #     - Estimation ------------------------------------------------
# 
# #ExperiencedExperiments
# BLRmodelH2CExperiencedExperiment <- glmer(h2CChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime)*factor(experimentalCondition),family=binomial(link='logit')
#                                           ,data=subset(dataH2C,experimentType == "experienced"))
# summary(BLRmodelH2CExperiencedExperiment)
# 
# BLRmodelH2CExperiencedExperimentPooled1 <- glmer(h2CChosen ~(1|participantId)+factor(largeAdvantageInTradeOffWTAndSameJourneyTime)+factor(moderateAdvantageInTradeOffWTButHigherJourneyTime)+factor(largeAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "experienced"))
# summary(BLRmodelH2CExperiencedExperimentPooled1)
# 
# BLRmodelH2CExperiencedExperimentPooled2 <- glmer(h2CChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "experienced"))
# summary(BLRmodelH2CExperiencedExperimentPooled2)
# 
# BLRmodelH2CExperiencedExperimentPooled3 <- glmer(h2CChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "experienced"))
# summary(BLRmodelH2CExperiencedExperimentPooled3)
# 
# #Descriptive Experiments
# BLRmodelH2CDescriptiveExperiment <- glmer(h2CChosen ~(1|participantId)+factor(experimentalCondition)*factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                           ,data=subset(dataH2C,experimentType == "descriptive"))
# summary(BLRmodelH2CDescriptiveExperiment)
# 
# BLRmodelH2CDescriptiveExperimentPooled1 <- glmer(h2CChosen ~(1|participantId)+factor(largeAdvantageInTradeOffWTAndSameJourneyTime)+factor(moderateAdvantageInTradeOffWTButHigherJourneyTime)+factor(largeAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "descriptive"))
# summary(BLRmodelH2CDescriptiveExperimentPooled1)
# 
# BLRmodelH2CDescriptiveExperimentPooled2 <- glmer(h2CChosen ~(1|participantId)+factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "descriptive"))
# summary(BLRmodelH2CDescriptiveExperimentPooled2)
# 
# BLRmodelH2CDescriptiveExperimentPooled3 <- glmer(h2CChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                  ,data=subset(dataH2C,experimentType == "descriptive"))
# summary(BLRmodelH2CDescriptiveExperimentPooled3)
# 
# 
# #Both
# BLRmodelH2CExperiencedAndDescriptiveExperiments <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(experimentalCondition)*factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                           ,data=subset(dataH2C))
# summary(BLRmodelH2CExperiencedAndDescriptiveExperiments)
# 
# BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled1 <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
#                                                                  ,data=subset(dataH2C))
# summary(BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled1)
# 
# # BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled2 <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
# #                                                                  ,data=subset(dataH2C))
# # summary(BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled2)
# # 
# # BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled3 <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(moderateOrHighAdvantageInTradeOffWTButHigherJourneyTime),family=binomial(link='logit')
# #                                                                  ,data=subset(dataH2C))
# # summary(BLRmodelH2CExperiencedAndDescriptiveExperimentsPooled3)
# 
# # #Increase power by not adding three way interaction with scenario.
# # BLRmodelH2CExperiencedAndDescriptiveExperiments2Way <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(experimentalCondition),family=binomial(link='logit')
# #                                                               ,data=subset(dataH2C))
# # summary(BLRmodelH2CExperiencedAndDescriptiveExperiments2Way)
# # 
# # 
# # BLRmodelH2CExperiencedAndDescriptiveExperimentsCity <-  glmer(h2CChosen ~(1|participantId)+factor(experimentType)*factor(experimentalCondition)*factor(largerAdvantageInTradeOffWTButHigherJourneyTime)*factor(city),family=binomial(link='logit')
# #                                                               ,data=subset(dataH2C))
# # summary(BLRmodelH2CExperiencedAndDescriptiveExperimentsCity)
# 
# #     - LR Tests --------------------------------------------------------------
# 
# #We do not reject in the two cases the null hypothesis that the moderate and high gains in trade-off in the longer journey times are statistically different
# lrtest(BLRmodelH2CExperiencedExperimentPooled1,BLRmodelH2CExperiencedExperimentPooled2)
# 
# lrtest(BLRmodelH2CDescriptiveExperimentPooled1,BLRmodelH2CDescriptiveExperimentPooled2)
# 
# #     - Conclusion -----------------------------------------------------------
# 
# #There is a reversal between the descriptive and experienced experiment. 
# #- In the descriptive experiment people prefer to have longer journeys
# #- In the experienced experiment people still prefer the shorter journeys.
# 
# 
# 
# 
# 
# 
# 

# # + H2': Travellers exhibit more aversion to time variability in waiting times than in in-vehicle times (S9,S12 vs.S10,S13) ---------------------
# 
# #       - Dataset ---------------------------------------------------------------
# dataH2E <- choiceResponses
# dataH2E <- subset(dataH2E,scenario %in% c("09","10","12","13"))
# 
# # View(dataH1)
# #Rational here means picking the route with no variability
# dataH2E$h2EChosen <- NA
# dataH2E$h2EChosen[dataH2E$rationalRouteChosen == 0] <- 0
# dataH2E$h2EChosen[dataH2E$rationalRouteChosen == 1] <- 1
# 
# #       - Covariates --------------------------------------------------------------
# 
# dataH2E$tradeOffWT <- with(dataH2E,averageWaitingTime/averageTravelTime)
# dataH2E$xWaitingTime <- with(dataH2E,averageWaitingTime/(averageJourneyTime))
# dataH2E$variabilityInWaitingTime <- ifelse(with(dataH2E,scenario == "09"|scenario =="12"),1,0)
# dataH2E$highVariability <- ifelse(with(dataH2E,scenario == "12"|scenario == "13"),1,0)
# 
# #       - Estimation ------------------------------------------------
# #         * BLR -------------------------------------------------------------------
# 
# 
# #Experienced
# BLRmodelH2EExperiencedExperiment <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime)*factor(highVariability),family=binomial(link='logit')
#                                           ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperiment)
# 
# BLRmodelH2EExperiencedExperimentPooled1 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperimentPooled1)
# 
# BLRmodelH2EExperiencedExperimentPooled2 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperimentPooled2)
# 
# BLRmodelH2EExperiencedExperimentPooled3 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperimentPooled3)
# 
# BLRmodelH2EExperiencedExperimentPooled4 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime)+factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperimentPooled4)
# 
# BLRmodelH2EExperiencedExperimentPooled5 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "experienced"))
# summary(BLRmodelH2EExperiencedExperimentPooled5)
# 
# #Descriptive
# BLRmodelH2EDescriptiveExperiment <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime)*factor(highVariability),family=binomial(link='logit')
#                                           ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperiment)
# 
# BLRmodelH2EDescriptiveExperimentPooled1 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled1)
# 
# BLRmodelH2EDescriptiveExperimentPooled2 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled2)
# 
# BLRmodelH2EDescriptiveExperimentPooled3 <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled3)
# 
# BLRmodelH2EDescriptiveExperimentPooled3 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled3)
# 
# BLRmodelH2EDescriptiveExperimentPooled4 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled4)
# 
# BLRmodelH2EDescriptiveExperimentPooled5 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime)+factor(highVariability),family=binomial(link='logit')
#                                                  ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentPooled5)
# 
# #Analysis by city
# 
# BLRmodelH2EDescriptiveExperimentCity <- glmer(h2EChosen ~(1|participantId)+factor(experimentalCondition)*factor(variabilityInWaitingTime)*factor(highVariability)*factor(city),family=binomial(link='logit')
#                                               ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentCity)
# 
# BLRmodelH2EDescriptiveExperimentCityPooled1 <- glmer(h2EChosen ~(1|participantId)+factor(variabilityInWaitingTime)*factor(highVariability)*factor(city),family=binomial(link='logit')
#                                                      ,data=subset(dataH2E,experimentType == "descriptive"))
# summary(BLRmodelH2EDescriptiveExperimentCityPooled1)
# 
# #Both
# BLRmodelH2EExperiencedAndDescriptiveExperiments <-  glmer(h2EChosen ~(1|participantId)+factor(experimentType)*factor(experimentalCondition)*factor(variabilityInWaitingTime),family=binomial(link='logit')
#                                                           ,data=subset(dataH2E))
# summary(BLRmodelH2EExperiencedAndDescriptiveExperiments)
# 
# BLRmodelH2EExperiencedAndDescriptiveExperimentsPooled1 <-  glmer(h2EChosen ~(1|participantId)+factor(experimentType)*factor(variabilityInWaitingTime),family=binomial(link='logit')
#                                                                  ,data=subset(dataH2E))
# summary(BLRmodelH2EExperiencedAndDescriptiveExperimentsPooled1)
# 
# BLRmodelH2EExperiencedAndDescriptiveExperimentsCity <-  glmer(h2EChosen ~(1|participantId)+factor(experimentType)*factor(experimentalCondition)*factor(variabilityInWaitingTime)*factor(city),family=binomial(link='logit')
#                                                               ,data=subset(dataH2E))
# summary(BLRmodelH2EExperiencedAndDescriptiveExperimentsCity)
# 
# #       - Conclusion (5 parameters were significant) -----------------------------------------------------------
# 
# #People prefer less variability in times(Positive intercept). 
# #In the treatment condition the preference is even higher. However, in the treatment condition of the experienced experiment there is less strong preference for deterministic times
# #... 
# #Variability in waiting times in the treatment conditions increase the preference for the deterministic alternative.
# 
# 

# 3) Tables --------------------------------------------------------------
#   + Functions -------------------------------------------------------------
#     - BLR Tables (format according to Ashby and Rakow, 2017) ------------------------------------------------------------
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
  model.df <- model.df[1:(dim(model.df)[1]-1),] #No fixed effects
  
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

exportTablesExperimentalModel <- function(models,digits,path,format,folder){
  
  
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

#     - McNemar Tables ----------------------------------------------------------

exportContingencyTablesMcNemarTest <- function(models,path,format,folder){
  
  for(element in 1:length(models)){
    # element = 1
    # contingencyTableMcNemar <- models[[element]][["model"]]$matrixMcNemar 
    model.df <- models[[element]][["model"]]$matrixMcNemar
    model.df<- as.data.frame(model.df)
    colnames(model.df) <- NULL
    row.names(model.df) <- NULL
    
    if(format=="txt"){
      
      filePath <- str_c(path,"Txt/",folder,"/",models[[element]][["filename"]])
      write.table(model.df , file = str_c(filePath,".txt"),row.names = FALSE,quote = FALSE)
    }
    
    # if(format=="doc"){
    #   # write.csv(model.df,file = str_c(filePath,".csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")  
    #   xTable <- xtable(model.df)
    #   filePath <- str_c(path,"Word/",folder,"/",models[[element]][["filename"]])
    #   print.xtable(xTable, type="html", file=str_c(filePath,".doc"), include.rownames=FALSE)
    # }
    # if(format == "tex"){
    #   xTable <- xtable(model.df)
    #   filePath <- str_c(path,"Latex/",folder,"/",models[[element]][["filename"]])
    #   print.xtable(xTable, type="latex", file=str_c(filePath,".tex"), include.rownames=FALSE,floating=FALSE,latex.environments=NULL,booktabs=TRUE)
    # }
  }
}
# filename <- "descriptiveExperiment"
exportTableMcNemarTest <- function(format, models,path, filename){
  
  tableMcNemarTest <- data.frame(hypothesis = character() , scenarios = character(),experiment = character(),chi = numeric(), pvalue = numeric(),stringsAsFactors=FALSE)
  
  for(element in 1:length(models)){
    modelTable.df <- models[[element]][["model"]]$tableMcNemarTest
    
    scenarios <-  models[[element]][["scenarios"]]
    idScenarios <- c("")
    
    for(nScenario in 1:length(scenarios)){
      idScenarios <- str_c(idScenarios,scenarios[nScenario])
    }
    
    hypothesis <-  models[[element]][["hypothesis"]]
    experiment <-  models[[element]][["experiment"]]
    filename <-  str_c(hypothesis,idScenarios,experiment,"Experiment")
    
    pValue <- round(as.numeric(modelTable.df$p.value),3)
    chiStatistic <-  round(as.numeric(modelTable.df$statistic),3)
    # experiment <-  modelTable.df$statistic
    
    #Appending information by hypothesis
    tableMcNemarTest <- rbind(tableMcNemarTest,data.frame(hypothesis = hypothesis , scenarios = idScenarios,experiment = experiment,chi = chiStatistic, pvalue = pValue))
    
  }
  
  #Export
  if(format=="csv"){
    
    filePath <- str_c(path,"Csv/",models[[element]][["experiment"]],"Experiment")
    write.csv(tableMcNemarTest , file = str_c(filePath,".csv"),row.names = FALSE,quote = FALSE)
  }

}


#   + Export ------------------------------------------------------------------
#     - BLR Tables ------------------------------------------------------------
#       * Parameters ------------------------------------------------------------
digits <- 2
format <- c("tex")
# format <- c("doc")
#       * Hypotheses ------------------------------------------------------------------
#         % Hypothesis H4 ------------------------------------------------------------------
degreeExperienceControlConditionPooled1<- list(
  # list(model = BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCity, filename = "H4S1S2ExperiencedAndDescriptiveExperimentsControlConditionCity")
  list(model = BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled1, filename = "H4AExperiencedAndDescriptiveExperimentsControlConditionPooled1")
  ,list(model = BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled1, filename = "H4BExperiencedAndDescriptiveExperimentsControlConditionPooled1")
  ,list(model = BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled1, filename = "H4CExperiencedAndDescriptiveExperimentsControlConditionPooled1")
  ,list(model = BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled1, filename = "H4DExperiencedAndDescriptiveExperimentsControlConditionPooled1")
)    

degreeExperienceControlConditionPooled2 <- list(
  # list(model = BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsControlConditionPooled1, filename = "H4S1S2ExperiencedAndDescriptiveExperimentsControlConditionPooled1")
  list(model = BLRmodelH4AExperiencedAndDescriptiveExperimentsControlConditionPooled2, filename = "H4AExperiencedAndDescriptiveExperimentsControlConditionPooled2")
  ,list(model = BLRmodelH4BExperiencedAndDescriptiveExperimentsControlConditionPooled2, filename = "H4BExperiencedAndDescriptiveExperimentsControlConditionPooled2")
  ,list(model = BLRmodelH4CExperiencedAndDescriptiveExperimentsControlConditionPooled2, filename = "H4CExperiencedAndDescriptiveExperimentsControlConditionPooled2")
  ,list(model = BLRmodelH4DExperiencedAndDescriptiveExperimentsControlConditionPooled2, filename = "H4DExperiencedAndDescriptiveExperimentsControlConditionPooled2")
)    

degreeExperienceTreatmentConditionPooled1 <- list(
  # list(model = BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1, filename = "H4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1")
  list(model = BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1, filename = "H4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1")
  ,list(model = BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1, filename = "H4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1")
  ,list(model = BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1, filename = "H4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1")
  ,list(model = BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1, filename = "H4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled1")
)    

degreeExperienceTreatmentConditionPooled2 <- list(
  # list(model = BLRmodelH4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2, filename = "H4S1S2ExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2")
  list(model = BLRmodelH4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2, filename = "H4AExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2")
  ,list(model = BLRmodelH4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2, filename = "H4BExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2")
  ,list(model = BLRmodelH4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2, filename = "H4CExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2")
  ,list(model = BLRmodelH4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2, filename = "H4DExperiencedAndDescriptiveExperimentsTreatmentConditionPooled2")
)    

#Export Files

#-Control Condition 
exportTablesExperimentalModel(format=format, models = degreeExperienceControlConditionPooled1,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeExperience/ControlCondition")
exportTablesExperimentalModel(format=format, models = degreeExperienceControlConditionPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeExperience/ControlCondition")

#-Treatment Condition 
exportTablesExperimentalModel(format=format, models = degreeExperienceTreatmentConditionPooled1,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeExperience/TreatmentCondition")
exportTablesExperimentalModel(format=format, models = degreeExperienceTreatmentConditionPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeExperience/TreatmentCondition")


#         % Hypothesis H5 ------------------------------------------------------------------

degreeInformationExperiencedExperimentPooled1 <- list(
  # list(model = BLRmodelH5S1S2ExperiencedExperimentPooled1, filename = "H5S1S2ExperiencedExperimentPooled1")
  list(model = BLRmodelH5AExperiencedExperimentPooled1, filename = "H5AExperiencedExperimentPooled1")
  ,list(model = BLRmodelH5BExperiencedExperimentPooled1, filename = "H5BExperiencedExperimentPooled1")
  ,list(model = BLRmodelH5CExperiencedExperimentPooled1, filename = "H5CExperiencedExperimentPooled1")
  ,list(model = BLRmodelH5DExperiencedExperimentPooled1, filename = "H5DExperiencedExperimentPooled1")
)  

degreeInformationExperiencedExperimentPooled2 <- list(
  # list(model = BLRmodelH5S1S2ExperiencedExperimentPooled2, filename = "H5S1S2ExperiencedExperimentPooled2")
  list(model = BLRmodelH5AExperiencedExperimentPooled2, filename = "H5AExperiencedExperimentPooled2")
  ,list(model = BLRmodelH5BExperiencedExperimentPooled2, filename = "H5BExperiencedExperimentPooled2")
  ,list(model = BLRmodelH5CExperiencedExperimentPooled2, filename = "H5CExperiencedExperimentPooled2")
  ,list(model = BLRmodelH5DExperiencedExperimentPooled2, filename = "H5DExperiencedExperimentPooled2")
)  

#Export Files
exportTablesExperimentalModel(format=format, models = degreeInformationExperiencedExperimentPooled1,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeInformationExperiencedExperiment/")
exportTablesExperimentalModel(format=format, models = degreeInformationExperiencedExperimentPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeInformationExperiencedExperiment/")


#         % Hypothesis H6 ------------------------------------------------------------------

degreeInformationDescriptiveExperimentPooled1 <- list(
  # list(model = BLRmodelH6S1S2DescriptiveExperimentPooled1, filename = "H6S1S2DescriptiveExperimentPooled1")
  list(model = BLRmodelH6ADescriptiveExperimentPooled1, filename = "H6ADescriptiveExperimentPooled1")
  ,list(model = BLRmodelH6BDescriptiveExperimentPooled1, filename = "H6BDescriptiveExperimentPooled1")
  ,list(model = BLRmodelH6CDescriptiveExperimentPooled1, filename = "H6CDescriptiveExperimentPooled1")
  ,list(model = BLRmodelH6DDescriptiveExperimentPooled1, filename = "H6DDescriptiveExperimentPooled1")
)  

degreeInformationDescriptiveExperimentPooled2 <- list(
  # list(model = BLRmodelH6S1S2DescriptiveExperimentPooled2, filename = "H6S1S2DescriptiveExperimentPooled2")
  list(model = BLRmodelH6ADescriptiveExperimentPooled2, filename = "H6ADescriptiveExperimentPooled2")
  ,list(model = BLRmodelH6BDescriptiveExperimentPooled2, filename = "H6BDescriptiveExperimentPooled2")
  ,list(model = BLRmodelH6CDescriptiveExperimentPooled2, filename = "H6CDescriptiveExperimentPooled2")
  ,list(model = BLRmodelH6DDescriptiveExperimentPooled2, filename = "H6DDescriptiveExperimentPooled2")
)  

#Export Files
exportTablesExperimentalModel(format=format, models = degreeInformationDescriptiveExperimentPooled1,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeInformationDescriptiveExperiment/")
exportTablesExperimentalModel(format=format, models = degreeInformationDescriptiveExperimentPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="DegreeInformationDescriptiveExperiment/")

#         % Hypothesis H7 ------------------------------------------------------------------

travelExperienceExperiencedExperimentPooled2<- list(
  # list(model = BLRmodelH7S1S2ExperiencedExperimentCity, filename = "H7S1S2ExperiencedExperimentCity")
  list(model = BLRmodelH7AExperiencedExperimentPooled2, filename = "H7AExperiencedExperimentPooled2")
  ,list(model = BLRmodelH7BExperiencedExperimentPooled2, filename = "H7BExperiencedExperimentPooled2")
  ,list(model = BLRmodelH7CExperiencedExperimentPooled2, filename = "H7CExperiencedExperimentPooled2")
  ,list(model = BLRmodelH7DExperiencedExperimentPooled2, filename = "H7DExperiencedExperimentPooled2")
)    

travelExperienceExperiencedExperimentPooled3 <- list(
  # list(model = BLRmodelH7S1S2ExperiencedExperimentPooled3, filename = "H7S1S2ExperiencedExperimentPooled3")
  list(model = BLRmodelH7AExperiencedExperimentPooled3, filename = "H7AExperiencedExperimentPooled3")
  ,list(model = BLRmodelH7BExperiencedExperimentPooled3, filename = "H7BExperiencedExperimentPooled3")
  ,list(model = BLRmodelH7CExperiencedExperimentPooled3, filename = "H7CExperiencedExperimentPooled3")
  ,list(model = BLRmodelH7DExperiencedExperimentPooled3, filename = "H7DExperiencedExperimentPooled3")
)    

travelExperienceDescriptiveExperimentPooled2 <- list(
  # list(model = BLRmodelH7S1S2DescriptiveExperimentPooled2, filename = "H7S1S2DescriptiveExperimentPooled2")
  list(model = BLRmodelH7ADescriptiveExperimentPooled2, filename = "H7ADescriptiveExperimentPooled2")
  ,list(model = BLRmodelH7BDescriptiveExperimentPooled2, filename = "H7BDescriptiveExperimentPooled2")
  ,list(model = BLRmodelH7CDescriptiveExperimentPooled2, filename = "H7CDescriptiveExperimentPooled2")
  ,list(model = BLRmodelH7DDescriptiveExperimentPooled2, filename = "H7DDescriptiveExperimentPooled2")
)    

travelExperienceDescriptiveExperimentPooled3 <- list(
  # list(model = BLRmodelH7S1S2DescriptiveExperimentPooled3, filename = "H7S1S2DescriptiveExperimentPooled3")
  list(model = BLRmodelH7ADescriptiveExperimentPooled3, filename = "H7ADescriptiveExperimentPooled3")
  ,list(model = BLRmodelH7BDescriptiveExperimentPooled3, filename = "H7BDescriptiveExperimentPooled3")
  ,list(model = BLRmodelH7CDescriptiveExperimentPooled3, filename = "H7CDescriptiveExperimentPooled3")
  ,list(model = BLRmodelH7DDescriptiveExperimentPooled3, filename = "H7DDescriptiveExperimentPooled3")
)    

#Export Files

#-Experienced Experiment
exportTablesExperimentalModel(format=format, models = travelExperienceExperiencedExperimentPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="TravelExperience/ExperiencedExperiment")
exportTablesExperimentalModel(format=format, models = travelExperienceExperiencedExperimentPooled3,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="TravelExperience/ExperiencedExperiment")

#-Descriptive Experiment
exportTablesExperimentalModel(format=format, models = travelExperienceDescriptiveExperimentPooled2,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="TravelExperience/DescriptiveExperiment")
exportTablesExperimentalModel(format=format, models = travelExperienceDescriptiveExperimentPooled3,digits = digits,path = str_c(dir,"/Export/Tables/HypothesisTesting/BLR/"),folder="TravelExperience/DescriptiveExperiment")

#     - McNemar Tables ----------------------------------------------------------

experiencedExperimentMcNemarTest <- list(
  list(model = McNemarTestH1BExperiencedExperiment, scenarios = c("S3","S4"), hypothesis = c("H1B"), experiment = "Experiential")
  , list(model = McNemarTestH1DS6S5ExperiencedExperiment, scenarios = c("S6","S5"), hypothesis = c("H1D"), experiment = "Experiential")
  , list(model = McNemarTestH1DS7S8ExperiencedExperiment, scenarios = c("S7","S8"), hypothesis = c("H1D"), experiment = "Experiential")
  , list(model = McNemarTestH2BExperiencedExperiment, scenarios = c("S9","S12"), hypothesis = c("H2B"), experiment = "Experiential")
  , list(model = McNemarTestH2DExperiencedExperiment, scenarios = c("S10","S13"), hypothesis = c("H2D"), experiment = "Experiential")
  , list(model = McNemarTestH3BExperiencedExperiment, scenarios = c("S11","S14"), hypothesis = c("H3B"), experiment = "Experiential")
)

descriptiveExperimentMcNemarTest <- list(
  list(model = McNemarTestH1BDescriptiveExperiment, scenarios = c("S3","S4"), hypothesis = c("H1B"), experiment = "Descriptive")
  , list(model = McNemarTestH1DS6S5DescriptiveExperiment, scenarios = c("S6","S5"), hypothesis = c("H1D"), experiment = "Descriptive")
  , list(model = McNemarTestH1DS7S8DescriptiveExperiment, scenarios = c("S7","S18"), hypothesis = c("H1D"), experiment = "Descriptive")
  , list(model = McNemarTestH2BDescriptiveExperiment, scenarios = c("S9","S12"), hypothesis = c("H2B"), experiment = "Descriptive")
  , list(model = McNemarTestH2DDescriptiveExperiment, scenarios = c("S10","S13"), hypothesis = c("H2D"), experiment = "Descriptive")
  , list(model = McNemarTestH3BDescriptiveExperiment, scenarios = c("S11","S14"), hypothesis = c("H3B"), experiment = "Descriptive")
)

format <- c("txt")

exportContingencyTablesMcNemarTest(format=format,models = experiencedExperimentMcNemarTest,path = str_c(dir,"/Export/Tables/HypothesisTesting/McNemar/"),folder="ExperiencedExperiment/")
exportContingencyTablesMcNemarTest(format=format,models = descriptiveExperimentMcNemarTest,path = str_c(dir,"/Export/Tables/HypothesisTesting/McNemar/"),folder="DescriptiveExperiment/")

format <- c("csv")
  
exportTableMcNemarTest(format=format,models = descriptiveExperimentMcNemarTest, path =  str_c(dir,"/Export/Tables/HypothesisTesting/McNemar/"), filename = "descriptiveExperiment")
exportTableMcNemarTest(format=format,models = experiencedExperimentMcNemarTest, path =  str_c(dir,"/Export/Tables/HypothesisTesting/McNemar/"), filename = "experiencedExperiment")



# tbl <- ftable(McNemarTestH1BExperiencedExperiment$data[c("choiceControl","choiceTreatment")])
# 
# xftbl <- xtableFtable(tbl, method = "compact")
# # options(xtable.tabular.environment = TRUE)
# print.xtableFtable(xftbl, booktabs = TRUE)

