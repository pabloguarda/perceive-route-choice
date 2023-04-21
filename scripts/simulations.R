########################################################################
###################### DCM USING SIMULATED DATA ########################
########################################################################

#   a) Packages ----------------------------------------------------------------

libraries <- c("randomizr","maxLik")

for(i in 1:length(libraries)){
  if(!require(package = libraries[i],character.only = TRUE)) 
    install.packages(libraries[i])
}

#   b) Data preparation -----------------------------------------------------
#   c) Experimental design --------------------------------------------------------------
#     i) Choice scenarios ------------------------------------------------------


choiceScenario1 <- c(w1 = 4, v1 = 6, w2 = 6, v2 = 6) 
choiceScenario2 <- c(w1 = 6, v1 = 4, w2 = 6, v2 = 6) 
choiceScenario3 <- c(w1 = 4, v1 = 6, w2 = 6, v2 = 4) 
choiceScenario4 <- c(w1 = 1, v1 = 9, w2 = 9, v2 = 1) 
choiceScenario5 <- c(w1 = 2, v1 = 9, w2 = 8, v2 = 2) 
choiceScenario6 <- c(w1 = 3, v1 = 8, w2 = 8, v2 = 2) 
choiceScenario7 <- c(w1 = 8, v1 = 1, w2 = 2, v2 = 8) 
choiceScenario8 <- c(w1 = 7, v1 = 2, w2 = 2, v2 = 8) 

choiceScenarios <- rbind(s1 = choiceScenario1,s2 = choiceScenario2, s3 = choiceScenario3,s4= choiceScenario4
                         , s5 = choiceScenario5, s6 = choiceScenario6, s7= choiceScenario7, s8 = choiceScenario8)

choiceScenarios <- as.data.frame(choiceScenarios)

choiceScenarios$scenario <- rownames(choiceScenarios); rownames(choiceScenarios) <- NULL

choiceScenarios <- choiceScenarios[,c("scenario","w1","v1","w2","v2")]

choiceScenarios


#     ii) Build dataset -----------------------------------------------------------
#       - Parameters ------------------------------------------------------------


cdata <- data.frame() # Choice data
nScenarios <- 8 # Number of choice scenario
nAlternatives <- 2 #Alternatives per scenario
nIndividuals <- 72 # Number of individuals (72)
nExperimentalConditions <- 2 #Control and Treatment conditions
nExperimentTypes <- 2 #Numeric or animated

#       - Create Dataset ------------------------------------------------------------

#Create 8 scenarios per individual 
for(nIndividual in 1:nIndividuals){
  
  cdata <- rbind(cdata,cbind(individual = nIndividual,choiceScenarios))
}

#Duplicate the observations per individuals as there are two types of experiment, with the same scenarios.
cdata  <- rbind(cbind(cdata, experimentType = "animated"),cbind(cdata, experimentType = "numeric"))

cdata$experimentType <- as.character(cdata$experimentType)

nObservations <- dim(cdata)[1]; nObservations

head(cdata)


#       - Random assignment -----------------------------------------------------


experimentalCondition <- complete_ra(N = nIndividuals, m = nIndividuals/2) 
experimentalCondition <- ifelse(experimentalCondition == 0, "control","treatment")
nIndividualsConditions <- data.frame(individual = 1:nIndividual,experimentalCondition = experimentalCondition)

cdata <- merge(cdata, nIndividualsConditions, by = "individual")

head(cdata)


#     iii) Choices --------------------------------------------------------------


#       - General parameters of utility function --------------------------------


simulationParametersNumeric <- list()
simulationParametersAnimated <- list()


#         + Preferences for waiting and in-vehicle times --------------------------



# Marginal rate of substitution of waiting for travelling
mrs <- 1.1

# Preferences for waiting and travelling
bv <- -1
bw <-mrs*bv

#Preferences are the same for all experiments
simulationParametersNumeric[["bw"]] <- simulationParametersAnimated[["bw"]] <- bw
simulationParametersNumeric[["bv"]] <- simulationParametersAnimated[["bv"]] <- bv


#         + Time perception functions ---------------------------------------------
#           * Numeric condition -----------------------------------------------------

#Waiting time
simulationParametersNumeric[["aw"]] <- 1 #Linear parameter
simulationParametersNumeric[["cw"]] <- 0.9 #Exponent 

#In-vehicle time
simulationParametersNumeric[["av"]] <- 1 #Linear parameter
simulationParametersNumeric[["cv"]] <- 0.9 #Exponent 

#           * Animated condition -----------------------------------------------------


#Waiting time
simulationParametersAnimated[["aw"]] <- 1 #Linear parameter
simulationParametersAnimated[["cw"]] <- 0.85 #Exponent 

#In-vehicle time
simulationParametersAnimated[["av"]] <- 2 #Linear parameter
simulationParametersAnimated[["cv"]] <- 0.75 #Exponent 

#Treatment condition
simulationParametersAnimated[["awt"]] <- simulationParametersAnimated[["aw"]] #Difference in linear parameter in treatment condition

#       - Generation of random errors ------------------------------------------



#The shape (mu: mode) and scale (beta>0) parameters for the error distribution
muError <- 0
betaError <- 1

#Function to generate numbers with gumbel distribution. Source: https://en.wikipedia.org/wiki/Gumbel_distribution

gumbel.dist <- function(mu,beta,n,seed = NA){
  
  if(!is.na(seed)){
    set.seed(seed)
  }
  
  else{
    
    set.seed(floor(runif(1, 1,Sys.time())))
  }
  
  numunif <- runif(n,0,1)
  rgumb <- mu-beta*log(-log(numunif))
  
  return(rgumb)
  
}

#Let's assume the same shape and scale parameters for the error distribution for each alternative and accross all choice scenarios
muErrorAnimated <- muError
betaErrorAnimated <- betaError
simulationParametersAnimated[["scale_gumbel"]] = betaErrorAnimated

muErrorNumeric <- muError
betaErrorNumeric <- betaError
simulationParametersNumeric[["scale_gumbel"]] = betaErrorNumeric

#Generation of errors for alternatives from Animated and Numeric experiments
errorAnimated <- data.frame(e1 = gumbel.dist(mu = muErrorAnimated, beta = betaErrorAnimated, nObservations/nExperimentTypes)
                            , e2 = gumbel.dist(mu = muErrorAnimated, beta = betaErrorAnimated, nObservations/nExperimentTypes)
)

errorNumeric <- data.frame(e1 = gumbel.dist(mu = muErrorNumeric, beta = betaErrorNumeric, nObservations/nExperimentTypes)
                           , e2 = gumbel.dist(mu = muErrorNumeric, beta = betaErrorNumeric, nObservations/nExperimentTypes)
)

head(errorAnimated); head(errorNumeric)


#       - Utility computation ------------------------------------------------

utilityComputation <- function(x,beta,error){
  
  aw <- beta[["aw"]]
  bw <- beta[["bw"]]
  cw <- beta[["cw"]]
  
  av <- beta[["av"]]
  bv <- beta[["bv"]]
  cv <- beta[["cv"]]
  
  #Animated experiment 
  if(unique(x$experimentType) == "animated"){ 
    awt <- beta[["awt"]]
    x$V1 <- bw*ifelse(x[,"experimentalCondition"]=="control",aw,aw+awt)*(xAnimated[,"w1"])^cw+bv*av*(x[,"v1"])^cv
    x$V2 <- bw*ifelse(x[,"experimentalCondition"]=="control",aw,aw+awt)*(xAnimated[,"w2"])^cw+bv*av*(x[,"v2"])^cv  
  }
  
  #Numeric experiment 
  if(unique(x$experimentType) == "numeric"){
    x$V1 <- bw*aw*(x[,"w1"])^cw+bv*av*(x[,"v1"])^cv
    x$V2 <- bw*aw*(x[,"w2"])^cw+bv*av*(x[,"v2"])^cv
  }
  
  x$e1 <- error$e1
  x$e2 <- error$e2
  
  x$U1 <- x$V1+x$e1
  x$U2 <- x$V2+x$e2
  
  return(x)
}

#         + Animated condition ----------------------------------------------------



betaAnimated <- simulationParametersAnimated
xAnimated <- subset(cdata,experimentType == "animated")
cdataAnimated <- utilityComputation(x=xAnimated,beta = betaAnimated, e = errorAnimated)

head(cdataAnimated)

#         + Numeric condition ----------------------------------------------------

betaNumeric <- simulationParametersNumeric
xNumeric <- subset(cdata,experimentType == "numeric")
cdataNumeric <- utilityComputation(x=xNumeric,beta = betaNumeric, error = errorNumeric)

head(cdataNumeric)


#       - Simulated choices ---------------------------------------------------
cdataNumeric$choice <- with(cdataNumeric, ifelse(U1>U2, 1, 2))
cdataAnimated$choice <- with(cdataAnimated, ifelse(U1>U2, 1, 2))

#Encapsulate all the code before so I can set a seed and produce choices assuming different preferences and time perception vectors
simulatingChoices <- function(seed
                              , simulationParametersAnimatedTemp
                              , simulationParametersNumericTemp
                              , cdataTemp = cdata
){
  
  
  # if(unique(is.na(scenarios))){
  #   cdata <- cdataTemp
  # }
  # 
  # else{
  #   
  #   cdata <- subset(cdataTemp,scenario %in% scenarios)
  # }
  
  cdataTemp <- cdata
  nObservations <- dim(cdata)[1]
  nExperimentTypes <-length(unique(cdata$experimentType))
  simulationParametersAnimated <- simulationParametersAnimatedTemp
  simulationParametersNumeric <- simulationParametersNumericTemp
  
  # set.seed(seed)
  
  # ii) Generation of random errors 
  
  # #The shape (mu: mode) and scale (beta>0) parameters for the error distribution
  # muError <- 0
  # betaError <- 1
  
  #Function to generate numbers with gumbel distribution. Source: https://en.wikipedia.org/wiki/Gumbel_distribution
  
  #Let's assume the same shape and scale parameters for the error distribution for each alternative and accross all choice scenarios
  muErrorAnimated <- muError
  betaErrorAnimated <-ifelse(is.null(simulationParametersAnimated[["scale_gumbel"]]),1,simulationParametersAnimated[["scale_gumbel"]]) #betaError
  
  muErrorNumeric <- muError
  betaErrorNumeric <- ifelse(is.null(simulationParametersNumeric[["scale_gumbel"]]),1,simulationParametersAnimated[["scale_gumbel"]]) #betaError
  
  #Generation of errors for alternatives from Animated and Numeric experiments
  errorAnimated <- data.frame(e1 = gumbel.dist(mu = muErrorAnimated, beta = betaErrorAnimated, n= nObservations/nExperimentTypes, seed = seed)
                              , e2 = gumbel.dist(mu = muErrorAnimated, beta = betaErrorAnimated,  n= nObservations/nExperimentTypes, seed = seed+1)
  )
  
  errorAnimated
  
  errorNumeric <- data.frame(e1 = gumbel.dist(mu = muErrorNumeric, beta = betaErrorNumeric,  n= nObservations/nExperimentTypes, seed = seed)
                             , e2 = gumbel.dist(mu = muErrorNumeric, beta = betaErrorNumeric,  n= nObservations/nExperimentTypes, seed = seed+1)
  )
  
  
  
  #     iii) Utility computation
  
  #       - Animated condition
  
  betaAnimated <- simulationParametersAnimated
  xAnimated <- subset(cdata,experimentType == "animated")
  cdataAnimated <- utilityComputation(x=xAnimated,beta = betaAnimated, e = errorAnimated)
  
  
  #       - Numeric condition
  
  betaNumeric <- simulationParametersNumeric
  xNumeric <- subset(cdata,experimentType == "numeric")
  cdataNumeric <- utilityComputation(x=xNumeric,beta = betaNumeric, error = errorNumeric)
  
  
  
  #     iv) Simulated choices
  cdataNumeric$choice <- with(cdataNumeric, ifelse(U1>U2, 1, 2))
  cdataAnimated$choice <- with(cdataAnimated, ifelse(U1>U2, 1, 2))
  
  return(rbind(cdataNumeric,cdataAnimated))
}


#     iv) Loglikelihood functions --------------------------------------------------------
library(maxLik)

logitProbability <- function(VA,VB){
  
  return(exp(VA)/(exp(VA)+exp(VB)))
  
}

LogitTimePerception <- function(DCMData, logitParameters, constraints, method
                                , simulationParameters, sameCurvature = FALSE, sameTimeWeighting = FALSE
                                , knownTimePerception = FALSE, boxCox = FALSE){
  
  if(sameCurvature == TRUE)
  { 
    logitParameters[["thetaP"]] <- simulationParameters[["cw"]]
    
    logitParameters[["thetaP"]] <- abs(logitParameters[["thetaP"]])
    
    if(c("thetaPW") %in% names(logitParameters)){
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaPW")] 
    }
    
    if(c("thetaPV") %in% names(logitParameters)){
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaPV")] 
    }
    
    if(knownTimePerception == TRUE){
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaP")]
    }
    
  }
  
  else{
    
    if(knownTimePerception == TRUE){
      
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaPW")]
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaPV")] 
    }
    
  }
  
  
  if(sameTimeWeighting == TRUE)
  { 
    logitParameters[["thetaA"]] <- simulationParameters[["aw"]]
    
    if(c("thetaAW") %in% names(logitParameters)){
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaAW")] 
    }
    
    if(c("thetaAV") %in% names(logitParameters)){
      logitParameters <- logitParameters[-which(names(logitParameters) == "thetaAV")] 
    }
    
  }
  
  
  
  LogLikTimePerception <<- function(parameters,data = DCMData){
    
    
    simulationParameters[["scale_gumbel"]] <- 1
    constant <- as.numeric(ifelse(is.na(parameters["constant"]),0,parameters[["constant"]]))     
    thetaW <- as.numeric(ifelse(is.na(parameters["thetaW"]),simulationParameters[["bw"]],parameters[["thetaW"]]))
    thetaWT <- as.numeric(ifelse(is.na(parameters["thetaWT"]),0,parameters[["thetaWT"]]))
    thetaV <-  as.numeric(ifelse(is.na(parameters["thetaV"]),simulationParameters[["bv"]],parameters[["thetaV"]]))
    thetaWeight <- as.numeric(ifelse(is.na(parameters["thetaWeight"]),1,parameters[["thetaWeight"]]))
    
    thetaAWT <-  as.numeric(ifelse(is.na(parameters["thetaAWT"]),1,parameters[["thetaAWT"]])) 
    
    if(sameCurvature == TRUE){
      thetaPW <-  as.numeric(ifelse(is.na(parameters["thetaP"]),simulationParameters[["cw"]],parameters[["thetaP"]]))
      thetaPV <-  as.numeric(ifelse(is.na(parameters["thetaP"]),simulationParameters[["cv"]],parameters[["thetaP"]]))
    }
    
    if(sameCurvature == FALSE){
      #sameCurvature = FALSE
      thetaPW <-  as.numeric(ifelse(is.na(parameters["thetaPW"]),simulationParameters[["cw"]],parameters[["thetaPW"]]))
      thetaPV <-  as.numeric(ifelse(is.na(parameters["thetaPV"]),simulationParameters[["cv"]],parameters[["thetaPV"]]))
      
    }
    
    if(sameTimeWeighting == TRUE){
      thetaAW <-  as.numeric(parameters[["thetaA"]])
      thetaAV <-  as.numeric(parameters[["thetaA"]])
    }
    
    if(sameTimeWeighting == FALSE){
      
      #sameTimeWeighting = FALSE
      thetaAW <-  as.numeric(ifelse(is.na(parameters["thetaAW"]),simulationParameters[["aw"]],parameters[["thetaAW"]]))
      thetaAV <-  as.numeric(ifelse(is.na(parameters["thetaAV"]),simulationParameters[["av"]],parameters[["thetaAV"]]))
      
    }
    # thetaPW <- abs(thetaPW)
    # thetaPV <- abs(thetaPV)
    
    if(boxCox == FALSE){
      if(unique(data$experimentType) == "numeric"){
        
        # V1 <- thetaWeight*thetaW*thetaAW*(data$w1)^(thetaPW) + thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV)
        # V2 <- thetaWeight*thetaW*thetaAW*(data$w2)^(thetaPW) +thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
        V1 <- thetaWeight*thetaW*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaWT*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",0,1)+thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV)
        V2 <- thetaWeight*thetaW*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaWT*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",0,1)+thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
        # V1 <- thetaWeight*thetaW*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV)
        # V2 <- thetaWeight*thetaW*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
        
      }
      
      if(unique(data$experimentType) == "animated"){
        
        #V1 <- constant+thetaW*thetaAW*(data$w1)^(thetaPW) + thetaV*thetaAV*(data$v1)^(thetaPV) 
        #V2 <- thetaW*thetaAW*(data$w2)^(thetaPW) + thetaV*thetaAV*(data$v2)^(thetaPV)
        
        # V1 <- thetaWeight*thetaW*thetaAW*(data$w1)^(thetaPW) + thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV) 
        # V2 <- thetaWeight*thetaW*thetaAW*(data$w2)^(thetaPW) +thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
        
        # V1 <- thetaWeight*thetaW*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV)
        # V2 <- thetaWeight*thetaW*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
        V1 <- thetaWeight*thetaW*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaWT*(data$w1)^(thetaPW)*ifelse(data$experimentalCondition == "control",0,1)+thetaWeight*thetaV*thetaAV*(data$v1)^(thetaPV)
        V2 <- thetaWeight*thetaW*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",thetaAW,thetaAWT)+thetaWeight*thetaWT*(data$w2)^(thetaPW)*ifelse(data$experimentalCondition == "control",0,1)+thetaWeight*thetaV*thetaAV*(data$v2)^(thetaPV)
      }
      
    }
    
    if(boxCox == TRUE){
      
      if(thetaPW !=0 & thetaPV!=0){
        VR1  <- thetaW*(data$w1^(thetaPW)-1)/thetaPW
        VR2 <-  thetaV*(data$v1^(thetaPV)-1)/thetaPV
        VI1 <- thetaW*(data$w2^(thetaPW)-1)/thetaPW
        VI2 <- thetaV*(data$v2^(thetaPV)-1)/thetaPV
      }
      
      if(thetaPW!=0 & thetaPV==0){
        VR1  <- thetaW*(data$w1^(thetaPW)-1)/thetaPW
        VR2 <-  thetaV*log(data$v1)
        VI1 <- thetaW*(data$w2^(thetaPW)-1)/thetaPW
        VI2 <- thetaV*log(data$v2)
      }
      # 
      if(thetaPW==0 & thetaPV!=0){
        VR1  <- thetaW*log(data$w1)
        VR2 <-  thetaV*(data$v1^(thetaPV)-1)/thetaPV
        VI1 <- thetaW*log(data$w2)
        VI2 <- thetaV*(data$v2^(thetaPV)-1)/thetaPV
      }
      
      if(thetaPW==0 & thetaPV==0){
        VR1  <- thetaW*log(data$w1)
        VR2 <-  thetaV*log(data$v1)
        VI1 <- thetaW*log(data$w2)
        VI2 <- thetaV*log(data$v2)
      }
      
      V1 = constant + VR1+VR2
      V2 = VI1+VI2
      
    }
    
    
    
    SUM1 <- ifelse(data$choice == 1,1,0)*log(logitProbability(VA = V1, VB = V2))
    SUM2 <- ifelse(data$choice == 2,1,0)*log(logitProbability(VA = V2, VB = V1))
    
    return(SUM1+SUM2)
  }
  
  
  MLParameters <- maxLik(logLik = LogLikTimePerception, start = logitParameters,method = method)
  
  OI <- solve(MLParameters$hessian)
  se <- sqrt(abs(diag(OI)))
  ttest <- MLParameters$estimate/se
  
  resultsML <- cbind(estimate = MLParameters$estimate, se = se, ttest = ttest)
  
  return(list(resultsML,MLParameters))
}


#   d) Plots themes and formatting ---------------------------------------------------------------

themePlotsDCMParametersPaper1 <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
                                       ,panel.background = element_blank(), axis.line = element_line(colour = "black")
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
                                       ,legend.text=element_text(size=16)
                                       # ,legend.position="bottom"
)

widthSimulationPlots <- 20 #To consider space for legend
heightSimulationPlots <- 20 #Originally 20
dpiSimulationPlots <- 300

#   e) Simulations results ------------------------------
#     S1) Effect of logit variance (different scale parameters gumbel, c = 1) -------------------------------------------
#       Plot parameters ---------------------------------------------------------

seqXThetaWS1 <- seq(-4,2,0.01)
seqXThetaVS1 <- seq(-4,2,0.01)
seqXThetaMRSS1 <- seq(0,2,0.001)
# seqXThetaCS1 <- seq(0,2,0.01)

#       Estimation --------------------------------------------------------------
JointEstimationNumericChoicesKnownTimePerceptionS1 <- function(nReplications, scenarios
                                                               , realSimulationParametersAnimated
                                                               , realSimulationParametersNumeric
                                                               , logitParameters, knownTimePerception = TRUE){
  
  estimations <- data.frame()
  
  for(nReplication in 1:nReplications){
    
    set.seed(1)
    rm(.Random.seed, envir=globalenv())
    seed <- floor(runif(1, 1,Sys.time())) 
    
    data <- simulatingChoices(seed = seed
                              ,simulationParametersNumericTemp = realSimulationParametersNumeric
                              ,simulationParametersAnimatedTemp =  realSimulationParametersAnimated)
    
    data <- subset(data,experimentType == "numeric" & scenario %in% scenarios)
    
    DCMNumeric <- LogitTimePerception(DCMData = data
                                      , logitParameters = logitParameters, method = "BHHH"
                                      , simulationParameters = realSimulationParametersNumeric
                                      , sameCurvature = FALSE, sameTimeWeighting = FALSE, knownTimePerception = TRUE, boxCox = FALSE)
    
    
    ratioWV <- DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]; ratioWV
    
    temp <- data.frame(replication = nReplication
                       # , c = DCMNumeric[[1]]["thetaP","estimate"]#, cv = DCMNumeric[[1]]["thetaPV","estimate"]
                       , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                       , ratioWV = ratioWV, ll = logLik(DCMNumeric[[2]]))
    
    estimations <- rbind(estimations,temp)
  }
  
  return(estimations)
  
}

JointEstimationNumericChoicesKnownTimePerceptionDifferentScaleParametersS1 <- function(scaleParameters,nReplications,scenarios
                                                                                       ,realSimulationParametersAnimated,realSimulationParametersNumeric
                                                                                       ,logitParameters, knownTimePerception = TRUE){
  
  result <- data.frame(scaleParameter = c(),c = c(),ratioWV = c(), thetaW = c(), thetaV = c())
  
  simulationParametersNumericTemp <- realSimulationParametersNumeric
  
  for(i in 1:length(scaleParameters)){
    
    simulationParametersNumericTemp["scale_gumbel"] <- scaleParameters[i]
    
    estimatesAllParameters <- JointEstimationNumericChoicesKnownTimePerceptionS1(nReplications = nReplications, scenarios = scenarios
                                                                                 , realSimulationParametersNumeric = simulationParametersNumericTemp
                                                                                 , realSimulationParametersAnimated = simulationParametersNumericTemp
                                                                                 , logitParameters = logitParameters, knownTimePerception = knownTimePerception)  
    
    #result <- rbind(result, cbind.data.frame(scaleParameter = scaleParameters[i], c = estimatesAllParameters[,c("c")]))
    result <- rbind(result,data.frame(scaleParameter = scaleParameters[i]
                                      , ratioWV = estimatesAllParameters[,c("ratioWV")]
                                      , thetaW = estimatesAllParameters[,c("thetaW")]
                                      , thetaV = estimatesAllParameters[,c("thetaV")]
    )
    )
  }
  
  return(result)
  
}

simulationParametersNumericS1 <- simulationParametersNumeric

simulationParametersNumericS1$cv <- 1
simulationParametersNumericS1$cw <- 1

logitParametersS1 <- c(thetaW = 0, thetaV = 0,constant = NULL
                       , thetaPW = 1,thetaPV = 1, thetaAV = NULL, thetaAW = NULL)

scaleParametersS1 = seq(1,2,0.25)
# scaleParametersS1 = 1

estimatesAllParametersDifferentScaleParametersS1 <- JointEstimationNumericChoicesKnownTimePerceptionDifferentScaleParametersS1(
  scaleParameters = scaleParametersS1, nReplications = 200
  , scenarios = c("s1","s2","s3","s4","s5","s6","s7","s8")
  , realSimulationParametersAnimated = simulationParametersNumericS1
  , realSimulationParametersNumeric = simulationParametersNumericS1
  , logitParameters = logitParametersS1
)

#       a) Plot Parameter Waiting -----------------------------------------------------------------------
estimatesAllParametersDifferentScaleParametersS1ThetaW <- estimatesAllParametersDifferentScaleParametersS1[,c("scaleParameter","thetaW")]
#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection <- estimatesAllParametersDifferentScaleParametersS1ThetaW 

tt     <- split(estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection,estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS1,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.WAS1 <- data.frame( x = seqXThetaWS1, y = unlist(fitted),
                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection$scaleParameter),
                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection$scaleParameter))))


plotWAS1 <- ggplot(estimatesAllParametersDifferentScaleParametersS1ThetaWNoCorrection, aes(x, group = scaleParameter, colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WAS1, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

plotWAS1

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1ThetaWA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#         - Correction for scaling --------------------------------------------------------------

#Lesson: When the scale is adjusted, the mean of the distribution is corrected and the standard deviation of the parameter is also reduced.

estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection <- estimatesAllParametersDifferentScaleParametersS1ThetaW 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection$thetaW <- with(estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection,thetaW*scaleParameter)
plot.data.WBS1 <- plot.data.WAS1
plot.data.WBS1$x <- plot.data.WAS1$scaleParameter*plot.data.WAS1$x

# tt     <- split(estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection,estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS1,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.WBS1 <- data.frame(x = seqXThetaWS1, y = unlist(fitted), 
#                         scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection$scaleParameter),
#                                              each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS1ThetaWCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WBS1, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1ThetaWB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       b) Plot Parameter In-vehicle Time -----------------------------------------------------------------------
estimatesAllParametersDifferentScaleParametersS1ThetaV <- estimatesAllParametersDifferentScaleParametersS1[,c("scaleParameter","thetaV")]
#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection <- estimatesAllParametersDifferentScaleParametersS1ThetaV 

tt     <- split(estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection,estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS1,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.VAS1 <- data.frame(x = seqXThetaVS1, y = unlist(fitted),
                             scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection$scaleParameter),
                                                  each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection$scaleParameter))))


plotVAS1 <- ggplot(estimatesAllParametersDifferentScaleParametersS1ThetaVNoCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VAS1, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1ThetaVA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#         - Correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection <- estimatesAllParametersDifferentScaleParametersS1ThetaV 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection$thetaV <- with(estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection,thetaV*scaleParameter)
plot.data.VBS1 <- plot.data.VAS1
plot.data.VBS1$x <- plot.data.VAS1$scaleParameter*plot.data.VAS1$x

# tt     <- split(estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection,estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS1,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.VBS1 <- data.frame(x = seqXThetaVS1,y = unlist(fitted), 
#                         scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection$scaleParameter),
#                                              each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS1ThetaVCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VBS1, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1ThetaVB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       c) Plot MRS -----------------------------------------------------------------------

#Lesson: The mean value of MRS is the same regardless of the scale parameter
#Surprisngly, as the scale parameter is higher, the MRS has less veriance.

estimatesAllParametersDifferentScaleParametersS1RatioWV <- estimatesAllParametersDifferentScaleParametersS1[,c("scaleParameter","ratioWV")]

tt     <- split(estimatesAllParametersDifferentScaleParametersS1RatioWV,estimatesAllParametersDifferentScaleParametersS1RatioWV$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$ratioWV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaMRSS1,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.MRSS1 <- data.frame(x = seqXThetaMRSS1, y = unlist(fitted),
                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS1RatioWV$scaleParameter),
                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS1RatioWV$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS1RatioWV, aes(x, group = as.factor(scaleParameter), color=scaleParameter))+
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.MRSS1 , aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dashed")+
  # scale_x_continuous(limits = c(with(simulationParametersNumeric,bw/bv)-0.2, with(simulationParametersNumeric,bw/bv)+0.2), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1MRS.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1MRS.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)





#       d) Bw & Bv (Corrected and Not Corrected) (Paper) ---------------------------------------------------------------------

#Combination of plots WBS1 and VBS1 with no correction

dataPlotAPaperS1 <- rbind.data.frame(cbind.data.frame(plot = "WTNC", plot.data.WAS1, type = "waiting nc")
                                     ,cbind.data.frame(plot = "TTNC", plot.data.VAS1, type = "travel nc")
                                     # ,cbind.data.frame(plot = "MRS NC", plot.data.MRSS1, type = "mrs nc")
                                     ,cbind.data.frame(plot = "WWC", plot.data.WBS1, type = "waiting c")
                                     ,cbind.data.frame(plot = "TTC", plot.data.VBS1, type = "travel c")
                                     # ,cbind.data.frame(plot = "MRS C", plot.data.MRSS1, type = "mrs c")
                                     #,cbind.data.frame(plot = "MRS", plot.data.MRSS1, type = "mrs")
)

#Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotAPaperS1$plot <- factor(dataPlotAPaperS1$plot, levels = c("WTNC","TTNC","WWC","TTC"),
                                labels = c("hat(theta)[w]","hat(theta)[v]","hat(theta)[w]/lambda","hat(theta)[v]/lambda"))

ggplot(dataPlotAPaperS1, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotAPaperS1 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]/lambda"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]/lambda"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  # scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )     

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1PlotAPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)




#       e) Bw & Bv (corrected) & MRS (Paper) ---------------------------------------------------------------------


dataPlotBPaperS1 <- rbind.data.frame(cbind.data.frame(plot = "MRS", subset(plot.data.MRSS1, x>=0 & x<=2), type = "mrs")
                                     ,cbind.data.frame(plot = "WT", subset(plot.data.WBS1, x>=-2 & x<=0), type = "waiting")
                                     ,cbind.data.frame(plot = "TT", subset(plot.data.VBS1, x>=-2 & x<=0), type = "travel")
)

#Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotBPaperS1$plot <- factor(dataPlotBPaperS1$plot, levels = c("MRS","WT","TT"),
                                labels = c("hat(theta)[w]/hat(theta)[v]","hat(theta)[w]","hat(theta)[v]")
)
#Free scale only in y 
#https://stackoverflow.com/questions/18046051/setting-individual-axis-limits-with-facet-wrap-and-scales-free-in-ggplot2

ggplot(dataPlotBPaperS1, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotBPaperS1 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot="hat(theta)[w]/hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dotdash",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=3, scales = "free_x", labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )

ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)
# 
# # dataPlotBPaperS1 <- rbind(cbind.data.frame(x = as.numeric(plot.data.MRSS1$x), y = as.numeric(plot.data.MRSS1$y,plot.data.MRSS1), scaleParameter =as.numeric(plot.data.MRSS1$scaleParameter),type = "mrs")
# #                           # ,cbind.data.frame(x = as.numeric(plot.data.WBS1$x), y = as.numeric(plot.data.WBS1$y), scaleParameter =as.numeric(plot.data.WBS1$scaleParameter),  type = "thetaW")
# #                           # ,cbind.data.frame(x = as.numeric(plot.data.VBS1$x), y = as.numeric(plot.data.VBS1$y), scaleParameter =as.numeric(plot.data.VBS1$scaleParameter),  type = "thetaV")
# # )
# # 
# # 
# # ggplot(dataPlotBPaperS1, aes(x, group = interaction(scaleParameter,type), color=scaleParameter))+
# #   geom_line(data = dataPlotBPaperS1 , aes(y = y), lwd = 0.5)+
# #   geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dotdash",show.legend=TRUE)+
# #   # geom_vline(xintercept = with(simulationParametersNumeric,bw),color = "black", linetype="dotted",show.legend=TRUE)+
# #   # geom_vline(xintercept = with(simulationParametersNumeric,bv),color = "black", linetype="dashed",show.legend=TRUE)+
# #   scale_color_gradient(low="lightgray",high="black")+
# #   # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
# #   # scale_linetype_manual(values = c("A","B","C"))+
# #   scale_x_continuous(expand = c(0, 0)) +
# #   scale_y_continuous(expand = c(0, 0)) +
# #   labs(x = "Parameters", y = "Density",colour = expression(paste("  ",lambda)))+
# #   themePlotsDCMParametersPaper1
# # 
# # ggsave(str_c("export","/figures/Paper1/Simulations/LogitScaleParameter/S1PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)
# 
# 

#     S2) Estimation of MRS ignoring perception sensitivity (different scale parameters gumbel, c fixed to 1, true c = 0.9) -------------------------------------------

#Lesson: Adding a exponent increases variance of the estimates. This is dramatic for the estimate of c, but it also occurs for the preference parameters estimates

#       Plot parameters ---------------------------------------------------------

seqXThetaWS2 <- seq(-4,2,0.01)
seqXThetaVS2 <- seq(-4,2,0.01)
seqXThetaMRSS2 <- seq(0,2,0.001)
seqXThetaCS2 <- seq(0,2,0.01)

#       Estimation --------------------------------------------------------------
#         - Functions -------------------------------------------------------------



JointEstimationNumericChoicesKnownTimePerceptionS2 <- function(nReplications, scenarios
                                                               , realSimulationParametersAnimated
                                                               , realSimulationParametersNumeric
                                                               , logitParameters, knownTimePerception,cAssumed){
  
  estimations <- data.frame()
  
  for(nReplication in 1:nReplications){
    
    set.seed(1)
    rm(.Random.seed, envir=globalenv())
    seed <- floor(runif(1, 1,Sys.time())) 
    
    data <- simulatingChoices(seed = seed
                              ,simulationParametersNumericTemp = realSimulationParametersNumeric
                              ,simulationParametersAnimatedTemp =  realSimulationParametersAnimated)
    
    data <- subset(data,experimentType == "numeric" & scenario %in% scenarios)
    
    realSimulationParametersNumericAssumed <- realSimulationParametersNumeric
    realSimulationParametersNumericAssumed$cw <- cAssumed
    realSimulationParametersNumericAssumed$cv <- cAssumed
    
    DCMNumeric <- LogitTimePerception(DCMData = data
                                      , logitParameters = logitParameters, method = "BHHH"
                                      , simulationParameters = realSimulationParametersNumericAssumed
                                      , sameCurvature = FALSE, sameTimeWeighting = FALSE, knownTimePerception = TRUE, boxCox = FALSE)
    
    
    ratioWV <- DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]; ratioWV
    
    temp <- data.frame(replication = nReplication
                       # , c = DCMNumeric[[1]]["thetaP","estimate"]#, cv = DCMNumeric[[1]]["thetaPV","estimate"]
                       , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                       , ratioWV = ratioWV, ll = logLik(DCMNumeric[[2]]))
    
    estimations <- rbind(estimations,temp)
  }
  
  return(estimations)
  
}

JointEstimationNumericChoicesKnownTimePerceptionDifferentScaleParametersS2 <- function(scaleParameters,nReplications,scenarios
                                                                                       ,realSimulationParametersAnimated,realSimulationParametersNumeric
                                                                                       ,logitParameters,cAssumed,knownTimePerception){
  
  result <- data.frame(scaleParameter = c(),c = c(),ratioWV = c(), thetaW = c(), thetaV = c())
  
  simulationParametersNumericTemp <- realSimulationParametersNumeric
  
  for(i in 1:length(scaleParameters)){
    
    simulationParametersNumericTemp["scale_gumbel"] <- scaleParameters[i]
    
    estimatesAllParameters <- JointEstimationNumericChoicesKnownTimePerceptionS2(nReplications = nReplications, scenarios = scenarios
                                                                                 , realSimulationParametersNumeric = simulationParametersNumericTemp
                                                                                 , realSimulationParametersAnimated = simulationParametersNumericTemp
                                                                                 , logitParameters = logitParameters, knownTimePerception = knownTimePerception
                                                                                 ,cAssumed)  
    
    #result <- rbind(result, cbind.data.frame(scaleParameter = scaleParameters[i], c = estimatesAllParameters[,c("c")]))
    result <- rbind(result,data.frame(scaleParameter = scaleParameters[i]
                                      , ratioWV = estimatesAllParameters[,c("ratioWV")]
                                      , thetaW = estimatesAllParameters[,c("thetaW")]
                                      , thetaV = estimatesAllParameters[,c("thetaV")]
    )
    )
  }
  
  return(result)
  
}


#     - Parameters ------------------------------------------------------------

nReplicationsS2 = 200

logitParametersS2 <- c(thetaW = 0, thetaV = 0,constant = NULL
                       , thetaPW = 1,thetaPV = 1, thetaAV = NULL, thetaAW = NULL)


simulationParametersNumericS2 <- simulationParametersNumeric

simulationParametersNumericS2$cw <- 0.9
simulationParametersNumericS2$cv <- 0.9


scaleParametersS2 <- seq(1,2,0.25)


#     - Results ---------------------------------------------------------------



estimatesAllParametersDifferentScaleParametersS2 <- JointEstimationNumericChoicesKnownTimePerceptionDifferentScaleParametersS2(
  scaleParameters = scaleParametersS2, nReplications = nReplicationsS2
  , scenarios = c("s1","s2","S2","s4","s5","s6","s7","s8")
  , realSimulationParametersAnimated = simulationParametersNumericS2
  , realSimulationParametersNumeric = simulationParametersNumericS2
  , logitParameters = logitParametersS2, cAssumed = 1, knownTimePerception = TRUE
)

#       a) Plot Parameter Waiting -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2ThetaW <- estimatesAllParametersDifferentScaleParametersS2[,c("scaleParameter","thetaW")]

#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection <- estimatesAllParametersDifferentScaleParametersS2ThetaW

tt     <- split(estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection,estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS2,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.WAS2 <- data.frame(x = seqXThetaWS2, y = unlist(fitted),
                             scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection$scaleParameter),
                                                  each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS2ThetaWNoCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WAS2, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2ThetaWA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#         - Correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection <- estimatesAllParametersDifferentScaleParametersS2ThetaW 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection$thetaW <- with(estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection,thetaW*scaleParameter)
plot.data.WBS2 <- plot.data.WAS2
plot.data.WBS2$x <- plot.data.WAS2$scaleParameter*plot.data.WAS2$x


# tt     <- split(estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection,estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS2,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.WBS2 <- data.frame(x = seqXThetaWS2, y = unlist(fitted),
#                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection$scaleParameter),
#                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS2ThetaWCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WBS2, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2ThetaWB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       b) Plot Parameter In-vehicle Time -----------------------------------------------------------------------
estimatesAllParametersDifferentScaleParametersS2ThetaV <- estimatesAllParametersDifferentScaleParametersS2[,c("scaleParameter","thetaV")]
#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection <- estimatesAllParametersDifferentScaleParametersS2ThetaV

tt     <- split(estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection,estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS2,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.VAS2 <- data.frame(x = seqXThetaVS2, y = unlist(fitted),
                             scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection$scaleParameter),
                                                  each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS2ThetaVNoCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VAS2, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2ThetaVA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#         - Correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection <- estimatesAllParametersDifferentScaleParametersS2ThetaV 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection$thetaV <- with(estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection,thetaV*scaleParameter)
plot.data.VBS2 <- plot.data.VAS2
plot.data.VBS2$x <- plot.data.VAS2$scaleParameter*plot.data.VAS2$x


# tt     <- split(estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection,estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS2,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.VBS2 <- data.frame(x = seqXThetaVS2, y = unlist(fitted), 
#                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection$scaleParameter),
#                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS2ThetaVCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VBS2, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2ThetaVB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       c) Plot MRS -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS2RatioWV <- estimatesAllParametersDifferentScaleParametersS2[,c("scaleParameter","ratioWV")]

tt     <- split(estimatesAllParametersDifferentScaleParametersS2RatioWV,estimatesAllParametersDifferentScaleParametersS2RatioWV$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$ratioWV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaMRSS2,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.MRSS2 <- data.frame(x = seqXThetaMRSS2, y = unlist(fitted), 
                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS2RatioWV$scaleParameter),
                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS2RatioWV$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS2RatioWV, aes(x, group = as.factor(scaleParameter), color=scaleParameter))+
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.MRSS2, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dashed")+
  # scale_x_continuous(limits = c(with(simulationParametersNumeric,bw/bv)-0.2, with(simulationParametersNumeric,bw/bv)+0.2), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2MRS.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)



#       d) Bw & Bv (Corrected and Not Corrected) (Paper) ---------------------------------------------------------------------

#Combination of plots WBS2 and VBS2 with no correction

dataPlotAPaperS2 <- rbind.data.frame(cbind.data.frame(plot = "WTNC", plot.data.WAS2, type = "waiting nc")
                                     ,cbind.data.frame(plot = "TTNC", plot.data.VAS2, type = "travel nc")
                                     # ,cbind.data.frame(plot = "MRS NC", plot.data.MRSS2, type = "mrs nc")
                                     ,cbind.data.frame(plot = "WWC", plot.data.WBS2, type = "waiting c")
                                     ,cbind.data.frame(plot = "TTC", plot.data.VBS2, type = "travel c")
                                     # ,cbind.data.frame(plot = "MRS C", plot.data.MRSS2, type = "mrs c")
                                     #,cbind.data.frame(plot = "MRS", plot.data.MRSS2, type = "mrs")
)

#Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotAPaperS2$plot <- factor(dataPlotAPaperS2$plot, levels = c("WTNC","TTNC","WWC","TTC"),
                                labels = c("hat(theta)[w]","hat(theta)[v]","hat(theta)[w]/lambda","hat(theta)[v]/lambda"))

ggplot(dataPlotAPaperS2, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotAPaperS2 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]/lambda"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]/lambda"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )     

ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2PlotAPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#     S3) Effect of time perception sensitivity (different scale parameters gumbel, estimating c) -------------------------------------------

#Lesson: Adding a exponent increases variance of the estimates. This is dramatic for the estimate of c, but it also occurs for the preference parameters estimates

#       Plot parameters ---------------------------------------------------------

seqXThetaWS3 <- seq(-4,2,0.01)
seqXThetaVS3 <- seq(-4,2,0.01)
seqXThetaMRSS3 <- seq(0,2,0.001)
seqXThetaCS3 <- seq(0,2,0.01)

#       Estimation --------------------------------------------------------------
#       - Functions -------------------------------------------------------------

JointEstimationNumericChoicesS3 <- function(nReplications,realSimulationParametersAnimated,realSimulationParametersNumeric, scenarios, logitParameters){
  
  estimationsConventional <- data.frame()
  estimationsBoxCox <- data.frame()
  
  simulationParametersAnimatedTemp <- realSimulationParametersAnimated
  simulationParametersNumericTemp <- realSimulationParametersNumeric
  
  for(nReplication in 1:nReplications){
    
    set.seed(1)
    rm(.Random.seed, envir=globalenv())
    seed <- floor(runif(1, 1,Sys.time())) 
    
    data <- simulatingChoices(seed = seed
                              ,simulationParametersNumeric = simulationParametersNumericTemp 
                              ,simulationParametersAnimated = simulationParametersAnimatedTemp)
    
    data <- subset(data,experimentType == "numeric" & scenario %in% scenarios)
    
    #Conventional estimation
    
    # browser()
    
    
    # tempConventional <- NULL
    
    tempConventional <- tryCatch(
      {
        DCMNumeric <- LogitTimePerception(DCMData = data
                                          , logitParameters = logitParameters, method = "BHHH"
                                          , simulationParameters = simulationParametersNumericTemp
                                          , sameCurvature = TRUE, sameTimeWeighting = FALSE, knownTimePerception = FALSE, boxCox = FALSE)
        
        
        ratioWV <- DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]; ratioWV
        
        # browser()
        
        data.frame(replication = nReplication
                   , c = DCMNumeric[[1]]["thetaP","estimate"]#, cv = DCMNumeric[[1]]["thetaPV","estimate"]
                   , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                   , ratioWV = ratioWV, ll = logLik(DCMNumeric[[2]]))
        
      }
      
      , error=function(e){message("Failed estimation")}
      ,finally = {
        # message("tryCatch is finished.")
        
        # browser()
        
        
      }
    )
    
    # print(tempConventional)
    
    
    
    estimationsConventional <- rbind(estimationsConventional,tempConventional)
    
    
    #Box-cox
    
    tempBoxCox <- tryCatch(
      {
        
        DCMNumericBoxCox <- LogitTimePerception(DCMData = data
                                                , logitParameters = logitParameters, method = "BHHH"
                                                , simulationParameters = simulationParametersNumericTemp
                                                , sameCurvature = TRUE, sameTimeWeighting = FALSE, knownTimePerception = FALSE, boxCox = TRUE)
        
        
        ratioWV <- DCMNumericBoxCox[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]; ratioWV
        
        data.frame(replication = nReplication
                   , c = DCMNumericBoxCox[[1]]["thetaP","estimate"]#, cv = DCMNumericBoxCox[[1]]["thetaPV","estimate"]
                   , thetaW = DCMNumericBoxCox[[1]]["thetaW","estimate"],thetaV = DCMNumericBoxCox[[1]]["thetaV","estimate"]
                   , ratioWV = ratioWV, ll = logLik(DCMNumericBoxCox[[2]]))
      }
      
      , error=function(e){message("Failed estimation")}
      ,finally = {
        # message("tryCatch is finished.")
      }
    )
    
    estimationsBoxCox <- rbind(estimationsBoxCox,tempBoxCox)
  }
  
  return(list(conventional = estimationsConventional, boxCox = estimationsBoxCox))
  
}

JointEstimationNumericChoicesDifferentScaleParametersS3 <- function(scaleParameters,nReplications,scenarios
                                                                    ,realSimulationParametersAnimated,realSimulationParametersNumeric
                                                                    ,logitParameters){
  
  
  
  resultConventional <- data.frame(scaleParameter = c(),c = c(),ratioWV = c(), thetaW = c(), thetaV = c())
  resultBoxCox <- data.frame(scaleParameter = c(),c = c(),ratioWV = c(), thetaW = c(), thetaV = c())
  
  simulationParametersNumericTemp <- realSimulationParametersNumeric
  
  for(i in 1:length(scaleParameters)){
    
    simulationParametersNumericTemp["scale_gumbel"] <- scaleParameters[i]
    
    
    estimatesAllParameters <- JointEstimationNumericChoicesS3(nReplications = nReplications, scenarios = scenarios, logitParameters = logitParameters
                                                              , realSimulationParametersNumeric = simulationParametersNumericTemp
                                                              , realSimulationParametersAnimated = simulationParametersNumericTemp
    )
    
    
    #Conventional
    
    estimatesAllParametersConventional <- estimatesAllParameters[["conventional"]]
    
    resultConventional <- rbind(resultConventional,data.frame(scaleParameter = scaleParameters[i]
                                                              , c = estimatesAllParametersConventional[,c("c")]
                                                              , ratioWV = estimatesAllParametersConventional[,c("ratioWV")]
                                                              , thetaW = estimatesAllParametersConventional[,c("thetaW")]
                                                              , thetaV = estimatesAllParametersConventional[,c("thetaV")]
    )
    )
    
    #Box-cox
    
    estimatesAllParametersBoxCox <- estimatesAllParameters[["boxCox"]]
    
    resultBoxCox <- rbind(resultBoxCox,data.frame(scaleParameter = scaleParameters[i]
                                                  , c = estimatesAllParametersBoxCox[,c("c")]
                                                  , ratioWV = estimatesAllParametersBoxCox[,c("ratioWV")]
                                                  , thetaW = estimatesAllParametersBoxCox[,c("thetaW")]
                                                  , thetaV = estimatesAllParametersBoxCox[,c("thetaV")]
    )
    )  
    
    
  }
  
  return(list(conventional = resultConventional,boxCox = resultBoxCox))
  
}


#       - Parameters ------------------------------------------------------------
logitParametersS3 <- c(thetaW = 0, thetaV = 0,constant = NULL
                       , thetaPW = 1,thetaPV = 1, thetaAV = NULL, thetaAW = NULL)


simulationParametersNumericS3 <- simulationParametersNumeric

simulationParametersNumericS3$cv <- simulationParametersNumeric$cv
simulationParametersNumericS3$cw <- simulationParametersNumeric$cw

scaleParametersS3 <- seq(1,2,0.25)

nReplicationsS3 = 100


#       - Estimation results ---------------------------------------------------------------

#Sometimes there are estimation problems here (Hessian is not invertible)

#TODO: catch exceptions in JointEstimationNumericChoicesDifferentScaleParametersS3 on those replicates where the Hessian is not invertible


estimatesAllParametersDifferentScaleParametersAllS3 <- JointEstimationNumericChoicesDifferentScaleParametersS3(
  scaleParameters = scaleParametersS3, nReplications = nReplicationsS3
  , scenarios = c("s1","S3","s3","s4","s5","s6","s7","s8")
  , realSimulationParametersAnimated = simulationParametersNumericS3
  , realSimulationParametersNumeric = simulationParametersNumericS3
  , logitParameters = logitParametersS3
)

#Conventional
estimatesAllParametersDifferentScaleParametersS3 <- estimatesAllParametersDifferentScaleParametersAllS3[["conventional"]]
#Box-cox
estimatesAllParametersDifferentScaleParametersBoxCoxS3 <- estimatesAllParametersDifferentScaleParametersAllS3[["boxCox"]]

mean(estimatesAllParametersDifferentScaleParametersS3$c)
mean(estimatesAllParametersDifferentScaleParametersBoxCoxS3$c)

#       a) Plot Parameter Waiting -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ThetaW <- estimatesAllParametersDifferentScaleParametersS3[,c("scaleParameter","thetaW")]

#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection <- estimatesAllParametersDifferentScaleParametersS3ThetaW 

tt     <- split(estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection,estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x =  seqXThetaWS3,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.WAS3 <- data.frame(x = seqXThetaWS3, y = unlist(fitted),
                             scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection$scaleParameter),
                                                  each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3ThetaWNoCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WAS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3ThetaWA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#         - Correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection <- estimatesAllParametersDifferentScaleParametersS3ThetaW 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection$thetaW <- with(estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection,thetaW*scaleParameter)
plot.data.WBS3 <- plot.data.WAS3
plot.data.WBS3$x <- plot.data.WAS3$scaleParameter*plot.data.WAS3$x

# tt     <- split(estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection,estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x =  seqXThetaWS3,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.WBS3 <- data.frame(x =  seqXThetaWS3, y = unlist(fitted), 
#                         scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection$scaleParameter),
#                                              each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3ThetaWCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WBS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3ThetaWB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       b) Plot Parameter In-vehicle Time -----------------------------------------------------------------------
estimatesAllParametersDifferentScaleParametersS3ThetaV <- estimatesAllParametersDifferentScaleParametersS3[,c("scaleParameter","thetaV")]
#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection <- estimatesAllParametersDifferentScaleParametersS3ThetaV 

tt     <- split(estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection,estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS3,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.VAS3 <- data.frame(x = seqXThetaVS3, y = unlist(fitted), 
                             scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection$scaleParameter),
                                                  each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3ThetaVNoCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VAS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3ThetaVA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#         - Correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection <- estimatesAllParametersDifferentScaleParametersS3ThetaV 

#Adjusting for scale
estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection$thetaV <- with(estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection,thetaV*scaleParameter)
plot.data.VBS3 <- plot.data.VAS3
plot.data.VBS3$x <- plot.data.VAS3$scaleParameter*plot.data.VAS3$x

# tt     <- split(estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection,estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection$scaleParameter)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS3,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.VBS3 <- data.frame(x = seqXThetaVS3, y = unlist(fitted), 
#                         scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection$scaleParameter),
#                                              each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3ThetaVCorrection, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VBS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3ThetaVB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       c) Plot MRS -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3RatioWV <- estimatesAllParametersDifferentScaleParametersS3[,c("scaleParameter","ratioWV")]

tt     <- split(estimatesAllParametersDifferentScaleParametersS3RatioWV,estimatesAllParametersDifferentScaleParametersS3RatioWV$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$ratioWV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaMRSS3,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.MRSS3 <- data.frame(x = seqXThetaMRSS3, y = unlist(fitted), 
                              scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3RatioWV$scaleParameter),
                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3RatioWV$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3RatioWV, aes(x, group = as.factor(scaleParameter), color=scaleParameter))+
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.MRSS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dashed")+
  # scale_x_continuous(limits = c(with(simulationParametersNumeric,bw/bv)-0.2, with(simulationParametersNumeric,bw/bv)+0.2), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/hat(theta)[v]), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3MRS.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# mean(estimatesAllParameters$c); sd(estimatesAllParameters$c)
# sd(estimatesAllParameters$c)/mean(estimatesAllParameters$c)
# 
# mean(estimatesAllParameters$c)-1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# mean(estimatesAllParameters$c)+1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# 
# 
# plot(x=estimatesAllParameters$c,estimatesAllParameters$ll)
# 
# #Exponent
# p1 <- ggplot(data = data.frame(x = c(0, 2)), aes(x)) +
#   stat_function(fun = dnorm, n = 101, args = list(mean = mean(estimatesAllParameters$c), sd = sd(estimatesAllParameters$c))) + ylab("") +
#   scale_y_continuous(breaks = NULL)
# p1


#       d1) Plot exponent (conventional) -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3Exponents <- estimatesAllParametersDifferentScaleParametersS3[,c("scaleParameter","c")]

tt     <- split(estimatesAllParametersDifferentScaleParametersS3Exponents,estimatesAllParametersDifferentScaleParametersS3Exponents$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$c, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaCS3,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.CS3 <- data.frame(x = seqXThetaCS3, y = unlist(fitted),
                            scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3Exponents$scaleParameter),
                                                 each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3Exponents$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3Exponents, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.CS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(c), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3Exponent.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# p1
# 
# mean(estimatesAllParameters$c); sd(estimatesAllParameters$c)
# sd(estimatesAllParameters$c)/mean(estimatesAllParameters$c)
# 
# mean(estimatesAllParameters$c)-1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# mean(estimatesAllParameters$c)+1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# 
# 
# plot(x=estimatesAllParameters$c,estimatesAllParameters$ll)
# 
# #Exponent
# p1 <- ggplot(data = data.frame(x = c(0, 2)), aes(x)) +
#   stat_function(fun = dnorm, n = 101, args = list(mean = mean(estimatesAllParameters$c), sd = sd(estimatesAllParameters$c))) + ylab("") +
#   scale_y_continuous(breaks = NULL)
# p1


#       d2) Plot exponent (box-plot) -----------------------------------------------------------------------

estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox <- estimatesAllParametersDifferentScaleParametersBoxCoxS3[,c("scaleParameter","c")]

tt     <- split(estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox,estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox$scaleParameter)

fits   <- lapply(tt, function(x) fitdistr(x$c, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaCS3,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.CBoxPlotS3 <- data.frame(x = seqXThetaCS3, y = unlist(fitted),
                                   scaleParameter = rep(unique(estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox$scaleParameter),
                                                        each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox$scaleParameter))))


ggplot(estimatesAllParametersDifferentScaleParametersS3ExponentsBoxCox, aes(x, group = as.factor(scaleParameter), colour=scaleParameter)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.CBoxPlotS3, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(c), y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3ExponentBoxPlot.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# p1
# 
# mean(estimatesAllParameters$c); sd(estimatesAllParameters$c)
# sd(estimatesAllParameters$c)/mean(estimatesAllParameters$c)
# 
# mean(estimatesAllParameters$c)-1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# mean(estimatesAllParameters$c)+1.96*sd(estimatesAllParameters$c)/sqrt(dim(estimatesAllParameters)[1])
# 
# 
# plot(x=estimatesAllParameters$c,estimatesAllParameters$ll)
# 
# #Exponent
# p1 <- ggplot(data = data.frame(x = c(0, 2)), aes(x)) +
#   stat_function(fun = dnorm, n = 101, args = list(mean = mean(estimatesAllParameters$c), sd = sd(estimatesAllParameters$c))) + ylab("") +
#   scale_y_continuous(breaks = NULL)
# p1


#       e) Bw & Bv (Corrected and Not Corrected) (Paper) ---------------------------------------------------------------------

#Combination of plots WBS3 and VBS3 with no correction

dataPlotAPaperS3 <- rbind.data.frame(cbind.data.frame(plot = "WTNC", plot.data.WAS3, type = "waiting nc")
                                     ,cbind.data.frame(plot = "TTNC", plot.data.VAS3, type = "travel nc")
                                     ,cbind.data.frame(plot = "WWC", plot.data.WBS3, type = "waiting c")
                                     ,cbind.data.frame(plot = "TTC", plot.data.VBS3, type = "travel c")
)

#Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotAPaperS3$plot <- factor(dataPlotAPaperS3$plot, levels = c("WTNC","TTNC","WWC","TTC"),
                                labels = c("hat(theta)[w]","hat(theta)[v]","hat(theta)[w]/lambda","hat(theta)[v]/lambda"))

ggplot(dataPlotAPaperS3, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotAPaperS3 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]/lambda"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]/lambda"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )     

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3PlotAPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#expression(hat(theta)[v])
#       f) MRS & exponent (paper) ------------------------------------------------------------------

dataPlotBPaperS3 <- rbind.data.frame(cbind.data.frame(plot = "MRS", plot.data.MRSS3, type = "mrs")
                                     ,cbind.data.frame(plot = "C", plot.data.CS3, type = "c")
)

dataPlotBPaperS3Labels <- c("hat(theta)[w]/hat(theta)[v]","hat(alpha)")


# #Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotBPaperS3$plot <- factor(dataPlotBPaperS3$plot, levels = c("MRS","C"),
                                labels = c(dataPlotBPaperS3Labels[1],dataPlotBPaperS3Labels[2])
)



ggplot(dataPlotBPaperS3, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotBPaperS3 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotBPaperS3Labels[1]), aes(xintercept = xint),color = "black", linetype="dotdash",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,cw),plot=dataPlotBPaperS3Labels[2]), aes(xintercept = xint),color = "black", linetype="dotdash",show.legend=TRUE)+
  # geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="Waiting time"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  # geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="In-vehicle time"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0), breaks = seq(min(seqXThetaWS3),max(seqXThetaWS3),1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )

ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3PlotBPaper.pdf"), width = widthSimulationPlots, height = 0.6*heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


# dataPlotS3D <- rbind(cbind.data.frame(x = as.numeric(plot.data.mrsS3$ratioWV), y = as.numeric(plot.data.mrsS3$y,plot.data.mrsS3), scaleParameter =as.numeric(plot.data.mrsS3$scaleParameter),type = "mrs")
#                    #  ,cbind.data.frame(x = as.numeric(plot.data.wS3$thetaW), y = as.numeric(plot.data.wS3$y), scaleParameter =as.numeric(plot.data.wS3$scaleParameter),  type = "thetaW")
#                    #  ,cbind.data.frame(x = as.numeric(plot.data.vS3$thetaV), y = as.numeric(plot.data.vS3$y), scaleParameter =as.numeric(plot.data.vS3$scaleParameter),  type = "thetaV")
#                      ,cbind.data.frame(x = as.numeric(plot.data.cS3$c), y = as.numeric(plot.data.cS3$y), scaleParameter =as.numeric(plot.data.cS3$scaleParameter),  type = "c")
# )

#Only pick the curves for waiting and in-vechiles times for the scale parameter equals to 1.

# plot.data.wS3Scale1 <- subset(plot.data.wS3,scaleParameter == 1)
# plot.data.vS3Scale1 <- subset(plot.data.vS3,scaleParameter == 1)
# 
# dataPlotS3D <- rbind(cbind.data.frame(x = as.numeric(plot.data.mrsS3$ratioWV), y = as.numeric(plot.data.mrsS3$y,plot.data.mrsS3), scaleParameter =as.numeric(plot.data.mrsS3$scaleParameter),type = "mrs")
#                     # ,cbind.data.frame(x = as.numeric(plot.data.wS3Scale1$thetaW), y = as.numeric(plot.data.wS3Scale1$y), scaleParameter =as.numeric(plot.data.wS3Scale1$scaleParameter),  type = "thetaW")
#                     # ,cbind.data.frame(x = as.numeric(plot.data.vS3Scale1$thetaV), y = as.numeric(plot.data.vS3Scale1$y), scaleParameter =as.numeric(plot.data.vS3Scale1$scaleParameter),  type = "thetaV")
#                      ,cbind.data.frame(x = as.numeric(plot.data.cS3$c), y = as.numeric(plot.data.cS3$y), scaleParameter =as.numeric(plot.data.cS3$scaleParameter),  type = "c")
# )
# 
# 
# ggplot(dataPlotS3D, aes(x, group = interaction(scaleParameter,type), color=scaleParameter))+
#   geom_line(data = dataPlotS3D , aes(y = y), lwd = 0.5)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dotdash",show.legend=TRUE)+
#   geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "black", linetype="solid",show.legend=TRUE)+
#   # geom_vline(xintercept = with(simulationParametersNumeric,bw),color = "black", linetype="dotted",show.legend=TRUE)+
#   # geom_vline(xintercept = with(simulationParametersNumeric,bv),color = "black", linetype="dashed",show.legend=TRUE)+
#   scale_color_gradient(low="lightgray",high="black")+
#   # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
#   # scale_linetype_manual(values = c("A","B","C"))+
#   scale_x_continuous(expand = c(0, 0), limits = c(0,2)) + #, limits = c(-5,5)
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/FreeCTimePerception/S3PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#       g) MRS S3 vs MRS S2 vs MRS S1 (PAPER) ---------------------------------------------------------------------


dataPlotAPaperS123 <- rbind.data.frame(cbind.data.frame(plot = "MRSS1", subset(plot.data.MRSS1, x>=0.9 & x<=1.3), type = "MRS")
                                       , cbind.data.frame(plot = "MRSS2", subset(plot.data.MRSS2, x>=0.9 & x<=1.3), type = "MRS")
                                       ,cbind.data.frame(plot = "MRSS3", subset(plot.data.MRSS3, x>=0.9 & x<=1.3), type = "MRS")
                                       # ,cbind.data.frame(plot = "Waiting time", plot.data.WBS3, type = "waiting")
                                       # ,cbind.data.frame(plot = "In-vehicle time", plot.data.VBS3, type = "travel")
)

# dataPlotAPaperS123Labels <- c("hat(theta)[w]/hat(theta)[v](bar(c)1)","hat(theta)[w]/hat(theta)[v](bar(c)0.9)","hat(theta)[w]/hat(theta)[v](chat(c))")
# 
dataPlotAPaperS123Labels <- c("S[1][]:hat(theta)[w]/hat(theta)[v]","S[2][]:hat(theta)[w]/hat(theta)[v]","S[3][]:hat(theta)[w]/hat(theta)[v]")


# #Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotAPaperS123$plot <- factor(dataPlotAPaperS123$plot, levels = c("MRSS1","MRSS2","MRSS3"),
                                  labels = c(dataPlotAPaperS123Labels[1],dataPlotAPaperS123Labels[2],dataPlotAPaperS123Labels[3])
)

ggplot(dataPlotAPaperS123, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotAPaperS123 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotAPaperS123Labels[1]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotAPaperS123Labels[2]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotAPaperS123Labels[3]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=3, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0)) + #, breaks = seq(min(seqXThetaWS3),max(seqXThetaWS3),1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )


ggsave(str_c("export","/figures/Paper1/Simulations/S123PlotAPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)



#Only pick the curves for waiting and in-vechiles times for the scale parameter equals to 1.

# plot.data.wS2Scale1 <- subset(plot.data.wS2,scaleParameter == 1)
# plot.data.vS2Scale1 <- subset(plot.data.vS2,scaleParameter == 1)
# 
# dataPlotS2D <- rbind(cbind.data.frame(x = as.numeric(plot.data.mrsS2$ratioWV), y = as.numeric(plot.data.mrsS2$y,plot.data.mrsS2), scaleParameter =as.numeric(plot.data.mrsS2$scaleParameter),type = "mrs")
#                      ,cbind.data.frame(x = as.numeric(plot.data.wS2Scale1$thetaW), y = as.numeric(plot.data.wS2Scale1$y), scaleParameter =as.numeric(plot.data.wS2Scale1$scaleParameter),  type = "thetaW")
#                      ,cbind.data.frame(x = as.numeric(plot.data.vS2Scale1$thetaV), y = as.numeric(plot.data.vS2Scale1$y), scaleParameter =as.numeric(plot.data.vS2Scale1$scaleParameter),  type = "thetaV")
# )
# 
# ggplot(dataPlotS2D, aes(x, group = interaction(scaleParameter,type), color=scaleParameter))+
#   geom_line(data = dataPlotS2D , aes(y = y), lwd = 0.5)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw),color = "black", linetype="dotted",show.legend=TRUE)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bv),color = "black", linetype="dashed",show.legend=TRUE)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dotdash",show.legend=TRUE)+
#   geom_vline(xintercept = 1,color = "black", linetype="solid",show.legend=TRUE)+ #c assumed equal 1
#   scale_color_gradient(low="lightgray",high="black")+
#   # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
#   # scale_linetype_manual(values = c("A","B","C"))+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)



#       h) C VS MRS S3 vs MRS S2 vs MRS S1 (PAPER) ---------------------------------------------------------------------

dataPlotBPaperS123 <- rbind.data.frame(cbind.data.frame(plot = "MRSS1", plot.data.MRSS1, type = "MRS")
                                       , cbind.data.frame(plot = "MRSS2", plot.data.MRSS2, type = "MRS")
                                       , cbind.data.frame(plot = "MRSS3", plot.data.MRSS3, type = "MRS")
                                       ,cbind.data.frame(plot = "C", plot.data.CS3, type = "C")
)

# dataPlotAPaperS123Labels <- c("hat(theta)[w]/hat(theta)[v](bar(c)1)","hat(theta)[w]/hat(theta)[v](bar(c)0.9)","hat(theta)[w]/hat(theta)[v](chat(c))")
# 
dataPlotBPaperS123Labels <- c("S[1][]:hat(theta)[w]/hat(theta)[v]","S[2][]:hat(theta)[w]/hat(theta)[v]","S[3][]:hat(theta)[w]/hat(theta)[v]","S[3][]:hat(alpha)")


# #Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotBPaperS123$plot <- factor(dataPlotBPaperS123$plot, levels = c("MRSS1","MRSS2","MRSS3","C"),
                                  labels = c(dataPlotBPaperS123Labels[1],dataPlotBPaperS123Labels[2],dataPlotBPaperS123Labels[3],dataPlotBPaperS123Labels[4])
)

ggplot(dataPlotBPaperS123, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotBPaperS123 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotBPaperS123Labels[1]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotBPaperS123Labels[2]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw/bv),plot=dataPlotBPaperS123Labels[3]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,cw),plot=dataPlotBPaperS123Labels[4]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(.~factor(plot),ncol=2, labeller=label_parsed)+
  # facet_grid(~factor(plot))+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0)) + #, breaks = seq(min(seqXThetaWS3),max(seqXThetaWS3),1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )


ggsave(str_c("export","/figures/Paper1/Simulations/S123PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)



#Only pick the curves for waiting and in-vechiles times for the scale parameter equals to 1.

# plot.data.wS2Scale1 <- subset(plot.data.wS2,scaleParameter == 1)
# plot.data.vS2Scale1 <- subset(plot.data.vS2,scaleParameter == 1)
# 
# dataPlotS2D <- rbind(cbind.data.frame(x = as.numeric(plot.data.mrsS2$ratioWV), y = as.numeric(plot.data.mrsS2$y,plot.data.mrsS2), scaleParameter =as.numeric(plot.data.mrsS2$scaleParameter),type = "mrs")
#                      ,cbind.data.frame(x = as.numeric(plot.data.wS2Scale1$thetaW), y = as.numeric(plot.data.wS2Scale1$y), scaleParameter =as.numeric(plot.data.wS2Scale1$scaleParameter),  type = "thetaW")
#                      ,cbind.data.frame(x = as.numeric(plot.data.vS2Scale1$thetaV), y = as.numeric(plot.data.vS2Scale1$y), scaleParameter =as.numeric(plot.data.vS2Scale1$scaleParameter),  type = "thetaV")
# )
# 
# ggplot(dataPlotS2D, aes(x, group = interaction(scaleParameter,type), color=scaleParameter))+
#   geom_line(data = dataPlotS2D , aes(y = y), lwd = 0.5)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw),color = "black", linetype="dotted",show.legend=TRUE)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bv),color = "black", linetype="dashed",show.legend=TRUE)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dotdash",show.legend=TRUE)+
#   geom_vline(xintercept = 1,color = "black", linetype="solid",show.legend=TRUE)+ #c assumed equal 1
#   scale_color_gradient(low="lightgray",high="black")+
#   # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
#   # scale_linetype_manual(values = c("A","B","C"))+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S2PlotBPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)





        
#       j) C Conventional VS C Box-Plot (NEW) ---------------------------------------------------------------------

dataPlotCPaperS3 <- rbind.data.frame(cbind.data.frame(plot = "C Box-Plot", plot.data.CBoxPlotS3, type = "Box-Plot")
                                     ,cbind.data.frame(plot = "C Conventional", plot.data.CS3, type = "Conventional")
)

# dataPlotAPaperS123Labels <- c("hat(theta)[w]/hat(theta)[v](bar(c)1)","hat(theta)[w]/hat(theta)[v](bar(c)0.9)","hat(theta)[w]/hat(theta)[v](chat(c))")
# 
dataPlotCPaperS3Labels <- c("S[3][]:hat(c_box)","S[3][]:hat(c)")


# #Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotCPaperS3$plot <- factor(dataPlotCPaperS3$plot, levels = c("C Box-Plot","C Conventional"),
                                labels = c(dataPlotCPaperS3Labels[1],dataPlotCPaperS3Labels[2])
)

ggplot(dataPlotCPaperS3, aes(x, group = interaction(scaleParameter,type,plot), color=scaleParameter))+
  geom_line(data = dataPlotCPaperS3 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,cw),plot=dataPlotCPaperS3Labels[1]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,cw),plot=dataPlotCPaperS3Labels[2]), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0)) + #, breaks = seq(min(seqXThetaWS3),max(seqXThetaWS3),1)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",lambda)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )


ggsave(str_c("export","/figures/Paper1/Simulations/S3PlotCPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#     S4) Time perception sensitivity as logit scale parameter (scale parameters gumbel = 1, different values of c, estimation assuming c = 1) -------------------------------------------

#Lesson: Adding a exponent increases variance of the estimates. This is dramatic for the estimate of c, but it also occurs for the preference parameters estimates

#       Plot parameters ---------------------------------------------------------

seqXThetaWS4 <- seq(-4,2,0.01)
seqXThetaVS4 <- seq(-4,2,0.01)
seqXThetaMRSS4 <- seq(0,2,0.001)
seqXThetaCS4 <- seq(0.5,1.5,0.25)

#       Estimation --------------------------------------------------------------
#         - Functions -------------------------------------------------------------


JointEstimationNumericChoicesFixedTimePerceptionS4 <- function(nReplications, scenarios
                                                               , realSimulationParametersAnimated
                                                               , realSimulationParametersNumeric
                                                               , logitParameters, knownTimePerception,cAssumed){
  
  estimations <- data.frame()
  
  for(nReplication in 1:nReplications){
    
    set.seed(1)
    rm(.Random.seed, envir=globalenv())
    seed <- floor(runif(1, 1,Sys.time())) 
    
    data <- simulatingChoices(seed = seed
                              ,simulationParametersNumericTemp = realSimulationParametersNumeric
                              ,simulationParametersAnimatedTemp =  realSimulationParametersAnimated)
    
    data <- subset(data,experimentType == "numeric" & scenario %in% scenarios)
    
    realSimulationParametersNumericAssumed <- realSimulationParametersNumeric
    realSimulationParametersNumericAssumed$cw <- cAssumed
    realSimulationParametersNumericAssumed$cv <- cAssumed
    
    DCMNumeric <- LogitTimePerception(DCMData = data
                                      , logitParameters = logitParameters, method = "BHHH"
                                      , simulationParameters = realSimulationParametersNumericAssumed
                                      , sameCurvature = FALSE, sameTimeWeighting = FALSE, knownTimePerception = TRUE, boxCox = FALSE)
    
    
    ratioWV <- DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]; ratioWV
    
    temp <- data.frame(replication = nReplication
                       # , c = DCMNumeric[[1]]["thetaP","estimate"]#, cv = DCMNumeric[[1]]["thetaPV","estimate"]
                       , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                       , ratioWV = ratioWV, ll = logLik(DCMNumeric[[2]]))
    
    estimations <- rbind(estimations,temp)
  }
  
  return(estimations)
  
}

JointEstimationNumericChoicesFixedTimePerceptionDifferentCValuesS4 <- function(cValues,nReplications,scenarios
                                                                               ,realSimulationParametersAnimated,realSimulationParametersNumeric
                                                                               ,logitParameters,cAssumed,knownTimePerception){
  
  result <- data.frame(scaleParameter = c(),c = c(),ratioWV = c(), thetaW = c(), thetaV = c())
  
  simulationParametersNumericTemp <- realSimulationParametersNumeric
  
  for(i in 1:length(cValues)){
    
    simulationParametersNumericTemp["cw"] <- cValues[i]
    simulationParametersNumericTemp["cv"] <- cValues[i]
    
    estimatesAllParameters <- JointEstimationNumericChoicesFixedTimePerceptionS4(nReplications = nReplications, scenarios = scenarios
                                                                                 , realSimulationParametersNumeric = simulationParametersNumericTemp
                                                                                 , realSimulationParametersAnimated = simulationParametersNumericTemp
                                                                                 , logitParameters = logitParameters, knownTimePerception = knownTimePerception
                                                                                 ,cAssumed = 1)  
    
    #result <- rbind(result, cbind.data.frame(scaleParameter = scaleParameters[i], c = estimatesAllParameters[,c("c")]))
    result <- rbind(result,data.frame(c = cValues[i]
                                      , ratioWV = estimatesAllParameters[,c("ratioWV")]
                                      , thetaW = estimatesAllParameters[,c("thetaW")]
                                      , thetaV = estimatesAllParameters[,c("thetaV")]
    )
    )
  }
  
  return(result)
  
}
#         - Parameters ------------------------------------------------------------



logitParametersS4 <- c(thetaW = 0, thetaV = 0,constant = NULL
                       , thetaPW = 1,thetaPV = 1, thetaAV = NULL, thetaAW = NULL)


simulationParametersNumericS4 <- simulationParametersNumeric

simulationParametersNumericS4$cw <- 0.9
simulationParametersNumericS4$cv <- 0.9

cValuesS4 <- seqXThetaCS4

nReplicationsS4 = 200

#         - Results ---------------------------------------------------------------




estimatesAllParametersDifferentCValuesS4 <- JointEstimationNumericChoicesFixedTimePerceptionDifferentCValuesS4(
  cValues =  cValuesS4, nReplications = nReplicationsS4
  , scenarios = c("s1","s2","s3","s4","s5","s6","s7","s8")
  , realSimulationParametersAnimated = simulationParametersNumericS4
  , realSimulationParametersNumeric = simulationParametersNumericS4
  , logitParameters = logitParametersS4, cAssumed = 1, knownTimePerception = TRUE
)

#       a) Plot Parameter Waiting -----------------------------------------------------------------------

estimatesAllParametersDifferentCValuesS4ThetaW <- estimatesAllParametersDifferentCValuesS4[,c("c","thetaW")]

#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection <- estimatesAllParametersDifferentCValuesS4ThetaW

tt     <- split(estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection,estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection$c)

fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS4,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.WAS4 <- data.frame(x = seqXThetaWS4, y = unlist(fitted),
                             c = rep(unique(estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection$c),
                                     each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection$c))))


ggplot(estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection, aes(x, group = as.factor(c), colour=c)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.WAS4, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]), y = "Density",colour = expression(paste("  ",c)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/RelationCScaleParameter/S4ThetaWA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


# #         - Correction for scaling --------------------------------------------------------------
# 
# #Plot to determine correction
# tt     <- split(estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection,estimatesAllParametersDifferentCValuesS4ThetaWNoCorrection$c)
# fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))
# meansBW <- lapply(fits, function(x) x$estimate[1])
# 
# meansBWGivenC <- data.frame(c = as.numeric(names(meansBW)), mean = abs(as.numeric(meansBW)))
# 
# plot(meansBWGivenC)
# 
# modelScaleC <- summary(lm(log(mean) ~ 1 + log(c), data = meansBWGivenC))
# 
# modelScaleC$coefficients[1]
# modelScaleC$coefficients[2]
# 
# plotRegressionC <- data.frame(x = seq(0.25,1.25,0.25), y = NA)
# 
# plotRegressionC$y <-  exp(modelScaleC$coefficients[1])*plotRegressionC$x^(modelScaleC$coefficients[2])
# 
# plot(x = plotRegressionC$x,plotRegressionC$y)
# 
# estimatesAllParametersDifferentCValuesS4ThetaWCorrection <- estimatesAllParametersDifferentCValuesS4ThetaW 
# 
# estimatesAllParametersDifferentCValuesS4ThetaWCorrection$scaleParameterC <- with(estimatesAllParametersDifferentCValuesS4ThetaWCorrection,exp(modelScaleC$coefficients[1])*c^(modelScaleC$coefficients[2]))
# 
# #Adjusting for scale
# estimatesAllParametersDifferentCValuesS4ThetaWCorrection$thetaW <- with(estimatesAllParametersDifferentCValuesS4ThetaWCorrection,thetaW/scaleParameterC)
# plot.data.WBS4 <- plot.data.WAS4
# plot.data.WBS4$scaleParameterC <- exp(modelScaleC$coefficients[1])*(plot.data.WBS4$c)^(modelScaleC$coefficients[2])
# plot.data.WBS4$x <- plot.data.WAS4$x/plot.data.WBS4$scaleParameterC
# 
# 
# # tt     <- split(estimatesAllParametersDifferentCValuesS4ThetaWCorrection,estimatesAllParametersDifferentCValuesS4ThetaWCorrection$c)
# # 
# # fits   <- lapply(tt, function(x) fitdistr(x$thetaW, "normal"))
# # 
# # ## Predict values
# # fitted <- lapply(fits, function(x) dnorm(x = seqXThetaWS4,
# #                                          mean = x$estimate[1], sd = x$estimate[2]))
# # 
# # 
# # plot.data.WBS4 <- data.frame(x = seqXThetaWS4, y = unlist(fitted),
# #                              c = rep(unique(estimatesAllParametersDifferentCValuesS4ThetaWCorrection$c),
# #                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentCValuesS4ThetaWCorrection$c))))
# 
# 
# ggplot(estimatesAllParametersDifferentCValuesS4ThetaWCorrection, aes(x, group = as.factor(c), colour=c)) +
#   scale_color_gradient(low="lightgray",high="black")+
#   geom_line(data = plot.data.WBS4, aes(y = y), lwd = 0.5)+
#   geom_vline(xintercept = simulationParametersNumeric$bw,color = "black", linetype="dashed")+
#   scale_x_continuous(expand = c(0, 0), limits = c(-4,2)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = expression(hat(theta)[w]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S4ThetaWB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

#       b) Plot Parameter In-vehicle Time -----------------------------------------------------------------------
estimatesAllParametersDifferentCValuesS4ThetaV <- estimatesAllParametersDifferentCValuesS4[,c("c","thetaV")]
#         - No correction for scaling --------------------------------------------------------------

estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection <- estimatesAllParametersDifferentCValuesS4ThetaV

tt     <- split(estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection,estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection$c)

fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS4,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.VAS4 <- data.frame(x = seqXThetaVS4, y = unlist(fitted),
                             c = rep(unique(estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection$c),
                                     each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection$c))))


ggplot(estimatesAllParametersDifferentCValuesS4ThetaVNoCorrection, aes(x, group = as.factor(c), colour=c)) +
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.VAS4, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[v]), y = "Density",colour = expression(paste("  ",c)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/RelationCScaleParameter/S4ThetaVA.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


# #         - Correction for scaling --------------------------------------------------------------
# 
# estimatesAllParametersDifferentCValuesS4ThetaVCorrection <- estimatesAllParametersDifferentCValuesS4ThetaV 
# 
# #Adjusting for scale
# estimatesAllParametersDifferentCValuesS4ThetaVCorrection$thetaV <- with(estimatesAllParametersDifferentCValuesS4ThetaVCorrection,thetaV*c)
# 
# tt     <- split(estimatesAllParametersDifferentCValuesS4ThetaVCorrection,estimatesAllParametersDifferentCValuesS4ThetaVCorrection$c)
# 
# fits   <- lapply(tt, function(x) fitdistr(x$thetaV, "normal"))
# 
# ## Predict values
# fitted <- lapply(fits, function(x) dnorm(x = seqXThetaVS4,
#                                          mean = x$estimate[1], sd = x$estimate[2]))
# 
# 
# plot.data.VBS4 <- data.frame(x = seqXThetaVS4, y = unlist(fitted), 
#                              c = rep(unique(estimatesAllParametersDifferentCValuesS4ThetaVCorrection$c),
#                                                   each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentCValuesS4ThetaVCorrection$c))))
# 
# 
# ggplot(estimatesAllParametersDifferentCValuesS4ThetaVCorrection, aes(x, group = as.factor(c), colour=c)) +
#   scale_color_gradient(low="lightgray",high="black")+
#   geom_line(data = plot.data.VBS4, aes(y = y), lwd = 0.5)+
#   geom_vline(xintercept = simulationParametersNumeric$bv,color = "black", linetype="dashed")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = expression(hat(theta)[v]/lambda), y = "Density",colour = expression(paste("  ",lambda)))+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/Simulations/FixedCTimePerception/S4ThetaVB.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)
# 
# 

#       c) Plot MRS -----------------------------------------------------------------------

estimatesAllParametersDifferentCValuesS4RatioWV <- estimatesAllParametersDifferentCValuesS4[,c("c","ratioWV")]

tt     <- split(estimatesAllParametersDifferentCValuesS4RatioWV,estimatesAllParametersDifferentCValuesS4RatioWV$c)

fits   <- lapply(tt, function(x) fitdistr(x$ratioWV, "normal"))

## Predict values
fitted <- lapply(fits, function(x) dnorm(x = seqXThetaMRSS4,
                                         mean = x$estimate[1], sd = x$estimate[2]))


plot.data.MRSS4 <- data.frame(x = seqXThetaMRSS4, y = unlist(fitted), 
                              c = rep(unique(estimatesAllParametersDifferentCValuesS4RatioWV$c),
                                      each = length(unlist(fitted))/length(unique(estimatesAllParametersDifferentCValuesS4RatioWV$c))))


ggplot(estimatesAllParametersDifferentCValuesS4RatioWV, aes(x, group = as.factor(c), color=c))+
  scale_color_gradient(low="lightgray",high="black")+
  geom_line(data = plot.data.MRSS4, aes(y = y), lwd = 0.5)+
  geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dashed")+
  # scale_x_continuous(limits = c(with(simulationParametersNumeric,bw/bv)-0.2, with(simulationParametersNumeric,bw/bv)+0.2), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0.8,1.3)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = expression(hat(theta)[w]/hat(theta)[v]), y = "Density",colour = expression(paste("  ",c)))+
  themePlotsDCMParametersPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/RelationCScaleParameter/S4MRS.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)



#       d) Bw & Bv (Not Corrected) (Paper) ---------------------------------------------------------------------

#Combination of plots WBS2 and VBS2 with no correction

dataPlotAPaperS4 <- rbind.data.frame(cbind.data.frame(plot = "WTNC", plot.data.WAS4, type = "waiting nc")
                                     ,cbind.data.frame(plot = "TTNC", plot.data.VAS4, type = "travel nc")
                                     # ,cbind.data.frame(plot = "MRS NC", plot.data.MRSS4, type = "mrs nc")
                                     # ,cbind.data.frame(plot = "WWC", plot.data.WBS4, type = "waiting c")
                                     # ,cbind.data.frame(plot = "TTC", plot.data.VBS4, type = "travel c")
                                     # ,cbind.data.frame(plot = "MRS C", plot.data.MRSS4, type = "mrs c")
                                     #,cbind.data.frame(plot = "MRS", plot.data.MRSS4, type = "mrs")
)

#Tutorial:https://stackoverflow.com/questions/35524202/as-labeller-with-expression-in-ggplot2-facet-wrap
dataPlotAPaperS4$plot <- factor(dataPlotAPaperS4$plot, levels = c("WTNC","TTNC"),
                                labels = c("hat(theta)[w]","hat(theta)[v]"))

ggplot(dataPlotAPaperS4, aes(x, group = interaction(c,type,plot), color=c))+
  geom_line(data = dataPlotAPaperS4 , aes(y = y), lwd = 0.5)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  # geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bw),plot="hat(theta)[w]/lambda"), aes(xintercept = xint),color = "black", linetype="dashed",show.legend=TRUE)+
  # geom_vline(data = data.frame(xint = with(simulationParametersNumeric,bv),plot="hat(theta)[v]/lambda"), aes(xintercept = xint),color = "black", linetype="dotted",show.legend=TRUE)+
  scale_color_gradient(low="lightgray",high="black")+
  facet_wrap(~plot,ncol=2, labeller=label_parsed)+
  # scale_linetype_manual(values = c(expression(hat(theta)[w]/hat(theta)[v]),expression(hat(theta)[w]),expression(hat(theta)[v])))+
  # scale_linetype_manual(values = c("A","B","C"))+
  scale_x_continuous(expand = c(0, 0))+#, breaks = seq(min(seqXThetaWS1),max(seqXThetaWS1),1), limits = c(-4,2)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Estimate", y = "Density",colour = expression(paste("  ",c)))+
  themePlotsDCMParametersPaper1+
  theme(panel.spacing.x = unit(1, "cm"), panel.border = element_rect(color="black", fill=NA), strip.text = element_text(size=25) #Size of columns' titles
        , strip.background =element_rect(fill="white", colour = "black") #Colour backgorund of columns' titles)
  )     

ggsave(str_c("export","/figures/Paper1/Simulations/RelationCScaleParameter/S4PlotAPaper.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# P6b) DCM SIMULATIONS ADDITIONAL ANALYSES -------------------------------------------------------------------
#     DCM1) Grid search: Bias and correlation between time perception sensitivity (c) and estimated preference parameters (thetaW,thetaV) --------------------------------------------------
#       - Estimation --------------------------------------------------------------
simulationParametersNumericDCM1 <- simulationParametersNumeric
simulationParametersNumericDCM1$cw <- simulationParametersNumeric$cw
simulationParametersNumericDCM1$cv <- simulationParametersNumeric$cv

logitParametersDCM1 <- c(thetaW = 0, thetaV = 0,constant = NULL
                         , thetaPW = simulationParametersNumericDCM1$cw,thetaPV = simulationParametersNumericDCM1$cv, thetaAV = NULL, thetaAW = NULL)

#Grid search
GridSearchOverTimePerception <- function(exponents,nReplications, scenarios
                                         , realSimulationParametersAnimated
                                         , realSimulationParametersNumeric
                                         , logitParameters){
  
  estimations <- data.frame()
  
  for(nReplication in 1:nReplications){
    
    set.seed(1)
    rm(.Random.seed, envir=globalenv())
    seed <- floor(runif(1, 1,Sys.time())) 
    
    data <- simulatingChoices(seed = seed
                              ,simulationParametersNumericTemp = realSimulationParametersNumeric
                              ,simulationParametersAnimatedTemp =  realSimulationParametersAnimated)
    
    data <- subset(data,experimentType == "numeric")
    
    tempRealSimulationParametersNumeric <- realSimulationParametersNumeric
    
    for(i in 1:length(exponents)){
      
      exponent <- exponents[i]
      
      tempRealSimulationParametersNumeric[["cw"]] <- exponent
      tempRealSimulationParametersNumeric[["cv"]] <- exponent
      
      DCMNumeric <- LogitTimePerception(DCMData = data
                                        , logitParameters = logitParameters, method = "BHHH"
                                        , simulationParameters = tempRealSimulationParametersNumeric
                                        , sameCurvature = FALSE, sameTimeWeighting = FALSE, knownTimePerception = TRUE, boxCox = FALSE)
      
      #Without grid search
      
      DCMNumeric_NGS <- LogitTimePerception(DCMData = data
                                            , logitParameters = logitParameters, method = "BHHH"
                                            , simulationParameters = tempRealSimulationParametersNumeric
                                            , sameCurvature = TRUE, sameTimeWeighting = FALSE, knownTimePerception = FALSE, boxCox = FALSE)
      
      
      temp <- data.frame(replication = nReplication, cw = tempRealSimulationParametersNumeric[["cw"]], cv = tempRealSimulationParametersNumeric[["cv"]]
                         , thetaW = DCMNumeric[[1]]["thetaW","estimate"],thetaV = DCMNumeric[[1]]["thetaV","estimate"]
                         , ratioWV = DCMNumeric[[1]]["thetaW","estimate"]/DCMNumeric[[1]]["thetaV","estimate"]
                         , ll = logLik(DCMNumeric[[2]])
                         , c_NGS = DCMNumeric_NGS[[1]]["thetaP","estimate"]
                         , thetaW_NGS = DCMNumeric_NGS[[1]]["thetaW","estimate"], thetaV_NGS = DCMNumeric_NGS[[1]]["thetaV","estimate"]
                         , ratioWV_NGS = DCMNumeric_NGS[[1]]["thetaW","estimate"]/DCMNumeric_NGS[[1]]["thetaV","estimate"]
                         , ll_NGS = logLik(DCMNumeric_NGS[[2]])
      )
      estimations <- rbind(estimations,temp)
      
      # numbers[[seed]] <- number
    }
    
  }
  # return(numbers)
  return(estimations)
  
}

#Part A and B
exponentsNumeric <- seq(0.5,1.5,0.01)
# exponentsNumeric <- 1

estimatesPlotsDCM1 <- GridSearchOverTimePerception(exponents = exponentsNumeric, nReplications = 1
                                                   , scenarios =  c("s1","s2","s3","s4","s5","s6","s7","s8")
                                                   , realSimulationParametersNumeric = simulationParametersNumericDCM1
                                                   , realSimulationParametersAnimated = simulationParametersNumericDCM1
                                                   , logitParameters = logitParametersDCM1)

estimatesPlotsDCM1

# #Part C (bias)
# #High computation time
# gridSearchEstimates <- GridSearchOverTimePerception(exponents = exponentsNumeric, nReplications = 100
#                                                     , scenarios =  c("s1","s2","s3","s4","s5","s6","s7","s8")
#                                                     , realSimulationParametersNumeric = simulationParametersNumericDCM1
#                                                     , realSimulationParametersAnimated = simulationParametersNumericDCM1
#                                                     , logitParameters = logitParametersDCM1)




#       - Plot Themes --------------------------------------------------------------

themePlotsDCM1APaper1 <- theme(panel.grid.major = element_blank()
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
                               ,legend.key.size= unit(1.3,"cm")
                               ,legend.text=element_text(size=16)
                               ,legend.position="bottom"
                               
)

themePlotsDCM1BPaper1 <- theme(panel.grid.major = element_blank()
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
                               # , legend.key.width = 
                               # , legend.margin = margin(r = 2, unit='cm')
                               ,legend.background = element_rect(colour = 'black', fill = "transparent" )
                               ,legend.box.background = element_blank()
                               ,legend.key = element_rect(colour="black", fill = "transparent" )
                               ,legend.key.size= unit(1.3,"cm")
                               ,legend.text=element_text(size=16)
                               ,legend.position="bottom"
                               
)

#       - Plot (Bw,Bv,TMS) vs C (PAPER)----------------------------------------------------------

estimatesPlotsDCM1BWBV <- rbind(data.frame(c = estimatesPlotsDCM1$cw, theta = estimatesPlotsDCM1$thetaW, type = "bw")
                                ,data.frame(c = estimatesPlotsDCM1$cw, theta = estimatesPlotsDCM1$thetaV, type = "bv")
                                ,data.frame(c = estimatesPlotsDCM1$cw, theta = estimatesPlotsDCM1$ratioWV, type = "mrs")
                                ,data.frame(c = estimatesPlotsDCM1$cw, theta = with(simulationParametersNumeric,bw/bv), type = "true mrs")
)

ggplot(estimatesPlotsDCM1BWBV, aes(x = c, y = theta, color = interaction(type), linetype = interaction(type))) +
  geom_line(aes(group = type))+
  scale_color_manual(name = "", values=c("black", "lightgray","purple","purple")
                     , labels = c(expression(paste(hat(theta)[w],"  ")),expression(paste(hat(theta)[v],"  "))
                                  ,expression(paste(hat(theta)[w]/hat(theta)[v]," ")),expression(paste(theta[w]/theta[v]," "))))+ 
  scale_linetype_manual(name = "", values=c("solid", "solid", "solid","dashed")
                        , labels = c(expression(paste(hat(theta)[w],"  ")),expression(paste(hat(theta)[v],"  "))
                                     ,expression(paste(hat(theta)[w]/hat(theta)[v]," ")),expression(paste(theta[w]/theta[v]," "))))+ 
  # geom_hline(yintercept = 1,color = "purple", linetype="dotted")+
  # geom_hline(yintercept = with(simulationParametersNumeric,bv),color = "blue", linetype="dashed")+
  # geom_vline(xintercept = with(simulationParametersNumeric,cw),color = "lightgray", linetype="dashed")+
  # geom_vline(xintercept = estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == max(estimatesPlotsDCM1$ll)),]$cw,color = "black", linetype="dashed")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(min(estimatesPlotsDCM1BWBV$theta),2)) +
  labs(x = expression(bar(c)), y = "Estimate")+
  themePlotsDCM1APaper1

ggsave(str_c("export","/figures/Paper1/Simulations/GridSearch/DCM1A.pdf")
       , width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)


#       - Plot LogLik vs C (Paper) ---------------------------------------------------------------------


# estimatesPlotsDCM1LLC <-subset(estimatesPlotsDCM1,cw <1.6)

#Only keep data points where the loglikelihood is greater than the maximum for values of c below the optimal

maxLL_GS <- max(estimatesPlotsDCM1$ll)
cMaxLL_GS <- estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll == maxLL_GS),]$cw

# estimatesPlotsDCM1LLC <- subset(estimatesPlotsDCM1, estimatesPlotsDCM1$cw <= cMaxLL | (estimatesPlotsDCM1$cw>cMaxLL & estimatesPlotsDCM1$ll> minLL))

# estimatesPlotsDCM1LLC <- estimatesPlotsDCM1

#Estimation c (no grid search)
maxLL_NGS <- max(estimatesPlotsDCM1$ll_NGS)
cMaxLL_NGS <- estimatesPlotsDCM1[which(estimatesPlotsDCM1$ll_NGS == maxLL_NGS)[1],]$c_NGS

#C = 1
maxLL_C1 <- estimatesPlotsDCM1[which(estimatesPlotsDCM1$cw == 1)[1],]$ll
cMaxLL_C1 <- 1


cMaxLL_NGS; cMaxLL_GS

maxLL_NGS;maxLL_GS

ggplot(estimatesPlotsDCM1, aes(x = cw, y = ll)) +
  # geom_line(aes(linetype = "A",color = "A"), lwd = 0.5)+
  geom_line(color = "black",linetype = "solid", lwd = 0.5)+
  
  #a) Vertical segments
  # i) Estimate (without gridsearch) -> it is accurate, not problem of local optima)
  geom_segment(x = cMaxLL_NGS, xend = cMaxLL_NGS
               , y = min(estimatesPlotsDCM1$ll), yend = maxLL_NGS
               , color = "gray",linetype="dotted")+
  
  # ii) True value
  geom_segment(x = with(simulationParametersNumeric,cw), xend = with(simulationParametersNumeric,cw)
               , y = min(estimatesPlotsDCM1$ll), yend = estimatesPlotsDCM1[which(estimatesPlotsDCM1$cw == with(simulationParametersNumeric,cw)),]$ll
               , color = "gray",linetype="solid")+
  # iii) C = 1
  geom_segment(x = cMaxLL_C1, xend = cMaxLL_C1
               , y = min(estimatesPlotsDCM1$ll), yend = maxLL_C1
               , color = "gray",linetype="dashed")+
  
  #b) Horizontal segments
  # i) Estimate (without gridsearch)
  geom_segment(aes(x = min(estimatesPlotsDCM1$cw), xend = cMaxLL_NGS
                   , y = maxLL_NGS, yend = maxLL_NGS
                   , linetype = "B", color="C"))+
  # ii) True value
  geom_segment(aes(x = min(estimatesPlotsDCM1$cw), xend = with(simulationParametersNumeric,cw)
                   , y = estimatesPlotsDCM1[which(estimatesPlotsDCM1$cw == with(simulationParametersNumeric,cw)),]$ll, yend = estimatesPlotsDCM1[which(estimatesPlotsDCM1$cw == with(simulationParametersNumeric,cw)),]$ll
                   , linetype = "C", color="B"))+
  # iii) C = 1
  geom_segment(aes(x = min(estimatesPlotsDCM1$cw), xend = cMaxLL_C1
                   , y = maxLL_C1, yend = maxLL_C1
                   , linetype = "D", color="D"))+
  
  scale_linetype_manual(name = "", values = c(B= "dotted", C = "solid", D = "dashed")
                        # , labels = c(expression(bar(c)),expression(bar(c)==c),expression(bar(c)==hat(c)),expression(bar(c) == 1))
                        , labels = c(expression(bar(c)==hat(c)),expression(bar(c)==c),expression(bar(c) == 1))
  )+ #name = "Exponent value", 
  scale_color_manual(name = "", values=c(B="gray",C="gray",D="gray")
                     # , labels = c(expression(bar(c)),expression(bar(c)==c),expression(bar(c)==hat(c)),expression(bar(c) == 1))
                     , labels = c(expression(bar(c)==hat(c)),expression(bar(c)==c),expression(bar(c) == 1))
  )+
  scale_x_continuous(expand = c(0, 0)) +#, limits = c(min(estimatesPlotsDCM1LLC$cw),max(estimatesPlotsDCM1LLC$cw))) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1), 
                                           add = c(0, 0))
                     , breaks = seq(round(min(estimatesPlotsDCM1$ll),0), round(max(estimatesPlotsDCM1$ll),0),1)
                     , limits = c(min(estimatesPlotsDCM1$ll),max(estimatesPlotsDCM1$ll)+0.2)
  ) +
  labs(x = expression(bar(c)), y = "Log-likelihood")+
  themePlotsDCM1BPaper1

ggsave(str_c("export","/figures/Paper1/Simulations/GridSearch/DCM1B.pdf"), width = widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# #     - Grid search versus traditional estimation (table and plot) ------------------------------------------------------------
# 
# #To avoid local optima we do grid search to determine optimal c value
# gridSearchEstimatesByReplication <- split(gridSearchEstimates,gridSearchEstimates$replication)
# 
# #Bias C
# cMaxEstimates <- data.frame()
# 
# for(i in 1:length(gridSearchEstimatesByReplication)){
#   cMaxEstimates <- rbind(cMaxEstimates,data.frame(thetaC = gridSearchEstimatesByReplication[[i]][which(gridSearchEstimatesByReplication[[i]]$ll == max(gridSearchEstimatesByReplication[[i]]$ll)),]$cw))
# }
# 
# cMaxEstimates
# 
# 
# mean(abs(cMaxEstimates$thetaC-simulationParametersNumeric$cw)) #Bias
# 
# mean(cMaxEstimates$thetaC); sd(cMaxEstimates$thetaC)
# hist(cMaxEstimates$thetaC)
# 
# #Bias MRS
# ratioWVForCMaxEstimates <- data.frame()
# 
# for(i in 1:length(gridSearchEstimatesByReplication)){
#   ratioWVForCMaxEstimates <- rbind(ratioWVForCMaxEstimates,data.frame(ratioWV = gridSearchEstimatesByReplication[[i]][which(gridSearchEstimatesByReplication[[i]]$ll == max(gridSearchEstimatesByReplication[[i]]$ll)),]$ratioWV))
# }
# 
# mean(abs(ratioWVForCMaxEstimates$ratioWV- with(simulationParametersNumeric,bw/bv))) #Bias
# 
# mean(ratioWVForCMaxEstimates$ratioWV); sd(ratioWVForCMaxEstimates$ratioWV)
# hist(ratioWVForCMaxEstimates$ratioWV)
# 
# 
# #Combined plot
# 
# fittedRatioWV <- data.frame(estimate = seq(0,2,0.001)
#                             , density = dnorm(x = seq(0,2,0.001),mean = mean(ratioWVForCMaxEstimates$ratioWV), sd = sd(ratioWVForCMaxEstimates$ratioWV)))
# 
# fittedC <- data.frame(estimate = seq(0,2,0.001)
#                       , density = dnorm(x = seq(0,2,0.001),mean = mean(cMaxEstimates$thetaC), sd = sd(cMaxEstimates$thetaC)))
# 
# 
# estimatesPlotsDCM1MRSC <- rbind(data.frame(estimate = fittedRatioWV$estimate, density = fittedRatioWV$density, type = "MRS")
#                               ,data.frame(estimate = fittedC$estimate, density = fittedC$density, type = "C")
# )
# 
# ggplot(estimatesPlotsDCM1MRSC, aes(x = estimate, y = density, color = type)) +
#   scale_color_manual(values=c("black", "lightgray"), labels = c(expression(hat(theta)[w]/hat(theta)[v]),expression(c)))+
#   geom_line(aes(group = type), lwd = 0.5)+
#   geom_vline(xintercept = with(simulationParametersNumeric,bw/bv),color = "black", linetype="dashed")+
#   geom_vline(xintercept = simulationParametersNumeric$cw,color = "lightgray", linetype="dashed")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(x = expression(c), y = "Density",colour = "Parameter")+
#   themePlotsDCMParametersPaper1
# 
# ggsave(str_c("export","/figures/Paper1/DCMAnalyses/GridSearch/DCM1C.pdf"), width = 1.1*widthSimulationPlots, height = heightSimulationPlots , unit="cm", dpi=dpiSimulationPlots)

# #     DCM3) Scale Adjustment (SP-RP model) -------------------------------------
# #     - Numeric (scale adjustment) -------------------------------------------------------
# 
# DCMData0 <- subset(DCMDataRaw,experimentType == "numeric" & experimentalCondition == "treatment")
# # DCMData0 <- subset(DCMDataRaw,experimentType == "numeric")
# # DCMData0 <- subset(DCMData0,scenario %in% c("03","04"))
# # DCMData0 <- subset(DCMData0,scenario %in% c("01","02","03","04"))
# 
# scaleFactor <- -1.948739/-1.558563; scaleFactor
# 
# DCMData0$w1[DCMData0$experimentalCondition == "treatment"] <- scaleFactor*DCMData0$w1[DCMData0$experimentalCondition == "treatment"]
# DCMData0$w2[DCMData0$experimentalCondition == "treatment"] <- scaleFactor*DCMData0$w2[DCMData0$experimentalCondition == "treatment"]
# DCMData0$v1[DCMData0$experimentalCondition == "treatment"] <- scaleFactor*DCMData0$v1[DCMData0$experimentalCondition == "treatment"]
# DCMData0$v2[DCMData0$experimentalCondition == "treatment"] <- scaleFactor*DCMData0$v2[DCMData0$experimentalCondition == "treatment"]
# 
# # DCMData0 <- subset(simulatedData,experimentType == "experienced" & experimentalCondition == "control" & city == "Santiago")
# DCMData0$choice <- DCMData0$realChoice
# 
# # DCMData0 <- subset(DCMData0,experimentType == "numeric" & experimentalCondition == "control")
# # DCMData0 <- subset(DCMData0,experimentType == "numeric" & experimentalCondition == "treatment")
# 
# #bw = -1.558563
# #bv = -1.285038
# 
# logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL ,constant = NULL
#                       , thetaPW = NULL,thetaPV = NULL, thetaAWT = NULL, thetaAV = NULL, thetaAW = NULL, thetaWeight = NULL)#, thetaWeight = 1)#, thetaWeight = 1
# 
# # logitParameters0 <- c(thetaW = 1, thetaV = 2, thetaT= NULL, thetaDW = NULL, thetaDV = NULL,constant = NULL, thetaPW = cw,thetaPV = cv, thetaWeight = NULL)
# 
# simulationParameters <- c(bw = 0, bv = 0, aw = 1, av = 1, cw = 1, cv =  1, scale_gumbel = 1
#                           , betaWeight =1,awt = 0, sw = 0, sv= 0) #Si no se sabe parámetro de escala, cambian mucho los resultados de las estimaciones
# 
# results3 <- LogitTimePerception(DCMData = DCMData0, logitParameters = logitParameters0, method = "BHHH"
#                                 , simulationParameters = simulationParameters
#                                 , sameCurvature = FALSE, sameTimeWeighting = FALSE)
# results3
# 
# #     - Animated (scale adjustment) ---------------------------------------
# 
# #We know that the scale parameter is the same, looking at the results of the time perception test for in-vehicle time, which resulted to be the same in both conditions
# #The difference may be purely attributed to the estimation procedure or difference in scale factor between conditions, that affect the preference parameter. 
# #In fact, interestingly, the participants in the treatment condition has a bigger scale factor too. This may be also evidence that preference are playing a role here. 
# #Any difference that still remains may be atrributed to the experiment design, such that, there are less variance in the treatment condition
# 
# -0.7748735/-0.5279484
# 
# 
# 
# 
# #We add an interaction to measure the impact of revealing waiting time. 
# 
# 
# 
# 
# 
# 
