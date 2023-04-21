# 1) SETUP  ------------------------------------------------------------------
# Clean -------------------------------------------------------------------
closeAllConnections() #Remove all the data
rm(list=ls()) #remove all variables
# a) Paths -------------------------------------------------------------------

#Working Directory
dir <- getwd()

setwd(dir)

#Root of database file
database <-  paste(dir,"/data/", sep = "") 

#Name of the project
project <- basename(dir)

# b) Libraries --------------------------------------------------------------------
# source(paste(dir,"/scripts/LibrariesCleaning.R", sep = ""), echo=TRUE) # Execute only to ensure replicability
source(paste(dir,"/scripts/setup-libraries.R", sep = ""), echo=TRUE) 

# c) Functions ---------------------------------------------------------------
#Execute Functions (Podr?a mantener las funciones que hay y agregar otras nuevas)
source(str_c(dir,"/scripts/functions.R"), echo=TRUE)

# 2) DATA READING AND WRITING ------------------------------------------------------------
# - Directories creation 
source(str_c(dir,"/scripts/create-directories.R"), echo=TRUE)

# e) Backing Up and Reading Data ---------------------------------------------

data.participantsChile <- read.csv(paste(database,"participantsChile.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.googleFormsParticipantsChile <- read.csv(paste(database,"googleFormsParticipantsChile.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.participantsUK <- read.csv(paste(database,"participantsUK.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.experiencedLearningResponses <- read.csv(paste(database,"experiencedLearningResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.experiencedChoiceResponses <- read.csv(paste(database,"experiencedChoiceResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.descriptiveLearningResponses <- read.csv(paste(database,"descriptiveLearningResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.descriptiveChoiceResponses <- read.csv(paste(database,"descriptiveChoiceResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.experimentalScenarios <- read.csv(paste(database,"experimentalScenarios.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.experimentsReactionTimeResponses <- read.csv(paste(database,"experimentsReactionTimeResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.travelBehaviourResponses<- read.csv(paste(database,"travelBehaviourResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.experimentDebriefResponses <- read.csv(paste(database,"experimentDebriefResponses.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")


# 3) EXECUTION -----------------------------------------------------------------------

#Copying data
participantsChile <- data.participantsChile
participantsUK <- data.participantsUK
googleFormsParticipantsChile <- data.googleFormsParticipantsChile
experiencedLearningResponses <- data.experiencedLearningResponses
experiencedChoiceResponses <- data.experiencedChoiceResponses
descriptiveLearningResponses <- data.descriptiveLearningResponses
descriptiveChoiceResponses <- data.descriptiveChoiceResponses
experimentalScenarios <- data.experimentalScenarios
experimentsReactionTimeResponses <- data.experimentsReactionTimeResponses
travelBehaviourResponses <- data.travelBehaviourResponses
experimentDebriefResponses <- data.experimentDebriefResponses

# a) Processing Data -------------------------------------------------

source(paste(dir,"/scripts/coding.R",sep=''), echo=TRUE,encoding = "ISO-8859-1")

source(paste(dir,"/scripts/feature-engineering.R",sep=''), echo=TRUE,encoding = "ISO-8859-1") 

# 4) Descriptive Statistics -----------------------------------------------------------------------

#Plots and functions
source(paste(dir,"/scripts/descriptive-statistics.R",sep=''), echo=TRUE,encoding = "ISO-8859-1")

# 5) Estimation ---------------------------------------------------------------

# An organized summary of the analyses conducted in the paper are available in the following scripts

# - Mixed effect logistic regression to compare choice proportions across experimental conditions
source(paste(dir,"/scripts/regression-analysis.R",sep=''), echo=TRUE,encoding = "ISO-8859-1")

# - Estimation of discrete choice models with synthetic data
source(paste(dir,"/scripts/simulations.R",sep=''), echo=TRUE,encoding = "ISO-8859-1")

# - Estimation of discrete choice models with participants choice data 
source(paste(dir,"/scripts/model-estimation.R",sep=''), echo=TRUE,encoding = "ISO-8859-1")