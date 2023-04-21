
# 1) Participants Information ------------------------------------------------

# a) Data from Google Forms --------------------------------------------------


# - Participant Id, Name and Last Name ------------------------------------------------------------------

googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(NAME="firstName"))
googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(LAST_NAME="lastName"))

googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(PARTICIPANT_ID="participantId"))
googleFormsParticipantsChile$participantId <- participant.id(googleFormsParticipantsChile$participantId)

# - Public Transport Use --------------------------------------------------

googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(FRECUENCIA_DE_USO_DE_TRANSPORTE_PUBLICO="publicTransportFrequency"))
googleFormsParticipantsChile$publicTransportFrequency <-as.character(googleFormsParticipantsChile$publicTransportFrequency)

googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(PUBLIC_TRANSPORT_USE="publicTransportUse")) #1:1-2 days per week, 2: 3-4 days per week, 3: More than 5 days per week
googleFormsParticipantsChile$publicTransportUse <-as.numeric(googleFormsParticipantsChile$publicTransportUse)

# - Education ----------------------------------------------------------

#General Study subject
googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(SUBJECT="eduSubject"))

#Speficif subject(in Spanish)
googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(CARRERA_Y_ESPECIALIDAD="specificEduSubject"))

#level of education
googleFormsParticipantsChile <- plyr::rename(googleFormsParticipantsChile,c(LEVEL_OF_STUDY="eduLevel"))


# b) Data Chilean Participants (Python) -----------------------------------------------------

# - Participant Id,Nickname ----------------------------------------------------------
participantsChile <- plyr::rename(participantsChile,c(PARTICIPANTID="participantId"))
participantsChile$participantId <- participant.id(participantsChile$participantId)
#The participant id is later used to merge data of Chilean participants with that obtained from the google form 

# - Demographic Data Participants (answers need to be translated to English) -------------------------------------------
# participantsChile <- plyr::rename(participantsChile,c(STARTTIME ="startingTimeExp"))
# participantsChile <- plyr::rename(participantsChile,c(ENDINGTIME ="endingTimeExperiment"))

#Gender 
participantsChile <- plyr::rename(participantsChile,c(GENDER ="gender"))
participantsChile$gender <- removeFirstBlankSpaceString(participantsChile$gender)
participantsChile$gender[participantsChile$gender == "Prefieronodecir"] <- "other"
participantsChile$gender[participantsChile$gender == "Femenino"] <- "female"
participantsChile$gender[participantsChile$gender == "Masculino"] <- "male"

#Age
participantsChile <- plyr::rename(participantsChile,c(AGE ="age"))
participantsChile$age <- as.numeric(participantsChile$age)

#Education level
participantsChile <- plyr::rename(participantsChile,c(EDUCATIONLEVEL ="eduLevel"))
participantsChile$eduLevel <- removeFirstBlankSpaceString(participantsChile$eduLevel)
participantsChile$eduLevel[participantsChile$eduLevel == "Pregrado"] <- "undergraduate"
participantsChile$eduLevel[participantsChile$eduLevel == "Posgrado"] <- "posgraduate"
participantsChile$eduLevel <- as.character(participantsChile$eduLevel)

# - Country, City of Residence  ----------------------------------------------------------------
participantsChile <- plyr::rename(participantsChile,c(COUNTRYOFRESIDENCE ="country"))
participantsChile$country <- removeFirstBlankSpaceString(participantsChile$country)
participantsChile$city <- "Santiago"
# - Experimental Condition --------------------------------------------------

participantsChile <- plyr::rename(participantsChile,c(EXPERIENCEDCONDITION="experiencedExperimentalCondition"))
participantsChile$experiencedExperimentalCondition = removeFirstBlankSpaceString(participantsChile$experiencedExperimentalCondition)
participantsChile$experiencedExperimentalCondition = as.character(participantsChile$experiencedExperimentalCondition)
participantsChile$experiencedExperimentalCondition[participantsChile$experiencedExperimentalCondition == "simulatedControlCondition"] <- "control"
participantsChile$experiencedExperimentalCondition[participantsChile$experiencedExperimentalCondition == "simulatedTreatmentCondition1"] <- "treatment"

participantsChile <- plyr::rename(participantsChile,c(DESCRIPTIVECONDITION="descriptiveExperimentalCondition"))
participantsChile$descriptiveExperimentalCondition = removeFirstBlankSpaceString(participantsChile$descriptiveExperimentalCondition)
participantsChile$descriptiveExperimentalCondition = as.character(participantsChile$descriptiveExperimentalCondition)
participantsChile$descriptiveExperimentalCondition[participantsChile$descriptiveExperimentalCondition == "descriptiveControl"] <- "control"
participantsChile$descriptiveExperimentalCondition[participantsChile$descriptiveExperimentalCondition == "descriptiveTreatmentCondition1"] <- "treatment"
# - Duration, date, starting and ending Time of the Experiment------------------------------------

participantsChile <- plyr::rename(participantsChile,c(STARTDATE ="date"))
participantsChile <- plyr::rename(participantsChile,c(STARTTIME ="startingTimeExp"))
participantsChile <- plyr::rename(participantsChile,c(ENDINGTIME ="endingTimeExperiment"))


participantsChile$date <- as.character(participantsChile$date)
participantsChile$date <- complete.date(participantsChile$date)

participantsChile$startingTimeExp <- as.character(participantsChile$startingTimeExp)
participantsChile$startingTimeExp <- complete.time(participantsChile$startingTimeExp)
participantsChile$startingTimeExp <- paste(participantsChile$date,participantsChile$startingTimeExp,sep='')
participantsChile$startingTimeExp <- as.POSIXlt(participantsChile$startingTimeExp,format="%d%m%Y%H%M%S")

participantsChile$endingTimeExp <- as.character(participantsChile$endingTimeExp)
participantsChile$endingTimeExp <- complete.time(participantsChile$endingTimeExp)
participantsChile$endingTimeExp <- paste(participantsChile$date,participantsChile$endingTimeExp,sep='')
participantsChile$endingTimeExp <- as.POSIXlt(participantsChile$endingTimeExp,format="%d%m%Y%H%M%S")

participantsChile$date = as.Date(participantsChile$date,format = "%d%m%Y") #This add hyphens between the date parameters

participantsChile$sessionDuration = round(with(participantsChile,endingTimeExp-startingTimeExp),2)


# - Computer Id -------------------------------------------------------------
participantsChile <- plyr::rename(participantsChile,c(COMPUTERID="computerId"))

# - Transportation Network Design -------------------------------------------
participantsChile$networkHeight = 40
participantsChile$networkWidth = 100

# - Bus Stops ------------------------------------------------

#The dwell times at bus stops was set in 1 seconds.
participantsChile$dwellTimeBusStop = 1

#Since a trip includes two bus stops
participantsChile$dwellTime = participantsChile$dwellTimeBusStop * 2

#Coordinate of bus stops. The origin is in the south west corner

#South West Bus Stop
participantsChile$busStop1X = participantsChile$networkWidth*1/10
participantsChile$busStop1Y = 0

#South East Bus Stop
participantsChile$busStop2X = participantsChile$networkWidth*(1-1/10)
participantsChile$busStop2Y = 0

#North East Bus Stop
participantsChile$busStop3X = participantsChile$networkWidth
participantsChile$busStop3Y = participantsChile$networkHeight

#North West Bus Stop
participantsChile$busStop4X = participantsChile$networkWidth*1/10
participantsChile$busStop4Y = participantsChile$networkHeight

# - Origin and Destination Locations ----------------------------------------

participantsChile$westLocationX = 0
participantsChile$westLocationY = participantsChile$networkHeight/2
participantsChile$eastLocationX = participantsChile$networkWidth
participantsChile$eastLocationY = participantsChile$networkHeight/2

# - Walking Time ------------------------------------------------------------

#The walking speed was set in 14 unit of distance per time (the double of the initial speed of 7)
participantsChile$walkingSpeed = 7 * 2

#You can choose bus stop 1 or 4 for the calculations. We asume this trip is from the west to east locations using the south section of the network
participantsChile$walkingDistanceOriginToFirstStop = with(participantsChile,abs(westLocationY-busStop1Y)+abs(westLocationX-busStop1X)) 
participantsChile$walkingDistanceLastStopToDestination = with(participantsChile,abs(busStop2Y-eastLocationY)+abs(busStop2X-eastLocationX))

participantsChile$walkingTimeOriginToFirstStop = round(with(participantsChile,walkingDistanceOriginToFirstStop/walkingSpeed),2)
participantsChile$walkingTimeLastStopToDestination = round(with(participantsChile,walkingDistanceLastStopToDestination/walkingSpeed),2)

participantsChile$walkingTime = round(with(participantsChile,walkingTimeOriginToFirstStop+walkingTimeLastStopToDestination),2)

# c) Merge participant dataset with data obtained from Google Form ---------------------------------------------
participantsChile = merge(participantsChile,googleFormsParticipantsChile[c("eduSubject","participantId","publicTransportUse")],by="participantId")

# d) Data British Participants (Python) ---------------------------------------
# - Participant Id,Nickname. name ----------------------------------------------------------
participantsUK <- plyr::rename(participantsUK,c(PARTICIPANTID="participantId"))
participantsUK$participantId <- participant.id(participantsUK$participantId)
# #The participant id is later used to merge data of British participants with that obtained from the prescreen information at SONA.
# participantsUK <- plyr::rename(participantsUK,c(PARTICIPANTNAME="firstName"))
# participantsUK <- plyr::rename(participantsUK,c(PARTICIPANTLASTNAME="lastName"))



# - Demographic Data Participants (answers nare already in English) -------------------------------------------

#Gender 
participantsUK <- plyr::rename(participantsUK,c(GENDER ="gender"))
participantsUK$gender <- removeFirstBlankSpaceString(participantsUK$gender)
participantsUK$gender[participantsUK$gender == "I prefer not to say"] <- "other"
participantsUK$gender[participantsUK$gender == "Female"] <- "female"
participantsUK$gender[participantsUK$gender == "Male"] <- "male"

#Age
participantsUK <- plyr::rename(participantsUK,c(AGE ="age"))
participantsUK$age <- as.numeric(participantsUK$age)

#Education level
participantsUK <- plyr::rename(participantsUK,c(EDUCATIONLEVEL ="eduLevel"))
participantsUK$eduLevel <- removeFirstBlankSpaceString(participantsUK$eduLevel)
participantsUK$eduLevel[participantsUK$eduLevel == "Undergraduate"] <- "undergraduate"
participantsUK$eduLevel[participantsUK$eduLevel == "Graduate"] <- "posgraduate"
participantsUK$eduLevel <- as.character(participantsUK$eduLevel)

# - Country, City of Residence  ----------------------------------------------------------------
participantsUK <- plyr::rename(participantsUK,c(COUNTRYOFRESIDENCE ="country"))
participantsUK$city <- "London"
# - Experimental Condition --------------------------------------------------

participantsUK <- plyr::rename(participantsUK,c(EXPERIENCEDCONDITION="experiencedExperimentalCondition"))
participantsUK$experiencedExperimentalCondition = removeFirstBlankSpaceString(participantsUK$experiencedExperimentalCondition)
participantsUK$experiencedExperimentalCondition = as.character(participantsUK$experiencedExperimentalCondition)
participantsUK$experiencedExperimentalCondition[participantsUK$experiencedExperimentalCondition == "simulatedControlCondition"] <- "control"
participantsUK$experiencedExperimentalCondition[participantsUK$experiencedExperimentalCondition == "simulatedTreatmentCondition1"] <- "treatment"

participantsUK <- plyr::rename(participantsUK,c(DESCRIPTIVECONDITION="descriptiveExperimentalCondition"))
participantsUK$descriptiveExperimentalCondition = removeFirstBlankSpaceString(participantsUK$descriptiveExperimentalCondition)
participantsUK$descriptiveExperimentalCondition = as.character(participantsUK$descriptiveExperimentalCondition)
participantsUK$descriptiveExperimentalCondition[participantsUK$descriptiveExperimentalCondition == "descriptiveControl"] <- "control"
participantsUK$descriptiveExperimentalCondition[participantsUK$descriptiveExperimentalCondition == "descriptiveTreatmentCondition1"] <- "treatment"
# - Duration, date, starting and ending Time of the Experiment------------------------------------

participantsUK <- plyr::rename(participantsUK,c(STARTDATE ="date"))
participantsUK <- plyr::rename(participantsUK,c(STARTTIME ="startingTimeExp"))
participantsUK <- plyr::rename(participantsUK,c(ENDINGTIME ="endingTimeExperiment"))

participantsUK$date <- as.character(participantsUK$date)
participantsUK$date <- complete.date(participantsUK$date)

participantsUK$startingTimeExp <- as.character(participantsUK$startingTimeExp)
participantsUK$startingTimeExp <- complete.time(participantsUK$startingTimeExp)
participantsUK$startingTimeExp <- paste(participantsUK$date,participantsUK$startingTimeExp,sep='')
participantsUK$startingTimeExp <- as.POSIXlt(participantsUK$startingTimeExp,format="%d%m%Y%H%M%S")

participantsUK$endingTimeExp <- as.character(participantsUK$endingTimeExp)
participantsUK$endingTimeExp <- complete.time(participantsUK$endingTimeExp)
participantsUK$endingTimeExp <- paste(participantsUK$date,participantsUK$endingTimeExp,sep='')
participantsUK$endingTimeExp <- as.POSIXlt(participantsUK$endingTimeExp,format="%d%m%Y%H%M%S")

participantsUK$date = as.Date(participantsUK$date,format = "%d%m%Y") #This add hyphens between the date parameters

participantsUK$sessionDuration = round(with(participantsUK,endingTimeExp-startingTimeExp),2)



# - Computer Id -------------------------------------------------------------
participantsUK <- plyr::rename(participantsUK,c(COMPUTERID="computerId"))

# - Transportation Network Design -------------------------------------------
participantsUK$networkHeight = 40
participantsUK$networkWidth = 100

# - Bus Stops ------------------------------------------------

#The dwell times at bus stops was set in 1 seconds.
participantsUK$dwellTimeBusStop = 1

#Since a trip includes two bus stops
participantsUK$dwellTime = participantsUK$dwellTimeBusStop * 2

#Coordinate of bus stops. The origin is in the south west corner

#South West Bus Stop
participantsUK$busStop1X = participantsUK$networkWidth*1/10
participantsUK$busStop1Y = 0

#South East Bus Stop
participantsUK$busStop2X = participantsUK$networkWidth*(1-1/10)
participantsUK$busStop2Y = 0

#North East Bus Stop
participantsUK$busStop3X = participantsUK$networkWidth
participantsUK$busStop3Y = participantsUK$networkHeight

#North West Bus Stop
participantsUK$busStop4X = participantsUK$networkWidth*1/10
participantsUK$busStop4Y = participantsUK$networkHeight

# - Origin and Destination Locations ----------------------------------------

participantsUK$westLocationX = 0
participantsUK$westLocationY = participantsUK$networkHeight/2
participantsUK$eastLocationX = participantsUK$networkWidth
participantsUK$eastLocationY = participantsUK$networkHeight/2

# - Walking Time ------------------------------------------------------------

#The walking speed was set in 14 unit of distance per time (the double of the initial speed of 7)
participantsUK$walkingSpeed = 7 * 2

#You can choose bus stop 1 or 4 for the calculations. We asume this trip is from the west to east locations using the south section of the network
participantsUK$walkingDistanceOriginToFirstStop = with(participantsUK,abs(westLocationY-busStop1Y)+abs(westLocationX-busStop1X)) 
participantsUK$walkingDistanceLastStopToDestination = with(participantsUK,abs(busStop2Y-eastLocationY)+abs(busStop2X-eastLocationX))

participantsUK$walkingTimeOriginToFirstStop = round(with(participantsUK,walkingDistanceOriginToFirstStop/walkingSpeed),2)
participantsUK$walkingTimeLastStopToDestination = round(with(participantsUK,walkingDistanceLastStopToDestination/walkingSpeed),2)

participantsUK$walkingTime = round(with(participantsUK,walkingTimeOriginToFirstStop+walkingTimeLastStopToDestination),2)

# e) Combining Data from British and Chilean Participants --------
# - Merge Datasets --------------------------------------------------------

#List of common variables between the two datasets (UK and Chile) with participants' information 

commonParticipantVars <- c("participantId","date","startingTimeExp","computerId","experiencedExperimentalCondition","descriptiveExperimentalCondition"
                           ,"age","gender","eduLevel","country", "endingTimeExp","sessionDuration","walkingTime","dwellTime")

#"publicTransportUse" and  "eduSubject" are only available for Chilean Participants

# #In the meanwhile, we only have data from chilean participants.
# participants = rbind(participantsChile,participantsUK[colnames(participantsChile),], fill=TRUE)
# 
# #I am temporally removing the data for British participants 
# participants = subset(participants,participants$country == "Chile")

# - Subset of Participants (UK or Chile) ------------------------------------
participants = rbind(participantsChile[,commonParticipantVars],participantsUK[,commonParticipantVars])

#Only participants from London
# participants = subset(participants, country == "UK")
# participants = subset(participants, country == "Chile")

# - Outliers --------------------------------------------------------------

#The total participant registered in the UK were 39. We have 20 control and 19 treatment. 
participants = subset(participants, participantId != "P42") #Gareth Humes (39 years old) -> Control Condition)
participants = subset(participants, participantId != "P75") #Declan Clear (Staff at Birbeck) -> Control Condition
participants = subset(participants, participantId != "P50") #Anthony de Rocha -> Treatment Condition 
#If we removed two control (people who are not students) and Anthony de Rocha who were not a current student 
#we have a balanced of 18 control and 18 treatment

# # participants = subset(participants, participantId != "P38") #Priscilla -> Treatment Condition
# participants = subset(participants, participantId != "P73") #Mohammad Shan (Answered the experiment in 20 minutes)->Control Condition


# - Experiment Duration ---------------------------------------------------
participants$startingTimeExp <- as.character(participants$startingTimeExp)
participants$endingTimeExp <- as.character(participants$endingTimeExp)

# - Experimental Condition --------------------------------------------------

participants$experimentalCondition <- NA
participants$experimentalCondition[with(participants,experiencedExperimentalCondition == "control" & descriptiveExperimentalCondition == "control")] <- "control"
participants$experimentalCondition[with(participants,experiencedExperimentalCondition == "treatment" & descriptiveExperimentalCondition == "treatment")] <- "treatment"

# - City ------------------------------------------------------------------
participants$city <- NA
participants$city[participants$country == "UK"] <- "London"
participants$city[participants$country == "Chile"] <- "Santiago"

# 2) Experimental Scenarios -----------------------------------------------


# - Experiment Block --------------------------------------------------------

experimentalScenarios <- plyr::rename(experimentalScenarios,c(EXPERIMENT="experimentalBlock"))
experimentalScenarios$experimentalBlock = experimentalBlock.id(experimentalScenarios$experimentalBlock)

# - Scenario ----------------------------------------------------------------

experimentalScenarios <- plyr::rename(experimentalScenarios,c(ABSDP="scenario"))
experimentalScenarios$scenario <- scenario.id(experimentalScenarios$scenario)

#Order of the scenarios relative to the experimental Block

experimentalScenarios <- plyr::rename(experimentalScenarios,c(RELDP="relativeOrderScenario"))
experimentalScenarios$relativeOrderScenario <- scenario.id(experimentalScenarios$relativeOrderScenario)


# - Names of Route Time Parameters----------------------------------------------

#The "control route" will be called R1 and the "treatment Route" R2

experimentalScenarios <- plyr::rename(experimentalScenarios,c(WAITING_C="waitingTimeR1"))
experimentalScenarios$waitingTimeR1 <- removeFirstBlankSpaceString(experimentalScenarios$waitingTimeR1)

experimentalScenarios <- plyr::rename(experimentalScenarios,c(TRAVEL_C="travelTimeR1"))
experimentalScenarios$travelTimeR1 <- removeFirstBlankSpaceString(experimentalScenarios$travelTimeR1)

experimentalScenarios <- plyr::rename(experimentalScenarios,c(JOURNEY_C="journeyTimeR1"))
experimentalScenarios$journeyTimeR1 <- removeFirstBlankSpaceString(experimentalScenarios$journeyTimeR1)

experimentalScenarios <- plyr::rename(experimentalScenarios,c(WAITING_T="waitingTimeR2"))
experimentalScenarios$waitingTimeR2 <- removeFirstBlankSpaceString(experimentalScenarios$waitingTimeR2)

experimentalScenarios <- plyr::rename(experimentalScenarios,c(TRAVEL_T="travelTimeR2"))
experimentalScenarios$travelTimeR2 <- removeFirstBlankSpaceString(experimentalScenarios$travelTimeR2)

experimentalScenarios <- plyr::rename(experimentalScenarios,c(JOURNEY_T="journeyTimeR2"))
experimentalScenarios$journeyTimeR2 <- removeFirstBlankSpaceString(experimentalScenarios$journeyTimeR2)

# View(experimentalScenarios)

# - Irrational/Rational Route -----------------------------------------------
experimentalScenarios$rationalRoute <- NA

experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "01"] <- "R1" #Dominating alternative
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "02"] <- "R1" #Dominating alternative
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "03"] <- "R1" #Lower proportion of waiting time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "04"] <- "R1" #Lower proportion of waiting time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "05"] <- "R1" #Longer Journey Time But Higher Expected Utility
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "06"] <- "R1" #Longer Journey Time But Higher Expected Utility
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "07"] <- "R2" #Longer Journey Time But Higher Expected Utility
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "08"] <- "R2" #Longer Journey Time But Higher Expected Utility
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "09"] <- "R2" #No variability, same average journey time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "10"] <- "R2" #No variability, same average journey time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "11"] <- "R2" #Prefer Variability in Travel Time than Waiting Time, , same average journey time and variability
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "12"] <- "R2" #No variability, same average journey time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "13"] <- "R2" #No variability, same average journey time
experimentalScenarios$rationalRoute[experimentalScenarios$scenario == "14"] <- "R2" #Prefer Variability in Travel Time than Waiting Time, , same average journey time and variability

experimentalScenarios$irrationalRoute <- NA
experimentalScenarios$irrationalRoute[experimentalScenarios$rationalRoute=="R1"] <- "R2"
experimentalScenarios$irrationalRoute[experimentalScenarios$rationalRoute=="R2"] <- "R1"

# - Long Format --------------------------------------------------------------

experimentalScenariosR1 <- experimentalScenarios[c("experimentalBlock","relativeOrderScenario","scenario","waitingTimeR1","travelTimeR1","journeyTimeR1","irrationalRoute","rationalRoute")]
experimentalScenariosR1 <- plyr::rename(experimentalScenariosR1,c(waitingTimeR1="waitingTime"))
experimentalScenariosR1 <- plyr::rename(experimentalScenariosR1,c(travelTimeR1="travelTime"))
experimentalScenariosR1 <- plyr::rename(experimentalScenariosR1,c(journeyTimeR1="journeyTime"))
experimentalScenariosR1$route = "R1"
experimentalScenariosR1$routeType <- "control"
experimentalScenariosR1 <- unique(experimentalScenariosR1)

experimentalScenariosR2 <- experimentalScenarios[c("experimentalBlock","relativeOrderScenario","scenario","waitingTimeR2","travelTimeR2","journeyTimeR2","irrationalRoute","rationalRoute")]
experimentalScenariosR2 <- plyr::rename(experimentalScenariosR2,c(waitingTimeR2="waitingTime"))
experimentalScenariosR2 <- plyr::rename(experimentalScenariosR2,c(travelTimeR2="travelTime"))
experimentalScenariosR2 <- plyr::rename(experimentalScenariosR2,c(journeyTimeR2="journeyTime"))
experimentalScenariosR2$route = "R2"
experimentalScenariosR2$routeType <- "treatment"
experimentalScenariosR2 <- unique(experimentalScenariosR2)

#Append datasets from both types of routes
experimentalScenarios = rbind(experimentalScenariosR1,experimentalScenariosR2)

experimentalScenarios$alternative <- NA
experimentalScenarios$alternative[with(experimentalScenarios,route == rationalRoute)] <- "rational"
experimentalScenarios$alternative[with(experimentalScenarios,route == irrationalRoute)] <- "irrational"


# - Waiting and Travel Times ------------------------------------------------

#EXPERIMENTAL BLOCKS 1 and 2
#Waiting Time
experimentalScenarios$averageWaitingTime <- NA
experimentalScenarios$averageWaitingTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")] <- experimentalScenarios$waitingTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")]
experimentalScenarios$averageWaitingTime <- as.numeric(experimentalScenarios$averageWaitingTime)
#Travel Time
experimentalScenarios$averageTravelTime <- NA
experimentalScenarios$averageTravelTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")] <- experimentalScenarios$travelTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")]
experimentalScenarios$averageTravelTime <- as.numeric(experimentalScenarios$averageTravelTime)
#Journey Time
experimentalScenarios$averageJourneyTime <- with(experimentalScenarios,averageWaitingTime+averageTravelTime)

#EXPERIMENTAL BLOCK 3
#We need to update waiting and travel times for the scenarios shown in the third experimental block

experimentalScenarios$averageWaitingTime[with(experimentalScenarios,experimentalBlock=="03")] <- 4
experimentalScenarios$averageWaitingTime <- as.numeric(experimentalScenarios$averageWaitingTime)

experimentalScenarios$averageTravelTime[with(experimentalScenarios, experimentalBlock=="03")] <- 6
experimentalScenarios$averageTravelTime <- as.numeric(experimentalScenarios$averageTravelTime) 

experimentalScenarios$averageJourneyTime[with(experimentalScenarios, experimentalBlock=="03")] <- 10
experimentalScenarios$averageJourneyTime <- as.numeric(experimentalScenarios$averageJourneyTime)


# - Time Variability ------------------------------------------------------

#EXPERIMENTAL BLOCKS 1 and 2 do not have time variability
experimentalScenarios$variabilityWaitingTime <- NA
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")] <- 0

experimentalScenarios$variabilityTravelTime <- NA
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")] <- 0

experimentalScenarios$variabilityJourneyTime <- NA
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,experimentalBlock=="01"|experimentalBlock=="02")] <- 0

#Variability only apploes in the scenarios of the third experimental block as follows

#SCENARIO 9

#Route1

experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "09" & route == "R1")] <- 2 #Low variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "09" & route == "R1")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "09" & route == "R1")] <- 2 #Same variability than waiting Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "09" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "09" & route == "R2")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "09" & route == "R2")] <- 0 #No variability

#SCENARIO 10
#Route1
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "10" & route == "R1")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "10" & route == "R1")] <- 2 #Low variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "10" & route == "R1")] <- 2 #Same variability than travel Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "10" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "10" & route == "R2")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "10" & route == "R2")] <- 0 #No variability

#SCENARIO 11
#Route1
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "11" & route == "R1")] <- 2 #Low variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "11" & route == "R1")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "11" & route == "R1")] <- 2 #Same variability than travel Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "11" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "11" & route == "R2")] <- 2 #Low variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "11" & route == "R2")] <- 2 #Same variability than travel Time

#SCENARIO 12

#Route1

experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "12" & route == "R1")] <- 4 #High variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "12" & route == "R1")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "12" & route == "R1")] <- 4 #Same variability than waiting Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "12" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "12" & route == "R2")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "12" & route == "R2")] <- 0 #No variability

#SCENARIO 13
#Route1
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "13" & route == "R1")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "13" & route == "R1")] <- 4 #High variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "13" & route == "R1")] <- 4 #Same variability than travel Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "13" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "13" & route == "R2")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "13" & route == "R2")] <- 0 #No variability

#SCENARIO 14
#Route1
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "14" & route == "R1")] <- 4 #High variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "14" & route == "R1")] <- 0 #No variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "14" & route == "R1")] <- 4 #Same variability than travel Time
#Route2 (no variability)
experimentalScenarios$variabilityWaitingTime[with(experimentalScenarios,scenario == "14" & route == "R2")] <- 0 #No variability waiting time
experimentalScenarios$variabilityTravelTime[with(experimentalScenarios,scenario == "14" & route == "R2")] <- 4 #High variability travel time
experimentalScenarios$variabilityJourneyTime[with(experimentalScenarios,scenario == "14" & route == "R2")] <- 4 #Same variability than travel Time


#Format to numeric
experimentalScenarios$variabilityWaitingTime <- as.numeric(experimentalScenarios$variabilityWaitingTime)
experimentalScenarios$variabilityTravelTime <- as.numeric(experimentalScenarios$variabilityTravelTime)
experimentalScenarios$variabilityJourneyTime <- as.numeric(experimentalScenarios$variabilityJourneyTime)


# 3) Experienced Learning Responses --------------------------------------------


#   a) General Information --------------------------------------------------
#     - Participant Id,Name ----------------------------------------------------------

experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(PARTICIPANTID="participantId"))
experiencedLearningResponses$participantId <- participant.id(experiencedLearningResponses$participantId)

#     - Scenario and Experimental Block ------------------------------------------------------

#Scenario (or Decision Problem)
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(DP="scenario"))
experiencedLearningResponses$scenario <- as.character(experiencedLearningResponses$scenario)
#Some ids have a blank space at the beginning of the string
experiencedLearningResponses$scenario = removeFirstBlankSpaceString(experiencedLearningResponses$scenario)
experiencedLearningResponses$scenario <- scenario.id(experiencedLearningResponses$scenario)

#Experiment
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(EXPERIMENT="experimentalBlock"))
experiencedLearningResponses$experimentalBlock = experimentalBlock.id(experiencedLearningResponses$experimentalBlock)

experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(DPS="nBlockScenarios"))


#     - Time and Date of Entries in File Writing --------------------------------------------------------------------
#This corresponds to the starting time of the choice response

experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(TIME="time"))

experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(DATE ="date"))
experiencedLearningResponses$date <- as.character(experiencedLearningResponses$date)
experiencedLearningResponses$date <- complete.date(experiencedLearningResponses$date)

experiencedLearningResponses$time <- as.character(experiencedLearningResponses$time)
experiencedLearningResponses$time <- complete.time(experiencedLearningResponses$time)
experiencedLearningResponses$time <- paste(experiencedLearningResponses$date,experiencedLearningResponses$time,sep='')
experiencedLearningResponses$time <- as.POSIXlt(experiencedLearningResponses$time,format="%d%m%Y%H%M%S")


#     - Walking Time ----------------------------------------------------------
# This value is obtained from the participants file
experiencedLearningResponses = merge(experiencedLearningResponses, participants[c("participantId","walkingTime")], by =c("participantId"))

#     - DwellTime -------------------------------------------------------------
experiencedLearningResponses = merge(experiencedLearningResponses, participants[c("participantId","dwellTime")], by =c("participantId"))

#     - Travel Time (in-vehicle) -------------------------------------------------------------
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(TRAVEL="inVehicleTime"))

#     - Waiting Time ------------------------------------------------------------
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(WAITING="waitingTime"))
#     - Journey Time ----------------------------------------------------------
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(JOURNEY="journeyTime"))

#     - Total Time --------------------------------------------------------------
experiencedLearningResponses$totalTime = with(experiencedLearningResponses,walkingTime/2 + dwellTime/2 + waitingTime + inVehicleTime+ dwellTime/2+ walkingTime/2)

# b) Experiment Steps --------------------------------------------------------
experiencedLearningResponses <- plyr::rename(experiencedLearningResponses,c(TYPETRIAL="experimentalStep"))
experiencedLearningResponses$experimentalStep <- removeFirstBlankSpaceString(experiencedLearningResponses$experimentalStep)
experiencedLearningResponses$experimentalStep <- as.character(experiencedLearningResponses$experimentalStep)

experiencedLearningResponses$experimentalStep[experiencedLearningResponses$experimentalStep == "learning"] = "learning"
experiencedLearningResponses$experimentalStep[experiencedLearningResponses$experimentalStep == "extraLearning"] = "extra-learning"
experiencedLearningResponses$experimentalStep[experiencedLearningResponses$experimentalStep == "consequence"] = "consequences"
experiencedLearningResponses$experimentalStep[experiencedLearningResponses$experimentalStep == "confirmation"] = "confirmation"


#   - Learning Step (learning, extra-learning) ----------------------------------

#This phase have information from 3 different steps so we first will split the dataset in three parts

# Starting Time

#The minimum value of the learning trials corresponds to the time when the user clicked to make the first trip


#Sometimes the learning trials are printed in different times that differs in one second. We picked the maximum value only
# experiencedLearningResponses$startingTimeLearningStep = with(experiencedLearningResponses, ave(time,interaction(participantId,scenario),FUN =function(x) max(x)))

# experiencedLearningResponses$startingTimeScenario <- as.character(experiencedLearningResponses$startingTimeScenario)
# experiencedLearningResponses$startingTimeScenario <- complete.time(experiencedLearningResponses$startingTimeScenario)
# 

# 
# 
# experiencedLearningResponses$date = as.Date(experiencedLearningResponses$date,format = "%d%m%Y") #This add hyphens between the date parameters

#Ending Time

#The maximum value of the learning trials corresponds to the time when the user clicked to go to the Decision Step. 
#It is not the time when the user click to make the fourth trip, which in fact is lost information but not relevant for our analyses

# temp = with(subset(experiencedLearningResponses,experimentalStep == "learning"|| experimentalStep == "extra-learning"), ave(time,interaction(participantId,scenario),FUN =function(x) max(x)))
# 
# merge(unique(temp[c("")])
# experiencedLearningResponses$endingTimeLearningStep


#   - Decision Step (Times) ----------------------------------

#The time registered in the choice response corresponds to the time when the user make a choice and goes to the consequence stage.

# experiencedLearningResponses$endingTimeDecisionStep = 

#   - Consequences Step (Times) ----------------------------------

#   - Confirmation Step (Times) ----------------------------------


# 4) Experienced Choice Responses --------------------------------------------


#   - Participant Id,Name ----------------------------------------------------------

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(PARTICIPANTID="participantId"))
experiencedChoiceResponses$participantId <- participant.id(experiencedChoiceResponses$participantId)

#   - Scenario and Experimental Block ------------------------------------------------------

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(DP="scenario"))

#Add zero in the left side (if the scenario has only one digit)
experiencedChoiceResponses$scenario <- scenario.id(experiencedChoiceResponses$scenario)

# label(experiencedChoiceResponses$decisionProblem) = "Decision Problem"

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(EXPERIMENT="experimentalBlock"))
experiencedChoiceResponses$experimentalBlock = experimentalBlock.id(experiencedChoiceResponses$experimentalBlock)

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(DPS="nBlockScenarios"))
experiencedChoiceResponses$experimentType = "experienced"

#Order of the scenarios during the experiment
experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(ABSORDER="absoluteOrderScenario"))
# descriptiveChoiceResponses$absoluteOrderScenario <- scenario.id(descriptiveChoiceResponses$absoluteOrderScenario)
experiencedChoiceResponses$absoluteOrderScenario <- as.numeric(experiencedChoiceResponses$absoluteOrderScenario)


#   - Experimental Step (Decision or Confirmation) --------------------------
experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(TYPE="experimentalStep"))
experiencedChoiceResponses$experimentalStep = as.character(experiencedChoiceResponses$experimentalStep)
experiencedChoiceResponses$experimentalStep = removeFirstBlankSpaceString(experiencedChoiceResponses$experimentalStep)

experiencedChoiceResponses$nExperimentalStep = NA
experiencedChoiceResponses$nExperimentalStep[experiencedChoiceResponses$experimentalStep =="decision"] = 2
experiencedChoiceResponses$nExperimentalStep[experiencedChoiceResponses$experimentalStep =="confirmation"] = 2
experiencedChoiceResponses$nExperimentalStep = as.numeric(experiencedChoiceResponses$nExperimentalStep)


#   - Id for observations (row) ------------------------------------------------------------------

experiencedChoiceResponses = experiencedChoiceResponses[order(as.character(with(experiencedChoiceResponses,interaction(participantId,scenario,nExperimentalStep)))),]

experiencedChoiceResponses$nObservation = seq(1,dim(experiencedChoiceResponses)[1])
row.names(experiencedChoiceResponses) = experiencedChoiceResponses$nObservation


#   - Extra Learning Trials ---------------------------------------------------
experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(NEXTRALEARNINGTRIALS="nExtraLearningTrials"))

#   - Time and Date of Entries in File Writing --------------------------------------------------------------------
#This corresponds to the starting time of the choice response

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(TIME="time"))

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(DATE ="date"))
experiencedChoiceResponses$date <- as.character(experiencedChoiceResponses$date)
experiencedChoiceResponses$date <- complete.date(experiencedChoiceResponses$date)

experiencedChoiceResponses$time <- as.character(experiencedChoiceResponses$time)
experiencedChoiceResponses$time <- complete.time(experiencedChoiceResponses$time)
experiencedChoiceResponses$time <- paste(experiencedChoiceResponses$date,experiencedChoiceResponses$time,sep='')
experiencedChoiceResponses$time <- as.POSIXlt(experiencedChoiceResponses$time,format="%d%m%Y%H%M%S")
#   - Experimental Conditions -------------------------------------------------

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(EXPERIMENTALCONDITION="experimentalCondition"))
experiencedChoiceResponses$experimentalCondition[experiencedChoiceResponses$experimentalCondition == "simulatedControlCondition"] <- "control"
experiencedChoiceResponses$experimentalCondition[experiencedChoiceResponses$experimentalCondition == "simulatedTreatmentCondition1"] <- "treatment"


table(experiencedChoiceResponses$experimentalCondition)

#   - Preferred Route (Choice) ---------------------------------------------------------

#I will change the labels to irrational and rational routes later on 

experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(PREFERREDROUTE="choice"))
experiencedChoiceResponses$choice = removeFirstBlankSpaceString(experiencedChoiceResponses$choice)

experiencedChoiceResponses$choice[experiencedChoiceResponses$choice == "control"] <- "R1"
experiencedChoiceResponses$choice[experiencedChoiceResponses$choice == "treatment"] <- "R2"

#   - Rational/Irrational Route -----------------------------------------------

experiencedChoiceResponses <- merge(experiencedChoiceResponses,experimentalScenarios[c("scenario","alternative","route","rationalRoute","irrationalRoute")],by.x =c("scenario","choice"),by.y = c("scenario","route"))

#Replace choice label by "irrational or rational route"
experiencedChoiceResponses$choice[with(experiencedChoiceResponses,choice == rationalRoute)] <- "rational"
experiencedChoiceResponses$choice[with(experiencedChoiceResponses,choice == irrationalRoute)] <- "irrational"

# experiencedChoiceResponses$choice2 <- NA
# experiencedChoiceResponses$choice2[with(experiencedChoiceResponses,choice == rationalRoute)] <- "rational"
# experiencedChoiceResponses$choice2[with(experiencedChoiceResponses,choice == irrationalRoute)] <- "irrational"
# 
# experiencedChoiceResponses$choice <- NULL
# experiencedChoiceResponses <- plyr::rename(experiencedChoiceResponses,c(choice2="choice"))

#   - Certainty Level -------------------------------------------------------

experiencedChoiceResponses = plyr::rename(experiencedChoiceResponses,c(CERTAINTYLEVEL="certaintyLevel"))
experiencedChoiceResponses$certaintyLevel = as.numeric(experiencedChoiceResponses$certaintyLevel)

# 5) DESCRIPTIVE LEARNING RESPONSES --------------------------------------------

# - Participant Id,Name ----------------------------------------------------------

descriptiveLearningResponses <- plyr::rename(descriptiveLearningResponses,c(PARTICIPANTID="participantId"))
descriptiveLearningResponses$participantId <- participant.id(descriptiveLearningResponses$participantId)

# - Scenario (Decision Problem) ------------------------------------------------------

descriptiveLearningResponses = plyr::rename(descriptiveLearningResponses,c(DP="scenario"))
descriptiveLearningResponses$scenario= scenario.id(descriptiveLearningResponses$scenario)


# - Time and Date --------------------------------------------------------------------
#This corresponds to the starting time of the choice response

descriptiveLearningResponses <- plyr::rename(descriptiveLearningResponses,c(TIME="time"))

#Sometimes the learning trials are printed in different times that differs in one second. We picked the maximum value only
descriptiveLearningResponses$startingTimeScenario = with(descriptiveLearningResponses, ave(time,interaction(participantId,scenario),FUN =function(x) max(x)))

descriptiveLearningResponses$startingTimeScenario <- as.character(descriptiveLearningResponses$startingTimeScenario)
descriptiveLearningResponses$startingTimeScenario <- complete.time(descriptiveLearningResponses$startingTimeScenario)

descriptiveLearningResponses <- plyr::rename(descriptiveLearningResponses,c(DATE ="date"))
descriptiveLearningResponses$date <- as.character(descriptiveLearningResponses$date)
descriptiveLearningResponses$date <- complete.date(descriptiveLearningResponses$date)

descriptiveLearningResponses$startingTimeScenario <- as.character(descriptiveLearningResponses$startingTimeScenario)
descriptiveLearningResponses$startingTimeScenario <- complete.time(descriptiveLearningResponses$startingTimeScenario)
descriptiveLearningResponses$startingTimeScenario <- paste(descriptiveLearningResponses$date,descriptiveLearningResponses$startingTimeScenario,sep='')
descriptiveLearningResponses$startingTimeScenario <- as.POSIXlt(descriptiveLearningResponses$startingTimeScenario,format="%d%m%Y%H%M%S")


descriptiveLearningResponses$date = as.Date(descriptiveLearningResponses$date,format = "%d%m%Y") #This add hyphens between the date parameters



# 5) DESCRIPTIVE CHOICE RESPONSES --------------------------------------------


# - Participant Id,Name ----------------------------------------------------------

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(PARTICIPANTID="participantId"))
descriptiveChoiceResponses$participantId <- participant.id(descriptiveChoiceResponses$participantId)

# - Experimental Condition --------------------------------------------------

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(EXPERIMENTALCONDITION="experimentalCondition"))
descriptiveChoiceResponses$experimentalCondition[descriptiveChoiceResponses$experimentalCondition == "descriptiveControl"] <- "control"
descriptiveChoiceResponses$experimentalCondition[descriptiveChoiceResponses$experimentalCondition == "descriptiveTreatmentCondition1"] <- "treatment"

# View(descriptiveChoiceResponses)
# - Scenario and Experiment  ------------------------------------------------------

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(DP="scenario"))
# label(descriptiveChoiceResponses$decisionProblem) = "Decision Problem"
descriptiveChoiceResponses$scenario <- scenario.id(descriptiveChoiceResponses$scenario)

#Order of the scenarios during the experiment

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(ABSORDER="absoluteOrderScenario"))
# descriptiveChoiceResponses$absoluteOrderScenario <- scenario.id(descriptiveChoiceResponses$absoluteOrderScenario)
descriptiveChoiceResponses$absoluteOrderScenario <- as.numeric(descriptiveChoiceResponses$absoluteOrderScenario)
#Experimental Block

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(EXPERIMENT="experimentalBlock"))
descriptiveChoiceResponses$experimentalBlock = experimentalBlock.id(descriptiveChoiceResponses$experimentalBlock)

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(DPS="nBlockScenarios"))

#Experiment type (experienced or descriptive)
descriptiveChoiceResponses$experimentType = "descriptive"


# - Choice (Preferred Route) ---------------------------------------------------------

#I will change the labels to irrational and rational routes later on 

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(PREFERREDROUTE="choice"))

descriptiveChoiceResponses$choice = removeFirstBlankSpaceString(descriptiveChoiceResponses$choice)

descriptiveChoiceResponses$choice[descriptiveChoiceResponses$choice == "control"] <- "R1"
descriptiveChoiceResponses$choice[descriptiveChoiceResponses$choice == "treatment"] <- "R2"


# # - Irrational/rational route ---------------------------------------------

#   - Rational/Irrational Route -----------------------------------------------
descriptiveChoiceResponses <-  merge(descriptiveChoiceResponses,experimentalScenarios[c("scenario","alternative","route","rationalRoute","irrationalRoute")],by.x =c("scenario","choice"),by.y = c("scenario","route"))
# descriptiveChoiceResponses <- merge(descriptiveChoiceResponses,experimentalScenarios[c("scenario","routeType","alternative","route","rationalRoute","irrationalRoute")],by.x =c("scenario","choice"),by.y = c("scenario","routeType"),all.x=TRUE)

#Replace choice label by "irrational or rational route"
descriptiveChoiceResponses$choice[with(descriptiveChoiceResponses,choice == rationalRoute)] <- "rational"
descriptiveChoiceResponses$choice[with(descriptiveChoiceResponses,choice == irrationalRoute)] <- "irrational"

# - Ending Time and Date Scenarios -------------------------------------------------------------

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(TIME="endingTimeScenario"))
descriptiveChoiceResponses$endingTimeScenario <- as.character(descriptiveChoiceResponses$endingTimeScenario)
descriptiveChoiceResponses$endingTimeScenario <- complete.time(descriptiveChoiceResponses$endingTimeScenario)

descriptiveChoiceResponses <- plyr::rename(descriptiveChoiceResponses,c(DATE ="date"))
descriptiveChoiceResponses$date <- as.character(descriptiveChoiceResponses$date)
descriptiveChoiceResponses$date <- complete.date(descriptiveChoiceResponses$date)

descriptiveChoiceResponses$endingTimeScenario <- as.character(descriptiveChoiceResponses$endingTimeScenario)
descriptiveChoiceResponses$endingTimeScenario <- complete.time(descriptiveChoiceResponses$endingTimeScenario)
descriptiveChoiceResponses$endingTimeScenario <- paste(descriptiveChoiceResponses$date,descriptiveChoiceResponses$endingTimeScenario,sep='')
descriptiveChoiceResponses$endingTimeScenario <- as.POSIXlt(descriptiveChoiceResponses$endingTimeScenario,format="%d%m%Y%H%M%S")

descriptiveChoiceResponses$date = as.Date(descriptiveChoiceResponses$date,format = "%d%m%Y") #This add hyphens between the date parameters











# 7) TRAVEL BEHAVIOUR INFORMATION --------------------------------------------


# - Participant Id ----------------------------------------------------------
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(PARTICIPANTID="participantId"))
travelBehaviourResponses$participantId <- participant.id(travelBehaviourResponses$participantId)

# - Public Transport Use ------------------------------------

travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(TRIPSPERWEEKBUSES="publicTransportTripsFrequency"))

# - Importance of Time Attributes -----------------------------------------
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALWAITINGTIMEIMPORTANCE="waitingTimeImportance"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALINVEHICLETIMEIMPORTANCE="travelTimeImportance"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALJOURNEYTIMEIMPORTANCE="journeyTimeImportance"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALWAITINGTIMERELIABILITYIMPORTANCE="waitingTimeReliabilityImportance"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALINVEHICLETIMERELIABILITYIMPORTANCE="travelTimeReliabilityImportance"))

# - Average Commuting Time Attributes ---------------------------------------

#Information about the Most Frequent Trip
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(WALKINGTIME="realWalkingTime"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(WAITINGTIME="realWaitingTime"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(INVEHICLETIME="realTravelTime"))
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(JOURNEYTIME="realJourneyTime"))

# - Origin/Destination Commuting ------------------------------------------

# travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(ORIGINPLACE="originPlaceMostFreqTrip"))


# ""                      "OTHERORIGINPLACE"                
# [4] "ORIGINBOROUGH"                    "OTHERORIGINBOROUGH"               "DESTINATIONPLACE"                
# [7] "OTHERDESTINATIONPLACE"            "DESTINATIONBOROUGH"               "OTHERDESTINATIONBOROUGH"         
# [10] "BUSROUTE"
# 
# "TRIPSPERWEEKMOSTFREQBUSROUTE"

# - Use of Real Time Information (as percentage of the trips) ------------------------------------------
travelBehaviourResponses <- plyr::rename(travelBehaviourResponses,c(REALTIMEINFORMATION="realTimeInformationUseFrequency"))
travelBehaviourResponses$realTimeInformationUseFrequency <- factor(travelBehaviourResponses$realTimeInformationUseFrequency
                          ,levels= c("Always (100% of the trips)", "Very Frequently (75% of the trips)","Frequently (50% of the trips)"           
                                     ,"Occasionally (25% of the trips)", "Never (0% of the trips)")
                          ,labels = c("100","75","50","25","0")
                          )

travelBehaviourResponses$realTimeInformationUseFrequency <- as.integer(travelBehaviourResponses$realTimeInformationUseFrequency)

# - Add information to participants' dataset ------------------------------------------
varList <- c("participantId","waitingTimeImportance","travelTimeImportance","journeyTimeImportance"
                                   ,"waitingTimeReliabilityImportance","travelTimeReliabilityImportance"
                                   ,"realWalkingTime","realWaitingTime","realTravelTime","realJourneyTime"
                                   ,"realTimeInformationUseFrequency","publicTransportTripsFrequency")
participants <- merge(participants,travelBehaviourResponses[varList],by = "participantId",all.x = TRUE)


# 8) EXPERIMENT DEBRIEF INFORMATION  --------------------------------------
#   - Participant Id ----------------------------------------------------------
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(PARTICIPANTID="participantId"))
experimentDebriefResponses$participantId <- participant.id(experimentDebriefResponses$participantId)

#   - Counting ---------------------------------------------------------------

experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTTIMECOUNTING="timeCounting"))
experimentDebriefResponses$timeCounting[experimentDebriefResponses$timeCounting=="no"] <- 0
experimentDebriefResponses$timeCounting[experimentDebriefResponses$timeCounting=="yes"] <- 1
experimentDebriefResponses$timeCounting <- as.numeric(experimentDebriefResponses$timeCounting)


#   - Identification with each type of experiment -----------------------------------
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTIDENTIFICATIONLEVEL="experiencedExperimentIdentification"))
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(DESCRIPTIVEEXPERIMENTIDENTIFICATIONLEVEL="descriptiveExperimentIdentification"))
#   - Experienced Times -----------------------------------------------------------------------

experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTWALKINGTIME="experiencedWalkingTime"))
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTWAITINGTIME="experiencedWaitingTime"))
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTINVEHICLETIME="experiencedTravelTime"))
experimentDebriefResponses <- plyr::rename(experimentDebriefResponses,c(EXPERIENCEDEXPERIMENTJOURNEYTIME="experiencedJourneyTime"))
#The journey times were not well-written in the experiment file and they are equal to the experiencedWaitingTime. I will remove this variable
experimentDebriefResponses$experiencedJourneyTime <- NULL
#   - Add information to participants' dataset ------------------------------------------
temp <- experimentDebriefResponses[c("participantId","timeCounting","experiencedExperimentIdentification","descriptiveExperimentIdentification"
                                     ,"experiencedWalkingTime","experiencedWaitingTime","experiencedTravelTime"
                                     )]
participants <- merge(participants,temp,by = "participantId",all.x = TRUE)

# 7) REACTION TIMES -------------------------------------------------------




#   a) Descriptive Experiment -----------------------------------------------

#     - Descriptive Experiment - Treatment Condition (Tables) -------------------------------------

# Reaction Time by Scenario
descriptiveChoiceResponsesTreatmentCondition = subset(descriptiveChoiceResponses,experimentalCondition == "treatment")
descriptiveChoiceResponsesTreatmentCondition = merge(descriptiveChoiceResponsesTreatmentCondition, unique(descriptiveLearningResponses[c("participantId","scenario","startingTimeScenario")]),by = c("participantId","scenario"), all.x = TRUE)
descriptiveChoiceResponsesTreatmentCondition$scenarioDuration = as.numeric(with(descriptiveChoiceResponsesTreatmentCondition,endingTimeScenario-startingTimeScenario))
descriptiveChoiceResponsesTreatmentCondition$reactionTime = descriptiveChoiceResponsesTreatmentCondition$scenarioDuration


#     - Descriptive Experiment - Control Condition (Prospects) ---------------------------

descriptiveChoiceResponsesControlCondition = subset(descriptiveChoiceResponses,experimentalCondition == "control")
temp = descriptiveChoiceResponsesControlCondition
temp$absoluteOrderScenario = scenario.id(as.integer(temp$absoluteOrderScenario)+1)
temp$startingTimeScenario = temp$endingTimeScenario

descriptiveChoiceResponsesControlCondition = merge(descriptiveChoiceResponsesControlCondition, temp[c("participantId","absoluteOrderScenario","startingTimeScenario")],by = c("participantId","absoluteOrderScenario"), all.x = TRUE)
descriptiveChoiceResponsesControlCondition$scenarioDuration = as.numeric(with(descriptiveChoiceResponsesControlCondition,endingTimeScenario-startingTimeScenario))
descriptiveChoiceResponsesControlCondition$reactionTime = descriptiveChoiceResponsesControlCondition$scenarioDuration
# View(descriptiveChoiceResponsesControlCondition)

# descriptiveChoiceResponsesControlCondition = merge(descriptiveChoiceResponsesControlCondition, temp[c("participantId","scenario","startingTimeScenario")],by = c("participantId","scenario"), all.x = TRUE)
# descriptiveChoiceResponsesControlCondition$scenarioDuration = as.numeric(with(descriptiveChoiceResponsesControlCondition,endingTimeScenario-startingTimeScenario))


#     - Descriptive Experiment Append Control and Treatment Condition --------

descriptiveChoiceResponses = rbind(descriptiveChoiceResponsesTreatmentCondition,descriptiveChoiceResponsesControlCondition)

rm(descriptiveChoiceResponsesControlCondition)
rm(descriptiveChoiceResponsesTreatmentCondition)

# View(descriptiveChoiceResponses)





#   b) Experienced Experiment -----------------------------------------------

#We will first determine the maximum time after the learning and extra learning steps. 

#- Reaction Times in Decision Step
experiencedLearningResponsesBeforeDecisionStep <-  subset(experiencedLearningResponses,with(experiencedLearningResponses,experimentalStep == "learning" | experimentalStep == "extra-learning"))
experiencedLearningResponsesBeforeDecisionStep$startingTimeExperimentalStep <-  with(experiencedLearningResponsesBeforeDecisionStep, ave(time,interaction(participantId,scenario),FUN =function(x) max(x)))
experiencedChoiceResponsesDecisionStep <- subset(experiencedChoiceResponses,experimentalStep == "decision")
experiencedChoiceResponsesDecisionStep <- merge(experiencedChoiceResponsesDecisionStep, unique(experiencedLearningResponsesBeforeDecisionStep[c("startingTimeExperimentalStep","participantId","scenario")])
                                                ,by=c("participantId","scenario"))
experiencedChoiceResponsesDecisionStep$endingTimeExperimentalStep = experiencedChoiceResponsesDecisionStep$time
experiencedChoiceResponsesDecisionStep$durationExperimentalStep = with(experiencedChoiceResponsesDecisionStep,endingTimeExperimentalStep-startingTimeExperimentalStep)
experiencedChoiceResponsesDecisionStep$reactionTime = experiencedChoiceResponsesDecisionStep$durationExperimentalStep

#- Reaction Times in Confirmation Step
experiencedLearningResponsesBeforeConfirmationStep = subset(experiencedLearningResponses,with(experiencedLearningResponses,experimentalStep == "consequences"))
experiencedLearningResponsesBeforeConfirmationStep$startingTimeExperimentalStep <-  with(experiencedLearningResponsesBeforeConfirmationStep, ave(time,interaction(participantId,scenario),FUN =function(x) max(x)))
experiencedChoiceResponsesConfirmationStep <- subset(experiencedChoiceResponses,experimentalStep == "confirmation")
experiencedChoiceResponsesConfirmationStep <- merge(experiencedChoiceResponsesConfirmationStep, unique(experiencedLearningResponsesBeforeConfirmationStep[c("startingTimeExperimentalStep","participantId","scenario")])
                                                    ,by=c("participantId","scenario"))
experiencedChoiceResponsesConfirmationStep$endingTimeExperimentalStep <- experiencedChoiceResponsesConfirmationStep$time
experiencedChoiceResponsesConfirmationStep$durationExperimentalStep = with(experiencedChoiceResponsesConfirmationStep,endingTimeExperimentalStep-startingTimeExperimentalStep)
experiencedChoiceResponsesConfirmationStep$reactionTime =experiencedChoiceResponsesConfirmationStep$durationExperimentalStep
#Now we append both datasets
experiencedChoiceResponses = rbind(experiencedChoiceResponsesDecisionStep,experiencedChoiceResponsesConfirmationStep)

# View(experiencedChoiceResponses)

# # 8) MAP LONDON -----------------------------------------------------------
# # - City --------------------------------------------------------------------
# mapLondon$CITY <- "London"
# 
# # - Postcode (or Postnr) ----------------------------------------------------
# mapLondon <- plyr::rename(mapLondon,c(Postcode="POSTNR"))
# # - County ------------------------------------------------------------------
# mapLondon <- plyr::rename(mapLondon,c(County="COUNTY"))
# # - Latitude ------------------------------------------------------------------
# mapLondon <- plyr::rename(mapLondon,c(Latitude="LAT"))
# 
# # - Longitude --------------------------------------------------------------
# mapLondon <- plyr::rename(mapLondon,c(Longitude="LON"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
