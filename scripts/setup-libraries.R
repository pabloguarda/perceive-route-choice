# a) Libraries --------------------------------------------------------------------

options(install.packages.compile.from.source = "never")

# - List of libraries ------------------------------------------------------------------

libraries1 <- c("broom.mixed") # readr: Read CSVs nicely, broom: Convert models to data frames
libraries2 <- c("pscl","data.table","mlogit","dfidx", "lme4","randomizr") #data.table: To read large csv faster
libraries3 <- NULL
libraries4 <- c("MASS","scales","grDevices","graphics")  #graphics: Math Formula,"emdbook"
libraries5 <- c("weights","stringi","nortest","stringr","pastecs","boot") #Yes
libraries6 <- c("plyr","dplyr","gridExtra","reshape2","gdata")
libraries7 <- c("devtools")
libraries_tables <- c('stargazer', 'texreg', 'xtable', 'memisc', 'knitr','markdown' ,'Hmisc') #Export Tables
libraries_graphics <- c('ggplot2','ggrepel',"ggjoy","ggthemes") #Plots
libraries_statisticalTests <- c("RVAideMemoire","AER")
libraries_dcm <- c("maxLik") 

#, 'apsrtable'


libraries <- c(libraries1,libraries2,libraries3,libraries4,libraries5,libraries6,libraries7
               ,libraries_tables,libraries_graphics,libraries_statisticalTests,libraries_dcm)

# - Loading/installing ---------------------------------------------------------------

for(i in 1:length(libraries)){
  if(!require(package = libraries[i],character.only = TRUE)){ 
    install.packages(libraries[i],character.only = TRUE, dependencies = TRUE)
  }
  library(libraries[i],character.only = TRUE)
}




