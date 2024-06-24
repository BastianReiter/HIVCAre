

################################################################################
#-------------------------------------------------------------------------------
#   HIVCAre: SITE PIPELINE                                                     
#-------------------------------------------------------------------------------
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clear Workspace
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#rm(list = ls())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Project Setup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("./Scripts/SETUP.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize Progress Bar
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#try(ls_Progress_MainTotal <- f_InitProgressBar(inp_Title = "Main Total",
 #                                              inp_ProgressTotalSteps = 14),    # Needs to be adjusted manually (How many times is f_UpdateProgressBar being called?)
#    silent = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Site Output Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here("Scripts/Main/1-Main-LoadData.R"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site Output Data Validation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



########## AGGREGATE and PROCESS DATA ##########################################

#source("./Scripts/Main/2-Main-ProcessData.R")
#Full Data Set
source("./Scripts/Main/3.1-Main-ProcessData.R")
#Matched Data
source("./Scripts/Main/3.2-Main-ProcessMatchedData.R")



########## ANALYSIS CALCULATIONS ###############################################

# Specify the time period for which the data should be analysed
# Variation 1: time period (tp) from 2004 to 2023
#tp_begin <- 2004 
#tp_end <- 2023

# Variation 2: time period (tp) from 2009 to 2021 (data avaible from all sites)
tp_begin <- 2009 
tp_end <- 2021

# Variation 3: time period (tp) from 2015 to 2021 (complete data avaible from Freibrug)
#tp_begin <- 2015 
#tp_end <- 2021

source("./Scripts/Main/5-Main-Analysis-v2.R")


########## OUTPUT OBJECTS ######################################################
path <- "~/Projekte/HIVandCancer/Analyse/Main/Plots"
source("./Scripts/Main/6-Main-Output-v2.R")
