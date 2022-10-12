# FC Training Data - Data Wrangling Script

  ## FC Training:
    ### 45 trials (15 of each forced-choice, 15 free choice) intermixed randomly
    ### Subjects nose poke in hole with active cue light to earn pellets (FR1)
    ### NP2 and NP4 available, cue light active when NP active.
    ### Trials last max 20s each, with 40s ITI.

  ## The purpose of this script is to transform the raw MedPC data for the forced-choice training sessions into data ready for analysis
  ## The main goals include producing summary data relating to:

     ### Number of correct vs incorrect responses for NP2 and NP4 individually
     ### Preference for NP2 vs NP4 during free-choice trials

# -----------------------------------------------------------------------------------------------------
# RUN THIS SECTION ONCE PER R SESSION
# Setup

setwd("~/Desktop/Uni/Honours/FP_MedPC_Data")

library(tidyverse)
library(papaja)
library(fluoR)
library(rmedpc)
library(ez)
library(ggpubr)
library(rstatix)
library(patchwork)
library(janitor)

# ----------------------------------------------------------------------------------------------------

# RUN THUS SECTION REPEATEDLY (FOR EACH SESSION/SUBJECT)
# Data Wrangling (must first separate MedPC data by subject and session)

  ## Import MedPC data, use file path "Analysis_Data/FC_Training/FC_Training_[session]/LK_FC_[session]_[subject].txt", assign subject and session. 
  
  med_data <- rmedpc::import_medpc("Analysis_Data/FC_Training/FC_Training_D7/LK_FC_D7_F2.txt") 
  
  subject <- as_tibble(unlist(med_data$Subject)); subject <- subject %>% rename(subject = value) %>% convert_as_factor(subject)
  
  session <- c("7"); session <- as_tibble(session); session <- session %>% rename(session = value) %>% convert_as_factor(session)
  
  ## Extract NP data for NP2 and NP4 (total active, inactive and free-choice)
  
    NP2_data <- as_tibble(unlist(med_data$D)); NP4_data <- as_tibble(unlist(med_data$F))
  
    ### EARLY SESSIONS (D1-D2) USE THIS CODE:
  
       # NP2_correct <- slice(NP2_data, 201:201); NP2_incorrect <- slice(NP2_data, 401:401); NP2_freechoice <- slice(NP2_data, 601:601)
        
       # NP4_correct <- slice(NP4_data, 401:401); NP4_incorrect <- slice(NP4_data, 201:201); NP4_freechoice <- slice(NP4_data, 601:601)
        
    ### LATE SESSIONS (D3-D7) USE THIS CODE:
  
        NP2_correct <- slice(NP2_data, 501:501); NP2_incorrect <- slice(NP2_data, 1001:1001); NP2_freechoice <- slice(NP2_data, 1501:1501)
        
        NP4_correct <- slice(NP4_data, 1001:1001); NP4_incorrect <- slice(NP4_data, 501:501); NP4_freechoice <- slice(NP4_data, 1501:1501)

 ## Rename variables and combine into single data frame
    
    NP2_correct <- NP2_correct %>% rename(NP2_correct = value); NP2_incorrect <- NP2_incorrect %>% rename(NP2_incorrect = value); NP2_freechoice <- NP2_freechoice %>% rename(NP2_freechoice = value)
    
    NP4_correct <- NP4_correct %>% rename(NP4_correct = value); NP4_incorrect <- NP4_incorrect %>% rename(NP4_incorrect = value); NP4_freechoice <- NP4_freechoice %>% rename(NP4_freechoice = value) 

    master_data <- cbind(subject, session, NP2_correct, NP2_incorrect, NP2_freechoice, NP4_correct, NP4_incorrect, NP4_freechoice)        

# Export final data to .csv file ready to be used in final data analysis. Use form "Final_Data/[Task]/[Task]_[Session]/[Task]_Final_[Session]_[Subject].csv"
    
    write_csv(master_data, "Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F2.csv")
    
# -----------------------------------------------------------------------------------------------------
# RUN THIS LAST
# Combine individual data files into one data file for FC training 
    
    ## Load in all individual data files
    
      FCTrain_D1_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_M3.csv")
      FCTrain_D2_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_M3.csv")
      FCTrain_D3_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_M3.csv")
      FCTrain_D4_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_M3.csv")
      FCTrain_D5_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_M3.csv")
      FCTrain_D6_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_M3.csv")
      FCTrain_D7_M3 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_M3.csv")
      
      FCTrain_D1_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_M4.csv")
      FCTrain_D2_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_M4.csv")
      FCTrain_D3_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_M4.csv")
      FCTrain_D4_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_M4.csv")
      FCTrain_D5_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_M4.csv")
      FCTrain_D6_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_M4.csv")
      FCTrain_D7_M4 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_M4.csv")
      
      FCTrain_D1_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_M5.csv")
      FCTrain_D2_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_M5.csv")
      FCTrain_D3_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_M5.csv")
      FCTrain_D4_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_M5.csv")
      FCTrain_D5_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_M5.csv")
      FCTrain_D6_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_M5.csv")
      FCTrain_D7_M5 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_M5.csv")
      
      FCTrain_D1_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_M9.csv")
      FCTrain_D2_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_M9.csv")
      FCTrain_D3_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_M9.csv")
      FCTrain_D4_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_M9.csv")
      FCTrain_D5_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_M9.csv")
      FCTrain_D6_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_M9.csv")
      FCTrain_D7_M9 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_M9.csv")
      
      FCTrain_D1_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_F7.csv")
      FCTrain_D2_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_F7.csv")
      FCTrain_D3_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_F7.csv")
      FCTrain_D4_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_F7.csv")
      FCTrain_D5_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_F7.csv")
      FCTrain_D6_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_F7.csv")
      FCTrain_D7_F7 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F7.csv")
      
      FCTrain_D1_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_F8.csv")
      FCTrain_D2_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_F8.csv")
      FCTrain_D3_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_F8.csv")
      FCTrain_D4_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_F8.csv")
      FCTrain_D5_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_F8.csv")
      FCTrain_D6_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_F8.csv")
      FCTrain_D7_F8 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F8.csv")
      
      FCTrain_D1_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_F9.csv")
      FCTrain_D2_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_F9.csv")
      FCTrain_D3_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_F9.csv")
      FCTrain_D4_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_F9.csv")
      FCTrain_D5_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_F9.csv")
      FCTrain_D6_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_F9.csv")
      FCTrain_D7_F9 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F9.csv")
      
      FCTrain_D1_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_F10.csv")
      FCTrain_D2_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_F10.csv")
      FCTrain_D3_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_F10.csv")
      FCTrain_D4_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_F10.csv")
      FCTrain_D5_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_F10.csv")
      FCTrain_D6_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_F10.csv")
      FCTrain_D7_F10 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F10.csv")
      
      FCTrain_D1_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D1/FC_Training_Final_D1_F2.csv")
      FCTrain_D2_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D2/FC_Training_Final_D2_F2.csv")
      FCTrain_D3_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D3/FC_Training_Final_D3_F2.csv")
      FCTrain_D4_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D4/FC_Training_Final_D4_F2.csv")
      FCTrain_D5_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D5/FC_Training_Final_D5_F2.csv")
      FCTrain_D6_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D6/FC_Training_Final_D6_F2.csv")
      FCTrain_D7_F2 <- read.csv("Final_Data/FC_Training/FC_Training_D7/FC_Training_Final_D7_F2.csv")
      
  ## Combine into single data frame and export to master file
      
      FCTrain_master <- bind_rows(FCTrain_D1_M3, FCTrain_D2_M3, FCTrain_D3_M3, FCTrain_D4_M3, FCTrain_D5_M3, FCTrain_D6_M3, FCTrain_D7_M3,
                                  FCTrain_D1_M4, FCTrain_D2_M4, FCTrain_D3_M4, FCTrain_D4_M4, FCTrain_D5_M4, FCTrain_D6_M4, FCTrain_D7_M4,
                                  FCTrain_D1_M5, FCTrain_D2_M5, FCTrain_D3_M5, FCTrain_D4_M5, FCTrain_D5_M5, FCTrain_D6_M5, FCTrain_D7_M5,
                                  FCTrain_D1_M9, FCTrain_D2_M9, FCTrain_D3_M9, FCTrain_D4_M9, FCTrain_D5_M9, FCTrain_D6_M9, FCTrain_D7_M9,
                                  FCTrain_D1_F7, FCTrain_D2_F7, FCTrain_D3_F7, FCTrain_D4_F7, FCTrain_D5_F7, FCTrain_D6_F7, FCTrain_D7_F7,
                                  FCTrain_D1_F8, FCTrain_D2_F8, FCTrain_D3_F8, FCTrain_D4_F8, FCTrain_D5_F8, FCTrain_D6_F8, FCTrain_D7_F8,
                                  FCTrain_D1_F9, FCTrain_D2_F9, FCTrain_D3_F9, FCTrain_D4_F9, FCTrain_D5_F9, FCTrain_D6_F9, FCTrain_D7_F9,
                                  FCTrain_D1_F10, FCTrain_D2_F10, FCTrain_D3_F10, FCTrain_D4_F10, FCTrain_D5_F10, FCTrain_D6_F10, FCTrain_D7_F10,
                                  FCTrain_D1_F2, FCTrain_D2_F2, FCTrain_D3_F2, FCTrain_D4_F2, FCTrain_D5_F2, FCTrain_D6_F2, FCTrain_D7_F2)

      write_csv(FCTrain_master, "Final_Data/FC_Training/FC_Training_Combined.csv")
      
        