# NP Training Data - Data Wrangling Script

  ## NP Training:
    ### 30 min duration (or max 100 trials)
    ### Subjects nose poke to earn pellets (FR1)
    ### NP3 available, cue light active when NP active, off for 5-sec after response

  ## MedPC variables of interest:
    ### D (NPs made in NP3): 1 = Total, 2:500 = Time stamps, 501:1001 = Duration
    ### E (Mag Entries): 1 = Total, 2:5001 = Time stamps, 5002:10001 = Duration
    ### G (Pellet Deliveries): 1 = Total, 2:1001 = Time stamps

  ## Compare NP across sessions, to measure learning of operant behavior

-----------------------------------------------------------------------------------------------------
# RUN THIS SECTION ONCE PER R SESSION
# Setup
  
  setwd("/Desktop/Uni/Honours/FP_MedPC_Data")
  
  library(tidyverse)
  library(papaja)
  library(fluoR)
  library(rmedpc)
  library(ez)
  library(ggpubr)
  library(rstatix)
  library(patchwork)
  library(janitor)

----------------------------------------------------------------------------------------------------

# RUN THUS SECTION REPEATEDLY (FOR EACH SESSION/SUBJECT)
# Data Wrangling (must first separate MedPC data by subject and session)
  
  ## Import MedPC data, use file path "Analysis_Data/NP_Training/NP_Training_[session]/LK_NP_Training_[session]_[subject].txt", assign subject and session. 
  
  med_data <- rmedpc::import_medpc("Analysis_Data/NP_Training/NP_Training_D6/LK_NP_Training_D6_F2.txt") 
  
  subject <- as_tibble(unlist(med_data$Subject)); subject <- subject %>% rename(subject = value) %>% convert_as_factor(subject)
  
  session <- c("6"); session <- as_tibble(session); session <- session %>% rename(session = value) %>% convert_as_factor(session)
  

  ## Extract total rewarded nose pokes from variable G
  
  NP_data <- as_tibble(unlist(med_data$G))
  
  NP_data <- slice(NP_data, 1:1); NP_data <- NP_data %>% rename(NP_total = value)
  
  ## Combine data frames
  
  master_data <- cbind(subject, session, NP_data)
  
# Export final data to .csv file ready to be used in final data analysis. Use form "Final_Data/[Task]/[Session]/NP_Train_Final_[Session]_[Subject].csv"
  
  write_csv(master_data, "Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F2.csv")
  
----------------------------------------------------------------------------------------------------
# RUN THIS LAST
# Combine individual data files into one data file for NP training with subject, session and total NP variables
  
  ## Load in all individual data files
    
    NPTrain_D1_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_M3.csv")
    NPTrain_D2_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_M3.csv")
    NPTrain_D3_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_M3.csv")
    NPTrain_D4_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_M3.csv")
    NPTrain_D5_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_M3.csv")
    NPTrain_D6_M3 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_M3.csv")
    
    NPTrain_D1_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_M4.csv")
    NPTrain_D2_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_M4.csv")
    NPTrain_D3_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_M4.csv")
    NPTrain_D4_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_M4.csv")
    NPTrain_D5_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_M4.csv")
    NPTrain_D6_M4 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_M4.csv")
    
    NPTrain_D1_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_M5.csv")
    NPTrain_D2_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_M5.csv")
    NPTrain_D3_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_M5.csv")
    NPTrain_D4_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_M5.csv")
    NPTrain_D5_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_M5.csv")
    NPTrain_D6_M5 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_M5.csv")
    
    NPTrain_D1_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_M9.csv")
    NPTrain_D2_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_M9.csv")
    NPTrain_D3_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_M9.csv")
    NPTrain_D4_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_M9.csv")
    NPTrain_D5_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_M9.csv")
    NPTrain_D6_M9 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_M9.csv")
    
    NPTrain_D1_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_F7.csv")
    NPTrain_D2_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_F7.csv")
    NPTrain_D3_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_F7.csv")
    NPTrain_D4_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_F7.csv")
    NPTrain_D5_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_F7.csv")
    NPTrain_D6_F7 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F7.csv")
    
    NPTrain_D1_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_F8.csv")
    NPTrain_D2_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_F8.csv")
    NPTrain_D3_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_F8.csv")
    NPTrain_D4_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_F8.csv")
    NPTrain_D5_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_F8.csv")
    NPTrain_D6_F8 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F8.csv")
    
    NPTrain_D1_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_F9.csv")
    NPTrain_D2_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_F9.csv")
    NPTrain_D3_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_F9.csv")
    NPTrain_D4_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_F9.csv")
    NPTrain_D5_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_F9.csv")
    NPTrain_D6_F9 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F9.csv")
    
    NPTrain_D1_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_F10.csv")
    NPTrain_D2_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_F10.csv")
    NPTrain_D3_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_F10.csv")
    NPTrain_D4_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_F10.csv")
    NPTrain_D5_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_F10.csv")
    NPTrain_D6_F10 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F10.csv")
    
    NPTrain_D1_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D1/NP_Training_Final_D1_F2.csv")
    NPTrain_D2_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D2/NP_Training_Final_D2_F2.csv")
    NPTrain_D3_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D3/NP_Training_Final_D3_F2.csv")
    NPTrain_D4_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D4/NP_Training_Final_D4_F2.csv")
    NPTrain_D5_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D5/NP_Training_Final_D5_F2.csv")
    NPTrain_D6_F2 <- read.csv("Final_Data/NP_Training/NP_Training_D6/NP_Training_Final_D6_F2.csv")
    
  ## Combine data frames into single data frame, and export into master data file
    
    NPTrain_master <- bind_rows(NPTrain_D1_M3, NPTrain_D2_M3, NPTrain_D3_M3, NPTrain_D4_M3, NPTrain_D5_M3, NPTrain_D6_M3,
                                NPTrain_D1_M4, NPTrain_D2_M4, NPTrain_D3_M4, NPTrain_D4_M4, NPTrain_D5_M4, NPTrain_D6_M4,
                                NPTrain_D1_M5, NPTrain_D2_M5, NPTrain_D3_M5, NPTrain_D4_M5, NPTrain_D5_M5, NPTrain_D6_M5,
                                NPTrain_D1_M9, NPTrain_D2_M9, NPTrain_D3_M9, NPTrain_D4_M9, NPTrain_D5_M9, NPTrain_D6_M9,
                                NPTrain_D1_F7, NPTrain_D2_F7, NPTrain_D3_F7, NPTrain_D4_F7, NPTrain_D5_F7, NPTrain_D6_F7,
                                NPTrain_D1_F8, NPTrain_D2_F8, NPTrain_D3_F8, NPTrain_D4_F8, NPTrain_D5_F8, NPTrain_D6_F8,
                                NPTrain_D1_F9, NPTrain_D2_F9, NPTrain_D3_F9, NPTrain_D4_F9, NPTrain_D5_F9, NPTrain_D6_F9,
                                NPTrain_D1_F10, NPTrain_D2_F10, NPTrain_D3_F10, NPTrain_D4_F10, NPTrain_D5_F10, NPTrain_D6_F10,
                                NPTrain_D1_F2, NPTrain_D2_F2, NPTrain_D3_F2, NPTrain_D4_F2, NPTrain_D5_F2, NPTrain_D6_F2)
    
    remove(NPTrain_D1_M3, NPTrain_D2_M3, NPTrain_D3_M3, NPTrain_D4_M3, NPTrain_D5_M3, NPTrain_D6_M3,
           NPTrain_D1_M4, NPTrain_D2_M4, NPTrain_D3_M4, NPTrain_D4_M4, NPTrain_D5_M4, NPTrain_D6_M4,
           NPTrain_D1_M5, NPTrain_D2_M5, NPTrain_D3_M5, NPTrain_D4_M5, NPTrain_D5_M5, NPTrain_D6_M5,
           NPTrain_D1_M9, NPTrain_D2_M9, NPTrain_D3_M9, NPTrain_D4_M9, NPTrain_D5_M9, NPTrain_D6_M9,
           NPTrain_D1_F7, NPTrain_D2_F7, NPTrain_D3_F7, NPTrain_D4_F7, NPTrain_D5_F7, NPTrain_D6_F7,
           NPTrain_D1_F8, NPTrain_D2_F8, NPTrain_D3_F8, NPTrain_D4_F8, NPTrain_D5_F8, NPTrain_D6_F8,
           NPTrain_D1_F9, NPTrain_D2_F9, NPTrain_D3_F9, NPTrain_D4_F9, NPTrain_D5_F9, NPTrain_D6_F9,
           NPTrain_D1_F10, NPTrain_D2_F10, NPTrain_D3_F10, NPTrain_D4_F10, NPTrain_D5_F10, NPTrain_D6_F10,
           NPTrain_D1_F2, NPTrain_D2_F2, NPTrain_D3_F2, NPTrain_D4_F2, NPTrain_D5_F2, NPTrain_D6_F2)
    
    write_csv(NPTrain_master, "Final_Data/NP_Training/NP_Training_Combined.csv")
    