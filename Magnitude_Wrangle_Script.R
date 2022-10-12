# Magnitude Task Data - Data Wrangling Script

  ## Magnitude Task:
    ### 45 trials (15 of each forced-choice, 15 free choice) intermixed randomly, except first 10 trials must be forced-choice
    ### Subjects nose poke in hole with active cue light to earn pellets (FR1)
    ### NP2 and NP4 available, cue light active when NP active.
    ### Trials last max 20s each, with 40s ITI.
    ### Allocation of small reward (1 pellet) and large reward (3 pellets) to NP holes counterbalanced across subjects
      #### 3_1: M3, M9, F8, F9
      #### 1_3: M4, M5, F7, F10, F2

  ## The purpose of this script is to transform the raw MedPC data for the magnitude task sessions into data ready for analysis
  ## The main goals include producing summary data relating to:

    ### Number of correct vs incorrect responses for small vs large reward forced-choice trials individually
    ### Preference for small vs large reward options during free-choice trials
    ### Shift in preference compared to forced-choice training

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

  ## Import MedPC data, use file path "Analysis_Data/Magnitude_Task/Magnitude_Task_[session]/LK_Magnitude_[session]_[subject].txt", assign subject and session. 
  
    med_data <- rmedpc::import_medpc("Analysis_Data/Magnitude_Task/Magnitude_Task_D10/LK_Magnitude_D10_F2.txt") 
    
    subject <- as_tibble(unlist(med_data$Subject)); subject <- subject %>% rename(subject = value) %>% convert_as_factor(subject)
    
    session <- c("10"); session <- as_tibble(session); session <- session %>% rename(session = value) %>% convert_as_factor(session)
  
  ## Extract NP data for NP2 and NP4, select out total correct, incorrect and free choice numbers
  
    NP2_data <- as_tibble(unlist(med_data$D)); NP4_data <- as_tibble(unlist(med_data$F))
  
    NP2_correct <- slice(NP2_data, 501:501); NP2_incorrect <- slice(NP2_data, 1001:1001); NP2_freechoice <- slice(NP2_data, 1501:1501)
    
    NP4_correct <- slice(NP4_data, 1001:1001); NP4_incorrect <- slice(NP4_data, 501:501); NP4_freechoice <- slice(NP4_data, 1501:1501)
    
  ## Rename variables and combine into single data frame, make sure columns are in same order across subjects
    
    NP2_correct <- NP2_correct %>% rename(NP2_correct = value); NP2_incorrect <- NP2_incorrect %>% rename(NP2_incorrect = value); NP2_freechoice <- NP2_freechoice %>% rename(NP2_freechoice = value)
    
    NP4_correct <- NP4_correct %>% rename(NP4_correct = value); NP4_incorrect <- NP4_incorrect %>% rename(NP4_incorrect = value); NP4_freechoice <- NP4_freechoice %>% rename(NP4_freechoice = value) 
    
    master_data <- cbind(subject, session, NP2_correct, NP2_incorrect, NP2_freechoice, NP4_correct, NP4_incorrect, NP4_freechoice)   
    
    if (master_data$subject == "M3" || master_data$subject == "M9" || master_data$subject == "F8" || master_data$subject == "F9") {
      master_data <- master_data %>% rename(large_correct = NP2_correct); master_data <- master_data %>% rename(large_incorrect = NP2_incorrect); 
      master_data <- master_data %>% rename(large_freechoice = NP2_freechoice); master_data <- master_data %>% rename(small_correct = NP4_correct); 
      master_data <- master_data %>% rename(small_incorrect = NP4_incorrect); master_data <- master_data %>% rename(small_freechoice = NP4_freechoice)
    } else {
      master_data <- master_data %>% rename(small_correct = NP2_correct); master_data <- master_data %>% rename(small_incorrect = NP2_incorrect); 
      master_data <- master_data %>% rename(small_freechoice = NP2_freechoice); master_data <- master_data %>% rename(large_correct = NP4_correct); 
      master_data <- master_data %>% rename(large_incorrect = NP4_incorrect); master_data <- master_data %>% rename(large_freechoice = NP4_freechoice);
      master_data <- master_data[, c(1, 2, 6, 7, 8, 3, 4, 5)]
    }

# Export final data to .csv file ready to be used in final data analysis. Use form "Final_Data/[Task]/[Task]_[Session]/[Task]_Final_[Session]_[Subject].csv"
    
  write_csv(master_data, "Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F2.csv")

# -----------------------------------------------------------------------------------------------------
# RUN THIS LAST
# Combine individual data files into one data file for magnitude task 
  
  ## Load in all individual data files

    Magnitude_D1_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_M3.csv")
    Magnitude_D2_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_M3.csv")
    Magnitude_D3_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_M3.csv")
    Magnitude_D4_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_M3.csv")
    Magnitude_D5_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_M3.csv")
    Magnitude_D6_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_M3.csv")
    Magnitude_D7_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_M3.csv")
    Magnitude_D8_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_M3.csv")
    Magnitude_D9_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_M3.csv")
    Magnitude_D10_M3 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_M3.csv")
    
    Magnitude_D1_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_M4.csv")
    Magnitude_D2_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_M4.csv")
    Magnitude_D3_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_M4.csv")
    Magnitude_D4_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_M4.csv")
    Magnitude_D5_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_M4.csv")
    Magnitude_D6_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_M4.csv")
    Magnitude_D7_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_M4.csv")
    Magnitude_D8_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_M4.csv")
    Magnitude_D9_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_M4.csv")
    Magnitude_D10_M4 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_M4.csv")
    
    Magnitude_D1_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_M5.csv")
    Magnitude_D2_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_M5.csv")
    Magnitude_D3_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_M5.csv")
    Magnitude_D4_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_M5.csv")
    Magnitude_D5_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_M5.csv")
    Magnitude_D6_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_M5.csv")
    Magnitude_D7_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_M5.csv")
    Magnitude_D8_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_M5.csv")
    Magnitude_D9_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_M5.csv")
    Magnitude_D10_M5 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_M5.csv")
    
    Magnitude_D1_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_M9.csv")
    Magnitude_D2_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_M9.csv")
    Magnitude_D3_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_M9.csv")
    Magnitude_D4_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_M9.csv")
    Magnitude_D5_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_M9.csv")
    Magnitude_D6_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_M9.csv")
    Magnitude_D7_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_M9.csv")
    Magnitude_D8_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_M9.csv")
    Magnitude_D9_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_M9.csv")
    Magnitude_D10_M9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_M9.csv")
    
    Magnitude_D1_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_F7.csv")
    Magnitude_D2_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_F7.csv")
    Magnitude_D3_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_F7.csv")
    Magnitude_D4_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_F7.csv")
    Magnitude_D5_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_F7.csv")
    Magnitude_D6_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_F7.csv")
    Magnitude_D7_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_F7.csv")
    Magnitude_D8_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_F7.csv")
    Magnitude_D9_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_F7.csv")
    Magnitude_D10_F7 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F7.csv")
    
    Magnitude_D1_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_F8.csv")
    Magnitude_D2_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_F8.csv")
    Magnitude_D3_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_F8.csv")
    Magnitude_D4_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_F8.csv")
    Magnitude_D5_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_F8.csv")
    Magnitude_D6_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_F8.csv")
    Magnitude_D7_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_F8.csv")
    Magnitude_D8_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_F8.csv")
    Magnitude_D9_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_F8.csv")
    Magnitude_D10_F8 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F8.csv")
    
    Magnitude_D1_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_F9.csv")
    Magnitude_D2_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_F9.csv")
    Magnitude_D3_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_F9.csv")
    Magnitude_D4_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_F9.csv")
    Magnitude_D5_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_F9.csv")
    Magnitude_D6_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_F9.csv")
    Magnitude_D7_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_F9.csv")
    Magnitude_D8_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_F9.csv")
    Magnitude_D9_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_F9.csv")
    Magnitude_D10_F9 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F9.csv")
    
    Magnitude_D1_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_F10.csv")
    Magnitude_D2_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_F10.csv")
    Magnitude_D3_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_F10.csv")
    Magnitude_D4_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_F10.csv")
    Magnitude_D5_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_F10.csv")
    Magnitude_D6_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_F10.csv")
    Magnitude_D7_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_F10.csv")
    Magnitude_D8_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_F10.csv")
    Magnitude_D9_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_F10.csv")
    Magnitude_D10_F10 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F10.csv")
    
    Magnitude_D1_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D1/Magnitude_Task_Final_D1_F2.csv")
    Magnitude_D2_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D2/Magnitude_Task_Final_D2_F2.csv")
    Magnitude_D3_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D3/Magnitude_Task_Final_D3_F2.csv")
    Magnitude_D4_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D4/Magnitude_Task_Final_D4_F2.csv")
    Magnitude_D5_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D5/Magnitude_Task_Final_D5_F2.csv")
    Magnitude_D6_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D6/Magnitude_Task_Final_D6_F2.csv")
    Magnitude_D7_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D7/Magnitude_Task_Final_D7_F2.csv")
    Magnitude_D8_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D8/Magnitude_Task_Final_D8_F2.csv")
    Magnitude_D9_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D9/Magnitude_Task_Final_D9_F2.csv")
    Magnitude_D10_F2 <- read.csv("Final_Data/Magnitude_Task/Magnitude_Task_D10/Magnitude_Task_Final_D10_F2.csv")
    
  ## Combine into single data frame and export to master file
    
    magnitude_master <- bind_rows(Magnitude_D1_M3, Magnitude_D2_M3, Magnitude_D3_M3, Magnitude_D4_M3, Magnitude_D5_M3, Magnitude_D6_M3, Magnitude_D7_M3, Magnitude_D8_M3, Magnitude_D9_M3, Magnitude_D10_M3,
                                  Magnitude_D1_M4, Magnitude_D2_M4, Magnitude_D3_M4, Magnitude_D4_M4, Magnitude_D5_M4, Magnitude_D6_M4, Magnitude_D7_M4, Magnitude_D8_M4, Magnitude_D9_M4, Magnitude_D10_M4,
                                  Magnitude_D1_M5, Magnitude_D2_M5, Magnitude_D3_M5, Magnitude_D4_M5, Magnitude_D5_M5, Magnitude_D6_M5, Magnitude_D7_M5, Magnitude_D8_M5, Magnitude_D9_M5, Magnitude_D10_M5,
                                  Magnitude_D1_M9, Magnitude_D2_M9, Magnitude_D3_M9, Magnitude_D4_M9, Magnitude_D5_M9, Magnitude_D6_M9, Magnitude_D7_M9, Magnitude_D8_M9, Magnitude_D9_M9, Magnitude_D10_M9,
                                  Magnitude_D1_F7, Magnitude_D2_F7, Magnitude_D3_F7, Magnitude_D4_F7, Magnitude_D5_F7, Magnitude_D6_F7, Magnitude_D7_F7, Magnitude_D8_F7, Magnitude_D9_F7, Magnitude_D10_F7,
                                  Magnitude_D1_F8, Magnitude_D2_F8, Magnitude_D3_F8, Magnitude_D4_F8, Magnitude_D5_F8, Magnitude_D6_F8, Magnitude_D7_F8, Magnitude_D8_F8, Magnitude_D9_F8, Magnitude_D10_F8,
                                  Magnitude_D1_F9, Magnitude_D2_F9, Magnitude_D3_F9, Magnitude_D4_F9, Magnitude_D5_F9, Magnitude_D6_F9, Magnitude_D7_F9, Magnitude_D8_F9, Magnitude_D9_F9, Magnitude_D10_F9,
                                  Magnitude_D1_F10, Magnitude_D2_F10, Magnitude_D3_F10, Magnitude_D4_F10, Magnitude_D5_F10, Magnitude_D6_F10, Magnitude_D7_F10, Magnitude_D8_F10, Magnitude_D9_F10, Magnitude_D10_F10,
                                  Magnitude_D1_F2, Magnitude_D2_F2, Magnitude_D3_F2, Magnitude_D4_F2, Magnitude_D5_F2, Magnitude_D6_F2, Magnitude_D7_F2, Magnitude_D8_F2, Magnitude_D9_F2, Magnitude_D10_F2)
    
    write_csv(magnitude_master, "Final_Data/Magnitude_Task/Magnitude_Task_Combined.csv")

