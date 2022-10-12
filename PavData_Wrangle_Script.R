# PavTask Data Analysis

## Variables of interest: 
  ### E (Mag Entries): 1 = Total, 2:5001 = Time stamps, 5002:10001 = Duration
  ### G (Pellet Deliveries): 1 = Total, 2:1001 = Time stamps
  ### H (Tone 1, 1 pellet: 5Hz for v1, 2.5Hz for v2): 1 = Total, 2:201 = Time stamps
  ### I (Tone 2, 3 pellets: 2.5Hz for v1, 5Hz for v2): 1 = Total, 2:201 = Time stamps

#--------------------------------------------------------------------------------------
# EXPORT DATA (RUN THIS LINE LAST)
  
  ## Export final data to .csv file ready to be used in final data analysis. Use form "Final_Data/[Task]/[Session]/Pav_Task_Final_[Session]_[Subject].csv"
  
      # write_csv(master_data, "Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F2.csv")

    write_csv(master2_data, "Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_M4.csv")

#--------------------------------------------------------------------------------------  

# Setup
  
setwd("~/Desktop/Uni/Honours/FP_MedPC_Data")

library(tidyverse)
library(papaja)
library(fluoR)
library(rmedpc)
library(ez)
library(ggpubr)
library(rstatix)

#--------------

# Data Wrangling (must first separate MedPC data by subject and session)

  ## Import MedPC data, use file path "PavTask_[session]/LK_PavTask_[session]_[subject].txt", assign subject, session and script. 

    med_data <- rmedpc::import_medpc("Analysis_Data/Pav_Task/PavTask_D2/LK_PavTask_D2_M4.txt")

    subject <- as_tibble(unlist(med_data$Subject)); subject <- subject %>% rename(subject = value) #%>% convert_as_factor(subject)

    session <- c("2"); session <- as_tibble(session); session <- session %>% rename(session = value) #%>% convert_as_factor(session)

    ## Mag Entry Data (Early)

    ### Select variable of interest and rename into interpretable name.

    mag_data <- as_tibble(unlist(med_data$E))

    mag_data <- rename(mag_data, ME = value)

    ### Slice data into ME time stamps and durations, rename and combine back together. Also remove split columns

    mag_data <- slice(mag_data, 2:2501); mag_data <- rename(mag_data, ME_timestamp = ME); mag_data <- na_if(mag_data, 0)

    ### Remove NA values

    mag_data <- na.omit(mag_data); mag_data <- arrange(mag_data, ME_timestamp)
    
  # ## Mag Entry Data (Late)
  #  
  #   ### Select variable of interest and rename into interpretable name.
  #   
  #     mag_data <- as_tibble(unlist(med_data$E))
  # 
  #     mag_data <- rename(mag_data, ME = value)
  #   
  #   ### Slice data into ME time stamps and durations, rename and combine back together. Also remove split columns
  #   
  #     mag_entry <- slice(mag_data, 2:5001); mag_entry <- rename(mag_entry, ME_timestamp = ME)
  #     
  #     mag_duration <- slice(mag_data, 5001:10000); mag_duration <- rename(mag_duration, ME_duration = ME)
  #     
  #     mag_data <- cbind(mag_entry, mag_duration); remove(mag_entry, mag_duration)
  #     
  #   ### Remove NA values
  #    
  #     mag_data <- na_if(mag_data, 0); mag_data <- na.omit(mag_data)


  ## Cue Data
  
    ### Select variables of interest, rename, remove first cell and empty cells
    
      CS_1P_data <- as_tibble(unlist(med_data$H)); CS_1P_data <- rename(CS_1P_data, CS_1P = value); CS_1P_data <- slice(CS_1P_data, 2:26)
      
      CS_3P_data <- as_tibble(unlist(med_data$I)); CS_3P_data <- rename(CS_3P_data, CS_3P = value); CS_3P_data <- slice(CS_3P_data, 2:26)
      
      CS_data <- cbind(CS_1P_data, CS_3P_data)
      
      CS_data <- as_tibble(CS_data)
  
    ### Transform into long format and arrange in chronological order
    
      CS_data <- pivot_longer(CS_data, cols = c(CS_1P, CS_3P), names_to = "CS_type", values_to = "CS_ON")
      
      CS_data <- arrange(CS_data, CS_type)
    
    ### Create new variables representing the bounds of the "% change in ME" window

      CS_data <- CS_data %>% mutate(CS_data, pre_CS = CS_ON - 5) %>%
        mutate(CS_data, CS_OFF = CS_ON + 5) %>% mutate(CS_data, post_CS = CS_OFF +5)
  
  ## % Change in ME from pre-cue to during cue
    
    ### Categorise MEs as to have occurred in pre-CS period, during CS, or post-CS period
    
      mag_data <- mag_data %>% mutate(mag_data, pre_post = 
                                        case_when((mag_data$ME_timestamp >= CS_data$pre_CS[1] & mag_data$ME_timestamp <= CS_data$CS_ON[1])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[2] & mag_data$ME_timestamp <= CS_data$CS_ON[2])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[3] & mag_data$ME_timestamp <= CS_data$CS_ON[3])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[4] & mag_data$ME_timestamp <= CS_data$CS_ON[4])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[5] & mag_data$ME_timestamp <= CS_data$CS_ON[5])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[6] & mag_data$ME_timestamp <= CS_data$CS_ON[6])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[7] & mag_data$ME_timestamp <= CS_data$CS_ON[7])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[8] & mag_data$ME_timestamp <= CS_data$CS_ON[8])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[9] & mag_data$ME_timestamp <= CS_data$CS_ON[9])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[10] & mag_data$ME_timestamp <= CS_data$CS_ON[10])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[11] & mag_data$ME_timestamp <= CS_data$CS_ON[11])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[12] & mag_data$ME_timestamp <= CS_data$CS_ON[12])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[13] & mag_data$ME_timestamp <= CS_data$CS_ON[13])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[14] & mag_data$ME_timestamp <= CS_data$CS_ON[14])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[15] & mag_data$ME_timestamp <= CS_data$CS_ON[15])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[16] & mag_data$ME_timestamp <= CS_data$CS_ON[16])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[17] & mag_data$ME_timestamp <= CS_data$CS_ON[17])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[18] & mag_data$ME_timestamp <= CS_data$CS_ON[18])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[19] & mag_data$ME_timestamp <= CS_data$CS_ON[19])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[20] & mag_data$ME_timestamp <= CS_data$CS_ON[20])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[21] & mag_data$ME_timestamp <= CS_data$CS_ON[21])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[22] & mag_data$ME_timestamp <= CS_data$CS_ON[22])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[23] & mag_data$ME_timestamp <= CS_data$CS_ON[23])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[24] & mag_data$ME_timestamp <= CS_data$CS_ON[24])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[25] & mag_data$ME_timestamp <= CS_data$CS_ON[25])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[26] & mag_data$ME_timestamp <= CS_data$CS_ON[26])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[27] & mag_data$ME_timestamp <= CS_data$CS_ON[27])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[28] & mag_data$ME_timestamp <= CS_data$CS_ON[28])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[29] & mag_data$ME_timestamp <= CS_data$CS_ON[29])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[30] & mag_data$ME_timestamp <= CS_data$CS_ON[30])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[31] & mag_data$ME_timestamp <= CS_data$CS_ON[31])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[32] & mag_data$ME_timestamp <= CS_data$CS_ON[32])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[33] & mag_data$ME_timestamp <= CS_data$CS_ON[33])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[34] & mag_data$ME_timestamp <= CS_data$CS_ON[34])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[35] & mag_data$ME_timestamp <= CS_data$CS_ON[35])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[36] & mag_data$ME_timestamp <= CS_data$CS_ON[36])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[37] & mag_data$ME_timestamp <= CS_data$CS_ON[37])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[38] & mag_data$ME_timestamp <= CS_data$CS_ON[38])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[39] & mag_data$ME_timestamp <= CS_data$CS_ON[39])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[40] & mag_data$ME_timestamp <= CS_data$CS_ON[40])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[41] & mag_data$ME_timestamp <= CS_data$CS_ON[41])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[42] & mag_data$ME_timestamp <= CS_data$CS_ON[42])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[43] & mag_data$ME_timestamp <= CS_data$CS_ON[43])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[44] & mag_data$ME_timestamp <= CS_data$CS_ON[44])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[45] & mag_data$ME_timestamp <= CS_data$CS_ON[45])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[46] & mag_data$ME_timestamp <= CS_data$CS_ON[46])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[47] & mag_data$ME_timestamp <= CS_data$CS_ON[47])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[48] & mag_data$ME_timestamp <= CS_data$CS_ON[48])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[49] & mag_data$ME_timestamp <= CS_data$CS_ON[49])|
                                                  (mag_data$ME_timestamp >= CS_data$pre_CS[50] & mag_data$ME_timestamp <= CS_data$CS_ON[50]) ~ 1,
                                                  
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[1] & mag_data$ME_timestamp <= CS_data$CS_OFF[1])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[2] & mag_data$ME_timestamp <= CS_data$CS_OFF[2])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[3] & mag_data$ME_timestamp <= CS_data$CS_OFF[3])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[4] & mag_data$ME_timestamp <= CS_data$CS_OFF[4])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[5] & mag_data$ME_timestamp <= CS_data$CS_OFF[5])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[6] & mag_data$ME_timestamp <= CS_data$CS_OFF[6])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[7] & mag_data$ME_timestamp <= CS_data$CS_OFF[7])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[8] & mag_data$ME_timestamp <= CS_data$CS_OFF[8])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[9] & mag_data$ME_timestamp <= CS_data$CS_OFF[9])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[10] & mag_data$ME_timestamp <= CS_data$CS_OFF[10])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[11] & mag_data$ME_timestamp <= CS_data$CS_OFF[11])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[12] & mag_data$ME_timestamp <= CS_data$CS_OFF[12])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[13] & mag_data$ME_timestamp <= CS_data$CS_OFF[13])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[14] & mag_data$ME_timestamp <= CS_data$CS_OFF[14])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[15] & mag_data$ME_timestamp <= CS_data$CS_OFF[15])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[16] & mag_data$ME_timestamp <= CS_data$CS_OFF[16])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[17] & mag_data$ME_timestamp <= CS_data$CS_OFF[17])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[18] & mag_data$ME_timestamp <= CS_data$CS_OFF[18])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[19] & mag_data$ME_timestamp <= CS_data$CS_OFF[19])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[20] & mag_data$ME_timestamp <= CS_data$CS_OFF[20])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[21] & mag_data$ME_timestamp <= CS_data$CS_OFF[21])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[22] & mag_data$ME_timestamp <= CS_data$CS_OFF[22])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[23] & mag_data$ME_timestamp <= CS_data$CS_OFF[23])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[24] & mag_data$ME_timestamp <= CS_data$CS_OFF[24])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[25] & mag_data$ME_timestamp <= CS_data$CS_OFF[25])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[26] & mag_data$ME_timestamp <= CS_data$CS_OFF[26])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[27] & mag_data$ME_timestamp <= CS_data$CS_OFF[27])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[28] & mag_data$ME_timestamp <= CS_data$CS_OFF[28])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[29] & mag_data$ME_timestamp <= CS_data$CS_OFF[29])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[30] & mag_data$ME_timestamp <= CS_data$CS_OFF[30])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[31] & mag_data$ME_timestamp <= CS_data$CS_OFF[31])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[32] & mag_data$ME_timestamp <= CS_data$CS_OFF[32])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[33] & mag_data$ME_timestamp <= CS_data$CS_OFF[33])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[34] & mag_data$ME_timestamp <= CS_data$CS_OFF[34])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[35] & mag_data$ME_timestamp <= CS_data$CS_OFF[35])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[36] & mag_data$ME_timestamp <= CS_data$CS_OFF[36])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[37] & mag_data$ME_timestamp <= CS_data$CS_OFF[37])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[38] & mag_data$ME_timestamp <= CS_data$CS_OFF[38])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[39] & mag_data$ME_timestamp <= CS_data$CS_OFF[39])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[40] & mag_data$ME_timestamp <= CS_data$CS_OFF[40])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[41] & mag_data$ME_timestamp <= CS_data$CS_OFF[41])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[42] & mag_data$ME_timestamp <= CS_data$CS_OFF[42])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[43] & mag_data$ME_timestamp <= CS_data$CS_OFF[43])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[44] & mag_data$ME_timestamp <= CS_data$CS_OFF[44])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[45] & mag_data$ME_timestamp <= CS_data$CS_OFF[45])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[46] & mag_data$ME_timestamp <= CS_data$CS_OFF[46])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[47] & mag_data$ME_timestamp <= CS_data$CS_OFF[47])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[48] & mag_data$ME_timestamp <= CS_data$CS_OFF[48])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[49] & mag_data$ME_timestamp <= CS_data$CS_OFF[49])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_ON[50] & mag_data$ME_timestamp <= CS_data$CS_OFF[50]) ~ 2,
                                                  
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[1] & mag_data$ME_timestamp <= CS_data$post_CS[1])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[2] & mag_data$ME_timestamp <= CS_data$post_CS[2])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[3] & mag_data$ME_timestamp <= CS_data$post_CS[3])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[4] & mag_data$ME_timestamp <= CS_data$post_CS[4])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[5] & mag_data$ME_timestamp <= CS_data$post_CS[5])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[6] & mag_data$ME_timestamp <= CS_data$post_CS[6])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[7] & mag_data$ME_timestamp <= CS_data$post_CS[7])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[8] & mag_data$ME_timestamp <= CS_data$post_CS[8])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[9] & mag_data$ME_timestamp <= CS_data$post_CS[9])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[10] & mag_data$ME_timestamp <= CS_data$post_CS[10])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[11] & mag_data$ME_timestamp <= CS_data$post_CS[11])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[12] & mag_data$ME_timestamp <= CS_data$post_CS[12])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[13] & mag_data$ME_timestamp <= CS_data$post_CS[13])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[14] & mag_data$ME_timestamp <= CS_data$post_CS[14])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[15] & mag_data$ME_timestamp <= CS_data$post_CS[15])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[16] & mag_data$ME_timestamp <= CS_data$post_CS[16])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[17] & mag_data$ME_timestamp <= CS_data$post_CS[17])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[18] & mag_data$ME_timestamp <= CS_data$post_CS[18])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[19] & mag_data$ME_timestamp <= CS_data$post_CS[19])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[20] & mag_data$ME_timestamp <= CS_data$post_CS[20])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[21] & mag_data$ME_timestamp <= CS_data$post_CS[21])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[22] & mag_data$ME_timestamp <= CS_data$post_CS[22])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[23] & mag_data$ME_timestamp <= CS_data$post_CS[23])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[24] & mag_data$ME_timestamp <= CS_data$post_CS[24])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[25] & mag_data$ME_timestamp <= CS_data$post_CS[25])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[26] & mag_data$ME_timestamp <= CS_data$post_CS[26])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[27] & mag_data$ME_timestamp <= CS_data$post_CS[27])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[28] & mag_data$ME_timestamp <= CS_data$post_CS[28])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[29] & mag_data$ME_timestamp <= CS_data$post_CS[29])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[30] & mag_data$ME_timestamp <= CS_data$post_CS[30])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[31] & mag_data$ME_timestamp <= CS_data$post_CS[31])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[32] & mag_data$ME_timestamp <= CS_data$post_CS[32])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[33] & mag_data$ME_timestamp <= CS_data$post_CS[33])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[34] & mag_data$ME_timestamp <= CS_data$post_CS[34])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[35] & mag_data$ME_timestamp <= CS_data$post_CS[35])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[36] & mag_data$ME_timestamp <= CS_data$post_CS[36])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[37] & mag_data$ME_timestamp <= CS_data$post_CS[37])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[38] & mag_data$ME_timestamp <= CS_data$post_CS[38])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[39] & mag_data$ME_timestamp <= CS_data$post_CS[39])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[40] & mag_data$ME_timestamp <= CS_data$post_CS[40])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[41] & mag_data$ME_timestamp <= CS_data$post_CS[41])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[42] & mag_data$ME_timestamp <= CS_data$post_CS[42])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[43] & mag_data$ME_timestamp <= CS_data$post_CS[43])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[44] & mag_data$ME_timestamp <= CS_data$post_CS[44])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[45] & mag_data$ME_timestamp <= CS_data$post_CS[45])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[46] & mag_data$ME_timestamp <= CS_data$post_CS[46])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[47] & mag_data$ME_timestamp <= CS_data$post_CS[47])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[48] & mag_data$ME_timestamp <= CS_data$post_CS[48])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[49] & mag_data$ME_timestamp <= CS_data$post_CS[49])|
                                                  (mag_data$ME_timestamp >= CS_data$CS_OFF[50] & mag_data$ME_timestamp <= CS_data$post_CS[50]) ~ 3,
                                                   TRUE ~ NA_real_)
                                      )
                   
      ### Further categorise MEs as to have occurred in during Tone 1 (CS_1P) or Tone 2 (CS_3P)
      
      mag_data <- mag_data %>% mutate(mag_data, CS_type = 
                                        case_when((mag_data$ME_timestamp >= CS_data$pre_CS[1] & mag_data$ME_timestamp <= CS_data$CS_ON[1])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[2] & mag_data$ME_timestamp <= CS_data$CS_ON[2])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[3] & mag_data$ME_timestamp <= CS_data$CS_ON[3])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[4] & mag_data$ME_timestamp <= CS_data$CS_ON[4])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[5] & mag_data$ME_timestamp <= CS_data$CS_ON[5])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[6] & mag_data$ME_timestamp <= CS_data$CS_ON[6])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[7] & mag_data$ME_timestamp <= CS_data$CS_ON[7])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[8] & mag_data$ME_timestamp <= CS_data$CS_ON[8])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[9] & mag_data$ME_timestamp <= CS_data$CS_ON[9])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[10] & mag_data$ME_timestamp <= CS_data$CS_ON[10])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[11] & mag_data$ME_timestamp <= CS_data$CS_ON[11])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[12] & mag_data$ME_timestamp <= CS_data$CS_ON[12])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[13] & mag_data$ME_timestamp <= CS_data$CS_ON[13])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[14] & mag_data$ME_timestamp <= CS_data$CS_ON[14])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[15] & mag_data$ME_timestamp <= CS_data$CS_ON[15])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[16] & mag_data$ME_timestamp <= CS_data$CS_ON[16])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[17] & mag_data$ME_timestamp <= CS_data$CS_ON[17])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[18] & mag_data$ME_timestamp <= CS_data$CS_ON[18])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[19] & mag_data$ME_timestamp <= CS_data$CS_ON[19])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[20] & mag_data$ME_timestamp <= CS_data$CS_ON[20])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[21] & mag_data$ME_timestamp <= CS_data$CS_ON[21])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[22] & mag_data$ME_timestamp <= CS_data$CS_ON[22])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[23] & mag_data$ME_timestamp <= CS_data$CS_ON[23])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[24] & mag_data$ME_timestamp <= CS_data$CS_ON[24])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[25] & mag_data$ME_timestamp <= CS_data$CS_ON[25])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[1] & mag_data$ME_timestamp <= CS_data$CS_OFF[1])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[2] & mag_data$ME_timestamp <= CS_data$CS_OFF[2])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[3] & mag_data$ME_timestamp <= CS_data$CS_OFF[3])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[4] & mag_data$ME_timestamp <= CS_data$CS_OFF[4])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[5] & mag_data$ME_timestamp <= CS_data$CS_OFF[5])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[6] & mag_data$ME_timestamp <= CS_data$CS_OFF[6])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[7] & mag_data$ME_timestamp <= CS_data$CS_OFF[7])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[8] & mag_data$ME_timestamp <= CS_data$CS_OFF[8])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[9] & mag_data$ME_timestamp <= CS_data$CS_OFF[9])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[10] & mag_data$ME_timestamp <= CS_data$CS_OFF[10])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[11] & mag_data$ME_timestamp <= CS_data$CS_OFF[11])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[12] & mag_data$ME_timestamp <= CS_data$CS_OFF[12])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[13] & mag_data$ME_timestamp <= CS_data$CS_OFF[13])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[14] & mag_data$ME_timestamp <= CS_data$CS_OFF[14])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[15] & mag_data$ME_timestamp <= CS_data$CS_OFF[15])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[16] & mag_data$ME_timestamp <= CS_data$CS_OFF[16])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[17] & mag_data$ME_timestamp <= CS_data$CS_OFF[17])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[18] & mag_data$ME_timestamp <= CS_data$CS_OFF[18])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[19] & mag_data$ME_timestamp <= CS_data$CS_OFF[19])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[20] & mag_data$ME_timestamp <= CS_data$CS_OFF[20])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[21] & mag_data$ME_timestamp <= CS_data$CS_OFF[21])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[22] & mag_data$ME_timestamp <= CS_data$CS_OFF[22])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[23] & mag_data$ME_timestamp <= CS_data$CS_OFF[23])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[24] & mag_data$ME_timestamp <= CS_data$CS_OFF[24])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[25] & mag_data$ME_timestamp <= CS_data$CS_OFF[25])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[1] & mag_data$ME_timestamp <= CS_data$post_CS[1])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[2] & mag_data$ME_timestamp <= CS_data$post_CS[2])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[3] & mag_data$ME_timestamp <= CS_data$post_CS[3])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[4] & mag_data$ME_timestamp <= CS_data$post_CS[4])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[5] & mag_data$ME_timestamp <= CS_data$post_CS[5])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[6] & mag_data$ME_timestamp <= CS_data$post_CS[6])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[7] & mag_data$ME_timestamp <= CS_data$post_CS[7])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[8] & mag_data$ME_timestamp <= CS_data$post_CS[8])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[9] & mag_data$ME_timestamp <= CS_data$post_CS[9])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[10] & mag_data$ME_timestamp <= CS_data$post_CS[10])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[11] & mag_data$ME_timestamp <= CS_data$post_CS[11])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[12] & mag_data$ME_timestamp <= CS_data$post_CS[12])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[13] & mag_data$ME_timestamp <= CS_data$post_CS[13])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[14] & mag_data$ME_timestamp <= CS_data$post_CS[14])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[15] & mag_data$ME_timestamp <= CS_data$post_CS[15])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[16] & mag_data$ME_timestamp <= CS_data$post_CS[16])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[17] & mag_data$ME_timestamp <= CS_data$post_CS[17])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[18] & mag_data$ME_timestamp <= CS_data$post_CS[18])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[19] & mag_data$ME_timestamp <= CS_data$post_CS[19])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[20] & mag_data$ME_timestamp <= CS_data$post_CS[20])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[21] & mag_data$ME_timestamp <= CS_data$post_CS[21])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[22] & mag_data$ME_timestamp <= CS_data$post_CS[22])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[23] & mag_data$ME_timestamp <= CS_data$post_CS[23])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[24] & mag_data$ME_timestamp <= CS_data$post_CS[24])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[25] & mag_data$ME_timestamp <= CS_data$post_CS[25]) ~ 1,
                                                
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[26] & mag_data$ME_timestamp <= CS_data$CS_ON[26])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[27] & mag_data$ME_timestamp <= CS_data$CS_ON[27])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[28] & mag_data$ME_timestamp <= CS_data$CS_ON[28])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[29] & mag_data$ME_timestamp <= CS_data$CS_ON[29])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[30] & mag_data$ME_timestamp <= CS_data$CS_ON[30])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[31] & mag_data$ME_timestamp <= CS_data$CS_ON[31])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[32] & mag_data$ME_timestamp <= CS_data$CS_ON[32])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[33] & mag_data$ME_timestamp <= CS_data$CS_ON[33])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[34] & mag_data$ME_timestamp <= CS_data$CS_ON[34])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[35] & mag_data$ME_timestamp <= CS_data$CS_ON[35])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[36] & mag_data$ME_timestamp <= CS_data$CS_ON[36])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[37] & mag_data$ME_timestamp <= CS_data$CS_ON[37])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[38] & mag_data$ME_timestamp <= CS_data$CS_ON[38])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[39] & mag_data$ME_timestamp <= CS_data$CS_ON[39])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[40] & mag_data$ME_timestamp <= CS_data$CS_ON[40])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[41] & mag_data$ME_timestamp <= CS_data$CS_ON[41])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[42] & mag_data$ME_timestamp <= CS_data$CS_ON[42])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[43] & mag_data$ME_timestamp <= CS_data$CS_ON[43])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[44] & mag_data$ME_timestamp <= CS_data$CS_ON[44])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[45] & mag_data$ME_timestamp <= CS_data$CS_ON[45])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[46] & mag_data$ME_timestamp <= CS_data$CS_ON[46])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[47] & mag_data$ME_timestamp <= CS_data$CS_ON[47])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[48] & mag_data$ME_timestamp <= CS_data$CS_ON[48])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[49] & mag_data$ME_timestamp <= CS_data$CS_ON[49])|
                                                    (mag_data$ME_timestamp >= CS_data$pre_CS[50] & mag_data$ME_timestamp <= CS_data$CS_ON[50])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[26] & mag_data$ME_timestamp <= CS_data$CS_OFF[26])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[27] & mag_data$ME_timestamp <= CS_data$CS_OFF[27])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[28] & mag_data$ME_timestamp <= CS_data$CS_OFF[28])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[29] & mag_data$ME_timestamp <= CS_data$CS_OFF[29])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[30] & mag_data$ME_timestamp <= CS_data$CS_OFF[30])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[31] & mag_data$ME_timestamp <= CS_data$CS_OFF[31])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[32] & mag_data$ME_timestamp <= CS_data$CS_OFF[32])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[33] & mag_data$ME_timestamp <= CS_data$CS_OFF[33])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[34] & mag_data$ME_timestamp <= CS_data$CS_OFF[34])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[35] & mag_data$ME_timestamp <= CS_data$CS_OFF[35])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[36] & mag_data$ME_timestamp <= CS_data$CS_OFF[36])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[37] & mag_data$ME_timestamp <= CS_data$CS_OFF[37])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[38] & mag_data$ME_timestamp <= CS_data$CS_OFF[38])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[39] & mag_data$ME_timestamp <= CS_data$CS_OFF[39])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[40] & mag_data$ME_timestamp <= CS_data$CS_OFF[40])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[41] & mag_data$ME_timestamp <= CS_data$CS_OFF[41])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[42] & mag_data$ME_timestamp <= CS_data$CS_OFF[42])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[43] & mag_data$ME_timestamp <= CS_data$CS_OFF[43])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[44] & mag_data$ME_timestamp <= CS_data$CS_OFF[44])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[45] & mag_data$ME_timestamp <= CS_data$CS_OFF[45])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[46] & mag_data$ME_timestamp <= CS_data$CS_OFF[46])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[47] & mag_data$ME_timestamp <= CS_data$CS_OFF[47])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[48] & mag_data$ME_timestamp <= CS_data$CS_OFF[48])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[49] & mag_data$ME_timestamp <= CS_data$CS_OFF[49])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_ON[50] & mag_data$ME_timestamp <= CS_data$CS_OFF[50])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[26] & mag_data$ME_timestamp <= CS_data$post_CS[26])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[27] & mag_data$ME_timestamp <= CS_data$post_CS[27])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[28] & mag_data$ME_timestamp <= CS_data$post_CS[28])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[29] & mag_data$ME_timestamp <= CS_data$post_CS[29])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[30] & mag_data$ME_timestamp <= CS_data$post_CS[30])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[31] & mag_data$ME_timestamp <= CS_data$post_CS[31])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[32] & mag_data$ME_timestamp <= CS_data$post_CS[32])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[33] & mag_data$ME_timestamp <= CS_data$post_CS[33])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[34] & mag_data$ME_timestamp <= CS_data$post_CS[34])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[35] & mag_data$ME_timestamp <= CS_data$post_CS[35])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[36] & mag_data$ME_timestamp <= CS_data$post_CS[36])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[37] & mag_data$ME_timestamp <= CS_data$post_CS[37])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[38] & mag_data$ME_timestamp <= CS_data$post_CS[38])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[39] & mag_data$ME_timestamp <= CS_data$post_CS[39])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[40] & mag_data$ME_timestamp <= CS_data$post_CS[40])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[41] & mag_data$ME_timestamp <= CS_data$post_CS[41])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[42] & mag_data$ME_timestamp <= CS_data$post_CS[42])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[43] & mag_data$ME_timestamp <= CS_data$post_CS[43])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[44] & mag_data$ME_timestamp <= CS_data$post_CS[44])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[45] & mag_data$ME_timestamp <= CS_data$post_CS[45])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[46] & mag_data$ME_timestamp <= CS_data$post_CS[46])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[47] & mag_data$ME_timestamp <= CS_data$post_CS[47])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[48] & mag_data$ME_timestamp <= CS_data$post_CS[48])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[49] & mag_data$ME_timestamp <= CS_data$post_CS[49])|
                                                    (mag_data$ME_timestamp >= CS_data$CS_OFF[50] & mag_data$ME_timestamp <= CS_data$post_CS[50]) ~ 2,
                                                    
                                                    TRUE ~ NA_real_))
                                 
      ### Remove NA values and turn pre_post & CS_type into factors 
    
        mag_data <- na.omit(mag_data)
      
        mag_data$pre_post <- factor(mag_data$pre_post, levels = c(1, 2, 3), labels = c("Pre_CS", "During_CS", "Post_CS"))
        
        mag_data$CS_type <- factor(mag_data$CS_type, levels = c(1, 2), labels = c("CS_1P", "CS_3P"))
        
        mag_data <- mag_data %>% mutate(CS_time = 
                                          case_when((mag_data$ME_timestamp >= CS_data$CS_ON[1] & mag_data$ME_timestamp <= CS_data$CS_OFF[1]) ~ CS_data$CS_ON[1],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[2] & mag_data$ME_timestamp <= CS_data$CS_OFF[2]) ~ CS_data$CS_ON[2],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[3] & mag_data$ME_timestamp <= CS_data$CS_OFF[3]) ~ CS_data$CS_ON[3],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[4] & mag_data$ME_timestamp <= CS_data$CS_OFF[4]) ~ CS_data$CS_ON[4],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[5] & mag_data$ME_timestamp <= CS_data$CS_OFF[5]) ~ CS_data$CS_ON[5],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[6] & mag_data$ME_timestamp <= CS_data$CS_OFF[6]) ~ CS_data$CS_ON[6],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[7] & mag_data$ME_timestamp <= CS_data$CS_OFF[7]) ~ CS_data$CS_ON[7],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[8] & mag_data$ME_timestamp <= CS_data$CS_OFF[8]) ~ CS_data$CS_ON[8],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[9] & mag_data$ME_timestamp <= CS_data$CS_OFF[9]) ~ CS_data$CS_ON[9],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[10] & mag_data$ME_timestamp <= CS_data$CS_OFF[10]) ~ CS_data$CS_ON[10],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[11] & mag_data$ME_timestamp <= CS_data$CS_OFF[11]) ~ CS_data$CS_ON[11],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[12] & mag_data$ME_timestamp <= CS_data$CS_OFF[12]) ~ CS_data$CS_ON[12],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[13] & mag_data$ME_timestamp <= CS_data$CS_OFF[13]) ~ CS_data$CS_ON[13],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[14] & mag_data$ME_timestamp <= CS_data$CS_OFF[14]) ~ CS_data$CS_ON[14],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[15] & mag_data$ME_timestamp <= CS_data$CS_OFF[15]) ~ CS_data$CS_ON[15], 
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[16] & mag_data$ME_timestamp <= CS_data$CS_OFF[16]) ~ CS_data$CS_ON[16],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[17] & mag_data$ME_timestamp <= CS_data$CS_OFF[17]) ~ CS_data$CS_ON[17],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[18] & mag_data$ME_timestamp <= CS_data$CS_OFF[18]) ~ CS_data$CS_ON[18],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[19] & mag_data$ME_timestamp <= CS_data$CS_OFF[19]) ~ CS_data$CS_ON[19],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[20] & mag_data$ME_timestamp <= CS_data$CS_OFF[20]) ~ CS_data$CS_ON[20],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[21] & mag_data$ME_timestamp <= CS_data$CS_OFF[21]) ~ CS_data$CS_ON[21],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[22] & mag_data$ME_timestamp <= CS_data$CS_OFF[22]) ~ CS_data$CS_ON[22],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[23] & mag_data$ME_timestamp <= CS_data$CS_OFF[23]) ~ CS_data$CS_ON[23],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[24] & mag_data$ME_timestamp <= CS_data$CS_OFF[24]) ~ CS_data$CS_ON[24],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[25] & mag_data$ME_timestamp <= CS_data$CS_OFF[25]) ~ CS_data$CS_ON[25],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[26] & mag_data$ME_timestamp <= CS_data$CS_OFF[26]) ~ CS_data$CS_ON[26],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[27] & mag_data$ME_timestamp <= CS_data$CS_OFF[27]) ~ CS_data$CS_ON[27],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[28] & mag_data$ME_timestamp <= CS_data$CS_OFF[28]) ~ CS_data$CS_ON[28],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[29] & mag_data$ME_timestamp <= CS_data$CS_OFF[29]) ~ CS_data$CS_ON[29],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[30] & mag_data$ME_timestamp <= CS_data$CS_OFF[30]) ~ CS_data$CS_ON[30],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[31] & mag_data$ME_timestamp <= CS_data$CS_OFF[31]) ~ CS_data$CS_ON[31],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[32] & mag_data$ME_timestamp <= CS_data$CS_OFF[32]) ~ CS_data$CS_ON[32],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[33] & mag_data$ME_timestamp <= CS_data$CS_OFF[33]) ~ CS_data$CS_ON[33],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[34] & mag_data$ME_timestamp <= CS_data$CS_OFF[34]) ~ CS_data$CS_ON[34],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[35] & mag_data$ME_timestamp <= CS_data$CS_OFF[35]) ~ CS_data$CS_ON[35],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[36] & mag_data$ME_timestamp <= CS_data$CS_OFF[36]) ~ CS_data$CS_ON[36],                                                      
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[37] & mag_data$ME_timestamp <= CS_data$CS_OFF[37]) ~ CS_data$CS_ON[37],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[38] & mag_data$ME_timestamp <= CS_data$CS_OFF[38]) ~ CS_data$CS_ON[38],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[39] & mag_data$ME_timestamp <= CS_data$CS_OFF[39]) ~ CS_data$CS_ON[39],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[40] & mag_data$ME_timestamp <= CS_data$CS_OFF[40]) ~ CS_data$CS_ON[40],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[41] & mag_data$ME_timestamp <= CS_data$CS_OFF[41]) ~ CS_data$CS_ON[41],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[42] & mag_data$ME_timestamp <= CS_data$CS_OFF[42]) ~ CS_data$CS_ON[42],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[43] & mag_data$ME_timestamp <= CS_data$CS_OFF[43]) ~ CS_data$CS_ON[43],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[44] & mag_data$ME_timestamp <= CS_data$CS_OFF[44]) ~ CS_data$CS_ON[44],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[45] & mag_data$ME_timestamp <= CS_data$CS_OFF[45]) ~ CS_data$CS_ON[45],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[46] & mag_data$ME_timestamp <= CS_data$CS_OFF[46]) ~ CS_data$CS_ON[46],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[47] & mag_data$ME_timestamp <= CS_data$CS_OFF[47]) ~ CS_data$CS_ON[47],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[48] & mag_data$ME_timestamp <= CS_data$CS_OFF[48]) ~ CS_data$CS_ON[48],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[49] & mag_data$ME_timestamp <= CS_data$CS_OFF[49]) ~ CS_data$CS_ON[49],
                                                      (mag_data$ME_timestamp >= CS_data$CS_ON[50] & mag_data$ME_timestamp <= CS_data$CS_OFF[50]) ~ CS_data$CS_ON[50],
                                                      TRUE ~ NA_real_))

      
  ## Summarise conditioned responding data, ready for entry into master data frame
        
        master_data <- mag_data %>% group_by(CS_type) %>% count(pre_post)
        
        master_data <- as.tibble(master_data)
        
        ME_duration_sum <- mag_data %>% group_by(CS_type, pre_post) %>% summarise(ME_duration_sum = sum(ME_duration))
        
        ME_duration_sum <- ME_duration_sum %>% mutate(adj_time = 125 - ME_duration_sum)
        
        master_data$subject[1:nrow(master_data)] <- subject$subject[1]
        
        master_data$session[1:nrow(master_data)] <- session$session[1]
        
        master_data <- master_data[, c(4, 5, 1, 2, 3)]
        
        master_data <- master_data %>% convert_as_factor(subject)
        
        master_data <- master_data %>% convert_as_factor(session)
        
        master2_data <- cbind(master_data, ME_duration_sum$ME_duration_sum, ME_duration_sum$adj_time)
        
        master2_data <- master2_data %>% rename(ME_duration_sum = `ME_duration_sum$ME_duration_sum`)
        
        master2_data <- master2_data %>% rename(adj_time = `ME_duration_sum$adj_time`)
        

#-----Combined Data Analysis------

# Combined Data Analysis
        
  ## Load all data files into data frames. Data filed separated by subject and session.

                  Pav_M3_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_M3.csv")
                  Pav_M3_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_M3.csv")
                  Pav_M3_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_M3.csv")
                  Pav_M3_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_M3.csv")
                  Pav_M3_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_M3.csv")
                  Pav_M3_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_M3.csv")
                  Pav_M3_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_M3.csv")
                  Pav_M3_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_M3.csv")
                  Pav_M3_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_M3.csv")
                  Pav_M3_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_M3.csv")
                  
                  Pav_M4_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_M4.csv")
                  Pav_M4_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_M4.csv")
                  Pav_M4_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_M4.csv")
                  Pav_M4_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_M4.csv")
                  Pav_M4_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_M4.csv")
                  Pav_M4_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_M4.csv")
                  Pav_M4_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_M4.csv")
                  Pav_M4_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_M4.csv")
                  Pav_M4_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_M4.csv")
                  Pav_M4_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_M4.csv")
                  
                  Pav_M5_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_M5.csv")
                  Pav_M5_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_M5.csv")
                  Pav_M5_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_M5.csv")
                  Pav_M5_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_M5.csv")
                  Pav_M5_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_M5.csv")
                  Pav_M5_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_M5.csv")
                  Pav_M5_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_M5.csv")
                  Pav_M5_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_M5.csv")
                  Pav_M5_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_M5.csv")
                  Pav_M5_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_M5.csv")
                  
                  Pav_M9_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_M9.csv")
                  Pav_M9_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_M9.csv")
                  Pav_M9_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_M9.csv")
                  Pav_M9_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_M9.csv")
                  Pav_M9_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_M9.csv")
                  Pav_M9_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_M9.csv")
                  Pav_M9_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_M9.csv")
                  Pav_M9_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_M9.csv")
                  Pav_M9_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_M9.csv")
                  Pav_M9_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_M9.csv")
                  
                  Pav_F7_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_F7.csv")
                  Pav_F7_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_F7.csv")
                  Pav_F7_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_F7.csv")
                  Pav_F7_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_F7.csv")
                  Pav_F7_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_F7.csv")
                  Pav_F7_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_F7.csv")
                  Pav_F7_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_F7.csv")
                  Pav_F7_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_F7.csv")
                  Pav_F7_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_F7.csv")
                  Pav_F7_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F7.csv")
                  
                  Pav_F8_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_F8.csv")
                  Pav_F8_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_F8.csv")
                  Pav_F8_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_F8.csv")
                  Pav_F8_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_F8.csv")
                  Pav_F8_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_F8.csv")
                  Pav_F8_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_F8.csv")
                  Pav_F8_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_F8.csv")
                  Pav_F8_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_F8.csv")
                  Pav_F8_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_F8.csv")
                  Pav_F8_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F8.csv")
                  
                  Pav_F9_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_F9.csv")
                  Pav_F9_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_F9.csv")
                  Pav_F9_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_F9.csv")
                  Pav_F9_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_F9.csv")
                  Pav_F9_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_F9.csv")
                  Pav_F9_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_F9.csv")
                  Pav_F9_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_F9.csv")
                  Pav_F9_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_F9.csv")
                  Pav_F9_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_F9.csv")
                  Pav_F9_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F9.csv")
                  
                  Pav_F10_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_F10.csv")
                  Pav_F10_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_F10.csv")
                  Pav_F10_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_F10.csv")
                  Pav_F10_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_F10.csv")
                  Pav_F10_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_F10.csv")
                  Pav_F10_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_F10.csv")
                  Pav_F10_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_F10.csv")
                  Pav_F10_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_F10.csv")
                  Pav_F10_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_F10.csv")
                  Pav_F10_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F10.csv")
                  
                  Pav_F2_D1 <- read.csv("Final_Data/Pav_Task/Pav_Task_D1/Pav_Task_Final_D1_F2.csv")
                  Pav_F2_D2 <- read.csv("Final_Data/Pav_Task/Pav_Task_D2/Pav_Task_Final_D2_F2.csv")
                  Pav_F2_D3 <- read.csv("Final_Data/Pav_Task/Pav_Task_D3/Pav_Task_Final_D3_F2.csv")
                  Pav_F2_D4 <- read.csv("Final_Data/Pav_Task/Pav_Task_D4/Pav_Task_Final_D4_F2.csv")
                  Pav_F2_D5 <- read.csv("Final_Data/Pav_Task/Pav_Task_D5/Pav_Task_Final_D5_F2.csv")
                  Pav_F2_D6 <- read.csv("Final_Data/Pav_Task/Pav_Task_D6/Pav_Task_Final_D6_F2.csv")
                  Pav_F2_D7 <- read.csv("Final_Data/Pav_Task/Pav_Task_D7/Pav_Task_Final_D7_F2.csv")
                  Pav_F2_D8 <- read.csv("Final_Data/Pav_Task/Pav_Task_D8/Pav_Task_Final_D8_F2.csv")
                  Pav_F2_D9 <- read.csv("Final_Data/Pav_Task/Pav_Task_D9/Pav_Task_Final_D9_F2.csv")
                  Pav_F2_D10 <- read.csv("Final_Data/Pav_Task/Pav_Task_D10/Pav_Task_Final_D10_F2.csv")
                  
  
  ## Combine data into single master data frame. Remove individual data frames from environment.
      
      Pav_M4_D2 <- Pav_M4_D2 %>% select(subject, session, CS_type, pre_post, n)
      
      Pav_M4_D2$ME_duration_sum <- Pav_M4_D3$ME_duration_sum
      Pav_M4_D2$adj_time <- Pav_M4_D3$adj_time
      
      Pav_M4_D2$session[1: nrow(Pav_M4_D2)] <- 2
      Pav_M4_D2$session <- as.integer(Pav_M4_D2$session)
 
                  
      pav_master <- bind_rows(Pav_M3_D1, Pav_M3_D2, Pav_M3_D3, Pav_M3_D4, Pav_M3_D5, Pav_M3_D6, Pav_M3_D7, Pav_M3_D8, Pav_M3_D9, Pav_M3_D10,
                              Pav_M4_D1, Pav_M4_D2, Pav_M4_D3, Pav_M4_D4, Pav_M4_D5, Pav_M4_D6, Pav_M4_D7, Pav_M4_D8, Pav_M4_D9, Pav_M4_D10,
                              Pav_M5_D1, Pav_M5_D2, Pav_M5_D3, Pav_M5_D4, Pav_M5_D5, Pav_M5_D6, Pav_M5_D7, Pav_M5_D8, Pav_M5_D9, Pav_M5_D10,
                              Pav_M9_D1, Pav_M9_D2, Pav_M9_D3, Pav_M9_D4, Pav_M9_D5, Pav_M9_D6, Pav_M9_D7, Pav_M9_D8, Pav_M9_D9, Pav_M9_D10,
                              Pav_F7_D1, Pav_F7_D2, Pav_F7_D3, Pav_F7_D4, Pav_F7_D5, Pav_F7_D6, Pav_F7_D7, Pav_F7_D8, Pav_F7_D9, Pav_F7_D10,
                              Pav_F8_D1, Pav_F8_D2, Pav_F8_D3, Pav_F8_D4, Pav_F8_D5, Pav_F8_D6, Pav_F8_D7, Pav_F8_D8, Pav_F8_D9, Pav_F8_D10,
                              Pav_F9_D1, Pav_F9_D2, Pav_F9_D3, Pav_F9_D4, Pav_F9_D5, Pav_F9_D6, Pav_F9_D7, Pav_F9_D8, Pav_F9_D9, Pav_F9_D10,
                              Pav_F10_D1, Pav_F10_D2, Pav_F10_D3, Pav_F10_D4, Pav_F10_D5, Pav_F10_D6, Pav_F10_D7, Pav_F10_D8, Pav_F10_D9, Pav_F10_D10,
                              Pav_F2_D1, Pav_F2_D2, Pav_F2_D3, Pav_F2_D4, Pav_F2_D5, Pav_F2_D6, Pav_F2_D7, Pav_F2_D8, Pav_F2_D9, Pav_F2_D10)
      
                       remove(Pav_M3_D1, Pav_M3_D2, Pav_M3_D3, Pav_M3_D4, Pav_M3_D5, Pav_M3_D6, Pav_M3_D7, Pav_M3_D8, Pav_M3_D9, Pav_M3_D10,
                              Pav_M4_D1, Pav_M4_D2, Pav_M4_D3, Pav_M4_D4, Pav_M4_D5, Pav_M4_D6, Pav_M4_D7, Pav_M4_D8, Pav_M4_D9, Pav_M4_D10,
                              Pav_M5_D1, Pav_M5_D2, Pav_M5_D3, Pav_M5_D4, Pav_M5_D5, Pav_M5_D6, Pav_M5_D7, Pav_M5_D8, Pav_M5_D9, Pav_M5_D10,
                              Pav_M9_D1, Pav_M9_D2, Pav_M9_D3, Pav_M9_D4, Pav_M9_D5, Pav_M9_D6, Pav_M9_D7, Pav_M9_D8, Pav_M9_D9, Pav_M9_D10,
                              Pav_F7_D1, Pav_F7_D2, Pav_F7_D3, Pav_F7_D4, Pav_F7_D5, Pav_F7_D6, Pav_F7_D7, Pav_F7_D8, Pav_F7_D9, Pav_F7_D10,
                              Pav_F8_D1, Pav_F8_D2, Pav_F8_D3, Pav_F8_D4, Pav_F8_D5, Pav_F8_D6, Pav_F8_D7, Pav_F8_D8, Pav_F8_D9, Pav_F8_D10,
                              Pav_F9_D1, Pav_F9_D2, Pav_F9_D3, Pav_F9_D4, Pav_F9_D5, Pav_F9_D6, Pav_F9_D7, Pav_F9_D8, Pav_F9_D9, Pav_F9_D10,
                              Pav_F10_D1, Pav_F10_D2, Pav_F10_D3, Pav_F10_D4, Pav_F10_D5, Pav_F10_D6, Pav_F10_D7, Pav_F10_D8, Pav_F10_D9, Pav_F10_D10,
                              Pav_F2_D1, Pav_F2_D2, Pav_F2_D3, Pav_F2_D4, Pav_F2_D5, Pav_F2_D6, Pav_F2_D7, Pav_F2_D8, Pav_F2_D9, Pav_F2_D10)
        
      
      pav_master <- pav_master %>% convert_as_factor(session)
      pav_master$n <- as.numeric(pav_master$n)
      pav_master$ME_duration_sum <- round(pav_master$ME_duration_sum, digits = 2)
      pav_master$adj_time <- round(pav_master$adj_time, digits = 2)
                       
      write_csv(pav_master, "Final_Data/Pav_Task/Pav_Combined2.csv")
        
     
# ----Latency------
  # ME Latency
      
      latency_data <- mag_data %>% select(ME_timestamp, CS_type, CS_time) %>% na.omit()
      
      latency_data <- latency_data %>% distinct(CS_time, .keep_all = TRUE)
      
      latency_data <- latency_data %>% mutate(latency = (ME_timestamp - CS_time)*1000)
      
      latency_data$latency <- round(latency_data$latency, digits = 2)
      
      latency_data$subject[1:nrow(latency_data)] <- subject$subject[1]
      
      latency_data$session[1:nrow(latency_data)] <- session$session[1]
      
      latency_data <- latency_data[, c(5, 6, 1, 2, 3, 4)]
      
      latency_data <- latency_data %>% convert_as_factor(subject)
      
      latency_data <- latency_data %>% convert_as_factor(session)
      
# ----Combined Latency Analysis------
  # Combined Data Analysis
      
    ## Load all data files into data frames. Data filed separated by subject and session.
      
      Latency_M3_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_M3.csv")
      Latency_M3_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_M3.csv")
      Latency_M3_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_M3.csv")
      Latency_M3_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_M3.csv")
      Latency_M3_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_M3.csv")
      Latency_M3_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_M3.csv")
      Latency_M3_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_M3.csv")
      Latency_M3_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_M3.csv")
      Latency_M3_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_M3.csv")
      Latency_M3_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_M3.csv")
      
      Latency_M4_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_M4.csv")
      Latency_M4_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_M4.csv")
      Latency_M4_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_M4.csv")
      Latency_M4_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_M4.csv")
      Latency_M4_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_M4.csv")
      Latency_M4_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_M4.csv")
      Latency_M4_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_M4.csv")
      Latency_M4_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_M4.csv")
      Latency_M4_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_M4.csv")
      Latency_M4_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_M4.csv")
      
      Latency_M5_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_M5.csv")
      Latency_M5_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_M5.csv")
      Latency_M5_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_M5.csv")
      Latency_M5_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_M5.csv")
      Latency_M5_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_M5.csv")
      Latency_M5_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_M5.csv")
      Latency_M5_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_M5.csv")
      Latency_M5_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_M5.csv")
      Latency_M5_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_M5.csv")
      Latency_M5_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_M5.csv")
      
      Latency_M9_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_M9.csv")
      Latency_M9_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_M9.csv")
      Latency_M9_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_M9.csv")
      Latency_M9_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_M9.csv")
      Latency_M9_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_M9.csv")
      Latency_M9_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_M9.csv")
      Latency_M9_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_M9.csv")
      Latency_M9_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_M9.csv")
      Latency_M9_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_M9.csv")
      Latency_M9_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_M9.csv")
      
      Latency_F7_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_F7.csv")
      Latency_F7_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_F7.csv")
      Latency_F7_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_F7.csv")
      Latency_F7_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_F7.csv")
      Latency_F7_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_F7.csv")
      Latency_F7_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_F7.csv")
      Latency_F7_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_F7.csv")
      Latency_F7_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_F7.csv")
      Latency_F7_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_F7.csv")
      Latency_F7_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_F7.csv")
      
      Latency_F8_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_F8.csv")
      Latency_F8_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_F8.csv")
      Latency_F8_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_F8.csv")
      Latency_F8_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_F8.csv")
      Latency_F8_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_F8.csv")
      Latency_F8_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_F8.csv")
      Latency_F8_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_F8.csv")
      Latency_F8_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_F8.csv")
      Latency_F8_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_F8.csv")
      Latency_F8_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_F8.csv")
      
      Latency_F9_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_F9.csv")
      Latency_F9_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_F9.csv")
      Latency_F9_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_F9.csv")
      Latency_F9_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_F9.csv")
      Latency_F9_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_F9.csv")
      Latency_F9_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_F9.csv")
      Latency_F9_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_F9.csv")
      Latency_F9_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_F9.csv")
      Latency_F9_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_F9.csv")
      Latency_F9_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_F9.csv")
      
      Latency_F10_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_F10.csv")
      Latency_F10_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_F10.csv")
      Latency_F10_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_F10.csv")
      Latency_F10_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_F10.csv")
      Latency_F10_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_F10.csv")
      Latency_F10_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_F10.csv")
      Latency_F10_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_F10.csv")
      Latency_F10_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_F10.csv")
      Latency_F10_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_F10.csv")
      Latency_F10_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_F10.csv")
      
      Latency_F2_D1 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D1_F2.csv")
      Latency_F2_D2 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D2_F2.csv")
      Latency_F2_D3 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D3_F2.csv")
      Latency_F2_D4 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D4_F2.csv")
      Latency_F2_D5 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D5_F2.csv")
      Latency_F2_D6 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D6_F2.csv")
      Latency_F2_D7 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D7_F2.csv")
      Latency_F2_D8 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D8_F2.csv")
      Latency_F2_D9 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D9_F2.csv")
      Latency_F2_D10 <- read.csv("Final_Data/Pav_Task/Latency/Latency_D10_F2.csv")
      
# ----CSV for Latency------
      
      latency_master <- bind_rows(Latency_M3_D1, Latency_M3_D2, Latency_M3_D3, Latency_M3_D4, Latency_M3_D5, Latency_M3_D6, Latency_M3_D7, Latency_M3_D8, Latency_M3_D9, Latency_M3_D10,
                                  Latency_M4_D1, Latency_M4_D2, Latency_M4_D3, Latency_M4_D4, Latency_M4_D5, Latency_M4_D6, Latency_M4_D7, Latency_M4_D8, Latency_M4_D9, Latency_M4_D10,
                                  Latency_M5_D1, Latency_M5_D2, Latency_M5_D3, Latency_M5_D4, Latency_M5_D5, Latency_M5_D6, Latency_M5_D7, Latency_M5_D8, Latency_M5_D9, Latency_M5_D10,
                                  Latency_M9_D1, Latency_M9_D2, Latency_M9_D3, Latency_M9_D4, Latency_M9_D5, Latency_M9_D6, Latency_M9_D7, Latency_M9_D8, Latency_M9_D9, Latency_M9_D10,
                                  Latency_F7_D1, Latency_F7_D2, Latency_F7_D3, Latency_F7_D4, Latency_F7_D5, Latency_F7_D6, Latency_F7_D7, Latency_F7_D8, Latency_F7_D9, Latency_F7_D10,
                                  Latency_F8_D1, Latency_F8_D2, Latency_F8_D3, Latency_F8_D4, Latency_F8_D5, Latency_F8_D6, Latency_F8_D7, Latency_F8_D8, Latency_F8_D9, Latency_F8_D10,
                                  Latency_F9_D1, Latency_F9_D2, Latency_F9_D3, Latency_F9_D4, Latency_F9_D5, Latency_F9_D6, Latency_F9_D7, Latency_F9_D8, Latency_F9_D9, Latency_F9_D10,
                                  Latency_F10_D1, Latency_F10_D2, Latency_F10_D3, Latency_F10_D4, Latency_F10_D5, Latency_F10_D6, Latency_F10_D7, Latency_F10_D8, Latency_F10_D9, Latency_F10_D10,
                                  Latency_F2_D1, Latency_F2_D2, Latency_F2_D3, Latency_F2_D4, Latency_F2_D5, Latency_F2_D6, Latency_F2_D7, Latency_F2_D8, Latency_F2_D9, Latency_F2_D10)
      
      remove(Latency_M3_D1, Latency_M3_D2, Latency_M3_D3, Latency_M3_D4, Latency_M3_D5, Latency_M3_D6, Latency_M3_D7, Latency_M3_D8, Latency_M3_D9, Latency_M3_D10,
             Latency_M4_D1, Latency_M4_D2, Latency_M4_D3, Latency_M4_D4, Latency_M4_D5, Latency_M4_D6, Latency_M4_D7, Latency_M4_D8, Latency_M4_D9, Latency_M4_D10,
             Latency_M5_D1, Latency_M5_D2, Latency_M5_D3, Latency_M5_D4, Latency_M5_D5, Latency_M5_D6, Latency_M5_D7, Latency_M5_D8, Latency_M5_D9, Latency_M5_D10,
             Latency_M9_D1, Latency_M9_D2, Latency_M9_D3, Latency_M9_D4, Latency_M9_D5, Latency_M9_D6, Latency_M9_D7, Latency_M9_D8, Latency_M9_D9, Latency_M9_D10,
             Latency_F7_D1, Latency_F7_D2, Latency_F7_D3, Latency_F7_D4, Latency_F7_D5, Latency_F7_D6, Latency_F7_D7, Latency_F7_D8, Latency_F7_D9, Latency_F7_D10,
             Latency_F8_D1, Latency_F8_D2, Latency_F8_D3, Latency_F8_D4, Latency_F8_D5, Latency_F8_D6, Latency_F8_D7, Latency_F8_D8, Latency_F8_D9, Latency_F8_D10,
             Latency_F9_D1, Latency_F9_D2, Latency_F9_D3, Latency_F9_D4, Latency_F9_D5, Latency_F9_D6, Latency_F9_D7, Latency_F9_D8, Latency_F9_D9, Latency_F9_D10,
             Latency_F10_D1, Latency_F10_D2, Latency_F10_D3, Latency_F10_D4, Latency_F10_D5, Latency_F10_D6, Latency_F10_D7, Latency_F10_D8, Latency_F10_D9, Latency_F10_D10,
             Latency_F2_D1, Latency_F2_D2, Latency_F2_D3, Latency_F2_D4, Latency_F2_D5, Latency_F2_D6, Latency_F2_D7, Latency_F2_D8, Latency_F2_D9, Latency_F2_D10)
      
      write_csv(latency_master, "Final_Data/Pav_Task/Latency_Combined.csv")
      

    