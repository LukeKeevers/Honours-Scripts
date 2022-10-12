# Magnitude Accuracy Latency
# --------------------------------------------------------------------------------------

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

# ------- Latency -----

timestamp_master <- read.csv("Event_Timestamps/Magnitude_Task/Box3 F2 D10.csv",
                             header = FALSE) %>%
  rename(c(timestamp = V1, event = V2)) %>%
  filter(event == 21 | event == 22 | event == 24 | event == 25 | event == 31 | event == 32)

subject = "F2"

session = "10"

save_path = "Magnitude_Latency/F2_D10_Latency.csv"

latency <- diff(timestamp_master$timestamp)

latency <- as.data.frame(latency)

latency <- latency %>% filter(latency < 39)

cues <- timestamp_master %>%
  filter(event == 31 | event == 32) %>%
  mutate(reward_size = case_when(event == 31 ~ "Small", event == 32 ~ "Large")) %>%
  convert_as_factor(reward_size)

latency_master <- cbind(cues, latency)

latency_sum <- latency_master %>%
  select(reward_size, latency) %>%
  group_by(reward_size) %>%
  get_summary_stats(latency, type = "mean_sd") %>%
  select(reward_size, mean) %>%
  rename(latency = mean) %>%
  mutate(subject = as.factor(subject)) %>%
  mutate(session = as.factor(session)) %>%
  relocate(subject, session)

write_csv(latency_sum, save_path) 
  

# ---- Combined Latency ----

F7_D1 <- read.csv("Magnitude_Latency/F7_D1_Latency.csv")
F7_D2 <- read.csv("Magnitude_Latency/F7_D2_Latency.csv")
F7_D3 <- read.csv("Magnitude_Latency/F7_D3_Latency.csv")
F7_D4 <- read.csv("Magnitude_Latency/F7_D4_Latency.csv")
F7_D5 <- read.csv("Magnitude_Latency/F7_D5_Latency.csv")
F7_D6 <- read.csv("Magnitude_Latency/F7_D6_Latency.csv")
F7_D7 <- read.csv("Magnitude_Latency/F7_D7_Latency.csv")
F7_D8 <- read.csv("Magnitude_Latency/F7_D8_Latency.csv")
F7_D9 <- read.csv("Magnitude_Latency/F7_D9_Latency.csv")
F7_D10 <- read.csv("Magnitude_Latency/F7_D10_Latency.csv")

F2_D1 <- read.csv("Magnitude_Latency/F2_D1_Latency.csv")
F2_D2 <- read.csv("Magnitude_Latency/F2_D2_Latency.csv")
F2_D3 <- read.csv("Magnitude_Latency/F2_D3_Latency.csv")
F2_D4 <- read.csv("Magnitude_Latency/F2_D4_Latency.csv")
F2_D5 <- read.csv("Magnitude_Latency/F2_D5_Latency.csv")
F2_D6 <- read.csv("Magnitude_Latency/F2_D6_Latency.csv")
F2_D7 <- read.csv("Magnitude_Latency/F2_D7_Latency.csv")
F2_D8 <- read.csv("Magnitude_Latency/F2_D8_Latency.csv")
F2_D9 <- read.csv("Magnitude_Latency/F2_D9_Latency.csv")
F2_D10 <- read.csv("Magnitude_Latency/F2_D10_Latency.csv")

F8_D1 <- read.csv("Magnitude_Latency/F8_D1_Latency.csv")
F8_D2 <- read.csv("Magnitude_Latency/F8_D2_Latency.csv")
F8_D3 <- read.csv("Magnitude_Latency/F8_D3_Latency.csv")
F8_D4 <- read.csv("Magnitude_Latency/F8_D4_Latency.csv")
F8_D5 <- read.csv("Magnitude_Latency/F8_D5_Latency.csv")
F8_D6 <- read.csv("Magnitude_Latency/F8_D6_Latency.csv")
F8_D7 <- read.csv("Magnitude_Latency/F8_D7_Latency.csv")
F8_D8 <- read.csv("Magnitude_Latency/F8_D8_Latency.csv")
F8_D9 <- read.csv("Magnitude_Latency/F8_D9_Latency.csv")
F8_D10 <- read.csv("Magnitude_Latency/F8_D10_Latency.csv")

F9_D1 <- read.csv("Magnitude_Latency/F9_D1_Latency.csv")
F9_D2 <- read.csv("Magnitude_Latency/F9_D2_Latency.csv")
F9_D3 <- read.csv("Magnitude_Latency/F9_D3_Latency.csv")
F9_D4 <- read.csv("Magnitude_Latency/F9_D4_Latency.csv")
F9_D5 <- read.csv("Magnitude_Latency/F9_D5_Latency.csv")
F9_D6 <- read.csv("Magnitude_Latency/F9_D6_Latency.csv")
F9_D7 <- read.csv("Magnitude_Latency/F9_D7_Latency.csv")
F9_D8 <- read.csv("Magnitude_Latency/F9_D8_Latency.csv")
F9_D9 <- read.csv("Magnitude_Latency/F9_D9_Latency.csv")
F9_D10 <- read.csv("Magnitude_Latency/F9_D10_Latency.csv")

F10_D1 <- read.csv("Magnitude_Latency/F10_D1_Latency.csv")
F10_D2 <- read.csv("Magnitude_Latency/F10_D2_Latency.csv")
F10_D3 <- read.csv("Magnitude_Latency/F10_D3_Latency.csv")
F10_D4 <- read.csv("Magnitude_Latency/F10_D4_Latency.csv")
F10_D5 <- read.csv("Magnitude_Latency/F10_D5_Latency.csv")
F10_D6 <- read.csv("Magnitude_Latency/F10_D6_Latency.csv")
F10_D7 <- read.csv("Magnitude_Latency/F10_D7_Latency.csv")
F10_D8 <- read.csv("Magnitude_Latency/F10_D8_Latency.csv")
F10_D9 <- read.csv("Magnitude_Latency/F10_D9_Latency.csv")
F10_D10 <- read.csv("Magnitude_Latency/F10_D10_Latency.csv")

M3_D1 <- read.csv("Magnitude_Latency/M3_D1_Latency.csv")
M3_D2 <- read.csv("Magnitude_Latency/M3_D2_Latency.csv")
M3_D3 <- read.csv("Magnitude_Latency/M3_D3_Latency.csv")
M3_D4 <- read.csv("Magnitude_Latency/M3_D4_Latency.csv")
M3_D5 <- read.csv("Magnitude_Latency/M3_D5_Latency.csv")
M3_D6 <- read.csv("Magnitude_Latency/M3_D6_Latency.csv")
M3_D7 <- read.csv("Magnitude_Latency/M3_D7_Latency.csv")
M3_D8 <- read.csv("Magnitude_Latency/M3_D8_Latency.csv")
M3_D9 <- read.csv("Magnitude_Latency/M3_D9_Latency.csv")
M3_D10 <- read.csv("Magnitude_Latency/M3_D10_Latency.csv")

M4_D1 <- read.csv("Magnitude_Latency/M4_D1_Latency.csv")
M4_D2 <- read.csv("Magnitude_Latency/M4_D2_Latency.csv")
M4_D3 <- read.csv("Magnitude_Latency/M4_D3_Latency.csv")
M4_D4 <- read.csv("Magnitude_Latency/M4_D4_Latency.csv")
M4_D5 <- read.csv("Magnitude_Latency/M4_D5_Latency.csv")
M4_D6 <- read.csv("Magnitude_Latency/M4_D6_Latency.csv")
M4_D7 <- read.csv("Magnitude_Latency/M4_D7_Latency.csv")
M4_D8 <- read.csv("Magnitude_Latency/M4_D8_Latency.csv")
M4_D9 <- read.csv("Magnitude_Latency/M4_D9_Latency.csv")
M4_D10 <- read.csv("Magnitude_Latency/M4_D10_Latency.csv")

M5_D1 <- read.csv("Magnitude_Latency/M5_D1_Latency.csv")
M5_D2 <- read.csv("Magnitude_Latency/M5_D2_Latency.csv")
M5_D3 <- read.csv("Magnitude_Latency/M5_D3_Latency.csv")
M5_D4 <- read.csv("Magnitude_Latency/M5_D4_Latency.csv")
M5_D5 <- read.csv("Magnitude_Latency/M5_D5_Latency.csv")
M5_D6 <- read.csv("Magnitude_Latency/M5_D6_Latency.csv")
M5_D7 <- read.csv("Magnitude_Latency/M5_D7_Latency.csv")
M5_D8 <- read.csv("Magnitude_Latency/M5_D8_Latency.csv")
M5_D9 <- read.csv("Magnitude_Latency/M5_D9_Latency.csv")
M5_D10 <- read.csv("Magnitude_Latency/M5_D10_Latency.csv")

M9_D1 <- read.csv("Magnitude_Latency/M9_D1_Latency.csv")
M9_D2 <- read.csv("Magnitude_Latency/M9_D2_Latency.csv")
M9_D3 <- read.csv("Magnitude_Latency/M9_D3_Latency.csv")
M9_D4 <- read.csv("Magnitude_Latency/M9_D4_Latency.csv")
M9_D5 <- read.csv("Magnitude_Latency/M9_D5_Latency.csv")
M9_D6 <- read.csv("Magnitude_Latency/M9_D6_Latency.csv")
M9_D7 <- read.csv("Magnitude_Latency/M9_D7_Latency.csv")
M9_D8 <- read.csv("Magnitude_Latency/M9_D8_Latency.csv")
M9_D9 <- read.csv("Magnitude_Latency/M9_D9_Latency.csv")
M9_D10 <- read.csv("Magnitude_Latency/M9_D10_Latency.csv")

combined_latency_master <- bind_rows(F7_D1, F7_D2, F7_D3, F7_D4, F7_D5, F7_D6, F7_D7, F7_D8, F7_D9, F7_D10,
                                     F8_D1, F8_D2, F8_D3, F8_D4, F8_D5, F8_D6, F8_D7, F8_D8, F8_D9, F8_D10,
                                     F9_D1, F9_D2, F9_D3, F9_D4, F9_D5, F9_D6, F9_D7, F9_D8, F9_D9, F9_D10,
                                     F10_D1, F10_D2, F10_D3, F10_D4, F10_D5, F10_D6, F10_D7, F10_D8, F10_D9, F10_D10,
                                     M3_D1, M3_D2, M3_D3, M3_D4, M3_D5, M3_D6, M3_D7, M3_D8, M3_D9, M3_D10,
                                     M4_D1, M4_D2, M4_D3, M4_D4, M4_D5, M4_D6, M4_D7, M4_D8, M4_D9, M4_D10,
                                     M5_D1, M5_D2, M5_D3, M5_D4, M5_D5, M5_D6, M5_D7, M5_D8, M5_D9, M5_D10,
                                     M9_D1, M9_D2, M9_D3, M9_D4, M9_D5, M9_D6, M9_D7, M9_D8, M9_D9, M9_D10,
                                     F2_D1, F2_D2, F2_D3, F2_D4, F2_D5, F2_D6, F2_D7, F2_D8, F2_D9, F2_D10)

remove(F7_D1, F7_D2, F7_D3, F7_D4, F7_D5, F7_D6, F7_D7, F7_D8, F7_D9, F7_D10,
       F8_D1, F8_D2, F8_D3, F8_D4, F8_D5, F8_D6, F8_D7, F8_D8, F8_D9, F8_D10,
       F9_D1, F9_D2, F9_D3, F9_D4, F9_D5, F9_D6, F9_D7, F9_D8, F9_D9, F9_D10,
       F10_D1, F10_D2, F10_D3, F10_D4, F10_D5, F10_D6, F10_D7, F10_D8, F10_D9, F10_D10,
       M3_D1, M3_D2, M3_D3, M3_D4, M3_D5, M3_D6, M3_D7, M3_D8, M3_D9, M3_D10,
       M4_D1, M4_D2, M4_D3, M4_D4, M4_D5, M4_D6, M4_D7, M4_D8, M4_D9, M4_D10,
       M5_D1, M5_D2, M5_D3, M5_D4, M5_D5, M5_D6, M5_D7, M5_D8, M5_D9, M5_D10,
       M9_D1, M9_D2, M9_D3, M9_D4, M9_D5, M9_D6, M9_D7, M9_D8, M9_D9, M9_D10,
       F2_D1, F2_D2, F2_D3, F2_D4, F2_D5, F2_D6, F2_D7, F2_D8, F2_D9, F2_D10)

write_csv(combined_latency_master, "Magnitude_Latency/Magnitude_Latency_Final.csv")
  






