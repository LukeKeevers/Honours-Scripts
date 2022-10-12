# Photometry Data Analysis

#----Setup----

setwd("~/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data")
wd <- getwd()

library(tidyverse)
library(papaja)
library(fluoR)
library(rmedpc)
library(ez)
library(ggpubr)
library(rstatix)
library(patchwork)
library(pracma)

#----Load Data----

  file_name = "Pav_Task_D10_CS_3P.csv"
  
  session = substring(file_name, # Change first and last values depending on task
                      first = 11,
                      last = nchar(file_name)-10)
  
  image_name = substring(file_name, 
                         first = 1, 
                         last = nchar(file_name)-4)
  image_name <- paste(c(image_name, "Plot.png"), collapse = "_")

  master_data <- read_csv(file_name, col_names = FALSE)
  
  colnames(master_data) <- c("time","F7_mean", "F7_sd", "F7_peak",
                                    "F8_mean", "F8_sd", "F8_peak",
                                    "F9_mean", "F9_sd", "F9_peak",
                                    "F10_mean", "F10_sd", "F10_peak",
                                    "M3_mean", "M3_sd", "M3_peak",
                                    "M4_mean", "M4_sd", "M4_peak",
                                    "M5_mean", "M5_sd", "M5_peak",
                                    "M9_mean", "M9_sd", "M9_peak")
#----Wrangle Data----
  
  ## Down sample by factor of 5
  
    master_data <- master_data[seq(1, nrow(master_data), 5), ] %>%
      select(-starts_with("M4")) #Exclude M4 because of bad signal
    
  ## Select out peak value data
  
    peak_data <- master_data %>%
      select(ends_with("peak")) %>%
      distinct(.keep_all = TRUE) %>%
      pivot_longer(cols = ends_with("peak"), 
                   names_to = "subject", 
                   values_to = "peak")
    
    peak_data$subject <- str_remove(peak_data$subject, "_peak")
    
    peak_data <- peak_data %>% 
      convert_as_factor(subject) %>%
      mutate(session = session) %>%
      convert_as_factor(session) %>%
      relocate(subject, session)
    
    peak_file_name <- substring(file_name,
                                first = 1, 
                                last = nchar(file_name)-4)
    
    peak_file_name <- paste(c(peak_file_name, "Peak.csv"), collapse = "_")
    
    peak_file_path <- paste(c(wd, "/Peak Data/", peak_file_name), collapse = "")
    
    write_csv(peak_data,
              peak_file_path)
    
    
  
  ## Select out mean and sd data
    
    epoch_mean <- master_data %>%
      select(time, ends_with("mean")) %>%
      pivot_longer(cols = ends_with("mean"), 
                   names_to = "subject", 
                   values_to = "zscore")
    
    epoch_sd <- master_data %>%
      select(time, ends_with("sd")) %>%
      pivot_longer(cols = ends_with("sd"), 
                   names_to = "subject", 
                   values_to = "sd")
  
    epoch_mean$subject <- str_remove(epoch_mean$subject, "_mean")
    epoch_sd$subject <- str_remove(epoch_sd$subject, "_sd")
   
  ## Combine data and summarise
    
    epoch_data <- cbind(epoch_mean, epoch_sd$sd) %>% 
      rename(sd = 'epoch_sd$sd') %>%
      convert_as_factor(subject) %>%
      #convert_as_factor(time) %>%
      relocate(subject)
    
    epoch_sum <- epoch_data %>%
      group_by(time) %>%
      get_summary_stats(zscore, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n)) %>% 
      mutate(session = session) %>%
      convert_as_factor(session) %>%
      relocate(session)
    
    sum_file_name <- substring(file_name,
                                first = 1, 
                                last = nchar(file_name)-4)
    
    sum_file_name <- paste(c(sum_file_name, "Summary.csv"), collapse = "_")
    
    sum_file_path <- paste(c(wd, "/Summary Data/", sum_file_name), collapse = "")
    
    write_csv(epoch_sum,
              sum_file_path)
    
    
#----Plot Data----
  
  epoch_plot <- ggplot(data = epoch_sum,
                       mapping = aes(x = time,
                                     y = mean)) +
    annotate(geom = "rect", xmin = 0, xmax = 2, ymin = -Inf, ymax = Inf,
             fill = "grey69", colour = NA, alpha = 0.6) +
    annotate(geom = "rect", xmin = 5, xmax = 7, ymin = -Inf, ymax = Inf,
             fill = "darkorange3", colour = NA, alpha = 0.2) +
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "black", size = 1) +
    geom_vline(xintercept = 5, linetype="dashed", 
               color = "black", size = 1) +
    geom_hline(yintercept = 0, linetype="dotted", 
               color = "black", size = 0.5) +
    geom_ribbon(mapping = aes(x = time, 
                              ymin = mean - sem,
                              ymax = mean + sem), 
                fill = "dodgerblue4", 
                alpha = 0.3) +
    geom_line(aes(group = 1), colour = "dodgerblue4", size = 1) +
    xlab("Time from Cue Onset (s)") +
    ylab("\u0394F/F (z-score)") +
    #ylim(-1, 3) + 
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)
                     ) +
    theme_apa() +
    theme(legend.position="none")
  
  print(epoch_plot)
  
  ggsave(image_name, 
         epoch_plot, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 15.92,
         height = 10,
         units = "cm",
         dpi = 300)
  
  t.test(peak_data$peak, mu = 0, alternative = "two.sided")
  

  
  
#----Prep for Stage Data----
  
  CS_1P_D1 <- read_csv("Pav_Task_D1_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "1") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D2 <- read_csv("Pav_Task_D2_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "2") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D3 <- read_csv("Pav_Task_D3_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "3") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D4 <- read_csv("Pav_Task_D4_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "4") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D5 <- read_csv("Pav_Task_D5_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "5") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D6 <- read_csv("Pav_Task_D6_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "6") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D7 <- read_csv("Pav_Task_D7_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "7") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D8 <- read_csv("Pav_Task_D8_CS_1P.csv", col_names = FALSE) %>% 
    mutate(session = "8") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D9 <- read_csv("Pav_Task_D9_CS_1P.csv", col_names = FALSE)%>% 
    mutate(session = "9") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_1P_D10 <- read_csv("Pav_Task_D10_CS_1P.csv", col_names = FALSE)%>% 
    mutate(session = "10") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  CS_3P_D1 <- read_csv("Pav_Task_D1_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "1") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D2 <- read_csv("Pav_Task_D2_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "2") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D3 <- read_csv("Pav_Task_D3_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "3") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D4 <- read_csv("Pav_Task_D4_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "4") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D5 <- read_csv("Pav_Task_D5_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "5") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D6 <- read_csv("Pav_Task_D6_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "6") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D7 <- read_csv("Pav_Task_D7_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "7") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D8 <- read_csv("Pav_Task_D8_CS_3P.csv", col_names = FALSE)   %>% 
    mutate(session = "8") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D9 <- read_csv("Pav_Task_D9_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "9") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  CS_3P_D10 <- read_csv("Pav_Task_D10_CS_3P.csv", col_names = FALSE) %>% 
    mutate(session = "10") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  
  master_data <- rbind(CS_1P_D1, CS_1P_D2, CS_1P_D3, CS_1P_D4, CS_1P_D5,
                       CS_1P_D6, CS_1P_D7, CS_1P_D8, CS_1P_D9, CS_1P_D10,
                       CS_3P_D1, CS_3P_D2, CS_3P_D3, CS_3P_D4, CS_3P_D5,
                       CS_3P_D6, CS_3P_D7, CS_3P_D8, CS_3P_D9, CS_3P_D10)
  
  remove(CS_1P_D1, CS_1P_D2, CS_1P_D3, CS_1P_D4, CS_1P_D5,
         CS_1P_D6, CS_1P_D7, CS_1P_D8, CS_1P_D9, CS_1P_D10,
         CS_3P_D1, CS_3P_D2, CS_3P_D3, CS_3P_D4, CS_3P_D5,
         CS_3P_D6, CS_3P_D7, CS_3P_D8, CS_3P_D9, CS_3P_D10)
  
  master_data <- master_data %>%
    convert_as_factor(session) %>%
    convert_as_factor(CS_type)
  
  colnames(master_data) <- c("session", "CS_type", "time",
                             "F7_mean", "F7_sd", "F7_peak",
                             "F8_mean", "F8_sd", "F8_peak",
                             "F9_mean", "F9_sd", "F9_peak",
                             "F10_mean", "F10_sd", "F10_peak",
                             "M3_mean", "M3_sd", "M3_peak",
                             "M4_mean", "M4_sd", "M4_peak",
                             "M5_mean", "M5_sd", "M5_peak",
                             "M9_mean", "M9_sd", "M9_peak")
  
  master_data <- master_data %>%
    select(-starts_with("M4"))
  
  master_data <- master_data[seq(1, nrow(master_data), 10), ]
    
  
  mean_data <- master_data %>%
    select(session, CS_type, time, ends_with("mean")) %>%
    pivot_longer(cols = ends_with("mean"), 
                 names_to = "subject", 
                 values_to = "zscore")
  
  mean_data$subject <- str_remove(mean_data$subject, "_mean")
  
  mean_data <- mean_data %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                           session == 3) ~ "Early",
                                        (session == 8 | session == 9 |
                                         session == 10) ~ "Late")) %>%
    convert_as_factor(stage)
  
  subject_sum <- mean_data %>%
    filter(stage == "Early" | stage == "Late") %>%
    group_by(subject, stage, CS_type, time) %>%
    get_summary_stats(zscore, type = "mean_sd")
  
  subject_session_sum <- mean_data %>%
    group_by(subject, session, CS_type, time) %>%
    get_summary_stats(zscore, type = "mean_sd")
    
  
  
  
#----CS AUC----
  
  CS_AUC <- subject_sum %>%
    filter(time > 0 & time < 1) %>%
    group_by(subject, stage, CS_type) %>%
    summarise(AUC = trapz(time, mean))
  
  CS_mean_AUC <- CS_AUC %>%
    group_by(stage, CS_type) %>%
    get_summary_stats(AUC, type = "mean_sd") %>%
    mutate(sem = sd/sqrt(n)) %>%
    arrange(stage) 
  
  
  CS_AUC_plot <- ggplot(CS_mean_AUC, 
                               mapping = aes(x = stage,
                                             y = mean,
                                             fill = CS_type)) +
    # stat_summary(fun.data = mean_se,
    #              geom = "errorbar",
    #              width = .2, position = position_dodge(width = 0.80)) +
    stat_summary(fun = identity,
                 geom = "bar",
                 position = position_dodge(0.8),
                 width = 0.7,
                 colour = "black",
                 alpha = 1) +
    geom_errorbar(data = CS_mean_AUC %>% filter(stage == "Early"),
                  aes(ymin = mean, ymax = mean + sem),width = .3, position = position_dodge(width = 0.80)) +
    geom_errorbar(data = CS_mean_AUC %>% filter(stage == "Late"),
                  aes(ymin = mean - sem, ymax = mean),width = .3, position = position_dodge(width = 0.80)) +
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", size = 0.5) +
    scale_fill_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
    scale_x_discrete(labels = c("Early", "Late")) +
    xlab("Stage of Training") +
    ylab("AUC") +
    ggtitle("C",
            subtitle = "AUC at CS Onset") +
    ylim(-0.7, 0.6) +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.20,
                                    #vjust = -2,
                                    size = 25),
          plot.subtitle = element_text(hjust = 0.5, size = 14))
  
  print(CS_AUC_plot)
  
  ggsave("Pav Task CS AUC Across Stages.png", 
         CS_AUC_plot, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 12,
         height = 14,
         units = "cm",
         dpi = 300)
  
  # CS_AUC$AUC <- round(CS_AUC$AUC, digits = 2)
  # 
  # CS_AUC <- CS_AUC %>% convert_as_factor(subject)
  # 
  # CS_AUC$AUC <- as.numeric(CS_AUC$AUC)
  
  CS_AUC_aov <- ezANOVA(data = CS_AUC,
                      dv = AUC,
                      wid = subject,
                      within = .(stage, CS_type),
                      type = 3)
  CS_AUC_aov
  
  CS_AUC %>%
    group_by(stage) %>%
    pairwise_t_test(AUC ~ CS_type,
                    paired = TRUE,
                    var.equal = FALSE,
                    p.adjust.method = "bonferroni")
  
#----Pellet AUC----
  Pellet_1P_D1 <- read_csv("Pav_Task_D1_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "1") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D2 <- read_csv("Pav_Task_D2_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "2") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D3 <- read_csv("Pav_Task_D3_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "3") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D4 <- read_csv("Pav_Task_D4_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "4") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D5 <- read_csv("Pav_Task_D5_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "5") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D6 <- read_csv("Pav_Task_D6_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "6") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D7 <- read_csv("Pav_Task_D7_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "7") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D8 <- read_csv("Pav_Task_D8_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "8") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D9 <- read_csv("Pav_Task_D9_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "9") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_1P_D10 <- read_csv("Pav_Task_D10_PelletDrop_1P.csv", col_names = FALSE) %>% 
    mutate(session = "10") %>%
    mutate(CS_type = "CS_1P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D1 <- read_csv("Pav_Task_D1_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "1") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D2 <- read_csv("Pav_Task_D2_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "2") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D3 <- read_csv("Pav_Task_D3_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "3") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D4 <- read_csv("Pav_Task_D4_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "4") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D5 <- read_csv("Pav_Task_D5_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "5") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D6 <- read_csv("Pav_Task_D6_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "6") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D7 <- read_csv("Pav_Task_D7_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "7") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D8 <- read_csv("Pav_Task_D8_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "8") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D9 <- read_csv("Pav_Task_D9_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "9") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  Pellet_3P_D10 <- read_csv("Pav_Task_D10_PelletDrop_3P.csv", col_names = FALSE) %>% 
    mutate(session = "10") %>%
    mutate(CS_type = "CS_3P") %>%
    relocate(session, CS_type)
  
  pellet_data <- rbind(Pellet_1P_D1, Pellet_1P_D2, Pellet_1P_D3, Pellet_1P_D4, Pellet_1P_D5,
                       Pellet_1P_D6, Pellet_1P_D7, Pellet_1P_D8, Pellet_1P_D9, Pellet_1P_D10,
                       Pellet_3P_D1, Pellet_3P_D2, Pellet_3P_D3, Pellet_3P_D4, Pellet_3P_D5,
                       Pellet_3P_D6, Pellet_3P_D7, Pellet_3P_D8, Pellet_3P_D9, Pellet_3P_D10)
  
  remove(Pellet_1P_D1, Pellet_1P_D2, Pellet_1P_D3, Pellet_1P_D4, Pellet_1P_D5,
         Pellet_1P_D6, Pellet_1P_D7, Pellet_1P_D8, Pellet_1P_D9, Pellet_1P_D10,
         Pellet_3P_D1, Pellet_3P_D2, Pellet_3P_D3, Pellet_3P_D4, Pellet_3P_D5,
         Pellet_3P_D6, Pellet_3P_D7, Pellet_3P_D8, Pellet_3P_D9, Pellet_3P_D10)
  
  pellet_data <- pellet_data %>%
    convert_as_factor(session) %>%
    convert_as_factor(CS_type)
  
  colnames(pellet_data) <- c("session", "CS_type", "time",
                             "F7_mean", "F7_sd", "F7_peak",
                             "F8_mean", "F8_sd", "F8_peak",
                             "F9_mean", "F9_sd", "F9_peak",
                             "F10_mean", "F10_sd", "F10_peak",
                             "M3_mean", "M3_sd", "M3_peak",
                             "M4_mean", "M4_sd", "M4_peak",
                             "M5_mean", "M5_sd", "M5_peak",
                             "M9_mean", "M9_sd", "M9_peak")
  
  pellet_data <- pellet_data %>%
    select(-starts_with("M4"))
  
  pellet_data <- pellet_data[seq(1, nrow(pellet_data), 10), ]
  
  pellet_mean_data <- pellet_data %>%
    select(session, CS_type, time, ends_with("mean")) %>%
    pivot_longer(cols = ends_with("mean"), 
                 names_to = "subject", 
                 values_to = "zscore")
  
  pellet_mean_data$subject <- str_remove(pellet_mean_data$subject, "_mean")
  
  pellet_mean_data <- pellet_mean_data %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                                         session == 3) ~ "Early",
                                                      (session == 8 | session == 9 |
                                                       session == 10) ~ "Late")) %>%
    convert_as_factor(stage)
  
  pellet_subject_sum <- pellet_mean_data %>%
    filter(stage == "Early" | stage == "Late") %>%
    group_by(subject, stage, CS_type, time) %>%
    get_summary_stats(zscore, type = "mean_sd")
  
  pellet_subject_session_sum <- pellet_mean_data %>%
    group_by(subject, session, CS_type, time) %>%
    get_summary_stats(zscore, type = "mean_sd")
  
  Pellet_AUC <- subject_sum %>%
    filter(time > 5 & time < 6) %>%
    group_by(subject, stage, CS_type) %>%
    summarise(AUC = trapz(time, mean))
  
  Pellet_mean_AUC <- Pellet_AUC %>%
    group_by(stage, CS_type) %>%
    get_summary_stats(AUC, type = "mean_sd") %>%
    mutate(sem = sd/sqrt(n)) %>%
    arrange(stage) 
  
  Pellet_mean_AUC$stage <- as.numeric(Pellet_mean_AUC$stage)
  
  Pellet_AUC_plot <- ggplot(Pellet_mean_AUC, 
                        mapping = aes(x = stage,
                                      y = mean,
                                      fill = CS_type)) +
    # stat_summary(fun.data = mean_se,
    #              geom = "errorbar",
    #              width = .2, position = position_dodge(width = 0.80)) +
    stat_summary(fun = identity,
                 geom = "bar",
                 position = position_dodge(0.8),
                 width = 0.7,
                 colour = "black",
                 alpha = 1) +
    geom_errorbar(data = Pellet_mean_AUC %>% filter(stage == "Early"),
                  aes(ymin = mean, ymax = mean + sem),width = .3, position = position_dodge(width = 0.80)) +
    geom_errorbar(data = Pellet_mean_AUC %>% filter(stage == "Late"),
                  aes(ymin = mean - sem, ymax = mean),width = .3, position = position_dodge(width = 0.80)) +
    geom_hline(yintercept = 0, linetype="dashed", 
               color = "black", size = 0.5) +
    scale_fill_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
    scale_x_discrete(labels = c("Early", "Late")) +
    xlab("Stage of Training") +
    ylab("AUC") +
    annotate(geom = "rect", xmin = 1.2, xmax = 2.2, ymin = 0.5, ymax = 0.5,
             fill = "black", colour = "black") +
    annotate(geom = "rect", xmin = 1.2, xmax = 1.2, ymin = 0.45, ymax = 0.5,
             fill = "black", colour = "black") +
    annotate(geom = "rect", xmin = 2.2, xmax = 2.2, ymin = 0.1, ymax = 0.5,
             fill = "black", colour = "black") +
    annotate(geom = "text", x = 1.7, y = 0.55, 
             label = "*",
             color = "black",
             size = 8) +
    ggtitle("D",
            subtitle = "AUC at Pellet Delivery") +
    ylim(-0.7, 0.6) +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(0, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.2,
                                    #vjust = -2,
                                    size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 14))
  
  print(Pellet_AUC_plot)
  
  ggsave("Pav Task Pellet AUC Across Stages.png", 
         Pellet_AUC_plot, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 12,
         height = 14,
         units = "cm",
         dpi = 300)
  
  pav_AUC_plots <- CS_AUC_plot | Pellet_AUC_plot
  
  ggsave("Pav Task AUC Plots.png", 
         pav_AUC_plots, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
         width = 25.92,
         height = 14,
         units = "cm",
         dpi = 300)
  
  # Pellet_AUC$AUC <- round(Pellet_AUC$AUC, digits = 2)
  # 
  # Pellet_AUC <- Pellet_AUC %>% convert_as_factor(subject)
  # 
  # Pellet_AUC$AUC <- as.numeric(Pellet_AUC$AUC)
  
  Pellet_AUC_aov <- ezANOVA(data = Pellet_AUC,
                        dv = AUC,
                        wid = subject,
                        within = .(stage, CS_type),
                        type = 3)
  Pellet_AUC_aov
  
  pelletAUC_t_tests <- Pellet_AUC %>%
    group_by(CS_type) %>%
    t_test(AUC ~ stage, paired = TRUE) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  
  pelletAUC_t_tests %>% select(-c(n1, n2, .y.))
  
  # Pellet_AUC %>% 
  #   group_by(CS_type) %>%
  #   pairwise_t_test(AUC ~ stage,
  #                 p.adjust.method = "bonferroni",
  #                 paired = TRUE,
  #                 var.equal = FALSE)

  #----Stage Data----
  stage_sum <- subject_sum %>%
    select(stage, CS_type, subject, time, mean) %>%
    group_by(stage, CS_type, time) %>%
    get_summary_stats(mean, type = "mean_sd") %>%
    mutate(sem = sd/sqrt(n))
  
  write_csv(stage_sum, 
            "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/Stage_Data.csv")
  
  stage_sum <- read_csv("Stage Data/Stage_Data.csv")
  
  stage_sum_early <- stage_sum %>%
    filter(stage == "Early" & time >-2)
  
  stage_sum_late <- stage_sum %>%
    filter(stage == "Late" & time >-2)
  
  
#----Combined Pav Cue Analysis----
  
#   CS_1P_D1 <- read_csv("Summary Data/Pav_Task_D1_CS_1P_Summary.csv")
#   CS_1P_D2 <- read_csv("Summary Data/Pav_Task_D2_CS_1P_Summary.csv")
#   CS_1P_D3 <- read_csv("Summary Data/Pav_Task_D3_CS_1P_Summary.csv")
#   CS_1P_D4 <- read_csv("Summary Data/Pav_Task_D4_CS_1P_Summary.csv")
#   CS_1P_D5 <- read_csv("Summary Data/Pav_Task_D5_CS_1P_Summary.csv")
#   CS_1P_D6 <- read_csv("Summary Data/Pav_Task_D6_CS_1P_Summary.csv")
#   CS_1P_D7 <- read_csv("Summary Data/Pav_Task_D7_CS_1P_Summary.csv")
#   CS_1P_D8 <- read_csv("Summary Data/Pav_Task_D8_CS_1P_Summary.csv")  
#   CS_1P_D9 <- read_csv("Summary Data/Pav_Task_D9_CS_1P_Summary.csv")
#   CS_1P_D10 <- read_csv("Summary Data/Pav_Task_D10_CS_1P_Summary.csv")
#   
#   CS_1P_master <- rbind(CS_1P_D1, CS_1P_D2, CS_1P_D3, CS_1P_D4, CS_1P_D5,
#                         CS_1P_D6, CS_1P_D7, CS_1P_D8, CS_1P_D9, CS_1P_D10)
#   
#   remove(CS_1P_D1, CS_1P_D2, CS_1P_D3, CS_1P_D4, CS_1P_D5,
#          CS_1P_D6, CS_1P_D7, CS_1P_D8, CS_1P_D9, CS_1P_D10)
#   
#   CS_1P_master <- CS_1P_master %>% 
#     mutate(CS_type = as.factor("CS_1P")) %>%
#     convert_as_factor(session) %>%
#     relocate(session, CS_type)
#   
#   CS_3P_D1 <- read_csv("Summary Data/Pav_Task_D1_CS_3P_Summary.csv")
#   CS_3P_D2 <- read_csv("Summary Data/Pav_Task_D2_CS_3P_Summary.csv")
#   CS_3P_D3 <- read_csv("Summary Data/Pav_Task_D3_CS_3P_Summary.csv")
#   CS_3P_D4 <- read_csv("Summary Data/Pav_Task_D4_CS_3P_Summary.csv")
#   CS_3P_D5 <- read_csv("Summary Data/Pav_Task_D5_CS_3P_Summary.csv")
#   CS_3P_D6 <- read_csv("Summary Data/Pav_Task_D6_CS_3P_Summary.csv")
#   CS_3P_D7 <- read_csv("Summary Data/Pav_Task_D7_CS_3P_Summary.csv")
#   CS_3P_D8 <- read_csv("Summary Data/Pav_Task_D8_CS_3P_Summary.csv")  
#   CS_3P_D9 <- read_csv("Summary Data/Pav_Task_D9_CS_3P_Summary.csv")
#   CS_3P_D10 <- read_csv("Summary Data/Pav_Task_D10_CS_3P_Summary.csv")
#   
#   CS_3P_master <- rbind(CS_3P_D1, CS_3P_D2, CS_3P_D3, CS_3P_D4, CS_3P_D5,
#                         CS_3P_D6, CS_3P_D7, CS_3P_D8, CS_3P_D9, CS_3P_D10)
#   
#   remove(CS_3P_D1, CS_3P_D2, CS_3P_D3, CS_3P_D4, CS_3P_D5,
#          CS_3P_D6, CS_3P_D7, CS_3P_D8, CS_3P_D9, CS_3P_D10)
#   
#   CS_3P_master <- CS_3P_master %>% 
#     mutate(CS_type = as.factor("CS_3P")) %>%
#     convert_as_factor(session) %>%
#     relocate(session, CS_type)
#   
#   CS_combined <- rbind(CS_1P_master, CS_3P_master)
#   
#   CS_combined <- CS_combined[seq(1, nrow(CS_combined), 10), ]
#   
#   CS_combined <- CS_combined %>% mutate(stage = case_when((session == 1 | session == 2 | 
#                                                           session == 3 | session == 4 |
#                                                           session == 5) ~ "Early",
#                                                        (session == 6 | session == 7 | 
#                                                           session == 8 | session == 9 |
#                                                           session == 10) ~ "Late")) %>%
#     convert_as_factor(stage)
#   
# #----Summarise Combined CS Data----
#   
#   CS_combined_sum_early <- CS_combined %>%
#     filter(stage == "Early" & time > -2)  %>%
#     select(session, CS_type, time, mean) %>%
#     group_by(CS_type, time) %>%
#     get_summary_stats(mean, type = "mean_sd") %>%
#     mutate(sem = sd/sqrt(n)) %>%
#     mutate(CI = sem*2.447)
  
  CS_plot_early <- ggplot(data = stage_sum_early,
                    mapping = aes(x = time,
                                  y = mean,
                                  colour = CS_type,
                                  group = CS_type,
                                  fill = CS_type)) +
    annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
             fill = "grey69", colour = NA, alpha = 0.6) +
    annotate(geom = "rect", xmin = 5, xmax = 6, ymin = -Inf, ymax = Inf,
             fill = "olivedrab4", colour = NA, alpha = 0.2) +
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "black", size = 1) +
    geom_vline(xintercept = 5, linetype="dashed", 
               color = "olivedrab4", size = 1) +
    geom_hline(yintercept = 0, linetype="dotted", 
               color = "black", size = 0.5) +
    geom_ribbon(mapping = aes(x = time,
                              ymin = mean - sem,
                              ymax = mean + sem),
                colour = NA,
                alpha = 0.3) +
    annotate(geom = "text", x = -1, y = 0.8, 
             label = "CS ON",
             color = "black",
             size = 4.5) +
    annotate(geom = "text", x = 4, y = 0.8, 
             label = "Pellet(s)",
             color = "olivedrab4",
             size = 4.5) +
    scale_fill_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
    geom_line(size = 0.8) +
    scale_colour_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
    xlab("Time from Cue Onset (s)") +
    ylab("\u0394F/F (z-score)") +
    ggtitle("A",
            subtitle = "Early Training (Sessions 1-3)") +
    ylim(-1.2, 1) + 
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-5, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.16,
                                    #vjust = -2,
                                    size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 14))
  
  print(CS_plot_early)
  
  ggsave("CS-Evoked Activity (Early).png", 
         CS_plot_early, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 15.92,
         height = 10,
         units = "cm",
         dpi = 300)
  
  # CS_combined_sum_late <- CS_combined %>%
  #   filter(stage == "Late" & time > -2) %>%
  #   select(session, CS_type, time, mean) %>%
  #   group_by(CS_type, time) %>%
  #   get_summary_stats(mean, type = "mean_sd") %>%
  #   mutate(sem = sd/sqrt(n))
  
  CS_plot_late <- ggplot(data = stage_sum_late,
                          mapping = aes(x = time,
                                        y = mean,
                                        colour = CS_type,
                                        group = CS_type,
                                        fill = CS_type)) +
    annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
             fill = "grey69", colour = NA, alpha = 0.6) +
    annotate(geom = "rect", xmin = 5, xmax = 6, ymin = -Inf, ymax = Inf,
             fill = "olivedrab4", colour = NA, alpha = 0.2) +
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "black", size = 1) +
    geom_vline(xintercept = 5, linetype="dashed", 
               color = "olivedrab4", size = 1) +
    geom_hline(yintercept = 0, linetype="dotted", 
               color = "black", size = 0.5) +
    geom_ribbon(mapping = aes(x = time,
                              ymin = mean - sem,
                              ymax = mean + sem),
                colour = NA,
                alpha = 0.3) +
    annotate(geom = "text", x = -1, y = 0.8, 
             label = "CS ON",
             color = "black",
             size = 4.5) +
    annotate(geom = "text", x = 4, y = 0.8, 
             label = "Pellet(s)",
             color = "olivedrab4",
             size = 4.5) +
    scale_fill_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
    geom_line(size = 0.8) +
    scale_colour_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
    xlab("Time from Cue Onset (s)") +
    ylab("\u0394F/F (z-score)") +
    ggtitle("B",
            subtitle = "Late Training (Sessions 8-10)") +
    ylim(-1.2, 1) + 
    scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-5, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.16,
                                    #vjust = -2,
                                    size = 25), 
          plot.subtitle = element_text(hjust = 0.5, size = 14))
  
  print(CS_plot_late)
  
  ggsave("CS-Evoked Activity (Late).png", 
         CS_plot_late, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 15.92,
         height = 10,
         units = "cm",
         dpi = 300)

  both_plots <- CS_plot_early / CS_plot_late 
  
  both_plots
  
  ggsave("CS-Evoked Activity (Early vs. Late).png", 
         both_plots, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
         width = 15.92,
         height = 20,
         units = "cm",
         dpi = 300)
  
#----All Sessions----
  
  # session_sum <- mean_data %>%
  #   select(-stage) %>%
  #   group_by(session, CS_type, time) %>%
  #   get_summary_stats(zscore, type = "mean_sd") %>%
  #   mutate(sem = sd/sqrt(n))
  # 
  # write_csv(session_sum, 
  #           "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/Session_Data.csv")
  # 
  # 
  # D1_sum <- session_sum %>% filter(session == "1" & time > -2)
  # D2_sum <- session_sum %>% filter(session == "2" & time > -2)
  # D3_sum <- session_sum %>% filter(session == "3" & time > -2)
  # D4_sum <- session_sum %>% filter(session == "4" & time > -2)
  # D5_sum <- session_sum %>% filter(session == "5" & time > -2)
  # D6_sum <- session_sum %>% filter(session == "6" & time > -2)
  # D7_sum <- session_sum %>% filter(session == "7" & time > -2)
  # D8_sum <- session_sum %>% filter(session == "8" & time > -2)
  # D9_sum <- session_sum %>% filter(session == "9" & time > -2)
  # D10_sum <- session_sum %>% filter(session == "10" & time > -2)
  # 
  # 
  # CS_plot_D10 <- ggplot(data = D10_sum,
  #                        mapping = aes(x = time,
  #                                      y = mean,
  #                                      colour = CS_type,
  #                                      group = CS_type,
  #                                      fill = CS_type)) +
  #   annotate(geom = "rect", xmin = 0, xmax = 2, ymin = -Inf, ymax = Inf,
  #            fill = "grey69", colour = NA, alpha = 0.6) +
  #   annotate(geom = "rect", xmin = 5, xmax = 7, ymin = -Inf, ymax = Inf,
  #            fill = "olivedrab4", colour = NA, alpha = 0.2) +
  #   geom_vline(xintercept = 0, linetype="dashed", 
  #              color = "black", size = 1) +
  #   geom_vline(xintercept = 5, linetype="dashed", 
  #              color = "olivedrab4", size = 1) +
  #   geom_hline(yintercept = 0, linetype="dotted", 
  #              color = "black", size = 0.5) +
  #   geom_ribbon(mapping = aes(x = time,
  #                             ymin = mean - sem,
  #                             ymax = mean + sem),
  #               colour = NA,
  #               alpha = 0.3) +
  #   annotate(geom = "text", x = -1, y = 1, 
  #            label = "CS ON",
  #            color = "black",
  #            size = 4.5) +
  #   annotate(geom = "text", x = 4, y = 1, 
  #            label = "Pellet(s)",
  #            color = "olivedrab4",
  #            size = 4.5) +
  #   scale_fill_manual(name = "Reward Size",
  #                     labels = c("Small Reward", "Large Reward"),
  #                     values = c("firebrick", "dodgerblue4")) +
  #   geom_line(size = 0.8) +
  #   scale_colour_manual(name = "Reward Size",
  #                       labels = c("Small Reward", "Large Reward"),
  #                       values = c("firebrick", "dodgerblue4")) +
  #   xlab("Time from Cue Onset (s)") +
  #   ylab("\u0394F/F (z-score)") +
  #   ggtitle("Session 10") +
  #   ylim(-1.5, 1.2) + 
  #   scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
  #   theme_apa() +
  #   theme(legend.position = "bottom",
  #         legend.title = element_blank(),
  #         legend.margin = margin(-5, 0, 0, 0)) +
  #   theme(plot.title = element_text(hjust = 0.5))
  # 
  # print(CS_plot_D10)
  # 
  # ggsave("CS-Evoked Activity D1.png", 
  #        CS_plot_D1, 
  #        device = png,
  #        path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
  #        width = 15.92,
  #        height = 10,
  #        units = "cm",
  #        dpi = 300)
  

  