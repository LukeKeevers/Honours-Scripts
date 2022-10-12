# Magnitude Pellet Analysis

setwd("~/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data")

library(tidyverse)
library(papaja)
library(fluoR)
library(rmedpc)
library(ez)
library(ggpubr)
library(rstatix)
library(patchwork)

#----Load Data----


NP_1P_D1 <- read_csv("Magnitude_Task_D1_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "1") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D2 <- read_csv("Magnitude_Task_D2_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "2") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D3 <- read_csv("Magnitude_Task_D3_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "3") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D4 <- read_csv("Magnitude_Task_D4_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "4") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D5 <- read_csv("Magnitude_Task_D5_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "5") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D6 <- read_csv("Magnitude_Task_D6_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "6") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D7 <- read_csv("Magnitude_Task_D7_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "7") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D8 <- read_csv("Magnitude_Task_D8_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "8") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D9 <- read_csv("Magnitude_Task_D9_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "9") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_1P_D10 <- read_csv("Magnitude_Task_D10_FC_1P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "10") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

NP_3P_D1 <- read_csv("Magnitude_Task_D1_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "1") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D2 <- read_csv("Magnitude_Task_D2_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "2") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D3 <- read_csv("Magnitude_Task_D3_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "3") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D4 <- read_csv("Magnitude_Task_D4_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "4") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D5 <- read_csv("Magnitude_Task_D5_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "5") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D6 <- read_csv("Magnitude_Task_D6_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "6") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D7 <- read_csv("Magnitude_Task_D7_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "7") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D8 <- read_csv("Magnitude_Task_D8_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "8") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D9 <- read_csv("Magnitude_Task_D9_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "9") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_3P_D10 <- read_csv("Magnitude_Task_D10_FC_3P_Correct.csv", col_names = FALSE) %>% 
  mutate(session = "10") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

NP_master <- rbind(NP_1P_D1, NP_1P_D2, NP_1P_D3, NP_1P_D4, NP_1P_D5,
                   NP_1P_D6, NP_1P_D7, NP_1P_D8, NP_1P_D9, NP_1P_D10,
                   NP_3P_D1, NP_3P_D2, NP_3P_D3, NP_3P_D4, NP_3P_D5,
                   NP_3P_D6, NP_3P_D7, NP_3P_D8, NP_3P_D9, NP_3P_D10)
remove(NP_1P_D1, NP_1P_D2, NP_1P_D3, NP_1P_D4, NP_1P_D5,
       NP_1P_D6, NP_1P_D7, NP_1P_D8, NP_1P_D9, NP_1P_D10,
       NP_3P_D1, NP_3P_D2, NP_3P_D3, NP_3P_D4, NP_3P_D5,
       NP_3P_D6, NP_3P_D7, NP_3P_D8, NP_3P_D9, NP_3P_D10)

NP_master <- NP_master %>%
  convert_as_factor(session) %>%
  convert_as_factor(CS_type)

colnames(NP_master) <- c("session", "CS_type", "time",
                                "F7_mean", "F7_sd", "F7_peak",
                                "F8_mean", "F8_sd", "F8_peak",
                                "F9_mean", "F9_sd", "F9_peak",
                                "F10_mean", "F10_sd", "F10_peak",
                                "M3_mean", "M3_sd", "M3_peak",
                                "M4_mean", "M4_sd", "M4_peak",
                                "M5_mean", "M5_sd", "M5_peak",
                                "M9_mean", "M9_sd", "M9_peak")

NP_master <- NP_master %>%
  select(-starts_with("M4")) %>%
  filter(time > -5)

NP_master <- NP_master[seq(1, nrow(NP_master), 3), ]

NP_mean <- NP_master %>%
  select(session, CS_type, time, ends_with("mean")) %>%
  pivot_longer(cols = ends_with("mean"), 
               names_to = "subject", 
               values_to = "zscore")

NP_mean$subject <- str_remove(NP_mean$subject, "_mean")

NP_mean <- NP_mean %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                                                 session == 3) ~ "Early",
                                                              (session == 8 | session == 9 |
                                                               session == 10) ~ "Late")) %>%
  convert_as_factor(stage) 

NP_mean <- NP_mean %>%
  filter(stage == "Early" | stage == "Late")

# NP_session_sum <- NP_mean %>%
#   select(-stage) %>%
#   group_by(session, CS_type, time) %>%
#   get_summary_stats(zscore, type = "mean_sd") %>%
#   mutate(sem = sd/sqrt(n))
# 
# write_csv(NP_session_sum, 
#           "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/NP Session Data.csv")
# 
# 
# NP_D1_sum <- NP_session_sum %>% filter(session == "1" & time > -4)
# NP_D2_sum <- NP_session_sum %>% filter(session == "2" & time > -4)
# NP_D3_sum <- NP_session_sum %>% filter(session == "3" & time > -4)
# NP_D4_sum <- NP_session_sum %>% filter(session == "4" & time > -4)
# NP_D5_sum <- NP_session_sum %>% filter(session == "5" & time > -4)
# NP_D6_sum <- NP_session_sum %>% filter(session == "6" & time > -4)
# NP_D7_sum <- NP_session_sum %>% filter(session == "7" & time > -4)
# NP_D8_sum <- NP_session_sum %>% filter(session == "8" & time > -4)
# NP_D9_sum <- NP_session_sum %>% filter(session == "9" & time > -4)
# NP_D10_sum <- NP_session_sum %>% filter(session == "10" & time > -4)

NP_subject_sum <- NP_mean %>%
  filter(stage == "Early" | stage == "Late") %>%
  group_by(subject, stage, CS_type, time) %>%
  get_summary_stats(zscore, type = "mean_sd")

NP_stage_sum <- NP_subject_sum %>%
  select(stage, CS_type, subject, time, mean) %>%
  group_by(stage, CS_type, time) %>%
  get_summary_stats(mean, type = "mean_sd") %>%
  mutate(sem = sd/sqrt(n))

write_csv(NP_stage_sum, 
          "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/Magnitude NP Stage Data.csv")

NP_stage_sum_early <- NP_stage_sum %>%
  filter(stage == "Early" & time >-3 & time < 4)

NP_stage_sum_late <- NP_stage_sum %>%
  filter(stage == "Late" & time >-3 & time < 4)


NP_plot_early <- ggplot(data = NP_stage_sum_early,
                               mapping = aes(x = time,
                                             y = mean,
                                             colour = CS_type,
                                             group = CS_type,
                                             fill = CS_type)) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "olivedrab4", colour = NA, alpha = 0.2) +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "olivedrab4", size = 1) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size = 0.5) +
  geom_ribbon(mapping = aes(x = time,
                            ymin = mean - sem,
                            ymax = mean + sem),
              colour = NA,
              alpha = 0.3) +
  annotate(geom = "text", x = -1.2, y = 0.8, 
           label = "NP + Pellet(s)",
           color = "olivedrab4",
           size = 4.5) +
  scale_fill_manual(name = "Reward Size",
                    labels = c("Small Reward", "Large Reward"),
                    values = c("firebrick", "dodgerblue4")) +
  geom_line(size = 0.8) +
  scale_colour_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
  xlab("Time from NP / Pellet Delivery (s)") +
  ylab("\u0394F/F (z-score)") +
  ggtitle("A",
          subtitle = "Early Training (Sessions 1-3)") +
  ylim(-1.7, 1.0) + 
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_apa() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-5, 0, 0, 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = -.10,
                                  #vjust = -2,
                                  size = 25), 
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(NP_plot_early)

ggsave("NP-Evoked Activity (Early).png", 
       NP_plot_early, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 15.92,
       height = 10,
       units = "cm",
       dpi = 300)

NP_plot_late <- ggplot(data = NP_stage_sum_late,
                              mapping = aes(x = time,
                                            y = mean,
                                            colour = CS_type,
                                            group = CS_type,
                                            fill = CS_type)) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "olivedrab4", colour = NA, alpha = 0.2) +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "olivedrab4", size = 1) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size = 0.5) +
  geom_ribbon(mapping = aes(x = time,
                            ymin = mean - sem,
                            ymax = mean + sem),
              colour = NA,
              alpha = 0.3) +
  annotate(geom = "text", x = -1.5, y = 0.8, 
           label = "NP + Pellets",
           color = "olivedrab4",
           size = 4.5) +
  scale_fill_manual(name = "Reward Size",
                    labels = c("Small Reward", "Large Reward"),
                    values = c("firebrick", "dodgerblue4")) +
  geom_line(size = 0.8) +
  scale_colour_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
  xlab("Time from NP / Pellet Delivery (s)") +
  ylab("\u0394F/F (z-score)") +
  ggtitle("B",
          subtitle = "Late Training (Sessions 8-10)") +
  ylim(-1.7, 1.0) +
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_apa() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-5, 0, 0, 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = -.10,
                                  #vjust = -2,
                                  size = 25), 
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(NP_plot_late)

ggsave("NP-Evoked Activity (Late).png", 
       NP_plot_late, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 15.92,
       height = 10,
       units = "cm",
       dpi = 300)



NP_AUC <- NP_mean %>%
  filter(time > 0 & time < 1) %>%
  group_by(subject, stage, CS_type) %>%
  summarise(AUC = trapz(time, zscore))

NP_mean_AUC <- NP_AUC %>%
  group_by(stage, CS_type) %>%
  get_summary_stats(AUC, type = "mean_sd") %>%
  mutate(sem = sd/sqrt(n))
  

NP_AUC_plot <- ggplot(NP_mean_AUC, 
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
 geom_errorbar(aes(ymin = mean - sem, ymax = mean),width = .3, position = position_dodge(width = 0.80)) +
  scale_fill_manual(name = "Reward Size",
                    labels = c("Small Reward", "Large Reward"),
                    values = c("firebrick", "dodgerblue4")) +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "black", size = 0.5) +
  xlab("Stage") +
  ylab("AUC") +
  scale_x_discrete(labels = c("Early", "Late")) +
  xlab("Stage of Training") +
  ggtitle("C",
          subtitle = "AUC at NP and Reward Delivery") +
  ylim(-1.2, 1) +
  theme_apa() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = -.17,
                                  #vjust = -2,
                                  size = 25), 
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(NP_AUC_plot)

ggsave("NP AUC Across Stages.png", 
       NP_AUC_plot, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 12,
       height = 14,
       units = "cm",
       dpi = 300)

NP_AUC_aov <- ezANOVA(data = NP_AUC,
                             dv = AUC,
                             wid = subject,
                             within = .(stage, CS_type),
                             type = 3)
NP_AUC_aov

