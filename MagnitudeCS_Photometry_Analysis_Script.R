# Magnitude Task Photometry Analysis Script

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

CS_1P_Alone_D1 <- read_csv("Magnitude_Task_D1_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "1") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D2 <- read_csv("Magnitude_Task_D2_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "2") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D3 <- read_csv("Magnitude_Task_D3_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "3") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D4 <- read_csv("Magnitude_Task_D4_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "4") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D5 <- read_csv("Magnitude_Task_D5_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "5") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D6 <- read_csv("Magnitude_Task_D6_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "6") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D7 <- read_csv("Magnitude_Task_D7_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "7") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D8 <- read_csv("Magnitude_Task_D8_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "8") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D9 <- read_csv("Magnitude_Task_D9_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "9") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_1P_Alone_D10 <- read_csv("Magnitude_Task_D10_CS_1P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "10") %>%
  mutate(CS_type = "CS_1P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D1 <- read_csv("Magnitude_Task_D1_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "1") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D2 <- read_csv("Magnitude_Task_D2_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "2") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D3 <- read_csv("Magnitude_Task_D3_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "3") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D4 <- read_csv("Magnitude_Task_D4_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "4") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D5 <- read_csv("Magnitude_Task_D5_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "5") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D6 <- read_csv("Magnitude_Task_D6_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "6") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D7 <- read_csv("Magnitude_Task_D7_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "7") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D8 <- read_csv("Magnitude_Task_D8_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "8") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D9 <- read_csv("Magnitude_Task_D9_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "9") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

CS_3P_Alone_D10 <- read_csv("Magnitude_Task_D10_CS_3P_Alone.csv", col_names = FALSE) %>% 
  mutate(session = "10") %>%
  mutate(CS_type = "CS_3P") %>%
  relocate(session, CS_type)

magnitude_master <- rbind(CS_1P_Alone_D1, CS_1P_Alone_D2, CS_1P_Alone_D3, CS_1P_Alone_D4, CS_1P_Alone_D5,
                          CS_1P_Alone_D6, CS_1P_Alone_D7, CS_1P_Alone_D8, CS_1P_Alone_D9, CS_1P_Alone_D10,
                          CS_3P_Alone_D1, CS_3P_Alone_D2, CS_3P_Alone_D3, CS_3P_Alone_D4, CS_3P_Alone_D5,
                          CS_3P_Alone_D6, CS_3P_Alone_D7, CS_3P_Alone_D8, CS_3P_Alone_D9, CS_3P_Alone_D10)

remove(CS_1P_Alone_D1, CS_1P_Alone_D2, CS_1P_Alone_D3, CS_1P_Alone_D4, CS_1P_Alone_D5,
       CS_1P_Alone_D6, CS_1P_Alone_D7, CS_1P_Alone_D8, CS_1P_Alone_D9, CS_1P_Alone_D10,
       CS_3P_Alone_D1, CS_3P_Alone_D2, CS_3P_Alone_D3, CS_3P_Alone_D4, CS_3P_Alone_D5,
       CS_3P_Alone_D6, CS_3P_Alone_D7, CS_3P_Alone_D8, CS_3P_Alone_D9, CS_3P_Alone_D10)

magnitude_master <- magnitude_master %>%
  convert_as_factor(session) %>%
  convert_as_factor(CS_type)

colnames(magnitude_master) <- c("session", "CS_type", "time",
                           "F7_mean", "F7_sd", "F7_peak",
                           "F8_mean", "F8_sd", "F8_peak",
                           "F9_mean", "F9_sd", "F9_peak",
                           "F10_mean", "F10_sd", "F10_peak",
                           "M3_mean", "M3_sd", "M3_peak",
                           "M4_mean", "M4_sd", "M4_peak",
                           "M5_mean", "M5_sd", "M5_peak",
                           "M9_mean", "M9_sd", "M9_peak")

magnitude_master <- magnitude_master %>%
  select(-starts_with("M4"))

magnitude_master <- magnitude_master[seq(1, nrow(magnitude_master), 10), ]

magnitude_mean <- magnitude_master %>%
  select(session, CS_type, time, ends_with("mean")) %>%
  pivot_longer(cols = ends_with("mean"), 
               names_to = "subject", 
               values_to = "zscore")

magnitude_mean$subject <- str_remove(magnitude_mean$subject, "_mean")

magnitude_mean <- magnitude_mean %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                                               session == 3) ~ "Early",
                                                    (session == 8 | session == 9 |
                                                     session == 10) ~ "Late")) %>%
  convert_as_factor(stage)

# magnitude_session_sum <- magnitude_mean %>%
#   select(-stage) %>%
#   group_by(session, CS_type, time) %>%
#   get_summary_stats(zscore, type = "mean_sd") %>%
#   mutate(sem = sd/sqrt(n))
# 
# write_csv(magnitude_session_sum, 
#           "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/Magnitude Task Session Data.csv")
# 
# 
# Magnitude_D1_sum <- magnitude_session_sum %>% filter(session == "1")
# Magnitude_D2_sum <- magnitude_session_sum %>% filter(session == "2")
# Magnitude_D3_sum <- magnitude_session_sum %>% filter(session == "3")
# Magnitude_D4_sum <- magnitude_session_sum %>% filter(session == "4")
# Magnitude_D5_sum <- magnitude_session_sum %>% filter(session == "5")
# Magnitude_D6_sum <- magnitude_session_sum %>% filter(session == "6")
# Magnitude_D7_sum <- magnitude_session_sum %>% filter(session == "7")
# Magnitude_D8_sum <- magnitude_session_sum %>% filter(session == "8")
# Magnitude_D9_sum <- magnitude_session_sum %>% filter(session == "9")
# Magnitude_D10_sum <- magnitude_session_sum %>% filter(session == "10")

magnitude_subject_sum <- magnitude_mean %>%
  filter(stage == "Early" | stage == "Late") %>%
  group_by(subject, stage, CS_type, time) %>%
  get_summary_stats(zscore, type = "mean_sd")

magnitude_stage_sum <- magnitude_subject_sum %>%
  select(stage, CS_type, subject, time, mean) %>%
  group_by(stage, CS_type, time) %>%
  get_summary_stats(mean, type = "mean_sd") %>%
  mutate(sem = sd/sqrt(n))

write_csv(magnitude_stage_sum, 
          "/Users/lukekeevers/Desktop/Uni/Honours/Photometry Data/TDTMatlabSDK/TDTSDK/Processed_Data/Stage Data/Magnitude CS Stage Data.csv")

magnitude_stage_sum <- read_csv("Stage Data/Magnitude CS Stage Data.csv")

magnitude_stage_sum_early <- magnitude_stage_sum %>%
  filter(stage == "Early" & time >-3 & time < 4)

magnitude_stage_sum_late <- magnitude_stage_sum %>%
  filter(stage == "Late" & time >-3 & time < 4)



magnitude_plot_early <- ggplot(data = magnitude_stage_sum_early,
                      mapping = aes(x = time,
                                    y = mean,
                                    colour = CS_type,
                                    group = CS_type,
                                    fill = CS_type)) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "grey69", colour = NA, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size = 0.5) +
  geom_ribbon(mapping = aes(x = time,
                            ymin = mean - sem,
                            ymax = mean + sem),
              colour = NA,
              alpha = 0.3) +
  annotate(geom = "text", x = -0.8, y = 0.7, 
           label = "CS ON",
           color = "black",
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
  ylim(-0.8, 0.8) + 
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_apa() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-5, 0, 0, 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = -.14,
                                  #vjust = -2,
                                  size = 25), 
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(magnitude_plot_early)

ggsave("Magnitude CS-Evoked Activity (Early).png", 
       magnitude_plot_early, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 15.92,
       height = 10,
       units = "cm",
       dpi = 300)

magnitude_plot_late <- ggplot(data = magnitude_stage_sum_late,
                               mapping = aes(x = time,
                                             y = mean,
                                             colour = CS_type,
                                             group = CS_type,
                                             fill = CS_type)) +
  annotate(geom = "rect", xmin = 0, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "grey69", colour = NA, alpha = 0.6) +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "black", size = 1) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size = 0.5) +
  geom_ribbon(mapping = aes(x = time,
                            ymin = mean - sem,
                            ymax = mean + sem),
              colour = NA,
              alpha = 0.3) +
  annotate(geom = "text", x = -0.8, y = 0.7, 
           label = "CS ON",
           color = "black",
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
  ylim(-0.8, 0.8) + 
  scale_x_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10)) +
  theme_apa() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(-5, 0, 0, 0),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = -.14,
                                  #vjust = -2,
                                  size = 25), 
        plot.subtitle = element_text(hjust = 0.5, size = 14))

print(magnitude_plot_late)

ggsave("Magnitude CS-Evoked Activity (Late).png", 
       magnitude_plot_late, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 15.92,
       height = 10,
       units = "cm",
       dpi = 300)

#----AUC Analysis----


magnitude_AUC <- magnitude_subject_sum %>%
  filter(time > 0 & time < 1) %>%
  group_by(subject, stage, CS_type) %>%
  summarise(AUC = trapz(time, mean))

magnitude_mean_AUC <- magnitude_AUC %>%
  group_by(stage, CS_type) %>%
  get_summary_stats(AUC, type = "mean_sd") %>%
  mutate(sem = sd/sqrt(n))
 

magnitude_AUC_plot <- ggplot(magnitude_mean_AUC, 
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
  geom_errorbar(aes(ymin = mean, ymax = mean + sem),width = .3, position = position_dodge(width = 0.80)) +
  # geom_errorbar(data = magnitude_mean_AUC %>% filter(stage == "Late"), 
  #               aes(ymin = mean, ymax = mean + sem),width = .3, position = position_dodge(width = 0.80)) +
  # annotate(geom = "rect", xmin = 0.63, xmax = 0.97, ymin = -0.075, ymax = 0,
  #          fill = "firebrick", colour = "black") +
  # annotate(geom = "rect", xmin = 1.03, xmax = 1.37, ymin = 0, ymax = 0.149,
  #          fill = "dodgerblue4", colour = "black") +
  geom_hline(yintercept = 0, linetype="dashed", 
             color = "black", size = 0.5) +
  scale_fill_manual(name = "Reward Size",
                    labels = c("Small Reward", "Large Reward"),
                    values = c("firebrick", "dodgerblue4")) +
  scale_x_discrete(labels = c("Early", "Late")) +
  xlab("Stage") +
  ylab("AUC") +
  xlab("Stage of Training") +
  ggtitle("C",
          subtitle = "AUC at CS Onset") +
  ylim(-.2, 0.4) +
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

print(magnitude_AUC_plot)

ggsave("Magnitude CS AUC Across Stages.png", 
       magnitude_AUC_plot, 
       device = png,
       path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
       width = 12,
       height = 14,
       units = "cm",
       dpi = 300)

magnitude_AUC_aov <- ezANOVA(data = magnitude_AUC,
                      dv = AUC,
                      wid = subject,
                      within = .(stage, CS_type),
                      type = 3)
magnitude_AUC_aov

magnitude_AUC %>%
  group_by(stage) %>%
  pairwise_t_test(AUC ~ CS_type, paired = TRUE,
                  p.adjust.method = "bonferroni")

magnitude_AUC %>%
  group_by(stage) %>%
  t_test(AUC ~ CS_type, paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>% 
  select(-c(n1, n2, .y., p.adj.signif))

one_way <- ezANOVA(data = magnitude_AUC,
                   dv = AUC,
                   wid = subject,
                   within = .(CS_type),
                   type = 3)

one_way

#----Peak Analysis----

# Magnitude_CS_peak <- magnitude_master %>%
#   select(session, CS_type, ends_with("peak")) %>%
#   distinct(.keep_all = TRUE) %>%
#   pivot_longer(cols = ends_with("peak"), 
#                names_to = "subject", 
#                values_to = "peak") %>%
#   relocate(subject, session, peak, CS_type)
# 
# Magnitude_CS_peak$subject <- str_remove(Magnitude_CS_peak$subject, "_peak")
# 
# Magnitude_CS_peak_sum <- Magnitude_CS_peak %>%
#   group_by(session, CS_type) %>%
#   get_summary_stats(peak, type = "mean_sd") %>%
#   mutate(sem = sd/sqrt(n)) %>%
#   mutate(session = session %>%
#            fct_relevel("10", after = 9)) %>%
#   arrange(session) 
# 
# Magnitude_CS_peak_plot <- ggplot(Magnitude_CS_peak_sum, 
#                                  mapping = aes(x = session,
#                                                y = mean,
#                                                fill = CS_type)) +
#   # stat_summary(fun.data = mean_se,
#   #              geom = "errorbar",
#   #              width = .2, position = position_dodge(width = 0.80)) +
#   stat_summary(fun = identity,
#                geom = "bar",
#                position = position_dodge(0.8),
#                width = 0.7,
#                colour = "black",
#                alpha = 1) +
#   geom_errorbar(aes(ymin = mean, ymax = mean + sem),width = .3, position = position_dodge(width = 0.80)) +
#   scale_fill_manual(name = "Reward Size",
#                     labels = c("Small Reward", "Large Reward"),
#                     values = c("firebrick", "dodgerblue4")) +
#   xlab("Session") +
#   ylab("Peak \u0394F/F (z-score)") +
#   #ylim(-0.5, 2.5) +
#   theme_apa() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         legend.margin = margin(0, 0, 0, 0)) +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# print(Magnitude_CS_peak_plot)
# 
# ggsave("Magnitude CS Peak Activity Across Sessions.png", 
#        Magnitude_CS_peak_plot, 
#        device = png,
#        path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures/Peri-Event Plots",
#        width = 15.92,
#        height = 10,
#        units = "cm",
#        dpi = 300)
# 
# Magnitude_CS_peak$peak <- round(Magnitude_CS_peak$peak, digits = 4)
# 
# Magnitude_CS_peak <- Magnitude_CS_peak %>% convert_as_factor(subject)
# 
# Magnitude_CS_peak %>% 
#   group_by(session, CS_type) %>%
#   shapiro_test(peak)
# 
# Magnitude_peak_aov <- ezANOVA(data = Magnitude_CS_peak,
#                     dv = peak,
#                     wid = subject,
#                     within = .(session, CS_type),
#                     type = 3,
#                     detailed = TRUE)
# Magnitude_peak_aov

