# Magnitude Task Data - Data Analysis Script

  ## Magnitude Task:
    ### 45 trials (15 of each forced-choice, 15 free choice) intermixed randomly, except first 10 trials must be forced-choice
    ### Subjects nose poke in hole with active cue light to earn pellets (FR1)
    ### NP2 and NP4 available, cue light active when NP active.
    ### Trials last max 20s each, with 40s ITI.
    ### Allocation of small reward (1 pellet) and large reward (3 pellets) to NP holes counterbalanced across subjects
      #### 3_1: M3, M9, F8, F9
      #### 1_3: M4, M5, F7, F10, F2

  ## The purpose of this script is to analyse the data from the Magnitude Task sessions
  ## The main goals include producing plots and statistical analyses for:

    ### Number of rewarded correct vs incorrect responses during forced-choice trials
    ### NP hole preference during free-choice trials

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

# Load Data

  Magnitude_master <- read_csv("Final_Data/Magnitude_Task/Magnitude_Task_Combined.csv")
  
  Magnitude_master <- Magnitude_master %>% convert_as_factor(subject)
  
  Magnitude_master <- Magnitude_master %>% convert_as_factor(session)
  
  Magnitude_master <- Magnitude_master %>% rename(small_wrong = large_incorrect); Magnitude_master <- Magnitude_master %>% rename(large_wrong = small_incorrect)
  
  Magnitude_master <- Magnitude_master %>% rename(small_incorrect = small_wrong); Magnitude_master <- Magnitude_master %>% rename(large_incorrect = large_wrong)
  
  Magnitude_master <- Magnitude_master[, c(1, 2, 3, 7, 5, 6, 4, 8)]
  
# ----------------------------------------------------------------------------------------------------

# Accuracy across session and trial type

  ## Separate master data into correct, incorrect and free choice responses, convert to long format and recombine

    correct <- Magnitude_master %>%
      select(subject, session, large_correct, small_correct) %>%
      pivot_longer(cols = c(large_correct, small_correct), 
                   names_to = "reward_size", 
                   values_to = "correct") %>%
      convert_as_factor(reward_size)
    
    levels(correct$reward_size) <- c("Large Reward", "Small Reward")
    
    incorrect <- Magnitude_master %>%
      select(large_incorrect, small_incorrect) %>%
      pivot_longer(cols = c(large_incorrect, small_incorrect), 
                   names_to = NULL, 
                   values_to = "incorrect")
    
    accuracy_data <- cbind(correct, incorrect)
    
    accuracy_data$reward_size <- relevel(accuracy_data$reward_size, "Small Reward")

  ## Calculate accuracy (% of correct responses) 

    accuracy_data <- accuracy_data %>%
      mutate(accuracy = correct/(correct + incorrect)*100) #correct + incorrect
    
    accuracy_data$accuracy <- round(accuracy_data$accuracy, digits = 2)

  ## Calculate mean, SD and SEM for accuracy data

    accuracy_sum <- accuracy_data %>% 
      select(subject, session, reward_size, accuracy) %>%
      group_by(session, reward_size) %>%
      get_summary_stats(accuracy, type = "mean_sd") %>%
      arrange(session) %>%
      mutate(sem = sd/sqrt(n))

  ## Plot accuracy data
    
    accuracy_sum$session <- as.numeric(accuracy_sum$session)

    accuracy_plot <- ggplot(accuracy_sum, mapping = aes(x = session,
                                                        y = mean,
                                                        colour = reward_size,
                                                        shape = reward_size,
                                                        group = reward_size)) +
      annotate(geom = "rect", xmin = 0.7, xmax = 3.3, ymin = -Inf, ymax = Inf,
               fill = "grey69", colour = NA, alpha = 0.4) +
      annotate(geom = "rect", xmin = 7.7, xmax = 10.3, ymin = -Inf, ymax = Inf,
               fill = "grey69", colour = NA, alpha = 0.4) +
      annotate(geom = "text", x = 2, y = 15, 
               label = "Early \n Training",
               color = "black",
               size = 4.5) +
      annotate(geom = "text", x = 9, y = 15, 
               label = "Late \n Training",
               color = "black",
               size = 4.5) +
      geom_line() +
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .1) +
      geom_point(size = 3, stroke = 1) +
      scale_colour_manual(name = "Trial Type",
                          labels = c("Forced Choice Small", "Forced Choice Large"),
                          values = c("firebrick", "dodgerblue4")) +   
      scale_shape_manual(name = "Trial Type",
                         labels = c("Forced Choice Small", "Forced Choice Large"),
                         values = c(16, 17)) +
      geom_hline(yintercept = 50, linetype = 2, colour = "black") +
      # annotate(geom = "text", x = 5.5, y = 42, 
      #          label = "Chance",
      #          color = "red",
      #          size = 4.5) +
      xlab("Session") +
      ylab("Accuracy (% Correct)") +
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      ggtitle("A") +
      ylim(0, 100) +
      theme_apa() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.14,
                                    #vjust = -2,
                                    size = 25))
    
    print(accuracy_plot)  
    
    ggsave("Magnitude Accuracy.png", 
           accuracy_plot, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 15.92,
           height = 12,
           units = "cm",
           dpi = 300)
    
  ## Accuracy Across Stages
    
    accuracy_data <- accuracy_data %>%
      mutate(stage = case_when((session == 1 | session == 2 | 
                                  session == 3) ~ "Early",
                               (session == 8 | session == 9 |
                                  session == 10) ~ "Late")) %>%
      convert_as_factor(stage)
    
    accuracy_subject_sum <- accuracy_data %>%
      select(subject, session, stage, reward_size, accuracy) %>%
      filter(stage == "Early" | stage == "Late") %>%
      group_by(subject, stage, reward_size) %>%
      get_summary_stats(accuracy, type = "mean_sd") %>%
      rename(accuracy = mean)
    
    accuracy_stage_sum <- accuracy_subject_sum %>%
      select(subject, stage, reward_size, accuracy) %>%
      group_by(stage, reward_size) %>%
      get_summary_stats(accuracy, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n)) %>%
      rename(accuracy = mean)
    
    
    
    accuracy_stage_plot <- ggplot(data = accuracy_stage_sum,
                                mapping = aes(x = stage,
                                              y = accuracy,
                                              fill = reward_size)) +
      # colour = reward_size,
      # shape = reward_size,
      # group = reward_size)) +
      stat_summary(fun = identity,
                   geom = "bar",
                   position = position_dodge(0.8),
                   width = 0.7,
                   colour = "black",
                   alpha = 1) +
      #geom_line() +
      geom_errorbar(aes(ymin = accuracy, ymax = accuracy + sem),width = .3, position = position_dodge(width = 0.80)) +
      scale_fill_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
      #geom_point(size = 3, stroke = 1) +
      # annotate(geom = "line", aes(xmin = 2.2, xmax = 2,2, ymin = 0.1, ymax = 0.235),
      #          colour = "black") +
      annotate(geom = "rect", xmin = 1.8, xmax = 2.2, ymin = 110, ymax = 110,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 1.8, xmax = 1.8, ymin = 84, ymax = 110,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 2.2, xmax = 2.2, ymin = 105, ymax = 110,
               fill = "black", colour = "black") +
      annotate(geom = "text", x = 2, y = 115, 
               label = "***",
               color = "black",
               size = 8) +
      scale_x_discrete(labels = c("Early", "Late")) +
      #scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
      scale_y_continuous(breaks = seq(0, 120, by = 50)) +
      xlab("Stage of Training") +
      ylab("Accuracy (% Correct)") +
      #ylim(0, 120) +
      ggtitle("B") +
      theme_apa() +
      # geom_signif(stat = "identity",
      #             aes(x = 2.2, xend = 2.2, y = 0.1, yend = 0.235,
      #                 annotation = c("**"))) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = -.23,
                                      #vjust = -2,
                                      size = 25))
    
    print(accuracy_stage_plot)
    
    
    magnitude_accuracy_plots <- accuracy_plot + accuracy_stage_plot + 
      plot_layout(widths = c(1.5, 1))
    
    ggsave("Magnitude Accuracy Plots.png", 
           magnitude_accuracy_plots, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 25.92,
           height = 12,
           units = "cm",
           dpi = 300)

  ## Check assumption of normality

    accuracy_data %>%
      group_by(session, reward_size) %>%
      shapiro_test(accuracy) %>%
      filter(p <.05)
    
    ggqqplot(accuracy_data, "accuracy", ggtheme = theme_apa()) +
      facet_grid(session ~ reward_size, labeller = "label_both")

  ## (7) X (2) ANOVA to test for differences in accuracy across time and trial type

    accuracy_aov <- ezANOVA(data = accuracy_subject_sum,
                            dv = accuracy,
                            wid = subject,
                            within = .(stage, reward_size),
                            type = 3)
    
    accuracy_aov
    
    accuracy_subject_sum %>%
      group_by(stage) %>%
      t_test(accuracy ~ reward_size, paired = TRUE) %>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance() %>% 
      select(-c(n1, n2, .y., p.adj.signif))
    
    # accuracy_oneway_aov <- accuracy_data %>%
    #   group_by(reward_size) %>%
    #   anova_test(dv = accuracy, wid = subject, within = session) %>%
    #   get_anova_table() %>%
    #   adjust_pvalue(method = "bonferroni")
    # 
    # accuracy_oneway_aov
    
## ----- Forced-Choice Latency ----
    
  magnitude_latency <- read.csv("Magnitude_Latency/Magnitude_Latency_Final.csv") %>%
      convert_as_factor(session) %>%
      mutate(stage = case_when((session == 1 | session == 2 | 
                                  session == 3) ~ "Early",
                               (session == 8 | session == 9 |
                                  session == 10) ~ "Late")) %>%
      convert_as_factor(stage)
    
    magnitude_latency_sum <- magnitude_latency %>%
      group_by(session, reward_size) %>%
      get_summary_stats(latency, type = "mean_sd") %>%
      arrange(session) %>%
      mutate(sem = sd/sqrt(n))
    
    magnitude_latency_subject_sum <- magnitude_latency %>%
      select(subject, stage, reward_size, latency) %>%
      filter(stage == "Early" | stage == "Late") %>%
      group_by(subject, stage, reward_size) %>%
      get_summary_stats(latency, type = "mean_sd") %>%
      rename(latency = mean)
    
    magnitude_latency_stage_sum <- magnitude_latency_subject_sum %>%
      select(subject, stage, reward_size, latency) %>%
      group_by(stage, reward_size) %>%
      get_summary_stats(latency, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n)) %>%
      rename(latency = mean)
    
    magnitude_latency_sum$session <- as.numeric(magnitude_latency_sum$session)
    

  
  magnitude_latency_plot <- ggplot(magnitude_latency_sum, mapping = aes(x = session,
                                                      y = mean,
                                                      colour = reward_size,
                                                      shape = reward_size,
                                                      group = reward_size)) +
    annotate(geom = "rect", xmin = 0.7, xmax = 3.3, ymin = -Inf, ymax = Inf,
             fill = "grey69", colour = NA, alpha = 0.4) +
    annotate(geom = "rect", xmin = 7.7, xmax = 10.3, ymin = -Inf, ymax = Inf,
             fill = "grey69", colour = NA, alpha = 0.4) +
    annotate(geom = "text", x = 2, y = 9, 
             label = "Early \n Training",
             color = "black",
             size = 4.5) +
    annotate(geom = "text", x = 9, y = 9, 
             label = "Late \n Training",
             color = "black",
             size = 4.5) +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .1) +
    geom_point(size = 3, stroke = 1) +
    scale_colour_manual(name = "Trial Type",
                        labels = c("Forced Choice Small", "Forced Choice Large"),
                        values = c("firebrick", "dodgerblue4")) +   
    scale_shape_manual(name = "Trial Type",
                       labels = c("Forced Choice Small", "Forced Choice Large"),
                       values = c(16, 17)) +
    xlab("Session") +
    ylab("Latency to Nose Poke (s)") +
    ylim(0, 10) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
    ggtitle("A") +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-5, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.14,
                                    #vjust = -2,
                                    size = 25))
  
  print(magnitude_latency_plot)  
  
  ggsave("Magnitude Latency.png", 
         magnitude_latency_plot, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
         width = 15.92,
         height = 10,
         units = "cm",
         dpi = 300)
  
  magnitude_latency_stage_plot <- ggplot(data = magnitude_latency_stage_sum,
                                mapping = aes(x = stage,
                                              y = latency,
                                              fill = reward_size)) +
    # colour = reward_size,
    # shape = reward_size,
    # group = reward_size)) +
    stat_summary(fun = identity,
                 geom = "bar",
                 position = position_dodge(0.8),
                 width = 0.7,
                 colour = "black",
                 alpha = 1) +
    #geom_line() +
    geom_errorbar(aes(ymin = latency, ymax = latency + sem),width = .3, position = position_dodge(width = 0.80)) +
    scale_fill_manual(name = "Reward Size",
                      labels = c("Small Reward", "Large Reward"),
                      values = c("firebrick", "dodgerblue4")) +
    #geom_point(size = 3, stroke = 1) +
    # annotate(geom = "line", aes(xmin = 2.2, xmax = 2,2, ymin = 0.1, ymax = 0.235),
    #          colour = "black") +
    annotate(geom = "rect", xmin = 1.8, xmax = 2.2, ymin = 6.5, ymax = 6.5,
             fill = "black", colour = "black") +
    annotate(geom = "rect", xmin = 1.8, xmax = 1.8, ymin = 3.8, ymax = 6.5,
             fill = "black", colour = "black") +
    annotate(geom = "rect", xmin = 2.2, xmax = 2.2, ymin = 5.5, ymax = 6.5,
             fill = "black", colour = "black") +
    annotate(geom = "text", x = 2, y = 7.5, 
             label = "**",
             color = "black",
             size = 8) +
    scale_x_discrete(labels = c("Early", "Late")) +
    #scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    xlab("Stage of Training") +
    ylab("Latency to Nose Poke (s)") +
    ylim(0, 10) +
    ggtitle("B") +
    theme_apa() +
    # geom_signif(stat = "identity",
    #             aes(x = 2.2, xend = 2.2, y = 0.1, yend = 0.235,
    #                 annotation = c("**"))) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-5, 0, 0, 0),
          legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = -.23,
                                    #vjust = -2,
                                    size = 25))
  
  print(magnitude_latency_stage_plot)
  
  
  magnitude_latency_plots <- magnitude_latency_plot + magnitude_latency_stage_plot + 
    plot_layout(widths = c(1.5, 1))
  
  ggsave("Magnitude Latency Plots.png", 
         magnitude_latency_plots, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
         width = 25.92,
         height = 12,
         units = "cm",
         dpi = 300)
  
  magnitude_latency_aov <- ezANOVA(data = magnitude_latency_subject_sum,
                          dv = latency,
                          wid = subject,
                          within = .(stage, reward_size),
                          type = 3)
  
  magnitude_latency_aov
  
  magnitude_latency_subject_sum %>%
    group_by(stage) %>%
    t_test(latency ~ reward_size, paired = TRUE) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance() %>% 
    select(-c(n1, n2, .y., p.adj.signif))
  
  # magnitude_latency_subject_sum %>%
  #   group_by(stage) %>%
  #   pairwise_t_test(latency ~ reward_size,
  #                   paired = TRUE,
  #                   var.equal = FALSE,
  #                   p.adjust.method = "bonferroni")
  
  
    
# ----------------------------------------------------------------------------------------------------

# Free-choice preference across session and NP_hole
    
  ## Calculate free choice preference scores
    
    preference_data <- Magnitude_master %>%
      select(subject, session, large_freechoice, small_freechoice) %>%
      mutate(large_preference = large_freechoice/(large_freechoice + small_freechoice)*100) %>%
      mutate(small_preference = small_freechoice/(large_freechoice + small_freechoice)*100) %>%
      pivot_longer(cols = c(large_preference, small_preference),
                   names_to = "reward_size",
                   values_to = "preference") %>%
      convert_as_factor(reward_size) %>%
      mutate(stage = case_when((session == 1 | session == 2 | 
                                  session == 3) ~ "Early",
                               (session == 8 | session == 9 |
                                  session == 10) ~ "Late")) %>%
    convert_as_factor(stage)
    
    preference_data$preference <- round(preference_data$preference, digits = 2)  
    
  ## Rename reward_size levels to large and small
    
    levels(preference_data$reward_size) <- c("Large Reward", "Small Reward")
    
    preference_data$reward_size <- relevel(preference_data$reward_size, "Small Reward")
    
  ## Create summary data for preference across session and reward_size
    
    preference_session_sum <- preference_data %>%
      select(subject, session, reward_size, preference) %>%
      group_by(session, reward_size) %>%
      get_summary_stats(preference, type = "mean_sd") %>%
      arrange(session) %>%
      rename(preference = mean) %>%
      mutate(sem = sd/sqrt(n))
    
    preference_subject_sum <- preference_data %>%
      select(subject, stage, reward_size, preference) %>%
      filter(stage == "Early" | stage == "Late") %>%
      group_by(subject, stage, reward_size) %>%
      get_summary_stats(preference, type = "mean_sd") %>%
      arrange(stage) %>%
      rename(preference = mean)
    
    preference_stage_sum <- preference_subject_sum %>%
      select(subject, stage, reward_size, preference) %>%
      group_by(stage, reward_size) %>%
      get_summary_stats(preference, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n)) %>%
      arrange(stage) %>%
      rename(preference = mean) %>%
      filter(reward_size == "Large Reward")
    
  ## Plot mean preference across session and reward_size
    
    preference_data$session <- as.numeric(preference_data$session)
    
    preference_plot <- ggplot(preference_data, mapping = aes(x = session,
                                                             y = preference,
                                                             fill = reward_size)) +
      annotate(geom = "rect", xmin = 0.5, xmax = 3.5, ymin = -Inf, ymax = Inf,
               fill = "grey69", colour = NA, alpha = 0.4) +
      annotate(geom = "rect", xmin = 7.5, xmax = 10.5, ymin = -Inf, ymax = Inf,
               fill = "grey69", colour = NA, alpha = 0.4) +
      annotate(geom = "text", x = 2, y = 105, 
               label = "Early \n Training",
               color = "black",
               size = 4.5) +
      annotate(geom = "text", x = 9, y = 105, 
               label = "Late \n Training",
               color = "black",
               size = 4.5) +
      stat_summary(fun = mean,
                   geom = "bar",
                   position = position_dodge(0.7),
                   width = 0.7,
                   colour = "black",
                   alpha = 0.9) +
      scale_fill_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
      xlab("Session") +
      ylab("Preference (% of Responses)") +
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
      ylim(0, 110) +
      ggtitle("A") +
      #scale_y_continuos(breaks = seq(0, 110, by = 30)) +
      theme_apa() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = -.14,
                                      #vjust = -2,
                                      size = 25))
    
    print(preference_plot)
    
    ggsave("Magnitude Preference.png", 
           preference_plot, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 15.92,
           height = 10,
           units = "cm",
           dpi = 300)
    
  ## Plot preference for individual subjects
    
    subject_preference_plot <- ggplot(preference_data, mapping = aes(x = session,
                                                                     y = preference,
                                                                     fill = reward_size)) +
      stat_summary(fun = identity,
                   geom = "bar",
                   position = position_dodge(0.7),
                   width = 0.7,
                   colour = "black",
                   alpha = 0.9) +
      scale_fill_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
      facet_wrap("subject") +
      xlab("Session") +
      ylab("Preference (% of Free Choice Responses)") +
      ylim(0, 100) +
      theme_apa() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(subject_preference_plot)
    
  ## Stage Preference Plot
    
    preference_stage_plot <- ggplot(data = preference_stage_sum,
                                    mapping = aes(x = stage,
                                                  y = preference,
                                                  fill = reward_size)) +
      # colour = reward_size,
      # shape = reward_size,
      # group = reward_size)) +
      stat_summary(fun = identity,
                   geom = "bar",
                   position = position_dodge(0.8),
                   width = 0.5,
                   colour = "black",
                   alpha = 1) +
      #geom_line() +
      geom_errorbar(aes(ymin = preference, ymax = preference + sem),width = .3, position = position_dodge(width = 0.80)) +
      scale_fill_manual(name = "Reward Size",
                        labels = c("Large Reward"),
                        values = c("dodgerblue4")) +
      #geom_point(size = 3, stroke = 1) +
      # annotate(geom = "line", aes(xmin = 2.2, xmax = 2,2, ymin = 0.1, ymax = 0.235),
      #          colour = "black") +
      annotate(geom = "rect", xmin = 1, xmax = 2, ymin = 100, ymax = 100,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 1, xmax = 1, ymin = 65, ymax = 100,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 2, xmax = 2, ymin = 96, ymax = 100,
               fill = "black", colour = "black") +
      annotate(geom = "text", x = 1.5, y = 105, 
               label = "***",
               color = "black",
               size = 8) +
      scale_x_discrete(labels = c("Early", "Late")) +
      #scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
      xlab("Stage of Training") +
      ylab("Preference (% of Responses)") +
      ylim(0, 110) +
      ggtitle("B") +
      theme_apa() +
      # geom_signif(stat = "identity",
      #             aes(x = 2.2, xend = 2.2, y = 0.1, yend = 0.235,
      #                 annotation = c("**"))) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = -.23,
                                      #vjust = -2,
                                      size = 25))
    
    print(preference_stage_plot)
    
    preference_plots <- preference_plot + preference_stage_plot + 
      plot_layout(widths = c(1.5, 1))
    
    ggsave("Magnitude Preference Plots.png", 
           preference_plots, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 25.92,
           height = 12,
           units = "cm",
           dpi = 300)
    
  ## (7) X (2) ANOVA to test for differences in preference across time and trial type
    
   preference_subject_sum %>%
      filter(reward_size == "Large Reward") %>%
      group_by(reward_size) %>%
      t_test(preference ~ stage, paired = TRUE) %>%
      adjust_pvalue(method = "bonferroni") %>%
      add_significance() %>% 
      select(-c(n1, n2, .y., p.adj.signif))
    
#     large_preference_data <- preference_data %>%
#       filter(reward_size == "Large Reward")
#     
#     large_preference_aov <- ezANOVA(data = large_preference_data,
#                               dv = preference,
#                               wid = subject,
#                               within = .(session),
#                               type = 3,
#                               detailed = TRUE)
#     
#     large_preference_aov
#     
#     large_preference_data %>%
#       pairwise_t_test(preference ~ session,
#                       paired = TRUE,
#                       var.equal = FALSE,
#                       p.adjust.method = "bonferroni") %>%
#       filter(p.adj < .05)
#     
# # ----------------------------------------------------------------------------------------------------
#     
#     accuracy_plot / preference_plot




