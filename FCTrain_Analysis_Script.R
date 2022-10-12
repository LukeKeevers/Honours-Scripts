# FC Training Data - Data Analysis Script

  ## FC Training:
    ### 45 trials (15 of each forced-choice, 15 free choice) intermixed randomly
    ### Subjects nose poke in hole with active cue light to earn pellets (FR1)
    ### NP2 and NP4 available, cue light active when NP active.
    ### Trials last max 20s each, with 40s ITI.

  ## The purpose of this script is to analyse the data from the FC Training sessions
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
library(grid)

# ----------------------------------------------------------------------------------------------------

# Load Data

FCTrain_master <- read_csv("Final_Data/FC_Training/FC_Training_Combined.csv")

FCTrain_master <- FCTrain_master %>% convert_as_factor(subject)

FCTrain_master <- FCTrain_master %>% convert_as_factor(session)

FCTrain_master <- FCTrain_master %>% rename(NP4_wrong = NP2_incorrect); FCTrain_master <- FCTrain_master %>% rename(NP2_wrong = NP4_incorrect)

FCTrain_master <- FCTrain_master %>% rename(NP4_incorrect = NP4_wrong); FCTrain_master <- FCTrain_master %>% rename(NP2_incorrect = NP2_wrong)

FCTrain_master <- FCTrain_master[, c(1, 2, 3, 7, 5, 6, 4, 8)]

# ----------------------------------------------------------------------------------------------------

# Accuracy across session and trial type

  ## Separate master data into correct, incorrect and free choice responses, convert to long format and recombine

      correct <- FCTrain_master %>%
        select(subject, session, NP2_correct, NP4_correct) %>%
        pivot_longer(cols = c(NP2_correct, NP4_correct), 
                     names_to = "NP_hole", 
                     values_to = "correct") %>%
        convert_as_factor(NP_hole)
      
      levels(correct$NP_hole) <- c("NP-Left", "NP-Right")
      
      incorrect <- FCTrain_master %>%
        select(NP2_incorrect, NP4_incorrect) %>%
        pivot_longer(cols = c(NP2_incorrect, NP4_incorrect), 
                     names_to = NULL, 
                     values_to = "incorrect")
      
      accuracy_data <- cbind(correct, incorrect)

  ## Calculate accuracy (% of correct responses) and replace NA values (sessions in which 0 responses were made) with chance responding

      accuracy_data <- accuracy_data %>%
        mutate(accuracy = correct/(correct + incorrect)*100) #correct + incorrect
      
      accuracy_data[is.na(accuracy_data)] <- 50
      
  ## Calculate mean, SD and SEM for accuracy data
      
      accuracy_sum <- accuracy_data %>% 
        select(subject, session, NP_hole, accuracy) %>%
        group_by(session, NP_hole) %>%
        get_summary_stats(accuracy, type = "mean_sd") %>%
        arrange(session) %>%
        mutate(sem = sd/sqrt(n))
  
  ## Plot accuracy data
  
      accuracy_plot <- ggplot(accuracy_sum, mapping = aes(x = session,
                                                         y = mean,
                                                         colour = NP_hole,
                                                         shape = NP_hole,
                                                         group = NP_hole)) +
        geom_line() +
        geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .1) +
        geom_point(size = 3, stroke = 1) +
        scale_colour_manual(name = "Trial Type",
                            labels = c("Forced Choice Left", "Forced Choice Right"),
                            values = c("turquoise3", "magenta4")) +   
        scale_shape_manual(name = "Trial Type",
                           labels = c("Forced Choice Left", "Forced Choice Right"),
                           values = c(16, 17)) +
        geom_hline(yintercept = 50, linetype = 2, colour = "black") +
        # annotate(geom = "text", x = 4, y = 42, 
        #          label = "Chance",
        #          color = "red",
        #          size = 4.5) +
        xlab("Session") +
        ylab("Accuracy (% Correct Responses)") +
        ylim(0, 100) +
        theme_apa() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(-5, 0, 0, 0),
              legend.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14))
      
      print(accuracy_plot)  
      
      ggsave("FC Train Accuracy.png", 
             accuracy_plot, 
             device = png,
             path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
             width = 15.92,
             height = 10,
             units = "cm",
             dpi = 300)
      
  ## Check assumption of normality
      
      accuracy_data %>%
        group_by(session, NP_hole) %>%
        shapiro_test(accuracy) %>%
        filter(p <.05)
      
      ggqqplot(accuracy_data, "accuracy", ggtheme = theme_apa()) +
        facet_grid(session ~ NP_hole, labeller = "label_both")
      
  ## (7) X (2) ANOVA to test for differences in accuracy across time and trial type
      
      accuracy_aov <- ezANOVA(data = accuracy_data,
                              dv = accuracy,
                              wid = subject,
                              within = .(session, NP_hole),
                              type = 3)
      
      accuracy_aov
      
      accuracy_data %>%
        pairwise_t_test(accuracy ~ session,
                        paired = TRUE,
                        var.equal = FALSE,
                        p.adjust.method = "bonferroni") %>%
        filter(p.adj < .05)

      

# ----------------------------------------------------------------------------------------------------

# Free-choice preference across session and NP_hole

  ## Calculate free choice preference scores
      
     preference_data <- FCTrain_master %>%
        select(subject, session, NP2_freechoice, NP4_freechoice) %>%
        mutate(NP2_preference = NP2_freechoice/(NP2_freechoice + NP4_freechoice)*100) %>%
        mutate(NP4_preference = NP4_freechoice/(NP2_freechoice + NP4_freechoice)*100) %>%
        pivot_longer(cols = c(NP2_preference, NP4_preference),
                     names_to = "NP_hole",
                     values_to = "preference") %>%
        convert_as_factor(NP_hole)
     
     preference_data$preference <- round(preference_data$preference, digits = 2)  
      
  ## Rename NP_hole levels to left and right
      
     levels(preference_data$NP_hole) <- c("NP-Left", "NP-Right")
     
  ## Create summary data for preference across session and NP_hole
     
     preference_sum <- preference_data %>% 
       select(subject, session, NP_hole, preference) %>%
       group_by(session, NP_hole) %>%
       get_summary_stats(preference, type = "mean_sd") %>%
       arrange(session) %>%
       mutate(sem = sd/sqrt(n))
     
  ## Plot mean preference across session and NP_hole
      
    preference_plot <- ggplot(preference_data, mapping = aes(x = session,
                                                            y = preference,
                                                            fill = NP_hole)) +
      stat_summary(fun = mean,
                   geom = "bar",
                   position = position_dodge(0.7),
                   width = 0.7,
                   colour = "black",
                   alpha = 0.9) +
      # stat_summary(fun.data = mean_se,
      #              geom = "errorbar",
      #              width = .2, position = position_dodge(width = 0.70)) +
      scale_fill_manual(name = "NP Hole",
                        labels = c("Left NP", "Right NP"),
                        values = c("turquoise3", "magenta4")) +
      xlab("Session") +
      ylab("Preference (% of Responses)") +
      ylim(0, 100) +
      theme_apa() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            legend.margin = margin(-5, 0, 0, 0),
            legend.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = 0.5))
      
    print(preference_plot)
    
    ggsave("FC Train Preference.png", 
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
                                                                     fill = NP_hole)) +
      stat_summary(fun = identity,
                   geom = "bar",
                   position = position_dodge(0.7),
                   width = 0.7,
                   colour = "black",
                   alpha = 0.9) +
      scale_fill_manual(name = "NP Hole",
                        labels = c("Left NP", "Right NP"),
                        values = c("turquoise3", "magenta4")) +
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
    
    ggsave("FC Train Preference per Subject.png", 
           subject_preference_plot, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 15.92,
           height = 18.92,
           units = "cm",
           dpi = 300)
    
  ## One-way ANOVA to test for differences in preference for NP-L across time 
    
    NP2_preference_data <- preference_data %>%
      filter(NP_hole == "NP-Left")
    
    NP2_preference_aov <- ezANOVA(data = NP2_preference_data,
                              dv = preference,
                              wid = subject,
                              within = .(session),
                              type = 3)
    
    NP2_preference_aov
    
    preference_data %>%
      pairwise_t_test(preference ~ NP_hole,
                      paired = TRUE,
                      var.equal = FALSE,
                      p.adjust.method = "bonferroni") 

# ----------------------------------------------------------------------------------------------------
    
    accuracy_plot / preference_plot


