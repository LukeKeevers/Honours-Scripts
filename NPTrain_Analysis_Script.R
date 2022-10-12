# NP Training Data - Data Analysis Script

  ## NP Training:
    ### 30 min duration (or max 100 trials)
    ### Subjects nose poke to earn pellets (FR1)
    ### NP3 available, cue light active when NP active, off for 5-sec after response

  ## The purpose of this script is to analyse the data from the NP Training sessions
  ## The main goals include producing plots and statistical analyses for:

    ### Number of rewarded NPs across sessions

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
  
  NPTrain_master <- read_csv("Final_Data/NP_Training/NP_Training_Combined.csv")
  
  NPTrain_master <- NPTrain_master %>% convert_as_factor(subject)
  
  NPTrain_master <- NPTrain_master %>% convert_as_factor(session)
  
# ----------------------------------------------------------------------------------------------------
  
# Number of rewarded NP across sessions
    
  ## Summarise NP data
  
    NP_sum <- NPTrain_master %>%
      group_by(session) %>%
      get_summary_stats(NP_total, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n))
  
  ## Plot mean rewarded NP per session and data per subject
    
    NP_plot <- ggplot(data = NP_sum,
                      mapping = aes(x = session,
                                    y = mean)) +
      geom_line(aes(group = 1)) + 
      geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = .1) +
      geom_point(shape = 21, fill = "black", size = 3, stroke = 1) +
      xlab("Session") +
      ylab("Number of Pellets Earned") +
      ylim(0, 100) +
      theme_apa() +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 14))
    
    print(NP_plot)
    
    ggsave("NP Training Plot.png", 
           NP_plot, 
           device = png,
           path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
           width = 15.92,
           height = 10,
           units = "cm",
           dpi = 300)
    
    subject_NP_plot <- ggplot(data = NPTrain_master,
                              mapping = aes(x = session,
                                            y = NP_total,
                                            colour = subject,
                                            group = subject)) +
      geom_line() +
      geom_point() +
      geom_line() +
      geom_point() +
      xlab("Session") +
      ylab("Number of Pellets Earned") +
      ylim(0, 100) +
      theme_apa() 
    
    print(subject_NP_plot)
    
    NP_plot + subject_NP_plot
  
  ## One-way ANOVA for rewarded NPs across sessions
    
    NP_aov <- ezANOVA(data = NPTrain_master,
                      dv = NP_total,
                      wid = subject,
                      within = .(session),
                      type = 3)

    NP_aov
    
    pairwise_t_test(data = NPTrain_master,
                    formula = NP_total ~ session,
                    paired = TRUE,
                    comparisons = list(c("6", "1")),
                    var.equal = FALSE,
                    p.adjust.method = "bonferroni") 
    
    
    
    
    
    
    
  