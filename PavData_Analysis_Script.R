# PavTask Data - Conditioned Responding Analysis

  ## The purpose of this script is to analyse the data from the Pavlovian task. 
  ## The main goals include producing plots and statistical analyses for:

    ### Baseline responding (Rate of MEs during 5s period pre-CS) 
    ### Conditioned responding (Rate of MEs during 5s CS period)
    ### Compare the above variables across session and reward size. 

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

# ---------Functions for Error Bar Corrections--------
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


# --------------------------------------------------------------------------------------

# Load Data
  
  pav_master <- read_csv("Final_Data/Pav_Task/Pav_Combined2.csv")
  
# Pivot data into wide form, clean column names
# Remove "D" from session value and convert session to factor
# Re-level factor to place Day 10 at end
# Re-label reward size levels
  
  pav_master <- pivot_wider(pav_master, 
                            id_cols = c(subject, session, CS_type), 
                            names_from = pre_post, 
                            values_from = c(n, ME_duration_sum, adj_time))
  
  pav_master <- pav_master %>% 
    rename(Pre_CS = n_Pre_CS) %>% 
    rename(During_CS = n_During_CS) %>% 
    rename(Post_CS = n_Post_CS)
  
  pav_master$session <- sub('D', '', pav_master$session)
  
  pav_master$session <- as.factor(pav_master$session)
  
  pav_master <- pav_master %>%
    mutate(session = session %>%
             fct_relevel("10", after = 9)) %>%
    convert_as_factor(subject) %>%
    mutate(pre_cs_rate = Pre_CS/adj_time_Pre_CS) %>%
    mutate(during_cs_rate = During_CS/adj_time_During_CS) %>%
    mutate(change_rate = during_cs_rate - pre_cs_rate) %>%
    rename(reward_size = CS_type) %>%
    convert_as_factor(reward_size) %>%
    clean_names()
  
  levels(pav_master$reward_size) <- c("Small Reward", "Large Reward")
  
# Replace missing data with values from MedPC
  
  pav_master$subject[93:94] <- "F7"
  pav_master$during_cs_rate[40] <- 0.513253710
  pav_master$change_rate[40] <- 0.413253710
  
pav_master <- pav_master %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                                       session == 3) ~ "Early",
                                                      (session == 8 | session == 9 |
                                                       session == 10) ~ "Late")) %>%
  convert_as_factor(stage)  

#--------
     pav_change <- pav_master %>%
       select(subject, session, stage, reward_size, change_rate) %>%
       group_by(session, reward_size) %>%
       get_summary_stats(change_rate, type = "mean_sd") %>%
       mutate(sem = sd/sqrt(n))
     
     corrected_summary <- summarySEwithin(pav_master, 
                                          measurevar="change_rate", withinvars=c("session", "reward_size"),
                                          idvar="subject", na.rm=FALSE, conf.interval=.95)
     
     # corrected_summary$session <- corrected_summary$session %>%
     #            fct_relevel("10", after = 9)
     
     corrected_summary <- corrected_summary %>% 
       arrange(session, reward_size) 
     
     corrected_summary$session <- as.numeric(corrected_summary$session)
     
     change_plot <- ggplot(data = corrected_summary,
                           mapping = aes(x = session,
                                         y = change_rate,
                                         colour = reward_size,
                                         shape = reward_size,
                                         group = reward_size)) +
       annotate(geom = "rect", xmin = 0.7, xmax = 3.3, ymin = -Inf, ymax = Inf,
                fill = "grey69", colour = NA, alpha = 0.4) +
       annotate(geom = "rect", xmin = 7.7, xmax = 10.3, ymin = -Inf, ymax = Inf,
                fill = "grey69", colour = NA, alpha = 0.4) +
       annotate(geom = "text", x = 2, y = 0.43, 
                label = "Early \n Training",
                color = "black",
                size = 4.5) +
       annotate(geom = "text", x = 9, y = 0.43, 
                label = "Late \n Training",
                color = "black",
                size = 4.5) +
       # geom_vline(xintercept = 0.7, linetype="dashed",
       #            color = "black", size = 0.5) +
       # geom_vline(xintercept = 3.3, linetype="dashed",
       #            color = "black", size = 0.5) +
       # geom_vline(xintercept = 7.7, linetype="dashed",
       #            color = "black", size = 0.5) +
       # geom_vline(xintercept = 10.3, linetype="dashed",
       #            color = "black", size = 0.5) +
       geom_line() +
       geom_errorbar(aes(ymin = change_rate - se, ymax = change_rate + se), width = .1) +
       geom_point(size = 3, stroke = 1) +
       scale_colour_manual(name = "Reward Size",
                           labels = c("Small Reward", "Large Reward"),
                           values = c("firebrick", "dodgerblue4")) +
       scale_shape_manual(name = "Reward Size",
                          labels = c("Small Reward", "Large Reward"),
                          values = c(16, 17)) +
       scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
       xlab("Session") +
       ylab("Change in Magazine Entries /s") +
       ggtitle("A",
               subtitle = "Conditioned Responding") +
       ylim(-0.03, 0.47) +
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
     
     change_plot
     
     ggsave("Pav Task Conditioned Responding.png", 
            change_plot, 
            device = png,
            path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
            width = 15.92,
            height = 10,
            units = "cm",
            dpi = 300)
     
     
     
# Plot change in ME rate from pre-cs to during-cs
  
    pav_change_stage <- pav_master %>%
      select(subject, session, stage, reward_size, change_rate) %>%
      filter(stage == "Early" | stage == "Late") %>%
      group_by(stage, reward_size) %>%
      get_summary_stats(change_rate, type = "mean_sd") %>%
      mutate(sem = sd/sqrt(n))
     
    stage_corrected_summary <- summarySEwithin(pav_master %>% filter(stage == "Early" | stage == "Late"), 
                                         measurevar="change_rate", withinvars=c("stage", "reward_size"),
                                         idvar="subject", na.rm=FALSE, conf.interval=.95)
    
    # corrected_summary$session <- corrected_summary$session %>%
    #            fct_relevel("10", after = 9)
    
    stage_corrected_summary <- stage_corrected_summary %>% 
      arrange(stage, reward_size)
    
    
    stage_change_plot <- ggplot(data = stage_corrected_summary,
                          mapping = aes(x = stage,
                                        y = change_rate,
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
      geom_errorbar(aes(ymin = change_rate, ymax = change_rate + se),width = .3, position = position_dodge(width = 0.80)) +
      scale_fill_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +
      #geom_point(size = 3, stroke = 1) +
      # annotate(geom = "line", aes(xmin = 2.2, xmax = 2,2, ymin = 0.1, ymax = 0.235),
      #          colour = "black") +
      annotate(geom = "rect", xmin = 1.2, xmax = 2.2, ymin = 0.35, ymax = 0.35,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 1.2, xmax = 1.2, ymin = 0.1, ymax = 0.35,
               fill = "black", colour = "black") +
      annotate(geom = "rect", xmin = 2.2, xmax = 2.2, ymin = 0.32, ymax = 0.35,
               fill = "black", colour = "black") +
      annotate(geom = "text", x = 1.7, y = 0.37, 
               label = "**",
               color = "black",
               size = 8) +
      # scale_colour_manual(name = "Reward Size",
      #                     labels = c("Small Reward", "Large Reward"),
      #                     values = c("firebrick", "dodgerblue4")) +
      # scale_shape_manual(name = "Reward Size",
      #                    labels = c("Small Reward", "Large Reward"),
      #                    values = c(16, 17)) +
      scale_x_discrete(labels = c("Early", "Late")) +
      xlab("Stage of Training") +
      ylab("Change in Magazine Entries /s") +
      ylim(-0.03, 0.47) +
      ggtitle("B",
              subtitle = "Conditioned Responding") +
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
                                      size = 25),
            plot.subtitle = element_text(hjust = 0.5, size = 14))

  print(stage_change_plot)
  
  ggsave("Pav Task Conditioned Responding (Stage).png", 
         stage_change_plot, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
         width = 15.92,
         height = 14,
         units = "cm",
         dpi = 300)
  
  cr_plots <- change_plot + stage_change_plot + 
    plot_layout(widths = c(1.5, 1))
  
  ggsave("Pav Task Conditioned Responding Plots.png", 
         cr_plots, 
         device = png,
         path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
         width = 25.92,
         height = 12,
         units = "cm",
         dpi = 300)
  
# (10) x (2) ANOVA for change in rate of head entries from Pre-CS to During-CS
  
  stage_change_aov <- ezANOVA(data = pav_master %>% filter(stage == "Early" | stage == "Late"),
                    dv = change_rate,
                    wid = subject,
                    within = .(stage, reward_size),
                    type = 3)
  stage_change_aov
  
  pav_stage <- pav_master %>%
    filter(stage == "Early" | stage == "Late") %>%
    group_by(subject, stage, reward_size) %>%
    get_summary_stats(change_rate, type = "mean_sd")
  
  # pav_test <- pav_master %>%
  #   group_by(subject, stage, reward_size) %>%
  #   get_summary_stats(change_rate, type = "mean_sd")
  # 
  # pav_test$stage <- addNA(pav_test$stage)
  
  # cr_t_tests<- pav_stage %>% 
  #   group_by(stage) %>%
  #   pairwise_t_test(mean ~ reward_size,
  #                 p.adjust.method = "bonferroni",
  #                 paired = TRUE,
  #                 var.equal = FALSE,
  #                 detailed = TRUE)
  
  cr_t_tests <- pav_stage %>%
    group_by(reward_size) %>%
    t_test(mean ~ stage, paired = TRUE) %>%
    adjust_pvalue(method = "bonferroni") %>%
    add_significance()
  
  cr_t_tests %>% select(-c(n1, n2, .y.))
  
#---- Plot change in mag entries by subject----
  
  subject_change_plot <- ggplot(data = pav_master, 
                        mapping = aes(x = session, 
                                      y = change_rate,
                                      colour = reward_size,
                                      shape = reward_size,
                                      group = reward_size)) +
    geom_line() + 
    geom_point(size = 3, stroke = 1) +
    scale_colour_manual(name = "Reward Size",
                        labels = c("Small Reward", "Large Reward"),
                        values = c("firebrick", "dodgerblue4")) +   
    scale_shape_manual(name = "Reward Size",
                       labels = c("Small Reward", "Large Reward"),
                       values = c(16, 17)) +
    facet_wrap("subject") +
    xlab("Session") +
    ylab("Mean Change in Magazine Entries /s") +
    theme_apa() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.margin = margin(-5, 0, 0, 0)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(subject_change_plot)  

  # --------------------------------------------------------------------------------------
  
  # Latency Data
  
    ## Summarise Latency Data
  
      latency_data <- read_csv("Final_Data/Pav_Task/Latency_Combined.csv")
  
      latency_data$latency <- latency_data$latency/1000
      
      latency_data <- latency_data %>%
        rename(reward_size = CS_type) %>%
        convert_as_factor(reward_size)
      
      levels(latency_data$reward_size) <- c("Small Reward", "Large Reward")
      
      latency_data <- latency_data %>% convert_as_factor(session)
      latency_data <- latency_data %>% convert_as_factor(subject)
      
      latency_data <- latency_data %>% mutate(stage = case_when((session == 1 | session == 2 | 
                                                                   session == 3) ~ "Early",
                                                                (session == 8 | session == 9 |
                                                                   session == 10) ~ "Late")) %>%
        convert_as_factor(stage)  
      
      latency_subject_sum <- latency_data %>%
        filter(stage == "Early" | stage == "Late") %>%
        group_by(subject, stage, reward_size) %>%
        get_summary_stats(latency, type = "mean_sd") %>%
        mutate(sem = sd/sqrt(n))
      
      library(plyr)
      
      latency_corrected_summary <- summarySEwithin(latency_subject_sum, measurevar="mean", withinvars=c("stage", "reward_size"),
                                           idvar="subject", na.rm=FALSE, conf.interval=.95)
      
      # latency_corrected_summary$session <- latency_corrected_summary$session %>%
      #   fct_relevel("10", after = 9)
      
      latency_corrected_summary <- latency_corrected_summary %>% arrange(stage, reward_size)
      
      latency_session_sum <- latency_data %>%
        group_by(session, reward_size) %>%
        get_summary_stats(latency, type = "mean_sd") %>%
        mutate(sem = sd/sqrt(n))
      
      latency_session_corrected_summary <- summarySEwithin(latency_data, measurevar="latency", withinvars=c("session", "reward_size"),
                                                   idvar="subject", na.rm=FALSE, conf.interval=.95)
      
      latency_session_corrected_summary$session <- latency_session_corrected_summary$session %>%
         fct_relevel("10", after = 9)
      
  ## Plot Latency Data
      
      latency_session_corrected_summary$session <- as.numeric(latency_session_corrected_summary$session)
  
      latency_plot <- ggplot(data = latency_session_corrected_summary, 
                                         mapping = aes(x = session, 
                                                       y = latency,
                                                       colour = reward_size,
                                                       shape = reward_size,
                                                       group = reward_size)) +
        annotate(geom = "rect", xmin = 0.7, xmax = 3.3, ymin = -Inf, ymax = Inf,
                 fill = "grey69", colour = NA, alpha = 0.4) +
        annotate(geom = "rect", xmin = 7.7, xmax = 10.3, ymin = -Inf, ymax = Inf,
                 fill = "grey69", colour = NA, alpha = 0.4) +
        annotate(geom = "text", x = 2, y = 3.3, 
                 label = "Early \n Training",
                 color = "black",
                 size = 4.5) +
        annotate(geom = "text", x = 9, y = 3.3, 
                 label = "Late \n Training",
                 color = "black",
                 size = 4.5) +
        geom_line() + 
        geom_errorbar(aes(ymin = latency - se, ymax = latency + se), width = .1) +
        geom_point(size = 3, stroke = 1) +
        scale_colour_manual(name = "Reward Size",
                            labels = c("Small Reward", "Large Reward"),
                            values = c("firebrick", "dodgerblue4")) +   
        scale_shape_manual(name = "Reward Size",
                           labels = c("Small Reward", "Large Reward"),
                           values = c(16, 17)) +
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
        ggtitle("C",
                subtitle = "Latency to First Mag Entry") +
        xlab("Session") +
        ylab("Latency (s)") +
        ylim(0, 3.5) +
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
      
      print(latency_plot) 
      
      ggsave("Pav Task Latency.png", 
             latency_plot, 
             device = png,
             path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
             width = 15.92,
             height = 10,
             units = "cm",
             dpi = 300)
      
  ## Latency per stage
      
      latency_stage_plot <- ggplot(data = latency_corrected_summary,
                                   mapping = aes(x = stage,
                                                 y = mean,
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
        geom_errorbar(aes(ymin = mean, ymax = mean + se),width = .3, position = position_dodge(width = 0.80)) +
        scale_fill_manual(name = "Reward Size",
                          labels = c("Small Reward", "Large Reward"),
                          values = c("firebrick", "dodgerblue4")) +
        #geom_point(size = 3, stroke = 1) +
        # annotate(geom = "line", aes(xmin = 2.2, xmax = 2,2, ymin = 0.1, ymax = 0.235),
        #          colour = "black") +
        # scale_colour_manual(name = "Reward Size",
        #                     labels = c("Small Reward", "Large Reward"),
        #                     values = c("firebrick", "dodgerblue4")) +
        # scale_shape_manual(name = "Reward Size",
        #                    labels = c("Small Reward", "Large Reward"),
        #                    values = c(16, 17)) +
        scale_x_discrete(labels = c("Early", "Late")) +
        xlab("Stage of Training") +
        ylab("Latency (s)") +
        ylim(0, 3.5) +
        ggtitle("D",
                subtitle = "Latency to First Mag Entry") +
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
                                        size = 25),
              plot.subtitle = element_text(hjust = 0.5, size = 14))
      
      latency_stage_plot
      
      ggsave("Pav Task Latency (Stage).png", 
             latency_stage_plot, 
             device = png,
             path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
             width = 15.92,
             height = 14,
             units = "cm",
             dpi = 300)
      
      latency_plots <- latency_plot + latency_stage_plot + 
        plot_layout(widths = c(1.5, 1))
      
      ggsave("Pav Task Latency Plots.png", 
             latency_plots, 
             device = png,
             path = "/Users/lukekeevers/Desktop/Uni/Honours/Thesis/Figures",
             width = 25.92,
             height = 12,
             units = "cm",
             dpi = 300)
      
      
  ## ANOVA for Latency Data
    
      # latency_subject_sum <- latency_data %>%
      #   group_by(subject, session, reward_size) %>%
      #   get_summary_stats(latency, type = "mean_sd") %>%
      #   rename(latency = mean) %>%
      #   select(subject, session, reward_size, latency)
      
      latency_aov <- ezANOVA(data = latency_subject_sum,
                            dv = mean,
                            wid = subject,
                            within = .(stage, reward_size),
                            type = 3)
      
      latency_aov
      
  ## Subject latency Plot
      
      subject_latency_sum <- latency_data %>%
        group_by(subject, session, reward_size) %>%
        get_summary_stats(latency, type = "mean_sd") %>%
        mutate(sem = sd/sqrt(n))
      
      
      subject_latency_plot <- ggplot(data = subject_latency_sum, 
                                    mapping = aes(x = session, 
                                                  y = mean,
                                                  colour = reward_size,
                                                  shape = reward_size,
                                                  group = reward_size)) +
        geom_line() + 
        geom_point(size = 3, stroke = 1) +
        scale_colour_manual(name = "Reward Size",
                            labels = c("Small Reward", "Large Reward"),
                            values = c("firebrick", "dodgerblue4")) +   
        scale_shape_manual(name = "Reward Size",
                           labels = c("Small Reward", "Large Reward"),
                           values = c(16, 17)) +
        facet_wrap("subject") +
        xlab("Session") +
        ylab("Mean Latency to Mag Entry (s)") +
        theme_apa() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(-5, 0, 0, 0)) +
        theme(plot.title = element_text(hjust = 0.5))
      
      print(subject_latency_plot)
      
    
  