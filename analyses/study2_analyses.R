############################
### Analyze Study 2 Data ###
############################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 11/8/24

# Inputs: study 2 learning task
# Computes:
# - statistical assessment of hypothesis 4
# Outputs:
# - txt file of hypothesis test into results/study2/hyp directory

#####################
### Set up Script ###
#####################

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       lmerTest, # for mixed effects models
       performance, # for posterior_predictive_check()
       car) # for Anova()

# Load shared variables and functions
source("utilities.R")

# Set path to data
data_path <- '../data/study2/'

# Set output paths
hyp_analyzed_data_path <- '../results/study2/hyp/'
comp_analyzed_data_path <- '../results/study2/comp/'

# Read in data
learn <- read_csv(paste0(data_path, "learn.csv"))

####################################################
### Prepping for Analysis - Testing Hypothesis 4 ###
####################################################

# Change variables to factors in learn
str(learn)
learn$participant <- as.factor(learn$participant)
learn$block_third <- factor(learn$block_third, levels = c("Early", "Middle", "Late"))
learn$Incentive <- factor(learn$Incentive, levels = c("1\u00A2", "10\u00A2"))
learn$Difficulty <- factor(learn$Difficulty, levels = c("Easy", "Hard"))
learn$AgeGroup <- factor(learn$AgeGroup, levels = c("Children", "Adolescents", "Adults"))
str(learn)

# Create df for accuracy stats
learn_noNA <- learn[!is.na(learn$correct), ]
learn_acc_stats <- learn_noNA %>%
  group_by(Difficulty, Incentive, block_third, ExactAge, participant) %>%
  dplyr::summarise(n_corr = sum(correct == 1), n_total = length(correct)) 

#########################################################
### Prepping for Analysis - Comparing Studies 1 and 2 ###
#########################################################

# Read in study 1 data
study1_learn <- read_csv("../data/study1/learn.csv")

# Create dfs for between-study accuracy comparisons 
study1_acc <- study1_learn %>% dplyr::group_by(participant) %>% dplyr::summarise(study1_mean = mean(correct, na.rm = TRUE))
study2_acc <- learn %>% dplyr::group_by(participant) %>% dplyr::summarise(study2_mean = mean(correct, na.rm = TRUE))

# Create dfs for between-study RT comparisons
study1_rt <- study1_learn %>% dplyr::group_by(participant) %>% dplyr::summarise(study1_mean = mean(keyTrial.rt, na.rm = TRUE))
study2_rt <- learn %>% dplyr::group_by(participant) %>% dplyr::summarise(study2_mean = mean(keyTrial.rt, na.rm = TRUE))

############################
### Testing Hypothesis 4 ###
############################

# Run accuracy model with Binomial distribution and assess
learning_acc_binom <- glmer(cbind(n_corr, n_total-n_corr) ~ ExactAge * Difficulty * Incentive * block_third + (1|participant), data = learn_acc_stats, family = binomial)
set.seed(123)
posterior_predictive_check(learning_acc_binom) # Awesome! Binomial distribution fulfills model assumptions

# Identify main effects and interactions from learning_acc_binom
sink(paste0(hyp_analyzed_data_path, 'TabS5.txt'))
print(Anova(learning_acc_binom, type = "II"))
sink()

#################################
### Comparing Studies 1 and 2 ###
#################################

# Accuracy (if lower accuracy in study 1 than 2, then evidence for study 1 being overall more challenging)
study_acc <- t.test(study1_acc$study1_mean, study2_acc$study2_mean, paired = FALSE, alternative = "less")
sink(paste0(comp_analyzed_data_path, 'comp_acc.txt'))
cat("Study 1 Mean:", mean(study1_acc$study1_mean), "\n")
cat("Study 1 Standard Deviation:", sd(study1_acc$study1_mean), "\n")
cat("Study 2 Mean:", mean(study2_acc$study2_mean), "\n")
cat("Study 2 Standard Deviation:", sd(study2_acc$study2_mean), "\n")
print(study_acc)
sink()

# RT (if higher RT in study 1 than 2, then evidence for study 1 being overall more challenging)
study_rt <- t.test(study1_rt$study1_mean, study2_rt$study2_mean, paired = FALSE, alternative = "greater")
sink(paste0(comp_analyzed_data_path, 'comp_rt.txt'))
cat("Study 1 Mean:", mean(study1_rt$study1_mean), "\n")
cat("Study 1 Standard Deviation:", sd(study1_rt$study1_mean), "\n")
cat("Study 2 Mean:", mean(study2_rt$study2_mean), "\n")
cat("Study 2 Standard Deviation:", sd(study2_rt$study2_mean), "\n")
print(study_rt)
sink()
