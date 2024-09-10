############################
### Analyze Study 1 Data ###
############################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 9/10/24

# Inputs: study 1 practice phase, learning task, post-task ratings, and questionnaire data
# Computes: 
# - visualizations and statistical assessments verifying reasonableness of pre-registered analysis decisions
# - visualizations and statistical assessments of hypotheses 1-3 and 5
# Outputs: 
# - png plots and txt files of verification checks into results/study1/verify directory
# - png plots and txt files of hypothesis tests into results/study1/hyp directory

#####################
### Set up Script ###
#####################

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr, # for %>% and other operators
       plotrix, # for std.error()
       ggplot2, # for plotting
       sjPlot, # for plot_model()
       lmerTest, # for mixed effects models
       performance, # for posterior_predictive_check()
       Rmisc, # for summarySE()
       car, # for Anova()
       ggeffects, # for ggpredict()
       emmeans) # for emmeans()

# Load shared variables and functions
source("utilities.R")

# Set path to data
data_path <- '../data/study1/'

# Set output paths
verify_analyzed_data_path <- '../results/study1/verify/'
hyp_analyzed_data_path <- '../results/study1/hyp/'

# Read in data
learn <- read_csv(paste0(data_path, "learn.csv"))
prac <- read_csv(paste0(data_path, "prac.csv"))
rate <- read_csv(paste0(data_path, "rate.csv"))
quest <- read_csv(paste0(data_path, "quest.csv"))
matreas <- read_csv(paste0(data_path, "matreas.csv"))
subval <- read_csv(paste0(data_path, "subval.csv"))
qa <- read_csv(paste0(data_path, "qa_demog.csv"))

######################################################
### Prepping for Analyses - Testing Hypotheses 1-3 ###
######################################################

# Change variables to factors in learn
str(learn)
learn$participant <- as.factor(learn$participant)
learn$block_third <- factor(learn$block_third, levels = c("Early", "Middle", "Late"))
learn$Incentive <- factor(learn$Incentive, levels = c("1\u00A2", "10\u00A2"))
learn$Difficulty <- factor(learn$Difficulty, levels = c("Easy", "Hard"))
learn$AgeGroup <- factor(learn$AgeGroup, levels = c("Children", "Adolescents", "Adults"))
str(learn)

# Change variables to factors in prac
str(prac)
prac$participant <- as.factor(prac$participant)
prac$AgeGroup <- factor(prac$AgeGroup, levels = c("Children", "Adolescents", "Adults"))
str(prac)

# Create df for accuracy stats
learn_noNA <- learn[!is.na(learn$correct), ]
learn_acc_stats <- learn_noNA %>%
  group_by(Difficulty, Incentive, block_third, ExactAge, participant) %>%
  dplyr::summarise(n_corr = sum(correct == 1), n_total = length(correct)) 

# Create df for questionnaire individual differences assessment
learn_acc_quest_stats <- merge(learn_acc_stats, quest, by = "participant")

# Create df for baseline individual differences assessment
hard <- learn[learn$Difficulty == "Hard", ]
easy <- learn[learn$Difficulty == "Easy", ]
end_prac <- prac[(prac$block_num == 3 | prac$block_num == 4), ]
baseline <- summarySE(end_prac, 
                      measurevar = "correct",
                      groupvars = c("participant", "trial_type"), 
                      na.rm = TRUE)
baseline_simp <- baseline[, c("participant", "trial_type", "correct")]
baseline_simp <- dplyr::rename(baseline_simp, "block_diff" = "trial_type")
baseline_simp <- dplyr::rename(baseline_simp, "mean_acc" = "correct")
baseline_hard_simp <- baseline_simp[baseline_simp$block_diff == "high_switch", ]
baseline_easy_simp <- baseline_simp[baseline_simp$block_diff == "low_switch", ]
hard_merge <- merge(hard, baseline_hard_simp, by = "participant")
easy_merge <- merge(easy, baseline_easy_simp, by = "participant")
full_merge <- rbind(hard_merge, easy_merge)
full_merge_noNA <- full_merge[!is.na(full_merge$correct), ]
learn_acc_baseline_stats <- full_merge_noNA %>%
  group_by(Difficulty, Incentive, block_third, mean_acc, ExactAge, participant) %>%
  dplyr::summarise(n_corr = sum(correct == 1), n_total = length(correct)) 

# Create df for RT stats
learn_rt_stats <- summarySE(learn, 
                            measurevar = "keyTrial.rt",
                            groupvars = c("Difficulty", "Incentive", "block_third", "ExactAge", "participant"),
                            na.rm = TRUE)

# Create df for sensitivity analysis (correct trials only)
learn_all_acc <- learn[!is.na(learn$correct) & (learn$correct == 1), ]
learn_rt_all_acc_stats <- summarySE(learn_all_acc, 
                                    measurevar = "keyTrial.rt",
                                    groupvars = c("Difficulty", "Incentive", "block_third", "ExactAge", "participant"),
                                    na.rm = TRUE)

####################################################
### Prepping for Analysis - Testing Hypothesis 5 ###
####################################################

# Change variables for factors in rate
str(rate)
rate$participant <- as.factor(rate$participant)
rate$Block <- factor(rate$Block, c("Easy 10\u00A2", "Easy 1\u00A2" ,"Hard 10\u00A2", "Hard 1\u00A2"))
rate$Difficulty <- factor(rate$Difficulty, levels = c("Easy", "Hard"))
rate$Incentive <- factor(rate$Incentive, levels = c("1\u00A2", "10\u00A2"))
rate$AgeGroup <- factor(rate$AgeGroup, levels = c("Children", "Adolescents", "Adults"))
str(rate)

# Create df for 1st ratings question about galaxy effort
eff_q <- rate[rate$post_game_question == "How hard did you try?", ]
eff_q_simp <- eff_q[, c("participant", "Block", "keyPostGameRating.keys")]

# Create df for 2nd ratings question about galaxy difficulty
diff_q <- rate[rate$post_game_question == "How hard was this galaxy?", ]

# Remove participant from learn with incomplete rate data
small_n <- unique(eff_q$participant)
big_n <- unique(learn$participant)
missing <- big_n[!(big_n %in% small_n)]
learn_nomissing <- learn[learn$participant != missing, ]

# Create df for effort rating (metacognition) stats
eff_q_stats <- summarySE(learn_nomissing,
                         measurevar = "correct",
                         groupvars = c("Difficulty", "Incentive", "ExactAge", "participant"),
                         na.rm = TRUE)
eff_q_stats$Block <- paste(eff_q_stats$Difficulty, eff_q_stats$Incentive)
eff_q_stats <- merge(eff_q_stats, eff_q_simp, by = c("participant", "Block"))

# Create df for effort rating (metacognition) graph -- post-task effort ratings component
eff_differ_long <- eff_q[, c("participant", "Difficulty", "keyPostGameRating.keys", "Incentive")]
eff_differ_wide <- pivot_wider(eff_differ_long, names_from = c("Incentive"), values_from = "keyPostGameRating.keys")
eff_differ_wide$rating_differ <- eff_differ_wide$`10¢` - eff_differ_wide$`1¢`
eff_differ_wide <- eff_differ_wide[, c("participant", "Difficulty", "rating_differ")]

# Create df for effort rating (metacognition) graph -- learning task accuracy component
acc_diff <- summarySE(learn_nomissing, 
                      measurevar = "correct",
                      groupvars = c("participant", "ExactAge", "Difficulty"),
                      na.rm = TRUE)
acc_diff <- acc_diff[, c("participant", "ExactAge", "Difficulty", "correct")]
acc_diff_easy <- acc_diff[acc_diff$Difficulty == "Easy", ]
acc_diff_hard <- acc_diff[acc_diff$Difficulty == "Hard", ]
avg_easy <- mean(acc_diff_easy$correct)
avg_hard <- mean(acc_diff_hard$correct)
acc_diff_easy <- acc_diff_easy %>% mutate(split = case_when(correct > avg_easy ~ "High-Accuracy",
                                                            correct <= avg_easy ~ "Low-Accuracy"))
acc_diff_hard <- acc_diff_hard %>% mutate(split = case_when(correct > avg_hard ~ "High-Accuracy",
                                                            correct <= avg_hard ~ "Low-Accuracy"))
acc_diff <- rbind(acc_diff_easy, acc_diff_hard)
age_acc_galaxy_interact_eff <- merge(acc_diff, eff_differ_wide, by = c("participant", "Difficulty"))
age_acc_galaxy_interact_eff$split <- factor(age_acc_galaxy_interact_eff$split, levels = c("Low-Accuracy", "High-Accuracy"))

###################################################
### Prepping for Analyses - Verification Checks ###
###################################################

# Create summaries for speed outlier verification check
total_prac_speed_outliers <- qa %>% dplyr::summarise(mean = mean(prac_num_speed_outliers), sd = sd(prac_num_speed_outliers), median = median(prac_num_speed_outliers), n = sum(prac_num_speed_outliers))
total_learn_speed_outliers <- qa %>% dplyr::summarise(mean = mean(learn_num_speed_outliers), sd = sd(learn_num_speed_outliers), median = median(learn_num_speed_outliers), n = sum(learn_num_speed_outliers))
total_proportion_speed_outliers <- (total_prac_speed_outliers$n + total_learn_speed_outliers$n) / (150 * (120 + 240))

# Create dfs for practice verification check graphs
prac_acc <- prac %>%
  group_by(participant, block_num, AgeGroup) %>%
  dplyr::summarise(avg = mean(correct, na.rm = TRUE), se = std.error(correct, na.rm = TRUE), n = sum(!is.na(correct)))
prac_rt <- prac %>%
  group_by(participant, block_num, AgeGroup) %>%
  dplyr::summarise(avg = mean(keyTrial.rt, na.rm = TRUE), se = std.error(keyTrial.rt, na.rm = TRUE), n = sum(!is.na(keyTrial.rt))) 

# Create df for fatigue verification check
learn_simp <- learn[, c("participant", "block_third", "correct", "keyTrial.rt")]
learn_simp <- learn_simp %>% mutate(time_chunk = case_when((block_third == "Early") ~ "Begin",
                                                           (block_third == "Middle") ~ "Begin",
                                                           (block_third == "Late") ~ "End"))
fatigue <- learn_simp %>% dplyr::group_by(participant, time_chunk) %>% dplyr::summarise(mean = mean(correct, na.rm = TRUE))
fatigue_wide <- fatigue %>% pivot_wider(names_from = time_chunk, values_from = mean)

# Add age to df for subjective value verification check
subval <- merge(subval, qa[, c("participant", "ExactAge")], by = "participant")

# Create dfs for accuracy verification check graphs 
acc_var <- summarySE(learn, 
                     measurevar = "correct",
                     groupvars = c("Difficulty", "Incentive", "ExactAge", "participant", "AgeGroup"),
                     na.rm = TRUE)
acc_var$block_type <- paste(as.character(acc_var$Difficulty), as.character(acc_var$Incentive))
learn_age_acc <- summarySE(learn, 
                           measurevar = "correct",
                           groupvars = c("participant", "ExactAge"), 
                           na.rm = TRUE)
learn_diff_acc_df <- learn %>% dplyr::group_by(participant, Difficulty) %>% dplyr::summarise(mean = mean(correct, na.rm = TRUE))
learn_diff_acc_df_wide <- learn_diff_acc_df %>% pivot_wider(names_from = Difficulty, values_from = mean)

# Create dfs for RT verification check graphs
rt_var <- summarySE(learn, 
                    measurevar = "keyTrial.rt",
                    groupvars = c("Difficulty", "Incentive", "ExactAge", "participant", "AgeGroup"),
                    na.rm = TRUE)
rt_var$block_type <- paste(as.character(rt_var$Difficulty), as.character(rt_var$Incentive))
learn_age_rt <- summarySE(learn, 
                          measurevar = "keyTrial.rt",
                          groupvars = c("participant", "ExactAge"), 
                          na.rm = TRUE)
learn_diff_rt_df <- learn %>% dplyr::group_by(participant, Difficulty) %>% dplyr::summarise(mean = mean(keyTrial.rt, na.rm = TRUE))
learn_diff_rt_df_wide <- learn_diff_rt_df %>% pivot_wider(names_from = Difficulty, values_from = mean)

#####################################################################
### Verification Check: High Instruction Compliance on Audio Test ###
#####################################################################

# Mostly 3 attempts on audio test (i.e., perfect compliance)
ggplot(data = qa, aes(x = ExactAge, y = num_tries)) +
  geom_hline(yintercept = c(4, 5, 6), colour = 'grey90') +
  geom_hline(yintercept = 3, colour = gold, linetype = "dashed") +
  scale_y_continuous(breaks = c(3, 4, 5, 6)) +
  scale_x_continuous(breaks = c(10:21)) +
  geom_point(alpha = 1, size = 2.75) +
  labs(x = "Age (Years)", y = "Number of\nAudio Test Attempts") +
  plot_theme
ggsave(paste0(verify_analyzed_data_path, 'aud_test.png'), width = norm_width, height = norm_height)

####################################################
### Verification Check: Few Speed Outlier Trials ###
####################################################

# Small proportion of speed outlier trials
sink(paste0(verify_analyzed_data_path, 'speed_outlier.txt'))
cat("Practice Phase Speed Outliers Across Sample\n")
print(total_prac_speed_outliers)
cat("\nLearning Task Speed Outliers Across Sample\n")
print(total_learn_speed_outliers)
cat("\nProportion of speed outlier trials:", total_proportion_speed_outliers, "\n")
sink() 

##################################################################
### Verification Check: Performance Plateaus in Practice Phase ###
##################################################################

# Accuracy
ggplot(data = prac_acc, aes(x = block_num, y = avg, color = AgeGroup, fill = AgeGroup)) +
  geom_hline(yintercept = mid_proportion_acc_format_y_axis, colour = 'grey90') +
  scale_y_continuous(breaks = mid_proportion_acc_format_y_axis) +
  geom_point(alpha = 0.6, size = 2.75, position = horizontal_jitter) +
  geom_smooth(size = 1.5) +
  scale_fill_manual(values = c(not_capt_purple, masc_purple, fem_purple)) + 
  scale_color_manual(values = c(not_capt_purple, masc_purple, fem_purple)) + 
  labs(x = "Block", y = "Mean Accuracy") +
  scale_x_continuous(breaks = c(1:4), labels = c("1st Easy", "1st Hard", "2nd Easy", "2nd Hard")) +
  plot_theme + theme(legend.title = element_blank())
ggsave(paste0(verify_analyzed_data_path, 'FigS3A.png'), width = mid_width, height = tall_height)

# RT
ggplot(data = prac_rt, aes(x = block_num, y = avg, color = AgeGroup, fill = AgeGroup)) +
  geom_hline(yintercept = wide_proportion_rt_format_y_axis, colour = 'grey90') +
  scale_y_continuous(breaks = wide_proportion_rt_format_y_axis) +
  geom_point(alpha = 0.6, size = 2.75, position = horizontal_jitter) +
  geom_smooth(size = 1.5) +
  scale_fill_manual(values = c(not_capt_purple, masc_purple, fem_purple)) + 
  scale_color_manual(values = c(not_capt_purple, masc_purple, fem_purple)) + 
  labs(x = "Block", y = "Mean Reaction Time (Seconds)") +
  scale_x_continuous(breaks = c(1:4), labels = c("1st Easy", "1st Hard", "2nd Easy", "2nd Hard")) +
  plot_theme + theme(legend.title = element_blank())
ggsave(paste0(verify_analyzed_data_path, 'FigS3B.png'), width = mid_width, height = tall_height)

#################################################################################################
### Verification Check: Age Invariance in Age-Adjusted T-Scores from Matrix Reasoning Subtest ###
#################################################################################################

# Run matrix reasoning model with Gaussian distribution and assess
wasi_t_age <- lm(t_score ~ ExactAge_MatReas, data = matreas)
set.seed(123)
posterior_predictive_check(wasi_t_age) # Good! Gaussian distribution fulfills model assumptions reasonably well

# Identify main effects and interactions from wasi_t_age
sink(paste0(verify_analyzed_data_path, 'matreas_lm.txt'))
print(summary(wasi_t_age))
sink()

##################################################################################################
### Verification Check: Age Invariance in Percent Changes from Subjective Value of Money Scale ###
##################################################################################################

# Confirm presence of extreme outliers
subval <- subval[subval$HowMuchDiff != Inf, ]
range(subval$HowMuchDiff)
hist(subval$HowMuchDiff)

# Remove outliers before assessing age-related change
# (Identifying extreme outliers, per https://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm)
subval_iqr <- IQR(subval$HowMuchDiff)
subval_iqr_lowerbound <- as.numeric(quantile(subval$HowMuchDiff, .25)) - (3 * subval_iqr) # lower outer fence
subval_iqr_upperbound <- as.numeric(quantile(subval$HowMuchDiff, .75)) + (3 * subval_iqr) # upper outer fence
  
# Mark outlier types and maintain non-outlier data
subval <- subval %>% mutate(outlier = case_when(HowMuchDiff < subval_iqr_lowerbound ~ "under_outlier",
                                                HowMuchDiff >= subval_iqr_lowerbound & HowMuchDiff <= subval_iqr_upperbound ~ "not_outlier",
                                                HowMuchDiff > subval_iqr_upperbound ~ "over_outlier"))
subval_nooutliers <- subset(subval, outlier == "not_outlier")
range(subval_nooutliers$HowMuchDiff)
hist(subval_nooutliers$HowMuchDiff)

# Run subjective value model with Gaussian distribution and assess
subval_age <- lm(HowMuchDiff ~ ExactAge, data = subval_nooutliers)
set.seed(123)
posterior_predictive_check(subval_age) # Ok, Gaussian distribution fulfills model assumptions well enough

# Identify main effects and interactions from subval_age
sink(paste0(verify_analyzed_data_path, "subval_lm.txt"))
print(summary(subval_age))
sink()

###########################################################
### Verification Check: No Evidence for Fatigue Effects ###
###########################################################

# No evidence for fatigue from beginning to end of learning task
fatigue_test <- t.test(fatigue_wide$Begin, fatigue_wide$End, paired = TRUE, alternative = "greater")
sink(paste0(verify_analyzed_data_path, 'fatigue.txt'))
cat("Begin Mean:", mean(fatigue_wide$Begin), "\n")
cat("Begin Standard Deviation:", sd(fatigue_wide$Begin), "\n")
cat("End Mean:", mean(fatigue_wide$End), "\n")
cat("End Standard Deviation:", sd(fatigue_wide$End), "\n")
print(fatigue_test)
sink()

############################################################
### Verification Check: Interrogate Variance Compression ###
############################################################

# No evidence of accuracy variance compression in any block type relative to the others
ggplot(data = acc_var, aes(x = block_type, y = correct, color = Difficulty, fill = Incentive)) +
  geom_hline(yintercept = wide_proportion_acc_format_y_axis, colour = 'grey90') +
  scale_y_continuous(breaks = wide_proportion_acc_format_y_axis) +
  geom_violin(alpha = 0.6, color = NA) +
  geom_point(position = position_jitter(width = .1), alpha = 0.3) +
  geom_boxplot(outlier.size = 1.5, width = 0.2, alpha = 0, outlier.color = 'black', outlier.alpha = .5) +
  scale_color_manual(values = c(easy_green, hard_blue)) + 
  scale_fill_manual(values = c(silver, gold)) +
  labs(x = "Block Type", y = "Mean Accuracy") +
  facet_grid(rows = vars(AgeGroup)) +
  plot_theme + theme(legend.position = "none")
ggsave(paste0(verify_analyzed_data_path, 'FigS5A.png'), width = mid_width, height = tall_height)

# No evidence of RT variance compression in any block type relative to the others
ggplot(data = rt_var, aes(x = block_type, y = keyTrial.rt, color = Difficulty, fill = Incentive)) +
  geom_hline(yintercept = mid_proportion_rt_format_y_axis, colour = 'grey90') +
  scale_y_continuous(breaks = mid_proportion_rt_format_y_axis) +
  geom_violin(alpha = 0.6, color = NA) +
  geom_point(position = position_jitter(width = .1), alpha = 0.3) +
  geom_boxplot(outlier.size = 1.5, width = 0.2, alpha = 0, outlier.color = 'black', outlier.alpha = .5) +
  scale_color_manual(values = c(easy_green, hard_blue)) + 
  scale_fill_manual(values = c(silver, gold)) +
  labs(x = "Block Type", y = "Mean Reaction Time (Seconds)") +
  facet_grid(rows = vars(AgeGroup)) +
  plot_theme + theme(legend.position = "none")
ggsave(paste0(verify_analyzed_data_path, 'FigS5B.png'), width = mid_width, height = tall_height)

#########################################################
### Verification Check: Performance Improves with Age ###
#########################################################

# Evidence for accuracy increasing with age
ggplot(data = learn_age_acc, aes(x = ExactAge, y = correct)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(.84, .88, .92, .96, 1), colour = 'grey90') +
  scale_y_continuous(breaks = c(.84, .88, .92, .96, 1)) +
  geom_point(fill = masc_purple, color = masc_purple, alpha = 0.6, size = 2.75) +
  geom_smooth(fill = masc_purple, color = masc_purple, method = "lm", size = 1.5) +
  labs(x = "Age (Years)", y = "Mean Accuracy") +
  plot_theme + theme(legend.position = "none")
ggsave(paste0(verify_analyzed_data_path, 'learn_acc_age.png'), width = narr_width, height = norm_height)

# Evidence for RT decreasing with age
ggplot(data = learn_age_rt, aes(x = ExactAge, y = keyTrial.rt)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = mid_proportion_rt_format_y_axis, colour = 'grey90') +
  scale_y_continuous(breaks = mid_proportion_rt_format_y_axis) +
  geom_point(fill = masc_purple, color = masc_purple, alpha = 0.6, size = 2.75) +
  geom_smooth(fill = masc_purple, color = masc_purple, method = "lm", size = 1.5) +
  labs(x = "Age (Years)", y = "Mean Reaction Time (Seconds)") +
  plot_theme + theme(legend.position = "none")
ggsave(paste0(verify_analyzed_data_path, 'learn_rt_age.png'), width = narr_width, height = norm_height)

#########################################################################
### Verification Check: Better Performance in Easy Difficulty Context ###
#########################################################################

# Accuracy
learn_diff_acc <- t.test(learn_diff_acc_df_wide$Easy, learn_diff_acc_df_wide$Hard, paired = TRUE, alternative = "greater")
sink(paste0(verify_analyzed_data_path, 'diff_acc.txt'))
cat("Easy Mean:", mean(learn_diff_acc_df_wide$Easy), "\n")
cat("Easy Standard Deviation:", sd(learn_diff_acc_df_wide$Easy), "\n")
cat("Hard Mean:", mean(learn_diff_acc_df_wide$Hard), "\n")
cat("Hard Standard Deviation:", sd(learn_diff_acc_df_wide$Hard), "\n")
print(learn_diff_acc)
sink()

# RT
learn_diff_rt <- t.test(learn_diff_rt_df_wide$Easy, learn_diff_rt_df_wide$Hard, paired = TRUE, alternative = "less")
sink(paste0(verify_analyzed_data_path, 'diff_rt.txt'))
cat("Easy Mean:", mean(learn_diff_rt_df_wide$Easy), "\n")
cat("Easy Standard Deviation:", sd(learn_diff_rt_df_wide$Easy), "\n")
cat("Hard Mean:", mean(learn_diff_rt_df_wide$Hard), "\n")
cat("Hard Standard Deviation:", sd(learn_diff_rt_df_wide$Hard), "\n")
print(learn_diff_rt)
sink()

##############################
### Testing Hypotheses 1-3 ###
##############################

# Run accuracy model with Binomial distribution and assess
learning_acc_binom <- glmer(cbind(n_corr, n_total-n_corr) ~ ExactAge * Difficulty * Incentive * block_third + (1|participant), data = learn_acc_stats, family = binomial)
set.seed(123)
posterior_predictive_check(learning_acc_binom) # Awesome! Binomial distribution fulfills model assumptions

# Identify main effects and interactions from learning_acc_binom
sink(paste0(hyp_analyzed_data_path, 'TabS2.txt'))
print(Anova(learning_acc_binom, type = "II"))
sink()

# Save visualizations of interactions from learning_acc_binom
age_diff_interact <- ggpredict(learning_acc_binom, terms = c("ExactAge [all]", "Difficulty"))
ggplot(age_diff_interact, aes(x, predicted)) + 
  ylim(.92, 1) +
  geom_line(aes(color = group), size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  scale_color_manual(values = c(easy_green, hard_blue)) +
  scale_fill_manual(values = c(easy_green, hard_blue)) +
  labs(x = "Age (Years)", y = "Mixed-Effects Model Predictions\nof Accuracy", color = "Difficulty", fill = "Difficulty") +
  plot_theme + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
ggsave(paste0(hyp_analyzed_data_path, 'Fig2B.png'), width = narr_width, height = norm_height) 
age_incent_interact <- ggpredict(learning_acc_binom, terms = c("ExactAge [all]", "Incentive"))
ggplot(age_incent_interact, aes(x, predicted)) + 
  ylim(.92, 1) +
  geom_line(aes(color = group), size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  scale_color_manual(values = c(silver, gold)) +
  scale_fill_manual(values = c(silver, gold)) +
  geom_vline(xintercept = c(10.5, 11.5), colour = not_capt_purple, linetype = 'dashed', size = 1.5) +
  geom_vline(xintercept = c(15, 16), colour = masc_purple, linetype = 'dashed', size = 1.5) +
  geom_vline(xintercept = c(19.5, 20.5), colour = fem_purple, linetype = 'dashed', size = 1.5) +
  labs(x = "Age (Years)", y = "Estimated Marginal\nMean Differences in Accuracy", color = "Incentive", fill = "Incentive") +
  plot_theme + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
ggsave(paste0(hyp_analyzed_data_path, 'FigS6.png'), width = mid_width, height = norm_height) 
age_incent_time_interact <- ggpredict(learning_acc_binom, terms = c("ExactAge [all]", "Incentive", "block_third"))
ggplot(age_incent_time_interact, aes(x = x, y = predicted)) + 
  ylim(.92, 1) +
  geom_line(aes(color = group), size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  scale_color_manual(values = c(silver, gold)) +
  scale_fill_manual(values = c(silver, gold)) +
  labs(x = "Age (Years)", y = "Mixed-Effects Model Predictions\nof Accuracy", color = "Incentive", fill = "Incentive") +
  facet_grid(cols = vars(facet)) +
  plot_theme + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
ggsave(paste0(hyp_analyzed_data_path, 'Fig2A.png'), width = mid_width, height = norm_height)

# Instructive age values clarify incentive differences in learning_acc_binom
age_pts <- c(10.5, 11.5, 15, 16, 19.5, 20.5) # age points of interest
em_incent_age_acc <- emmeans(learning_acc_binom, specs = c("Incentive"), by = c("ExactAge"), at = list(ExactAge = age_pts))
em_incent_age_acc # marginal means for each point of interest                       
comp_em_incent_age_acc <- pairs(em_incent_age_acc)
comp_em_incent_age_acc
sink(paste0(hyp_analyzed_data_path, 'marg_means.txt'))
print(age_pts)
print(p.adjust(data.frame(comp_em_incent_age_acc)[,"p.value"], method = "holm")) # p-values corrected for multiple comparisons
sink()

# Worse marginal R2 values indicate that the extra random effects are not worth the parsimony trade-offs
learning_acc_binom_time_ran <- glmer(cbind(n_corr, n_total-n_corr) ~ ExactAge * Difficulty * Incentive * block_third + (1 + block_third|participant), data = learn_acc_stats, family = binomial)
learning_acc_binom_incent_ran <- glmer(cbind(n_corr, n_total-n_corr) ~ ExactAge * Difficulty * Incentive * block_third + (1 + Incentive|participant), data = learn_acc_stats, family = binomial)
learning_acc_binom_diff_ran <- glmer(cbind(n_corr, n_total-n_corr) ~ ExactAge * Difficulty * Incentive * block_third + (1 + Difficulty|participant), data = learn_acc_stats, family = binomial)
sink(paste0(hyp_analyzed_data_path, 'ran_slopes.txt'))
cat("No random slopes:\n")
print(performance::r2(learning_acc_binom))
cat("\nRandom slope for time:\n")
print(performance::r2(learning_acc_binom_time_ran))
cat("\nRandom slope for incentive:\n")
print(performance::r2(learning_acc_binom_incent_ran))
cat("\nRandom slope for difficulty:\n")
print(performance::r2(learning_acc_binom_diff_ran))
sink()

# Run NFC model with Binomial distribution and assess
learning_nfc_acc_binom <- glmer(cbind(n_corr, n_total-n_corr) ~ nfc_total * Difficulty * Incentive * block_third + ExactAge + (1|participant), data = learn_acc_quest_stats, family = binomial)
set.seed(123)
posterior_predictive_check(learning_nfc_acc_binom) # Awesome! Binomial distribution fulfills model assumptions

# Identify main effects and interactions from learning_nfc_acc_binom
sink(paste0(hyp_analyzed_data_path, 'TabS4NFC.txt'))
print(Anova(learning_nfc_acc_binom, type = "II"))
sink()

# Run BAS-Drive model with Binomial distribution and assess
learning_bas_acc_binom <- glmer(cbind(n_corr, n_total-n_corr) ~ bas_drive_total * Difficulty * Incentive * block_third + ExactAge + (1|participant), data = learn_acc_quest_stats, family = binomial)
set.seed(123)
posterior_predictive_check(learning_bas_acc_binom) # Awesome! Binomial distribution fulfills model assumptions

# Identify main effects and interactions from learning_bas_acc_binom
sink(paste0(hyp_analyzed_data_path, 'TabS4BAS.txt'))
print(Anova(learning_bas_acc_binom, type = "II"))
sink()

# Run Baseline model with Binomial distribution and assess
learning_baseline_acc_binom <- glmer(cbind(n_corr, n_total-n_corr) ~ mean_acc * Difficulty * Incentive * block_third + ExactAge + (1|participant), data = learn_acc_baseline_stats, family = binomial)
set.seed(123)
posterior_predictive_check(learning_baseline_acc_binom) # Awesome! Binomial distribution fulfills model assumptions

# Identify main effects and interactions from learning_baseline_acc_binom
sink(paste0(hyp_analyzed_data_path, 'TabS4Baseline.txt'))
print(Anova(learning_baseline_acc_binom, type = "II"))
sink()

# Run RT model with Gaussian distribution and assess
learning_rt_lmer <- lmer(keyTrial.rt ~ ExactAge * Difficulty * Incentive * block_third + (1|participant), data = learn_rt_stats)
set.seed(123)
posterior_predictive_check(learning_rt_lmer) # Awesome! Gaussian distribution fulfills model assumptions

# Identify main effects and interactions from learning_rt_lmer
sink(paste0(hyp_analyzed_data_path, 'TabS3RT.txt'))
print(Anova(learning_rt_lmer, type = "II"))
sink()

# Save visualizations of interactions from learning_rt_lmer
age_diff_interact_rt <- ggpredict(learning_rt_lmer, terms = c("ExactAge [all]", "Difficulty"))
ggplot(age_diff_interact_rt, aes(x, predicted)) + 
  ylim(.55, .85) +
  geom_line(aes(color = group), size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  scale_color_manual(values = c(easy_green, hard_blue)) +
  scale_fill_manual(values = c(easy_green, hard_blue)) +
  labs(x = "Age (Years)", y = "Mixed-Effects Model Predictions\nof Reaction Time (Seconds)", color = "Difficulty", fill = "Difficulty") +
  plot_theme + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
ggsave(paste0(hyp_analyzed_data_path, 'FigS7A.png'), width = narr_width, height = norm_height)
age_incent_diff_interact_rt <- ggpredict(learning_rt_lmer, terms = c("ExactAge [all]", "Difficulty", "Incentive"))
ggplot(age_incent_diff_interact_rt, aes(x, predicted)) + 
  ylim(.55, .85) +
  geom_line(aes(color = group), size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  scale_color_manual(values = c(easy_green, hard_blue)) +
  scale_fill_manual(values = c(easy_green, hard_blue)) +
  labs(x = "Age (Years)", y = "Mixed-Effects Model Predictions\nof Reaction Time (Seconds)", color = "Difficulty", fill = "Difficulty") +
  facet_grid(cols = vars(facet)) +
  plot_theme + theme(axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
ggsave(paste0(hyp_analyzed_data_path, 'FigS7B.png'), width = mid_width, height = norm_height)

# Run RT sensitivity analysis model with Gaussian distribution and assess
learning_rt_all_acc_lmer <- lmer(keyTrial.rt ~ ExactAge * Difficulty * Incentive * block_third + (1|participant), data = learn_rt_all_acc_stats)
set.seed(123)
posterior_predictive_check(learning_rt_all_acc_lmer) # Awesome! Gaussian distribution fulfills model assumptions

# Identify main effects and interactions from learning_rt_all_acc_lmer
sink(paste0(hyp_analyzed_data_path, 'TabS3SensRT.txt'))
print(Anova(learning_rt_all_acc_lmer, type = "II"))
sink()

############################
### Testing Hypothesis 5 ###
############################

# Run effort rating model with Gaussian distribution and assess
eff_rating_lmer <- lmer(keyPostGameRating.keys ~ correct * Block * ExactAge + (1|participant), data = eff_q_stats)
set.seed(123)
posterior_predictive_check(eff_rating_lmer) # Ok, Gaussian distribution fulfills model assumptions well enough

# Identify main effects and interactions from eff_rating_lmer
sink(paste0(hyp_analyzed_data_path, 'rating_eff.txt'))
Anova(eff_rating_lmer, type = "II")
sink()

# Run difficulty rating model with Gaussian distribution and assess
rating_diff_lmer <- lmer(keyPostGameRating.keys ~ ExactAge * Block + (1|participant), data = diff_q)
set.seed(123)
posterior_predictive_check(rating_diff_lmer) # Ok, Gaussian distribution fulfills model assumptions well enough

# Identify main effects and interactions from rating_diff_lmer
sink(paste0(hyp_analyzed_data_path, 'rating_diff.txt'))
print(Anova(rating_diff_lmer, type = "II"))
sink()

# Save visualization of interactions from eff_rating_lmer
ggplot(data = age_acc_galaxy_interact_eff, aes(x = ExactAge, y = rating_differ, color = Difficulty, fill = Difficulty)) +
  scale_x_continuous(breaks = seq(10, 20, 2)) +
  geom_hline(yintercept = c(-3, 3, 6), colour = 'grey90') +
  geom_hline(yintercept = c(0), colour = 'black', linetype = 'dashed') +
  scale_y_continuous(breaks = c(-3, 0, 3, 6)) +  
  geom_point(alpha = 0.6, size = 2.75) +
  geom_smooth(method = "lm", size = 1.5) +
  scale_color_manual(values = c(easy_green, hard_blue)) +
  scale_fill_manual(values = c(easy_green, hard_blue)) +
  labs(x = "Age (Years)", y = "Mean Effort Rating Difference\n10¢ - 1¢ Incentive") +
  facet_grid(cols = vars(split)) +
  plot_theme
ggsave(paste0(hyp_analyzed_data_path, 'Fig3.png'), width = mid_width, height = norm_height)
