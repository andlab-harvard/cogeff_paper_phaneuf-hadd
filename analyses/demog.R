############################################
### Calculate Demographic Data Summaries ###
############################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 9/5/24

# Inputs: study 1 and study 2 demographic data
# Computes: 
# - visual summary of sample (study 1)
# - statistical summary of sample (study 1, study 2)
# Outputs: 
# - png plot of study 1 age-gender distribution into results/study1/demog directory
# - txt files of demographic stats into results/study1/demog and results/study2/demog directories

#####################
### Set up Script ###
#####################

# Load needed libraries
require(pacman) # for p_load()
p_load(tidyverse, # for df manipulation
       dplyr) # for %>% and other operators

# Load shared variables and functions
source("utilities.R")

# Set paths to demographic data
study1_demog_data_path <- '../data/study1/'
study2_demog_data_path <- '../data/study2/'

# Set output paths
study1_analyzed_data_path <- '../results/study1/demog/'
study2_analyzed_data_path <- '../results/study2/demog/'

# Read in demographic data
study1_demog <- read_csv(paste0(study1_demog_data_path, "qa_demog.csv"))
study2_demog <- read_csv(paste0(study2_demog_data_path, "qa_demog.csv"))

############################################
### Save Study 1 Age-Gender Distribution ###
############################################

ggplot(data = study1_demog, aes(x = FlooredAge, fill = Gender, color = Gender)) +
  scale_fill_manual(values = c(fem_purple, masc_purple, not_capt_purple)) + 
  scale_color_manual(values = c(fem_purple, masc_purple, not_capt_purple)) + 
  scale_x_continuous(breaks = c(10:20)) +
  geom_hline(yintercept = seq(0, 15, by = 3), colour = 'grey90') +
  scale_y_continuous(breaks = seq(0, 15, by = 3)) + 
  geom_histogram(binwidth = 1, alpha = .75) +
  labs(x = "Age (Years)", y = "Number of Participants") +
  plot_theme
ggsave(paste0(study1_analyzed_data_path, "FigS1.png"), plot = last_plot(), width = norm_width, height = norm_height)

##################################################
### Save Study 1 and Study 2 Demographic Stats ###
##################################################

# Study 1
n1 <- length(study1_demog$participant)
sink(paste0(study1_analyzed_data_path, 'TabS1Primary.txt')) # write to file instead of the terminal
cat("All Participants\n")
cat("Total N:", n1, "\n")
cat("\nAGE\n")
cat("Total Children:", sum(study1_demog$AgeGroup == "Children"), "\n")
cat("Total Adolescents:", sum(study1_demog$AgeGroup == "Adolescents"), "\n")
cat("Total Adults:", sum(study1_demog$AgeGroup == "Adults"), "\n")
cat("Mean Age:", mean(study1_demog$ExactAge), "\n")
cat("SD Age:", sd(study1_demog$ExactAge), "\n")
cat("\nGENDER\n")
cat("Total Feminine:", sum(study1_demog$Gender == "Feminine"), "\n")
cat("Total Masculine:", sum(study1_demog$Gender == "Masculine"), "\n")
cat("Total Not Captured by Options:", sum(study1_demog$Gender == "Not Captured by Options"), "\n")
cat("\nRACE\n")
cat("Proportion Asian:", sum(study1_demog$Race == "Asian", na.rm = TRUE) / n1, "\n")
cat("Proportion Black or African American:", sum(study1_demog$Race == "Black or African American", na.rm = TRUE) / n1, "\n")
cat("Proportion Native Hawaiian or other Pacific Islander:", sum(study1_demog$Race == "Native Hawaiian or other Pacific Islander", na.rm = TRUE) / n1, "\n")
cat("Proportion White:", sum(study1_demog$Race == "White", na.rm = TRUE) / n1, "\n")
cat("Proportion Not Captured by Options:", sum(study1_demog$Race == "Not Captured by Options") / n1, "\n")
cat("\nETHNICITY\n")
cat("Proportion Hispanic:", sum(study1_demog$Ethnicity == "Hispanic or Latino") / n1, "\n")
cat("Proportion Not Hispanic:", sum(study1_demog$Ethnicity == "Not Hispanic or Latino") / n1, "\n")
sink() # stop writing to file

# Study 2
n2 <- length(study2_demog$participant)
sink(paste0(study2_analyzed_data_path, 'TabS1Secondary.txt')) # write to file instead of the terminal
cat("All Participants\n")
cat("Total N:", length(study2_demog$participant), "\n")
cat("\nAGE\n")
cat("Total Children:", sum(study2_demog$AgeGroup == "Children"), "\n")
cat("Total Adolescents:", sum(study2_demog$AgeGroup == "Adolescents"), "\n")
cat("Total Adults:", sum(study2_demog$AgeGroup == "Adults"), "\n")
cat("Mean Age:", mean(study2_demog$ExactAge), "\n")
cat("SD Age:", sd(study2_demog$ExactAge), "\n")
cat("\nGENDER\n")
cat("Total Feminine:", sum(study2_demog$Gender == "Feminine"), "\n")
cat("Total Masculine:", sum(study2_demog$Gender == "Masculine"), "\n")
cat("Total Not Captured by Options:", sum(study2_demog$Gender == "Not Captured by Options"), "\n")
cat("\nRACE\n")
cat("Proportion Asian:", sum(study2_demog$Race == "Asian", na.rm = TRUE) / n2, "\n")
cat("Proportion Black or African American:", sum(study2_demog$Race == "Black or African American", na.rm = TRUE) / n2, "\n")
cat("Proportion Native Hawaiian or other Pacific Islander:", sum(study2_demog$Race == "Native Hawaiian or other Pacific Islander", na.rm = TRUE) / n2, "\n")
cat("Proportion White:", sum(study2_demog$Race == "White", na.rm = TRUE) / n2, "\n")
cat("Proportion Not Captured by Options:", sum(study2_demog$Race == "Not Captured by Options") / n2, "\n")
cat("\nETHNICITY\n")
cat("Proportion Hispanic:", sum(study2_demog$Ethnicity == "Hispanic or Latino") / n2, "\n")
cat("Proportion Not Hispanic:", sum(study2_demog$Ethnicity == "Not Hispanic or Latino") / n2, "\n")
sink() # stop writing to file
