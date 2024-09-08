#############################################
### Define Shared Variables and Functions ###
#############################################

# Written by: Camille Phaneuf-Hadd (cphaneuf@g.harvard.edu)
# Last updated: 9/5/24

#########################
### General Utilities ###
#########################

# Define addAgeGroup() function (credit: Kate Nussenbaum)
addAgeGroup <- function(df){
  df %>% mutate(AgeGroup = case_when(ExactAge < 13 ~ "Children",
                                     ExactAge >= 13 & ExactAge < 18 ~ "Adolescents",
                                     ExactAge >= 18 ~ "Adults"),
                AgeGroup = factor(AgeGroup, levels = c("Children", "Adolescents", "Adults")))
}

##########################
### Plotting Utilities ###
##########################

# Set common plotting variables
wide_proportion_acc_format_y_axis <- c(.65, .72, .79, .86, .93, 1)
wide_proportion_rt_format_y_axis <- c(.5, .8, 1.1, 1.4, 1.7)
mid_proportion_acc_format_y_axis <- c(.72, .79, .86, .93, 1)
mid_proportion_rt_format_y_axis <- c(.5, .73, .96, 1.19, 1.42)
proportion_acc_format_y_axis <- c(.92, .94, .96, .98, 1)
proportion_rt_format_y_axis <- c(.65, .77, .89, 1.01, 1.13)
jitter <- position_jitter(width = .1, height = .1)
horizontal_jitter <- position_jitter(width = .2)
rating_format_y_axis <- c(1:7)

# Set plotting color scheme
fem_purple <- "#553262"
masc_purple <- "#96587D"
not_capt_purple <- "#BC90AB"
easy_green <- "#2F4B26"
hard_blue <- "#026C7C"
gold <- "#BE8D00"
silver <- "#A6A6A6"
# Note: grey90 is used for background plotting features

# Set plotting dimensions
wide_width <- 11
tall_height <- 9
norm_width <- 9
norm_height <- 7
mid_width <- 7
narr_width <- 5

# Set plotting theme
plot_theme <- theme(title = element_text(size = 24, face = "bold", family = "Avenir"),
                    plot.title = element_text(hjust = .5),
                    axis.title.x = element_text(size = 24, family = "Avenir"),
                    axis.title.y = element_text(size = 24, family = "Avenir"),
                    axis.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                    axis.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                    legend.text = element_text(size = 18, colour = "black", family = "Avenir"),
                    legend.position = "bottom",
                    legend.key = element_rect(fill = "transparent", color = NA),
                    strip.text.x = element_text(size = 18, colour = "black", family = "Avenir"),
                    strip.text.y = element_text(size = 18, colour = "black", family = "Avenir"),
                    panel.grid.major = element_blank(), # remove grid marks
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))
