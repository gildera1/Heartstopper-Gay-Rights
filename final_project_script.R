## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
table(data$number_people_told)
mean(data$number_people_told)
sd(data$number_people_told)
describe(data$number_people_told)
summary(data$number_people_told)

# EXAMINE QUANT_VAR2
table(data$number_mental_health_mentioned)
mean(data$number_mental_health_mentioned)
sd(data$number_mental_health_mentioned)
describe(data$number_mental_health_mentioned)
summary(data$number_mental_health_mentioned)

# EXAMINE QUAL_VAR1
table(data$sexual_identity)
describe(data$sexual_identity)

# EXAMINE QUAL_VAR2
table(data$coming_out)
describe(data$coming_out)

# EXAMINE QUAL_VAR3
table(data$reactions)
describe(data$reactions)

# EXAMINE QUAL_VAR4
table(data$change_mental_health)
describe(data$change_mental_health)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$reactions,data$change_mental_health)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$reactions, data$change_mental_health))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(number_mental_health_mentioned ~ change_mental_health, data = data)
summary(anova_adapted)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
plot(data$number_mental_health_mentioned, data$number_people_told)
cor(data$number_mental_health_mentioned, data$number_people_told)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(number_mental_health_mentioned ~ number_people_told, data = data)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$number_mental_health_mentioned, data$number_people_told)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$number_mental_health_mentioned, residuals(linear_relationship))
abline(h = 0, col = "red")