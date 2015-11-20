# Creator: Ellen Bledsoe
# This code is for exploring kangaroo rat capture data for the Portal project

# load packages
library(dplyr)
library(ggplot2)

# load data
surveys <- read.csv("data/surveys.csv", header = TRUE)

# get counts of DM and DO by period
periods <- select(surveys, period, species)
DM_DO <- filter(surveys, species == 'DM' | species == 'DO', period > 0)
krats_period <- group_by(DM_DO, period, species)
count_by_period <- summarise(krats_period, count = n())
