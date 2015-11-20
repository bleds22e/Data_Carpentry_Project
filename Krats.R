# Creator: Ellen Bledsoe
# This code is for exploring kangaroo rat capture data for the Portal project

# load packages
library(dplyr)
library(ggplot2)

# load data
surveys <- read.csv("data/surveys.csv", header = TRUE)

###
# Krat counts by period
###

# get counts of DM and DO by period
DM_DO_count <- select(surveys, period, species) %>% 
               filter(species == 'DM' | species == 'DO', period > 0) %>% 
               group_by(period, species) %>% 
               summarise(count = n())
# plot DM and DO counts by period
ggplot(DM_DO_count, aes(x = period, y = count)) + 
  geom_point(aes(color = species)) +
  geom_line(aes(color = species)) +
  xlab("Period") +
  ylab("# Individuals")

# get counts of DM, DO, and DS by period
Dipo_count <- select(surveys, period, species) %>% 
              filter(species == 'DM' | species == 'DO' | species == 'DS', period > 0) %>% 
              group_by(period, species) %>% 
              summarise(count = n())

# plot DM, DO, and DS by period
ggplot(Dipo_count, aes(x = period, y = count)) + 
  geom_point(aes(color = species)) +
  geom_line(aes(color = species)) +
  xlab("Period") +
  ylab("# Individuals")
