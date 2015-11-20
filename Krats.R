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

###
# Relative abundance of Krats
###

# relative abundance of dipos by period
total_dipo_period <- Dipo_count %>% 
                     group_by(period) %>% 
                     summarise(total_dipo_period = sum(count))
rel_ab <- merge(Dipo_count, total_dipo_period, by = "period", all = TRUE)
relative_abundance <- rel_ab %>% 
                      mutate(count/total_dipo_period)
relative_abundance$rel_abund <- relative_abundance$count/total_dipo_period
names(relative_abundance)[names(relative_abundance) == 'count/total_dipo_period'] <- 'rel_abund'

