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
Dipo_count_period <- select(surveys, period, species) %>% 
                     filter(species == 'DM' | species == 'DO' | species == 'DS', period > 0) %>% 
                     group_by(period, species) %>% 
                     summarise(count = n())

# plot DM, DO, and DS by period
ggplot(Dipo_count_period, aes(x = period, y = count)) + 
  geom_point(aes(color = species)) +
  geom_line(aes(color = species)) +
  xlab("Period") +
  ylab("# Individuals")

###
# Relative abundance of Krats
###

# relative abundance of dipos by period
total_dipo_period <- Dipo_count_period %>% 
                     group_by(period) %>% 
                     summarise(total_dipo_period = sum(count))
rel_ab <- merge(Dipo_count_period, total_dipo_period, by = "period", all = TRUE)
relative_abundance <- rel_ab %>% 
                      mutate(count/total_dipo_period)
names(relative_abundance)[names(relative_abundance) == 'count/total_dipo_period'] <- 'rel_abund'

# relative abundance of dipos by year
Dipo_count_year <- select(surveys, yr, period, species) %>% 
                   filter(species == 'DM' | species == 'DO' | species == 'DS', period > 0) %>% 
                      # need to keep period > 0 in because some trapping done outside normal periods
                      # marked by negative period entries
                   group_by(yr, species) %>% 
                   summarise(count = n())

total_dipo_year <- Dipo_count_year %>% 
                   group_by(yr) %>% 
                   summarise(total_dipo_year = sum(count))
rel_ab_yr <- merge(Dipo_count_year, total_dipo_year, by = "yr", all = TRUE)
relative_abundance_yr <- rel_ab_yr %>% 
                         mutate(rel_abund = count/total_dipo_year)
rename(relative_abundance_yr, relative_abund = count/total_dipo_year)
