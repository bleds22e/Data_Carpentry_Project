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
rel_ab_period <- merge(Dipo_count_period, total_dipo_period, by = "period", all = TRUE)
relative_abundance_per <- rel_ab_period %>% 
                      mutate(rel_abund = count/total_dipo_period)

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

###
# All dipos grouped by season
###

# all dipos in periods before the plot switches
Dipos <- filter(surveys, species == 'DM' | species == 'DO' | species == 'DS', period > 0 & period < 437) %>% 
         arrange(yr, mo, plot, species)

find_season <- function(month){
  # function putting each month is a seasonal category
  if (any(month == '1' | month == '2' | month == '3')){
    season = "1"
  } else if (any(month == '4' | month == '5' | month == '6')){
    season = "2"
  } else if (any(month == '7' | month == '8' | month == '9')){
    season = "3"
  } else {
    season = "4"
  }
  return(season)
} 

# adding a column for season to the data 
month_list <- as.list(Dipos$mo)
Dipos$season <- rapply(month_list, find_season)

# grouping by plot/year/season
season_per_year <- select(Dipos, yr, season, plot, species) %>% 
                   group_by(plot, yr, season, species) %>% 
                   summarise(count = n())

# adding seasonal relative abundance column
season_total <- season_per_year %>% 
  group_by(plot, yr, season) %>% 
  summarise(season_total = sum(count))
rel_ab_season <- merge(season_per_year, season_total, by.x = c("plot", "yr", "season"), all = TRUE)
season_rel_abund <- rel_ab_season %>% 
  mutate(rel_abund = count/season_total)

# give each year/season combo a unique id
yr_season_ID <- select(seasonal_rel_abund, yr, season) %>% 
  distinct() 
ordered <- arrange(yr_season_ID, yr)
ordered$season_id <- seq_len(nrow(yr_season_ID))
season_ordered <- merge(seasonal_rel_abund, ordered, by.x = c("yr", "season"), all = TRUE) %>% 
  arrange(yr, season, plot, species)

## Plotting Relative Abundance of Dipos
library(scales)

# relative abundance of all dipos per period
ggplot(relative_abundance_per, aes(x = period, y = rel_abund, fill = species)) +
  geom_bar(postition = 'fill', stat = "identity", bin_width = 6) +
  scale_y_continuous(labels = percent_format())

# relative abundance of all dipos per year
ggplot(relative_abundance_yr, aes(x = yr, y = rel_abund, fill = species)) +
  geom_bar(postition = 'fill', stat = "identity", bin_width = 6) +
  scale_y_continuous(labels = percent_format())

# relative abundance of all dipos per plot per season
ggplot(season_ordered, aes(x = season_id, y = relative_abundance)) +
  geom_bar(stat = "identity", aes(color = species)) +
  facet_wrap(~plot, nrow = 6, ncol = 4)
