# Creator: Ellen Bledsoe
# This code is for exploring kangaroo rat capture data for the Portal project

# load packages
library(dplyr)
library(ggplot2)
library(scales)

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

# save plot output as png
dev.copy(png, 'DM_DO_counts_period.png')
dev.off()

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

# save plot output as png
dev.copy(png, 'dipo_count_by_period.png')
dev.off()

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

# plot relative abundance of all dipos per period
ggplot(relative_abundance_per, aes(x = period, y = rel_abund, fill = species)) +
  geom_bar(postition = 'fill', stat = "identity", bin_width = 6, colour = NA) +
  scale_y_continuous(labels = percent_format())

# save plot output as png
dev.copy(png, 'rel_abund_dipos_period.png')
dev.off()

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

# relative abundance of all dipos per year
ggplot(relative_abundance_yr, aes(x = yr, y = rel_abund, fill = species)) +
  geom_bar(postition = 'fill', stat = "identity", bin_width = 6) +
  scale_y_continuous(labels = percent_format())

# save plot output as png
dev.copy(png, 'rel_abund_dipos_year.png')
dev.off()

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
yr_season_ID <- select(season_rel_abund, yr, season) %>% 
  distinct() 
ordered <- arrange(yr_season_ID, yr)
ordered$season_id <- seq_len(nrow(yr_season_ID))
season_ordered <- merge(season_rel_abund, ordered, by.x = c("yr", "season"), all = TRUE) %>% 
  arrange(yr, season, plot, species)

# plot relative abundance of all dipos per plot per season
ggplot(season_ordered, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(color = species)) +
  facet_wrap(~plot, nrow = 6, ncol = 4)

# save plot output as png
dev.copy(png, 'rel_abund_dipo_season.png')
dev.off()

###
# Explore krat relationship with PBs
###

# Look at Krats and PBs by period
PB_count_period <- select(surveys, period, species) %>% 
                   filter(species == 'PB') %>% 
                   group_by(period) %>% 
                   summarise(count = n())
Krat_count_period <- select(surveys, period, species) %>% 
                     filter(species == 'DM' | species == 'DS' | species == 'DO') %>% 
                     group_by(period) %>% 
                     summarise(count = n())

# want to add zeros when there are no rodents per period
periods1 <- unique(surveys$period)

fullGrid <- expand.grid(period = periods1)
fullGrid_krats <- merge(Krat_count_period, fullGrid, by = c('period'), all = TRUE)
fullGrid_PB <- merge(PB_count_period, fullGrid, by = c('period'), all = TRUE)

fullGrid_krats$count[is.na(fullGrid_krats$count)] = 0
fullGrid_PB$count[is.na(fullGrid_PB$count)] = 0

# add identifying column for plotting
fullGrid_krats$species <- rep('krat', nrow(fullGrid_krats))
fullGrid_PB$species <- rep('PB', nrow(fullGrid_PB))

# merging PB and krat dataframes
all_krat_PB <- bind_rows(fullGrid_krats, fullGrid_PB)

# remove negative periods
vec <- all_krat_PB$period > 0
all_krat_PB <- all_krat_PB[vec,]

#plotting all_krat_PB
ggplot(all_krat_PB, aes(x = period, y = count, color = species)) +
  geom_point() +
  geom_line() +
  xlab("Period") +
  ylab("# Individuals")

# save plot output as png
dev.copy(png, 'krat_PB_count_period.png')
dev.off()
