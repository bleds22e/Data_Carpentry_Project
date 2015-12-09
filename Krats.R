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
# Dipos and grass/shrubs
###

# read in transects file
transects <- read.csv('data/Transect_04_09_totals.csv')

# krat abundance by season, years 2002 through 2011
dipos_04_09 <- filter(season_ordered, yr > 2002 & yr < 2011)

# plotting only 2002 through 2011
ggplot(dipos_04_09, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(color = species, fill = species)) +
  facet_wrap(~plot, nrow = 6, ncol = 4)

# save plot output as png
dev.copy(png, 'dipos_02_11.png')
dev.off()

# adding percent column to plants
dipos_pl_per <- transects %>% 
  mutate(Prop*100)
colnames(dipos_pl_per) = c("Year", "Plot", "Type", "Hits", "Prop", "Percent")

season_04_09 <- function(year){
  # function to add estimated season to year
  if (year == '2004'){
    season = '109'
  } else {
    season = '129'
  }
  return(season)
}

# adding season column to transect data
year_list <- as.list(dipos_pl_per$Year)
dipos_pl_per$Season <- rapply(year_list, season_04_09)


# add plant abundance lines
dipos_pl_per$Season <- as.numeric(dipos_pl_per$Season)
dipos_pl_per$Prop <- as.numeric(dipos_pl_per$Prop)

ggplot(dipos_04_09, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = dipos_pl_per, aes(x = Season, y = Prop, color = Type)) +
  facet_wrap(~plot, nrow = 6, ncol = 4)

# save plot output as png
dev.copy(png, 'dipos_plants_04_09.png')
dev.off()

# plotting some of the individual plots to take a better look
  # still very preliminary

library(RColorBrewer)
pdf(file = "dipos_plants_04-09.pdf", width = 6.25, height = 4, onefile = TRUE)

# plot 1

dipos_1 <- filter(dipos_04_09, plot == '1')
transect_1 <- filter(dipos_pl_per, Plot == '1')

ggplot(dipos_1, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_1, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 1")

# plot 2

dipos_2 <- filter(dipos_04_09, plot == '2')
transect_2 <- filter(dipos_pl_per, Plot == '2')

ggplot(dipos_2, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_2, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 2")

# plot 4

dipos_4 <- filter(dipos_04_09, plot == '4')
transect_4 <- filter(dipos_pl_per, Plot == '4')

ggplot(dipos_4, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_4, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 4")

# plot 8
dipos_8 <- filter(dipos_04_09, plot == '8')
transect_8 <- filter(dipos_pl_per, Plot == '8')

ggplot(dipos_8, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_8, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 8")

# plot 9

dipos_9 <- filter(dipos_04_09, plot == '9')
transect_9 <- filter(dipos_pl_per, Plot == '9')

ggplot(dipos_9, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_9, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 9")

# plot 11

dipos_11 <- filter(dipos_04_09, plot == '11')
transect_11 <- filter(dipos_pl_per, Plot == '11')

ggplot(dipos_11, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_11, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 11")

# plot 12

dipos_12 <- filter(dipos_04_09, plot == '12')
transect_12 <- filter(dipos_pl_per, Plot == '12')

ggplot(dipos_12, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_12, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 12")

#plot 14

dipos_14 <- filter(dipos_04_09, plot == '14')
transect_14 <- filter(dipos_pl_per, Plot == '14')

ggplot(dipos_14, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_14, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 14")

# plot 17

dipos_17 <- filter(dipos_04_09, plot == '17')
transect_17 <- filter(dipos_pl_per, Plot == '17')

ggplot(dipos_17, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_17, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 17")

# plot 22

dipos_22 <- filter(dipos_04_09, plot == '22')
transect_22 <- filter(dipos_pl_per, Plot == '22')

ggplot(dipos_22, aes(x = season_id, y = rel_abund)) +
  geom_bar(stat = "identity", aes(fill = species)) +
  geom_line(data = transect_22, size = 2, aes(x = Season, y = Prop, color = Type)) +
  scale_colour_grey() +
  ggtitle("Plot 22")

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



