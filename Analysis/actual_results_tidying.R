library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

Week <- 9

##################

Char_Week <- as.character(Week)


actual_passing <- read_csv(eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/Week_", Char_Week, "_actual_passing.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(actual_passing)[names(actual_passing) == 'na'] <- 'home_away'
actual_passing <- actual_passing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

actual_rushing <- read_csv(eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/Week_", Char_Week, "_actual_rushing.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(actual_rushing)[names(actual_rushing) == 'na'] <- 'home_away'
actual_rushing <- actual_rushing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

actual_receiving <- read_csv(eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/Week_", Char_Week, "_actual_receiving.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(actual_receiving)[names(actual_receiving) == 'na'] <- 'home_away'
actual_receiving <- actual_receiving %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))



names(actual_passing) <- paste0(names(actual_passing[]), "_pas")
names(actual_receiving) <- paste0(names(actual_receiving), "_rec")
names(actual_rushing) <- paste0(names(actual_rushing), "_rus")

actual_passing[ , c(14:31, 3, 4)] <- apply(actual_passing[ , c(14:31, 3, 4)], 2,           
                                           function(x) as.character(x))

actual_receiving[ , c(14:24, 3, 4)] <- apply(actual_receiving[ , c(14:24, 3, 4)], 2,            
                                             function(x) as.character(x))

actual_rushing[ , c(14:21, 3, 4)] <- apply(actual_rushing[ , c(14:21, 3, 4)], 2,            
                                           function(x) as.character(x))

actual_passing[is.na(actual_passing)] <- "0"
actual_receiving[is.na(actual_receiving)] <- "0"
actual_rushing[is.na(actual_rushing)] <- "0"

#

complete <- full_join(actual_receiving, actual_rushing, by = c("player_rec" = "player_rus", "week_rec" = "week_rus")) %>% 
  full_join(actual_passing, by = c("player_rec" = "player_pas", "week_rec" = "week_pas"))



complete <- complete %>% 
  mutate(team = coalesce(team_rec, team_rus, team_pas),
         opp = coalesce(opp_rec, opp_rus, opp_pas),
         home_away = coalesce(home_away_rus, home_away_rec, home_away_pas),
         pos = coalesce(pos_rec, pos_rus, pos_pas),
         player = player_rec,
         week = week_rec,
         g_number = coalesce(g_number_rus, g_number_pas, g_number_rec),
         age = coalesce(age_rec, age_rus, age_pas)) 

complete[is.na(complete)] <- "0"

complete <- complete %>% 
  select(player, pos, age, g_number, week, team, opp, home_away, yds_rec, tgt_rec, yds_2_rec, tgt_2_rec:y_tgt_rec, yds_rus:yds_2_rus, att_2_rus:td_rus, 
         att_pas, cmp_pas:y_c_pas)

complete[ , c(4:5, 9:44)] <- apply(complete[ , c(4:5, 9:44)], 2,            
                                                         function(x) as.numeric(as.character(x)))

complete <- complete %>% 
  mutate(points = rec_rec*0.5 + yds_rec*0.1 + yds_rus*0.1 + td_rec*6 + td_rus*6 + yds_pas*0.04 +td_pas*4 - int_pas)

#

complete$player <- str_replace_all(complete$player, "[^[:alnum:]]", " ")
complete$player <- str_replace_all(complete$player, "\\s+", " ")
complete$player <- trimws(complete$player)


####

write_csv(complete, eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/week_", Char_Week, "_actual_complete.csv", sep = "")))





