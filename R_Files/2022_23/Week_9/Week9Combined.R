library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

My_Weight <- 0.7
O_weight <- 0.3

Week <- 9

###########################

Char_Week <- as.character(Week)  

Simple_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Model_Results/Week_", Char_Week, "/My_Model_Week_", Char_Week ,"_QB.csv", sep = "")))
Simple_Flex <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Model_Results/Week_", Char_Week, "/My_Model_Week_", Char_Week ,"_Flex.csv", sep = "")))

Online_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", Char_Week, "/FantasyPros_2022_Week_", Char_Week ,"_QB_Rankings.csv", sep = ""))) %>% 
  select(c(-1))
Online_RB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", Char_Week, "/FantasyPros_2022_Week_", Char_Week ,"_RB_Rankings.csv", sep = ""))) %>% 
  select(c(-1))
Online_WR <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", Char_Week, "/FantasyPros_2022_Week_", Char_Week ,"_WR_Rankings.csv", sep = ""))) %>% 
  select(c(-1))
Online_TE <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", Char_Week, "/FantasyPros_2022_Week_", Char_Week ,"_TE_Rankings.csv", sep = ""))) %>% 
  select(c(-1))

###


names(Online_QB)[1] <- 'player'
names(Online_RB)[1] <- 'player'
names(Online_WR)[1] <- 'player'
names(Online_TE)[1] <- 'player'

Online_Flex <- rbind(Online_RB, Online_WR, Online_TE)

names(Online_QB)[6] <- 'Online_proj'
names(Online_Flex)[6] <- 'Online_proj'

names(Online_QB)[5] <- 'Start_Sit'
names(Online_Flex)[5] <- 'Start_Sit'

Online_QB$player <- str_replace_all(Online_QB$player, "[^[:alnum:]]", " ")
Online_QB$player <- str_replace_all(Online_QB$player, "\\s+", " ")
Online_QB$player <- trimws(Online_QB$player)

Online_Flex$player <- str_replace_all(Online_Flex$player, "[^[:alnum:]]", " ")
Online_Flex$player <- str_replace_all(Online_Flex$player, "\\s+", " ")
Online_Flex$player <- trimws(Online_Flex$player)


Combined_QB <- full_join(Simple_QB, Online_QB, by = c("player")) %>% 
  select(player, Position_yah, TEAM, Opponent_yah, Salary_yah, expected_points, ex_ppd, Online_proj, MATCHUP, Start_Sit, home_away) %>% 
  mutate(Online_proj = as.numeric(Online_proj)) %>% 
  mutate(combined_projection = expected_points*My_Weight + Online_proj*O_weight) %>% 
  mutate(combined_fppd = combined_projection/Salary_yah) %>% 
  select(c(1:4, 11, 12, 5, 13, 6:10)) %>% 
  filter(!is.na(combined_projection))

Combined_Flex <- full_join(Simple_Flex, Online_Flex, by = c("player")) %>% 
  select(player, Position_yah, TEAM, Opponent_yah, Salary_yah, expected_points, ex_ppd, Online_proj, MATCHUP, Start_Sit, home_away) %>%  
  mutate(Online_proj = as.numeric(Online_proj)) %>%
  mutate(combined_projection = (expected_points*My_Weight + Online_proj*O_weight)) %>% 
  mutate(combined_fppd = combined_projection/Salary_yah) %>% 
  select(c(1:4, 11, 12, 5, 13, 6:10)) %>% 
  filter(!is.na(combined_projection))

write_csv(Combined_QB, eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week ,"_QB.csv", sep = "")))
write_csv(Combined_Flex, eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week ,"_Flex.csv", sep = "")))
