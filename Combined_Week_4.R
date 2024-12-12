library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

My_Weight <- 0.7
O_weight <- 0.3

#####


Simple_QB <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/Simple_QB.csv")
Simple_Flex <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/Simple_Flex.csv")

Online_QB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_QB_Rankings.csv") %>% 
  select(c(-1))
Online_RB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_RB_Rankings.csv") %>% 
  select(c(-1))
Online_WR <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_WR_Rankings.csv") %>% 
  select(c(-1))
Online_TE <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_TE_Rankings.csv") %>% 
  select(c(-1))


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
  select(player, Position_yah, TEAM, Opponent_yah, Salary_yah, expected_points, ex_ppd, Online_proj, MATCHUP, Start_Sit) %>% 
  mutate(Online_proj = as.numeric(Online_proj)) %>% 
  mutate(combined_projection = expected_points*My_Weight + Online_proj*O_weight) %>% 
  mutate(combined_fppd = combined_projection/Salary_yah) %>% 
  select(c(1, 2, 12, 11, 3:10)) %>% 
  filter(!is.na(combined_projection))

Combined_Flex <- full_join(Simple_Flex, Online_Flex, by = c("player")) %>% 
  select(player, Position_yah, TEAM, Opponent_yah, Salary_yah, expected_points, ex_ppd, Online_proj, MATCHUP, Start_Sit) %>%  
  mutate(Online_proj = as.numeric(Online_proj)) %>%
  mutate(combined_projection = (expected_points*My_Weight + Online_proj*O_weight)) %>% 
  mutate(combined_fppd = combined_projection/Salary_yah) %>% 
  select(c(1, 2, 12, 11, 3:10)) %>% 
  filter(!is.na(combined_projection))

write_csv(Combined_QB, "~/R Stuff/FantasyFootball/R_Interim_Data/Combined_QB.csv")
write_csv(Combined_Flex, "~/R Stuff/FantasyFootball/R_Interim_Data/Combined_Flex.csv")
