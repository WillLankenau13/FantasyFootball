library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")



Week_1_Passing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_1/Week_1_actual_passing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
  names(Week_1_Passing)[names(Week_1_Passing) == 'na'] <- 'home_away'
Week_1_Passing <- Week_1_Passing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_1_Rushing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_1/Week_1_actual_rushing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_1_Rushing)[names(Week_1_Rushing) == 'na'] <- 'home_away'
Week_1_Rushing <- Week_1_Rushing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_1_Receiving <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_1/Week_1_actual_receiving.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_1_Receiving)[names(Week_1_Receiving) == 'na'] <- 'home_away'
Week_1_Receiving <- Week_1_Receiving %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

###

Week_2_Passing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_2/Week_2_actual_passing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_2_Passing)[names(Week_2_Passing) == 'na'] <- 'home_away'
Week_2_Passing <- Week_2_Passing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_2_Rushing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_2/Week_2_actual_rushing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_2_Rushing)[names(Week_2_Rushing) == 'na'] <- 'home_away'
Week_2_Rushing <- Week_2_Rushing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_2_Receiving <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_2/Week_2_actual_receiving.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_2_Receiving)[names(Week_2_Receiving) == 'na'] <- 'home_away'
Week_2_Receiving <- Week_2_Receiving %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

###

Week_3_Passing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_3/Week_3_actual_passing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_3_Passing)[names(Week_3_Passing) == 'na'] <- 'home_away'
Week_3_Passing <- Week_3_Passing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_3_Rushing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_3/Week_3_actual_rushing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_3_Rushing)[names(Week_3_Rushing) == 'na'] <- 'home_away'
Week_3_Rushing <- Week_3_Rushing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_3_Receiving <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_3/Week_3_actual_receiving.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_3_Receiving)[names(Week_3_Receiving) == 'na'] <- 'home_away'
Week_3_Receiving <- Week_3_Receiving %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

###

Week_4_Passing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_4/Week_4_actual_passing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_4_Passing)[names(Week_4_Passing) == 'na'] <- 'home_away'
Week_4_Passing <- Week_4_Passing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_4_Rushing <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_4/Week_4_actual_rushing.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_4_Rushing)[names(Week_4_Rushing) == 'na'] <- 'home_away'
Week_4_Rushing <- Week_4_Rushing %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))

Week_4_Receiving <- read_csv("~/R Stuff/FantasyFootball/2022_Stats/Week_4/Week_4_actual_receiving.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names
names(Week_4_Receiving)[names(Week_4_Receiving) == 'na'] <- 'home_away'
Week_4_Receiving <- Week_4_Receiving %>% 
  mutate(home_away = ifelse(is.na(home_away), "home", "away"))



actual_passing <- rbind(Week_1_Passing, Week_2_Passing, Week_3_Passing, Week_4_Passing)
actual_receiving <- rbind(Week_1_Receiving, Week_2_Receiving, Week_3_Receiving, Week_4_Receiving)
actual_rushing <- rbind(Week_1_Rushing, Week_2_Rushing, Week_3_Rushing, Week_4_Rushing)

names(actual_passing) <- paste0(names(actual_passing[]), "_pas")
names(actual_receiving) <- paste0(names(actual_receiving), "_rec")
names(actual_rushing) <- paste0(names(actual_rushing), "_rus")

actual_passing[ , c(14:31, 3, 4)] <- apply(actual_passing[ , c(14:31, 3, 4)], 2,           
                                           function(x) as.character(x))

actual_receiving[ , c(14:21, 3, 4)] <- apply(actual_receiving[ , c(14:21, 3, 4)], 2,            
                                             function(x) as.character(x))

actual_rushing[ , c(14:18, 3, 4)] <- apply(actual_rushing[ , c(14:18, 3, 4)], 2,            
                                           function(x) as.character(x))

actual_passing[is.na(actual_passing)] <- "0"
actual_receiving[is.na(actual_receiving)] <- "0"
actual_rushing[is.na(actual_rushing)] <- "0"

actual_passing[ , c(14:31, 3, 4)] <- apply(actual_passing[ , c(14:31, 3, 4)], 2,           
                    function(x) as.numeric(as.character(x)))

actual_receiving[ , c(14:21, 3, 4)] <- apply(actual_receiving[ , c(14:21, 3, 4)], 2,            
                    function(x) as.numeric(as.character(x)))

actual_rushing[ , c(14:18, 3, 4)] <- apply(actual_rushing[ , c(14:18, 3, 4)], 2,            
                    function(x) as.numeric(as.character(x)))

#

actual_flex <- full_join(actual_receiving, actual_rushing, by = c("player_rec" = "player_rus", "week_rec" = "week_rus")) 

actual_flex[ , c(1:41)] <- apply(actual_flex[ , c(1:41)], 2,            
                               function(x) as.character(x))

actual_flex[is.na(actual_flex)] <- "0"

actual_flex[ , c(14:21, 3, 4, 24:28, 35:40, 7)] <- apply(actual_flex[ , c(14:21, 3, 4, 24:28, 35:40, 7)], 2,            
                                             function(x) as.numeric(as.character(x)))

actual_flex <- actual_flex %>% 
  mutate(points = rec_rec*0.5 + yds_rec*0.1 + yds_rus*0.1 + td_rec*6 + td_rus*6)

#

actual_QB <- full_join(actual_passing, actual_rushing, by = c("player_pas" = "player_rus", "week_pas" = "week_rus")) 

actual_QB[ , c(1:51)] <- apply(actual_QB[ , c(1:51)], 2,            
                                                    function(x) as.character(x))

actual_QB[is.na(actual_QB)] <- "0"

actual_QB[ , c(14:31, 3, 4, 34:36, 45:49, 7)] <- apply(actual_QB[ , c(14:31, 3, 4, 34:36, 45:49, 7)], 2,            
                                                      function(x) as.numeric(as.character(x)))

actual_QB <- actual_QB %>% 
  mutate(points = yds_pas*0.04 + yds_rus*0.1 + td_pas*4 + td_rus*6 - int_pas) %>% 
  select(c(-1, -39))

#

actual_flex[ , 7] <- apply(actual_flex[ , 7], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
actual_QB[ , 7] <- apply(actual_QB[ , 7], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

actual_flex$player_rec <- str_replace_all(actual_flex$player_rec, "[^[:alnum:]]", " ")
actual_flex$player_rec <- str_replace_all(actual_flex$player_rec, "\\s+", " ")
actual_flex$player_rec <- trimws(actual_flex$player_rec)

actual_QB$player_pas <- str_replace_all(actual_QB$player_pas, "[^[:alnum:]]", " ")
actual_QB$player_pas <- str_replace_all(actual_QB$player_pas, "\\s+", " ")
actual_QB$player_pas <- trimws(actual_QB$player_pas)

######

Week_3_Online_QB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_3/FantasyPros_Week_3_QB.csv") %>% 
  select(c(-1))
Week_3_Online_RB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_3/FantasyPros_Week_3_RB.csv") %>% 
  select(c(-1))
Week_3_Online_WR <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_3/FantasyPros_Week_3_WR.csv") %>% 
  select(c(-1))
Week_3_Online_TE <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_3/FantasyPros_Week_3_TE.csv") %>% 
  select(c(-1))


names(Week_3_Online_QB)[1] <- 'player'
names(Week_3_Online_RB)[1] <- 'player'
names(Week_3_Online_WR)[1] <- 'player'
names(Week_3_Online_TE)[1] <- 'player'

Week_3_Online_Flex <- rbind(Week_3_Online_RB, Week_3_Online_WR, Week_3_Online_TE)

names(Week_3_Online_QB)[6] <- 'Online_proj'
names(Week_3_Online_Flex)[6] <- 'Online_proj'

names(Week_3_Online_QB)[5] <- 'Start_Sit'
names(Week_3_Online_Flex)[5] <- 'Start_Sit'

Week_3_Online_QB$player <- str_replace_all(Week_3_Online_QB$player, "[^[:alnum:]]", " ")
Week_3_Online_QB$player <- str_replace_all(Week_3_Online_QB$player, "\\s+", " ")
Week_3_Online_QB$player <- trimws(Week_3_Online_QB$player)

Week_3_Online_Flex$player <- str_replace_all(Week_3_Online_Flex$player, "[^[:alnum:]]", " ")
Week_3_Online_Flex$player <- str_replace_all(Week_3_Online_Flex$player, "\\s+", " ")
Week_3_Online_Flex$player <- trimws(Week_3_Online_Flex$player)

Week_3_Online_QB <- Week_3_Online_QB %>% 
  mutate(week = 3)
Week_3_Online_Flex <- Week_3_Online_Flex %>% 
  mutate(week = 3)

##

Week_4_Online_QB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_QB_Rankings.csv") %>% 
  select(c(-1))
Week_4_Online_RB <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_RB_Rankings.csv") %>% 
  select(c(-1))
Week_4_Online_WR <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_WR_Rankings.csv") %>% 
  select(c(-1))
Week_4_Online_TE <- read_csv("~/R Stuff/FantasyFootball/Online_Models/Week_4/FantasyPros_2022_Week_4_TE_Rankings.csv") %>% 
  select(c(-1))

names(Week_4_Online_QB)[1] <- 'player'
names(Week_4_Online_RB)[1] <- 'player'
names(Week_4_Online_WR)[1] <- 'player'
names(Week_4_Online_TE)[1] <- 'player'

Week_4_Online_Flex <- rbind(Week_4_Online_RB, Week_4_Online_WR, Week_4_Online_TE)

names(Week_4_Online_QB)[6] <- 'Online_proj'
names(Week_4_Online_Flex)[6] <- 'Online_proj'

names(Week_4_Online_QB)[5] <- 'Start_Sit'
names(Week_4_Online_Flex)[5] <- 'Start_Sit'

Week_4_Online_QB$player <- str_replace_all(Week_4_Online_QB$player, "[^[:alnum:]]", " ")
Week_4_Online_QB$player <- str_replace_all(Week_4_Online_QB$player, "\\s+", " ")
Week_4_Online_QB$player <- trimws(Week_4_Online_QB$player)

Week_4_Online_Flex$player <- str_replace_all(Week_4_Online_Flex$player, "[^[:alnum:]]", " ")
Week_4_Online_Flex$player <- str_replace_all(Week_4_Online_Flex$player, "\\s+", " ")
Week_4_Online_Flex$player <- trimws(Week_4_Online_Flex$player)

Week_4_Online_QB <- Week_4_Online_QB %>% 
  mutate(week = 4)
Week_4_Online_Flex <- Week_4_Online_Flex %>% 
  mutate(week = 4)


Online_flex <- rbind(Week_3_Online_Flex, Week_4_Online_Flex)
Online_QB <- rbind(Week_3_Online_QB, Week_4_Online_QB)

######

Week_1_my_model_flex <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_1/My_Model_Week_1_Flex.csv") %>% 
  select(!Long_Name.x) %>% 
  mutate(week = 1)
Week_2_my_model_flex <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_2/My_Model_Week_2_Flex.csv") %>% 
  mutate(week = 2)
Week_3_my_model_flex <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_3/My_Model_Week_3_Flex.csv") %>% 
  mutate(week = 3)
Week_4_my_model_flex <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_4/My_Model_Week_4_Flex.csv") %>% 
  mutate(week = 4)

Week_1_my_model_QB <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_1/My_Model_Week_1_QB.csv") %>% 
  select(!Long_Name.x) %>% 
  mutate(week = 1)
Week_2_my_model_QB <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_2/My_Model_Week_2_QB.csv") %>% 
  mutate(week = 2)
Week_3_my_model_QB <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_3/My_Model_Week_3_QB.csv") %>% 
  mutate(week = 3)
Week_4_my_model_QB <- read_csv("~/R Stuff/FantasyFootball/Model_Results/Week_4/My_Model_Week_4_QB.csv") %>% 
  mutate(week = 4)

names(Week_1_my_model_flex) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_2_my_model_flex) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_3_my_model_flex) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_4_my_model_flex) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")

names(Week_1_my_model_QB) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_2_my_model_QB) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_3_my_model_QB) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")
names(Week_4_my_model_QB) <- c("player", "position", "opponent", "expected_points", "salary", "ex_ppd", "week")

My_model_flex <- rbind(Week_1_my_model_flex, Week_2_my_model_flex, Week_3_my_model_flex, Week_4_my_model_flex)
My_model_QB <- rbind(Week_1_my_model_QB, Week_2_my_model_QB, Week_3_my_model_QB, Week_4_my_model_QB)


########

Char_Week = "1"

Week_1_Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Yahoo/Week_", Char_Week, "_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`)) %>% 
  mutate(week = 1)


Week_1_Yahoo <- Week_1_Yahoo[complete.cases(Week_1_Yahoo[,c("Last Name")]),]

Week_1_Yahoo$full_name <- str_replace_all(Week_1_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_1_Yahoo$full_name <- str_replace_all(Week_1_Yahoo$full_name, "\\s+", " ")
Week_1_Yahoo$full_name <- trimws(Week_1_Yahoo$full_name)

#

Char_Week = "2"

Week_2_Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Yahoo/Week_", Char_Week, "_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`)) %>% 
  mutate(week = 2)


Week_2_Yahoo <- Week_2_Yahoo[complete.cases(Week_2_Yahoo[,c("Last Name")]),]

Week_2_Yahoo$full_name <- str_replace_all(Week_2_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_2_Yahoo$full_name <- str_replace_all(Week_2_Yahoo$full_name, "\\s+", " ")
Week_2_Yahoo$full_name <- trimws(Week_2_Yahoo$full_name)

#

Char_Week = "3"

Week_3_Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Yahoo/Week_", Char_Week, "_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`)) %>% 
  mutate(week = 3)


Week_3_Yahoo <- Week_3_Yahoo[complete.cases(Week_3_Yahoo[,c("Last Name")]),]

Week_3_Yahoo$full_name <- str_replace_all(Week_3_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_3_Yahoo$full_name <- str_replace_all(Week_3_Yahoo$full_name, "\\s+", " ")
Week_3_Yahoo$full_name <- trimws(Week_3_Yahoo$full_name)

#

Char_Week = "4"

Week_4_Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Yahoo/Week_", Char_Week, "_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`)) %>% 
  mutate(week = 4)


Week_4_Yahoo <- Week_4_Yahoo[complete.cases(Week_4_Yahoo[,c("Last Name")]),]

Week_4_Yahoo$full_name <- str_replace_all(Week_4_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_4_Yahoo$full_name <- str_replace_all(Week_4_Yahoo$full_name, "\\s+", " ")
Week_4_Yahoo$full_name <- trimws(Week_4_Yahoo$full_name)

##

full_yahoo <- rbind(Week_1_Yahoo, Week_2_Yahoo, Week_3_Yahoo, Week_4_Yahoo) %>% 
  select(full_name, FPPG, week)


####

for_flex_analysis <- My_model_flex %>% 
  left_join(Online_flex, by = c("player", "week")) %>% 
  filter(!is.na(Online_proj)) %>% 
  left_join(actual_flex, by = c("player" = "player_rec", "week" = "week_rec")) %>% 
  left_join(full_yahoo, by = c("player" = "full_name", "week"))

for_QB_analysis <- My_model_QB %>% 
  left_join(Online_QB, by = c("player", "week")) %>% 
  filter(!is.na(Online_proj)) %>% 
  left_join(actual_QB, by = c("player" = "player_pas", "week" = "week_pas")) %>% 
  left_join(full_yahoo, by = c("player" = "full_name", "week"))


write_csv(for_flex_analysis, "~/R Stuff/FantasyFootball/Analysis/week_4_for_flex.csv")
write_csv(for_QB_analysis, "~/R Stuff/FantasyFootball/Analysis/week_4_for_QB.csv")





