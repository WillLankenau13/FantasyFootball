library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")


Week <- 18

##

Year <- "2022_23"

##########################

Prior_Weight <- 0.1
This_Years_Weight <- 0.9

QB_Home_W <- 0.035
RB_Home_W <- 0.049
WR_Home_W <- 0
TE_Home_W <- 0.0435

QB_Injury_W <- 0
Flex_Injury_W <- 0.1

Games_Played_Weight_1 <- 0.8
Games_Played_Weight_2 <- 0.9

#####################

Char_Week <- as.character(Week)

QB_Expected_Points_Prior <- read_csv((eval(paste("~/R Stuff/FantasyFootball/Prior/", Year, "/Week_", Char_Week, "/Week_", Char_Week, "_QB_Prior.csv", sep = ""))))
RB_Expected_Points_Prior <- read_csv((eval(paste("~/R Stuff/FantasyFootball/Prior/", Year, "/Week_", Char_Week, "/Week_", Char_Week, "_RB_Prior.csv", sep = ""))))
Receiving_Expected_Points_Prior <- read_csv((eval(paste("~/R Stuff/FantasyFootball/Prior/", Year, "/Week_", Char_Week, "/Week_", Char_Week, "_Rec_Prior.csv", sep = ""))))

QB_Expected_Points <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Middle/Week_", Char_Week, "/Week_", Char_Week, "_QB_Input.csv", sep = "")))
RB_Expected_Points <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Middle/Week_", Char_Week, "/Week_", Char_Week, "_RB_Input.csv", sep = "")))
Receiving_Expected_Points <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Middle/Week_", Char_Week, "/Week_", Char_Week, "_Rec_Input.csv", sep = "")))

Injury_Status <- read_csv((eval(paste("~/R Stuff/FantasyFootball/Injury_Status/Injury_Status_Week_", Char_Week, ".csv", sep = ""))))

QB_Expected_Points_Prior[is.na(QB_Expected_Points_Prior)] <- 0




#### QB ####

Combined_QB <- full_join(QB_Expected_Points_Prior, QB_Expected_Points, by = c("player" = "Player")) 

Combined_QB[, 5:13][is.na(Combined_QB[, 5:13])] <- 0

Combined_QB <- Combined_QB %>% 
  mutate(ex_pas_yds = ovr_ex_pas_yds.x*Prior_Weight + ovr_ex_pas_yds.y*This_Years_Weight,
         ex_pas_td = ovr_ex_pas_td.x*Prior_Weight + ovr_ex_pas_td.y*This_Years_Weight,
         ex_rus_yds = ovr_ex_rus_yds.x*Prior_Weight + ovr_ex_rus_yds.y*This_Years_Weight, 
         ex_rus_td = ovr_ex_rus_td.x*Prior_Weight + ovr_ex_rus_td.y*This_Years_Weight, 
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*This_Years_Weight,
         ex_int = pl_int_game*Prior_Weight + Int_pas*This_Years_Weight) %>% 
  mutate(expected_points = ex_pas_yds*0.04 + ex_rus_yds*0.1 + ex_pas_td*4 + ex_rus_td*6- 2*ex_fmb - ex_int)

Simple_QB <- Combined_QB %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games) %>% 
  filter(Salary_yah > 10)

#### RB ####

Combined_RB <- full_join(RB_Expected_Points_Prior, RB_Expected_Points, by = c("player" = "Player"))

Combined_RB[, 5:13][is.na(Combined_RB[, 5:13])] <- 0

Combined_RB <- Combined_RB %>% 
  mutate(ex_rus_yds = ovr_ex_rus_yds.x*Prior_Weight + ovr_ex_rus_yds.y*This_Years_Weight, 
         ex_rus_td = ovr_ex_rus_td.x*Prior_Weight + ovr_ex_rus_td.y*This_Years_Weight, 
         ex_rec = ovr_ex_rec.x*Prior_Weight + ovr_ex_rec.y*This_Years_Weight,
         ex_rec_yds = ovr_ex_rec_yds.x*Prior_Weight + ovr_ex_rec_yds.y*This_Years_Weight,
         ex_rec_td = ovr_ex_rec_td.x*Prior_Weight + ovr_ex_rec_td.y*This_Years_Weight,
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*This_Years_Weight) %>% 
  mutate(expected_points = ex_rec*0.5 + ex_rec_yds*0.1 + ex_rus_yds*0.1 + ex_rec_td*6 + ex_rus_td*6 - 2*ex_fmb)

Simple_RB <- Combined_RB %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games) %>% 
  filter(Salary_yah > 10)

#### Receiving ####

Combined_Receiving <- full_join(Receiving_Expected_Points_Prior, Receiving_Expected_Points, by = c("player" = "Player")) 

Combined_Receiving[, 5:11][is.na(Combined_Receiving[, 5:11])] <- 0

Combined_Receiving <- Combined_Receiving %>% 
  mutate(ex_rec = ovr_ex_rec.x*Prior_Weight + ovr_ex_rec.y*This_Years_Weight,
         ex_rec_yds = ovr_ex_rec_yds.x*Prior_Weight + ovr_ex_rec_yds.y*This_Years_Weight,
         ex_rec_td = ovr_ex_rec_td.x*Prior_Weight + ovr_ex_rec_td.y*This_Years_Weight,
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*This_Years_Weight) %>% 
  mutate(expected_points = ex_rec*0.5 + ex_rec_yds*0.1 +  ex_rec_td*6 - 2*ex_fmb)

Simple_Receiving <- Combined_Receiving %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games) %>% 
  filter(Salary_yah > 10)


#### Flex ####

Simple_Flex = rbind(Simple_RB, Simple_Receiving)


#### Home_Away ####

Simple_QB <- Simple_QB %>% 
  mutate(expected_points = ifelse(home_away == "Home", expected_points*(1+QB_Home_W), expected_points*(1-QB_Home_W)))%>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games)

Simple_Flex <- Simple_Flex %>% 
  mutate(Home_W = ifelse(Position_yah == "RB", RB_Home_W, ifelse(Position_yah == "WR", WR_Home_W, TE_Home_W))) %>% 
  mutate(expected_points = ifelse(home_away == "Home", 
                                      expected_points*(1+Home_W), 
                                      expected_points*(1-Home_W))) %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games)


### Injuries ###

Simple_QB <- Simple_QB %>% 
  full_join(Injury_Status, by = c("player" = "Player")) %>% 
  mutate(expected_points = ifelse(is.na(Status), expected_points, ifelse(Status == "Q", expected_points*(1-QB_Injury_W), expected_points)))%>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games)

Simple_Flex <- Simple_Flex %>% 
  full_join(Injury_Status, by = c("player" = "Player")) %>% 
  mutate(expected_points = ifelse(is.na(Status), expected_points, ifelse(Status == "Q", expected_points*(1-Flex_Injury_W), expected_points))) %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah, home_away, games)


### Games Played Weight ###
  
Simple_QB <- Simple_QB %>% 
  mutate(expected_points = ifelse(games <= 1, expected_points*Games_Played_Weight_1, 
                                  ifelse(games <= 2, expected_points*Games_Played_Weight_2, expected_points))) %>% 
  filter(games != Week)

Simple_Flex <- Simple_Flex %>% 
  mutate(expected_points = ifelse(games <= 1, expected_points*Games_Played_Weight_1, 
                                  ifelse(games <= 2, expected_points*Games_Played_Weight_2, expected_points))) %>% 
  filter(games != Week)

#### Expected Points Per Dollar ####

Simple_QB <- Simple_QB %>% 
  mutate(ex_ppd = expected_points/Salary_yah)

Simple_Flex <- Simple_Flex %>% 
  mutate(ex_ppd = expected_points/Salary_yah)



####

write_csv(Simple_QB, eval(paste("~/R Stuff/FantasyFootball/Model_Results/Week_", Char_Week, "/My_Model_Week_", Char_Week ,"_QB.csv", sep = "")))
write_csv(Simple_Flex, eval(paste("~/R Stuff/FantasyFootball/Model_Results/Week_", Char_Week, "/My_Model_Week_", Char_Week ,"_Flex.csv", sep = "")))


