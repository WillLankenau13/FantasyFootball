library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

Prior_Weight <- 0.85
Weeks_Weight <- 0.15

####

QB_Expected_Points_Prior <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/QB_Expected_Points_Prior.csv")
RB_Expected_Points_Prior <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/RB_Expected_Points_Prior.csv")
Receiving_Expected_Points_Prior <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/Receiving_Expected_Points_Prior.csv")

QB_Expected_Points <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/QB_Expected_Points.csv")
RB_Expected_Points <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/RB_Expected_Points.csv")
Receiving_Expected_Points <- read_csv("~/R Stuff/FantasyFootball/R_Interim_Data/Receiving_Expected_Points.csv")

QB_Expected_Points_Prior[is.na(QB_Expected_Points_Prior)] <- 0




#### QB ####

Combined_QB <- full_join(QB_Expected_Points_Prior, QB_Expected_Points, by = c("player" = "Player")) 

Combined_QB[, 5:13][is.na(Combined_QB[, 5:13])] <- 0

Combined_QB <- Combined_QB %>% 
  mutate(ex_pas_yds = ovr_ex_pas_yds.x*Prior_Weight + ovr_ex_pas_yds.y*Weeks_Weight,
         ex_pas_td = ovr_ex_pas_td.x*Prior_Weight + ovr_ex_pas_td.y*Weeks_Weight,
         ex_rus_yds = ovr_ex_rus_yds.x*Prior_Weight + ovr_ex_rus_yds.y*Weeks_Weight, 
         ex_rus_td = ovr_ex_rus_td.x*Prior_Weight + ovr_ex_rus_td.y*Weeks_Weight, 
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*Weeks_Weight,
         ex_int = pl_int_game*Prior_Weight + Int_pas*Weeks_Weight) %>% 
  mutate(expected_points = ex_pas_yds*0.04 + ex_rus_yds*0.1 + ex_pas_td*4 + ex_rus_td*6- 2*ex_fmb - ex_int)
    
Simple_QB <- Combined_QB %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)

#### RB ####

Combined_RB <- full_join(RB_Expected_Points_Prior, RB_Expected_Points, by = c("player" = "Player"))
  
Combined_RB[, 5:13][is.na(Combined_RB[, 5:13])] <- 0

Combined_RB <- Combined_RB %>% 
  mutate(ex_rus_yds = ovr_ex_rus_yds.x*Prior_Weight + ovr_ex_rus_yds.y*Weeks_Weight, 
         ex_rus_td = ovr_ex_rus_td.x*Prior_Weight + ovr_ex_rus_td.y*Weeks_Weight, 
         ex_rec = ovr_ex_rec.x*Prior_Weight + ovr_ex_rec.y*Weeks_Weight,
         ex_rec_yds = ovr_ex_rec_yds.x*Prior_Weight + ovr_ex_rec_yds.y*Weeks_Weight,
         ex_rec_td = ovr_ex_rec_td.x*Prior_Weight + ovr_ex_rec_td.y*Weeks_Weight,
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*Weeks_Weight) %>% 
  mutate(expected_points = ex_rec*0.5 + ex_rec_yds*0.1 + ex_rus_yds*0.1 + ex_rec_td*6 + ex_rus_td*6 - 2*ex_fmb)

Simple_RB <- Combined_RB %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)

#### Receiving ####

Combined_Receiving <- full_join(Receiving_Expected_Points_Prior, Receiving_Expected_Points, by = c("player" = "Player")) 

Combined_Receiving[, 5:11][is.na(Combined_Receiving[, 5:11])] <- 0

Combined_Receiving <- Combined_Receiving %>% 
  mutate(ex_rec = ovr_ex_rec.x*Prior_Weight + ovr_ex_rec.y*Weeks_Weight,
         ex_rec_yds = ovr_ex_rec_yds.x*Prior_Weight + ovr_ex_rec_yds.y*Weeks_Weight,
         ex_rec_td = ovr_ex_rec_td.x*Prior_Weight + ovr_ex_rec_td.y*Weeks_Weight,
         ex_fmb = (fl/17)*Prior_Weight + fmb_scrim*Weeks_Weight) %>% 
  mutate(expected_points = ex_rec*0.5 + ex_rec_yds*0.1 +  ex_rec_td*6 - 2*ex_fmb)

Simple_Receiving <- Combined_Receiving %>% 
  select(player, Position_yah, Opponent_yah, expected_points, Salary_yah) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


#### Flex ####

Simple_Flex = rbind(Simple_RB, Simple_Receiving)



write_csv(Simple_QB, "~/R Stuff/FantasyFootball/R_Interim_Data/Simple_QB.csv")
write_csv(Simple_Flex, "~/R Stuff/FantasyFootball/R_Interim_Data/Simple_Flex.csv")




