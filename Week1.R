library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")



Week <- 1

Teams <- read_csv("~/R Stuff/FantasyFootball/teams.csv")
Schedule <- read_csv("~/R Stuff/FantasyFootball/schedule.csv") %>% 
  filter(Week == {{Week}}) %>% 
  select(VisTm, HomeTm)
Fantasy_2021 <- read_csv("~/R Stuff/FantasyFootball/2021_Stats/Fantasy_2021.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names %>% 
  filter(!is.na(fant_pos))
Team_Offense_2021 <- read_csv("~/R Stuff/FantasyFootball/2021_Stats/Team_Offense_2021.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Passing_Defense_2021 <- read_csv("~/R Stuff/FantasyFootball/2021_Stats/Passing_Defense_2021.csv") 
colnames(Passing_Defense_2021)[7] <- "yds"
Rushing_Defense_2021 <- read_csv("~/R Stuff/FantasyFootball/2021_Stats/Rushing_Defense_2021.csv") 
colnames(Rushing_Defense_2021)[5] <- "yds"


Passing_Defense_2021 <- Passing_Defense_2021 %>% 
  select(Tm, Cmp, yds, TD) %>% 
  mutate(cmp_game = Cmp/17,
         yds_game = yds/17,
         td_game = TD/17)

Rushing_Defense_2021 <- Rushing_Defense_2021 %>% 
  select(Tm, yds, TD) %>% 
  mutate(yds_game = yds/17,
         td_game = TD/17)


Week_1_Yahoo <- read_csv("~/R Stuff/FantasyFootball/Yahoo/Week_1_Yahoo.csv") %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`))

Week_1_Yahoo <- Week_1_Yahoo[complete.cases(Week_1_Yahoo[,c("Last Name")]),]


Fantasy_2021[ , 5:33] <- apply(Fantasy_2021[ , 5:33], 2,            
                               function(x) as.numeric(as.character(x)))
Team_Offense_2021[ , 3:28] <- apply(Team_Offense_2021[ , 3:28], 2,            
                               function(x) as.numeric(as.character(x)))


Fantasy_2021[is.na(Fantasy_2021)] <- 0


Fantasy_2021$player <- str_replace_all(Fantasy_2021$player, "[^[:alnum:]]", " ")
Fantasy_2021$player <- str_replace_all(Fantasy_2021$player, "\\s+", " ")
Fantasy_2021$player <- trimws(Fantasy_2021$player)
Week_1_Yahoo$full_name <- str_replace_all(Week_1_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_1_Yahoo$full_name <- str_replace_all(Week_1_Yahoo$full_name, "\\s+", " ")
Week_1_Yahoo$full_name <- trimws(Week_1_Yahoo$full_name)


Fantasy_2021 <- Fantasy_2021 %>% 
  full_join(Week_1_Yahoo, by = c("player" = "full_name"))

Fantasy_2021 <- Fantasy_2021[complete.cases(Fantasy_2021[,c("tm")]),]


QB_Stats_2021 <- Fantasy_2021 %>% 
  filter(fant_pos == "QB") %>% 
  select(player, Team, Opponent, fant_pos:int, yds_2, td_2, fl, x2pm, Salary) %>% 
  mutate(total_points = yds*0.04 + td*4 - int + yds_2*0.1 + td_2*6 - fl*2 + x2pm*2, 
         games = (3*g + gs)/4,
         ppg = total_points/games)

RB_Stats_2021 <- Fantasy_2021 %>% 
  filter(fant_pos == "RB") %>% 
  select(player, Team, Opponent, fant_pos:gs, att_2:fl, x2pm, Salary) %>% 
  mutate(total_points = yds_2*0.1 + td_2*6 + rec*0.5 + yds_3*0.1 + td_3*6 - fl*2 + x2pm*2, 
         games = (3*g + gs)/4,
         ppg = total_points/games)

Receiving_Stats_2021 <- Fantasy_2021 %>% 
  filter(fant_pos == "WR" | fant_pos == "TE") %>% 
  select(player, Team, Opponent, fant_pos:gs, att_2:fl, x2pm, td_4, Salary) %>% 
  mutate(total_points = yds_2*0.1 + td_2*6 + rec*0.5 + yds_3*0.1 + td_3*6 - fl*2 + x2pm*2, 
         games = (3*g + gs)/4,
         ppg = total_points/games)

#### Receiving ####

Team_Receiving_Stats_2021 <- Team_Offense_2021 %>% 
  select(tm, cmp:td) %>% 
  mutate(fantasy_points = cmp*0.5 + yds_2*0.1 + td*6, 
         fppg = fantasy_points/17)

Receiving_Stats_2021 <- Receiving_Stats_2021 %>%
  full_join(Teams, by = c("Team" = "Short_Name"))

colnames(Receiving_Stats_2021)[ncol(Receiving_Stats_2021)] <- "Long_Name"

Receiving_Stats_2021 <-  full_join(Receiving_Stats_2021, Team_Receiving_Stats_2021, by = c("Long_Name" = "tm")) %>% 
  mutate(per_rec_yds = (yds_3/games) / (yds_2.y/17),
         per_rec = (rec/games) / (cmp/17),
         per_td = (td_3/games) / (td/17)) %>% 
  mutate(
    pl_rec_game = (rec/games),
    pl_yds_game = yds_3/games,
    pl_td_game = td_3/games
  )

Receiving_Stats_2021 <- Receiving_Stats_2021 %>%
  full_join(Teams, by = c("Opponent" = "Short_Name"))

Receiving_Stats_2021 <- full_join(Receiving_Stats_2021, Passing_Defense_2021, by = c("Long_Name.y" = "Tm")) %>% 
  mutate(def_ex_cmp = cmp_game*per_rec,
         def_ex_yds = yds_game*per_rec_yds,
         def_ex_td = td_game*per_td) %>% 
  mutate(ovr_ex_cmp = (pl_rec_game + def_ex_cmp) / 2,
         ovr_ex_yds = (pl_yds_game + def_ex_yds) / 2,
         ovr_ex_td = (pl_td_game + def_ex_td) / 2) %>% 
  mutate(expected_points = ovr_ex_cmp*0.5 + ovr_ex_yds*0.1 + ovr_ex_td*6 - 2*fl/games)

Receiving_Expected_Points_Prior <- Receiving_Stats_2021 %>% 
  select(player, fant_pos, Long_Name.x, Opponent, expected_points, Salary) %>% 
  mutate(ex_ppd = expected_points/Salary) %>% 
  filter(Salary > 10)

#### Rushing ####


Team_Rushing_Stats_2021 <- Team_Offense_2021 %>% 
  select(tm, att_2:y_a) %>% 
  mutate(fantasy_points = yds_3*0.1 + td_2*6, 
         fppg = fantasy_points/17)

RB_Stats_2021 <- RB_Stats_2021 %>%
  full_join(Teams, by = c("Team" = "Short_Name"))

colnames(RB_Stats_2021)[ncol(RB_Stats_2021)] <- "Long_Name"

RB_Stats_2021 <-  full_join(RB_Stats_2021, Team_Rushing_Stats_2021, by = c("Long_Name" = "tm")) %>%
  full_join(Team_Receiving_Stats_2021, by = c("Long_Name" = "tm")) %>% 
  mutate(per_rec_yds = (yds_3.x/games) / (yds_2.y/17),
         per_rec = (rec/games) / (cmp/17),
         per_rec_td = (td_3/games) / (td/17),
         per_rus_yds = (yds_2.x/games) / (yds_3.y/17),
         per_rus_td = (td_2.x/games) / (td_2.y/17)) %>% 
  mutate(
    pl_rec_game = (rec/games),
    pl_rec_yds_game = yds_3.x/games,
    pl_rec_td_game = td_3/games,
    pl_rus_yds_game = yds_2.x/games,
    pl_rus_td_game = td_2.x/games
  )

RB_Stats_2021 <- RB_Stats_2021 %>%
  full_join(Teams, by = c("Opponent" = "Short_Name"))

RB_Stats_2021 <- full_join(RB_Stats_2021, Rushing_Defense_2021, by = c("Long_Name.y" = "Tm")) %>%
  full_join(Passing_Defense_2021, by = c("Long_Name.y" = "Tm")) %>%
  mutate(def_ex_cmp = cmp_game*per_rec,
         def_ex_rec_yds = yds_game.y*per_rec_yds,
         def_ex_rec_td = td_game.y*per_rec_td,
         def_ex_rus_yds = yds_game.x*per_rus_yds,
         def_ex_rus_td = td_game.x*per_rus_td) %>% 
  mutate(ovr_ex_cmp = (pl_rec_game + def_ex_cmp) / 2,
         ovr_ex_rec_yds = (pl_rec_yds_game + def_ex_rec_yds) / 2,
         ovr_ex_rec_td = (pl_rec_td_game + def_ex_rec_td) / 2,
         ovr_ex_rus_yds = (pl_rus_yds_game + def_ex_rus_yds) / 2,
         ovr_ex_rus_td = (pl_rus_td_game + def_ex_rus_td) / 2) %>% 
  mutate(expected_points = ovr_ex_cmp*0.5 + ovr_ex_rec_yds*0.1 + ovr_ex_rus_yds*0.1 + ovr_ex_rec_td*6 + ovr_ex_rus_td*6- 2*fl/games)

Rushing_Expected_Points_Prior <- RB_Stats_2021 %>% 
  select(player, fant_pos, Long_Name.x, Opponent, expected_points, Salary) %>% 
  mutate(ex_ppd = expected_points/Salary) %>% 
  filter(Salary > 10)

#### QB ####

QB_Stats_2021 <- QB_Stats_2021 %>%
  full_join(Teams, by = c("Team" = "Short_Name"))

QB_Stats_2021 <- QB_Stats_2021 %>%
  full_join(Teams, by = c("Opponent" = "Short_Name"))

QB_Stats_2021 <-  full_join(QB_Stats_2021, Team_Rushing_Stats_2021, by = c("Long_Name.x" = "tm")) %>%
  mutate(per_rus_yds = (yds_2/games) / (yds_3/17),
         per_rus_td = (td_2.x/games) / (td_2.y/17)) %>% 
  mutate(pl_pas_yds_game = yds/games,
    pl_pas_td_game = td/games,
    pl_int_game = int/games,
    pl_rus_yds_game = yds_2/games,
    pl_rus_td_game = td_2.x/games
  )

QB_Stats_2021 <- QB_Stats_2021 %>% 
  full_join(Passing_Defense_2021, by = c("Long_Name.y" = "Tm")) %>% 
  full_join(Rushing_Defense_2021, by = c("Long_Name.y" = "Tm")) %>% 
  mutate(def_ex_rus_yds = yds_game.y*per_rus_yds,
         def_ex_rus_td = td_game.y*per_rus_td) %>% 
  mutate(ovr_ex_pas_yds = (pl_pas_yds_game + yds_game.x) / 2,
         ovr_ex_pas_td = (pl_pas_td_game + td_game.x) / 2,
         ovr_ex_rus_yds = (pl_rus_yds_game + def_ex_rus_yds) / 2,
         ovr_ex_rus_td = (pl_rus_td_game + def_ex_rus_td) / 2) %>% 
  mutate(expected_points = ovr_ex_pas_yds*0.04 + ovr_ex_rus_yds*0.1 + ovr_ex_pas_td*4 + ovr_ex_rus_td*6- 2*fl/games - pl_int_game)

QB_Expected_Points_Prior <- QB_Stats_2021 %>% 
  select(player, fant_pos, Long_Name.x, Opponent, expected_points, Salary) %>% 
  mutate(ex_ppd = expected_points/Salary) %>% 
  filter(Salary > 10)


#### Flex ####

Flex_Expected_Points_Prior = rbind(Receiving_Expected_Points_Prior, Rushing_Expected_Points_Prior)

write_csv(QB_Expected_Points_Prior, "~/R Stuff/FantasyFootball/Past_Results/My_Model_Week_1_QB.csv")
write_csv(Flex_Expected_Points_Prior, "~/R Stuff/FantasyFootball/Past_Results/My_Model_Week_1_Flex.csv")
  
