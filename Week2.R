library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")


Teams <- read_csv("~/R Stuff/FantasyFootball/teams.csv")
Teams[32, 1] = "Washington Commanders"

Week_1_Passing <- read_csv("~/R Stuff/FantasyFootball/Weeks/Week_1/Week_1_Passing.csv")
Week_1_Scrimmage <- read_csv("~/R Stuff/FantasyFootball/Weeks/Week_1/Week_1_Scrimmage.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Week_1_Defense <- read_csv("~/R Stuff/FantasyFootball/Weeks/Week_1/Week_1_Defense.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Week_1_Team_Offense <- read_csv("~/R Stuff/FantasyFootball/Weeks/Week_1/Week_1_Team_Offense.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 

Week_2_Yahoo <- read_csv("~/R Stuff/FantasyFootball/Yahoo/Week_2_Yahoo.csv") %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`))

Week_2_Yahoo <- Week_2_Yahoo[complete.cases(Week_2_Yahoo[,c("Last Name")]),]


Week_1_Passing$Player <- str_replace_all(Week_1_Passing$Player, "[^[:alnum:]]", " ")
Week_1_Passing$Player <- str_replace_all(Week_1_Passing$Player, "\\s+", " ")
Week_1_Passing$Player <- trimws(Week_1_Passing$Player)

Week_1_Scrimmage$player <- str_replace_all(Week_1_Scrimmage$player, "[^[:alnum:]]", " ")
Week_1_Scrimmage$player <- str_replace_all(Week_1_Scrimmage$player, "\\s+", " ")
Week_1_Scrimmage$player <- trimws(Week_1_Scrimmage$player)

Week_2_Yahoo$full_name <- str_replace_all(Week_2_Yahoo$full_name, "[^[:alnum:]]", " ")
Week_2_Yahoo$full_name <- str_replace_all(Week_2_Yahoo$full_name, "\\s+", " ")
Week_2_Yahoo$full_name <- trimws(Week_2_Yahoo$full_name)

Week_1_Passing[ , 5:31] <- apply(Week_1_Passing[ , 5:31], 2,            
                               function(x) as.numeric(as.character(x)))

Week_1_Scrimmage[ , 5:31] <- apply(Week_1_Scrimmage[ , 5:31], 2,            
                                 function(x) as.numeric(as.character(x)))

Week_1_Team_Offense[ , 3:28] <- apply(Week_1_Team_Offense[ , 3:28], 2,            
                                 function(x) as.numeric(as.character(x)))

Week_1_Defense[ , 3:28] <- apply(Week_1_Defense[ , 3:28], 2,            
                                 function(x) as.numeric(as.character(x)))


names(Week_1_Passing) <- paste0(names(Week_1_Passing), "_pas")
names(Week_1_Scrimmage) <- paste0(names(Week_1_Scrimmage), "_scrim")
names(Week_2_Yahoo) <- paste0(names(Week_2_Yahoo), "_yah")
names(Week_1_Defense) <- paste0(names(Week_1_Defense), "_def")
names(Week_1_Team_Offense) <- paste0(names(Week_1_Team_Offense), "_tm")


Overall <- Week_1_Passing %>% 
  full_join(Week_1_Scrimmage, by = c("Player_pas" = "player_scrim")) %>% 
  full_join(Week_2_Yahoo, by = c("Player_pas" = "full_name_yah")) %>% 
  full_join(Teams, by = c("Team_yah" = "Short_Name")) %>% 
  full_join(Teams, by = c("Opponent_yah" = "Short_Name"))

colnames(Overall)[75] <- "Teamx"
colnames(Overall)[76] <- "Opponentx"
colnames(Week_1_Defense)[2] <- "Opponentx"
colnames(Week_1_Team_Offense)[2] <- "Teamx"

Overall <- Overall %>% 
  full_join(Week_1_Defense, by = c("Opponentx")) %>%
  full_join(Week_1_Team_Offense, by = "Teamx")

colnames(Overall)[12] <- "yds_pas"
colnames(Overall)[2] <- "Player"

Overall <- Overall %>% 
  filter(!is.na(Player)) %>% 
  filter(!is.na(Team_yah))

Injury_Status <- Overall %>% 
  select(`Injury Status_yah`)

Overall <- Overall[ , -which(names(Overall) %in% c("Tm_pas", "rk_scrim", "tm_scrim", "age_scrim", "Injury Status_yah", "rk_def"))]
Overall <- Overall[ , -which(names(Overall) %in% c("rk_tm"))]
Overall[is.na(Overall)] <- 0


#### QB ####

QB_Stats_Week_1 <- Overall %>% 
  filter(Position_yah == "QB") %>% 
  mutate(per_rus_yds = yds_2_scrim / (yds_3_tm),
         per_rus_td = ifelse(td_2_tm == 0, 0, (td_2_scrim) / (td_2_tm))) %>% 
  mutate(def_ex_rus_yds = yds_3_def*per_rus_yds,
         def_ex_rus_td = td_2_def*per_rus_td) %>% 
  mutate(ovr_ex_pas_yds = (yds_pas + yds_2_def) / 2,
         ovr_ex_pas_td = (TD_pas + td_def) / 2,
         ovr_ex_rus_yds = (yds_2_scrim + def_ex_rus_yds) / 2,
         ovr_ex_rus_td = (td_2_scrim + def_ex_rus_td) / 2) %>% 
  mutate(expected_points = ovr_ex_pas_yds*0.04 + ovr_ex_rus_yds*0.1 + ovr_ex_pas_td*4 + ovr_ex_rus_td*6- 2*fmb_scrim - Int_pas)

QB_Expected_Points_Week_1 <- QB_Stats_Week_1 %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_pas_yds, ovr_ex_pas_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim, Int_pas) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


#### RB ####

RB_Stats_Week_1 <- Overall %>% 
  filter(Position_yah == "RB") %>% 
  mutate(per_rec_yds = (yds_scrim) / (yds_2_tm),
         per_rec = (rec_scrim) / (cmp_tm),
         per_rec_td = ifelse(td_tm == 0, 0, (td_scrim) / (td_tm)),
         per_rus_yds = yds_2_scrim / (yds_3_tm),
         per_rus_td = ifelse(td_2_tm == 0, 0, (td_2_scrim) / (td_2_tm))) %>% 
  mutate(
    pl_rec_game = (rec_scrim),
    pl_rec_yds_game = yds_scrim,
    pl_rec_td_game = td_scrim,
    pl_rus_yds_game = yds_2_scrim,
    pl_rus_td_game = td_2_scrim
  ) %>% 
  mutate(def_ex_cmp = cmp_def*per_rec,
         def_ex_rec_yds = yds_2_def*per_rec_yds,
         def_ex_rec_td = td_def*per_rec_td,
         def_ex_rus_yds = yds_3_def*per_rus_yds,
         def_ex_rus_td = td_2_def*per_rus_td) %>% 
    mutate(ovr_ex_rec = (pl_rec_game + def_ex_cmp) / 2,
           ovr_ex_rec_yds = (pl_rec_yds_game + def_ex_rec_yds) / 2,
           ovr_ex_rec_td = (pl_rec_td_game + def_ex_rec_td) / 2,
           ovr_ex_rus_yds = (pl_rus_yds_game + def_ex_rus_yds) / 2,
           ovr_ex_rus_td = (pl_rus_td_game + def_ex_rus_td) / 2) %>% 
    mutate(expected_points = ovr_ex_rec*0.5 + ovr_ex_rec_yds*0.1 + ovr_ex_rus_yds*0.1 + ovr_ex_rec_td*6 + ovr_ex_rus_td*6- 2*fmb_scrim)
  
RB_Expected_Points_Week_1 <- RB_Stats_Week_1 %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


#### Receiving ####

Receiving_Stats_Week_1 <- Overall %>% 
  filter(Position_yah == "WR" | Position_yah == "TE") %>% 
  mutate(per_rec_yds = (yds_scrim) / (yds_2_tm),
         per_rec = (rec_scrim) / (cmp_tm),
         per_rec_td = ifelse(td_tm == 0, 0, (td_scrim) / (td_tm))) %>% 
  mutate(
    pl_rec_game = (rec_scrim),
    pl_rec_yds_game = yds_scrim,
    pl_rec_td_game = td_scrim
  ) %>% 
  mutate(def_ex_cmp = cmp_def*per_rec,
         def_ex_rec_yds = yds_2_def*per_rec_yds,
         def_ex_rec_td = td_def*per_rec_td) %>% 
  mutate(ovr_ex_rec = (pl_rec_game + def_ex_cmp) / 2,
         ovr_ex_rec_yds = (pl_rec_yds_game + def_ex_rec_yds) / 2,
         ovr_ex_rec_td = (pl_rec_td_game + def_ex_rec_td) / 2) %>% 
  mutate(expected_points = ovr_ex_rec*0.5 + ovr_ex_rec_yds*0.1 + ovr_ex_rec_td*6 - 2*fmb_scrim)

Receiving_Expected_Points_Week_1 <- Receiving_Stats_Week_1 %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, fmb_scrim) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)




