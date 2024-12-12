library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")


Passing <- read_csv("~/R Stuff/FantasyFootball/Weeks/Weeks_1_2/Passing.csv")
Scrimmage <- read_csv("~/R Stuff/FantasyFootball/Weeks/Weeks_1_2/Scrimmage.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Defense <- read_csv("~/R Stuff/FantasyFootball/Weeks/Weeks_1_2/Defense.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Team_Offense <- read_csv("~/R Stuff/FantasyFootball/Weeks/Weeks_1_2/Team_Offense.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 

Yahoo <- read_csv("~/R Stuff/FantasyFootball/Yahoo/Week_3_Yahoo.csv") %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`))

####

Teams <- read_csv("~/R Stuff/FantasyFootball/teams.csv")
Teams[32, 1] = "Washington Commanders"

Yahoo <- Yahoo[complete.cases(Yahoo[,c("Last Name")]),]


Passing$Player <- str_replace_all(Passing$Player, "[^[:alnum:]]", " ")
Passing$Player <- str_replace_all(Passing$Player, "\\s+", " ")
Passing$Player <- trimws(Passing$Player)

Scrimmage$player <- str_replace_all(Scrimmage$player, "[^[:alnum:]]", " ")
Scrimmage$player <- str_replace_all(Scrimmage$player, "\\s+", " ")
Scrimmage$player <- trimws(Scrimmage$player)

Yahoo$full_name <- str_replace_all(Yahoo$full_name, "[^[:alnum:]]", " ")
Yahoo$full_name <- str_replace_all(Yahoo$full_name, "\\s+", " ")
Yahoo$full_name <- trimws(Yahoo$full_name)

Passing[ , 5:31] <- apply(Passing[ , 5:31], 2,            
                                 function(x) as.numeric(as.character(x)))

Scrimmage[ , 5:31] <- apply(Scrimmage[ , 5:31], 2,            
                                   function(x) as.numeric(as.character(x)))

Team_Offense[ , 3:28] <- apply(Team_Offense[ , 3:28], 2,            
                                      function(x) as.numeric(as.character(x)))

Defense[ , 3:28] <- apply(Defense[ , 3:28], 2,            
                                 function(x) as.numeric(as.character(x)))


names(Passing) <- paste0(names(Passing), "_pas")
names(Scrimmage) <- paste0(names(Scrimmage), "_scrim")
names(Yahoo) <- paste0(names(Yahoo), "_yah")
names(Defense) <- paste0(names(Defense), "_def")
names(Team_Offense) <- paste0(names(Team_Offense), "_tm")


Overall <- Passing %>% 
  full_join(Scrimmage, by = c("Player_pas" = "player_scrim")) %>% 
  full_join(Yahoo, by = c("Player_pas" = "full_name_yah")) %>% 
  full_join(Teams, by = c("Team_yah" = "Short_Name")) %>% 
  full_join(Teams, by = c("Opponent_yah" = "Short_Name"))

colnames(Overall)[75] <- "Teamx"
colnames(Overall)[76] <- "Opponentx"
colnames(Defense)[2] <- "Opponentx"
colnames(Team_Offense)[2] <- "Teamx"

Overall <- Overall %>% 
  full_join(Defense, by = c("Opponentx")) %>%
  full_join(Team_Offense, by = "Teamx")

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

QB_Stats <- Overall %>% 
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

QB_Expected_Points <- QB_Stats %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_pas_yds, ovr_ex_pas_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim, Int_pas) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


#### RB ####

RB_Stats <- Overall %>% 
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

RB_Expected_Points <- RB_Stats %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


#### Receiving ####

Receiving_Stats <- Overall %>% 
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

Receiving_Expected_Points <- Receiving_Stats %>% 
  select(Player, Position_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, fmb_scrim) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10)


write_csv(QB_Expected_Points, "~/R Stuff/FantasyFootball/R_Interim_Data/QB_Expected_Points.csv")
write_csv(RB_Expected_Points, "~/R Stuff/FantasyFootball/R_Interim_Data/RB_Expected_Points.csv")
write_csv(Receiving_Expected_Points, "~/R Stuff/FantasyFootball/R_Interim_Data/Receiving_Expected_Points.csv")



