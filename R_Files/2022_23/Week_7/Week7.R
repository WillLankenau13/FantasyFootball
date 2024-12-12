library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")


Week <- 7

################################

Char_Week <- as.character(Week)
Char_Week_m_1 <- as.character(Week - 1)

Passing <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Weekly_Stats/Weeks_1_", Char_Week_m_1 ,"/Passing.csv", sep = "")))
Scrimmage <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Weekly_Stats/Weeks_1_", Char_Week_m_1 ,"/Scrimmage.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Defense <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Weekly_Stats/Weeks_1_", Char_Week_m_1 ,"/Defense.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 
Team_Offense <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Weekly_Stats/Weeks_1_", Char_Week_m_1 ,"/Team_Offense.csv", sep = ""))) %>% 
  row_to_names(row_number = 1) %>% 
  clean_names 

Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Yahoo/Week_", Char_Week, "_Yahoo.csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`))

###

Home_Away <- Yahoo %>% 
  select(Game) %>% 
  filter(!is.na(Game))

Home_Away <- Home_Away[!duplicated(Home_Away), ]

Home_Away <- Home_Away %>% separate(Game, c('Away', 'Home'))

Manual_names <- read_csv("~/R Stuff/FantasyFootball/manual_player_names.csv") 

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
  full_join(Manual_names, by = c("Player_pas" = "pro_football_name")) %>% 
  mutate(Player_pas = ifelse(is.na(yahoo_name), Player_pas, yahoo_name)) %>% 
  full_join(Yahoo, by = c("Player_pas" = "full_name_yah")) %>% 
  full_join(Teams, by = c("Team_yah" = "Short_Name")) %>% 
  full_join(Teams, by = c("Opponent_yah" = "Short_Name"))

na_players <- Overall %>%
  select(Player_pas, Cmp_pas, yds_scrim, yds_2_scrim, 'First Name_yah', 'Last Name_yah', Position_yah, Long_Name.x) %>%
  mutate(combined = (coalesce(Cmp_pas, yds_scrim, yds_2_scrim)),
         full_name_yah = paste(`First Name_yah`, `Last Name_yah`))

na_players$full_name_yah <- str_replace_all(na_players$full_name_yah, "[^[:alnum:]]", " ")
na_players$full_name_yah <- str_replace_all(na_players$full_name_yah, "\\s+", " ")
na_players$full_name_yah <- trimws(na_players$full_name_yah)

na_players <- na_players %>%
  mutate(matched = ifelse((Player_pas == full_name_yah), "yes", "no")) %>%
  filter(matched == "no")

Overall <- Overall[ , -which(names(Overall) %in% c("yahoo_name"))]

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
  select(Player, `Injury Status_yah`)

colnames(Injury_Status)[2] <- "Status"

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
  mutate(expected_points = ovr_ex_pas_yds*0.04 + ovr_ex_rus_yds*0.1 + ovr_ex_pas_td*4 + ovr_ex_rus_td*6- 2*fmb_scrim - Int_pas,
         games = (3*G_pas + GS_pas)/4,)

QB_Expected_Points <- QB_Stats %>% 
  select(Player, Position_yah, Team_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_pas_yds, ovr_ex_pas_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim, Int_pas, games) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10) %>% 
  mutate(home_away = ifelse(Team_yah %in% Home_Away$Home, "Home", "Away"))


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
  mutate(expected_points = ovr_ex_rec*0.5 + ovr_ex_rec_yds*0.1 + ovr_ex_rus_yds*0.1 + ovr_ex_rec_td*6 + ovr_ex_rus_td*6- 2*fmb_scrim,
         games = (3*g_scrim + gs_scrim)/4,)

RB_Expected_Points <- RB_Stats %>% 
  select(Player, Position_yah, Team_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, ovr_ex_rus_yds, ovr_ex_rus_td, fmb_scrim, games) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10) %>% 
  mutate(home_away = ifelse(Team_yah %in% Home_Away$Home, "Home", "Away"))


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
  mutate(expected_points = ovr_ex_rec*0.5 + ovr_ex_rec_yds*0.1 + ovr_ex_rec_td*6 - 2*fmb_scrim,
         games = (3*g_scrim + gs_scrim)/4,)

Receiving_Expected_Points <- Receiving_Stats %>% 
  select(Player, Position_yah, Team_yah, Opponent_yah, expected_points, Salary_yah, ovr_ex_rec, ovr_ex_rec_yds, ovr_ex_rec_td, fmb_scrim, games) %>% 
  mutate(ex_ppd = expected_points/Salary_yah) %>% 
  filter(Salary_yah > 10) %>% 
  mutate(home_away = ifelse(Team_yah %in% Home_Away$Home, "Home", "Away"))


write_csv(QB_Expected_Points, (eval(paste("~/R Stuff/FantasyFootball/Input/Week_", Char_Week, "/Week_", Char_Week, "_QB_Input.csv", sep = ""))))
write_csv(RB_Expected_Points, (eval(paste("~/R Stuff/FantasyFootball/Input/Week_", Char_Week, "/Week_", Char_Week, "_RB_Input.csv", sep = ""))))
write_csv(Receiving_Expected_Points, (eval(paste("~/R Stuff/FantasyFootball/Input/Week_", Char_Week, "/Week_", Char_Week, "_Rec_Input.csv", sep = ""))))
write_csv(na_players, "~/R Stuff/FantasyFootball/na_players.csv")
write_csv(Injury_Status, (eval(paste("~/R Stuff/FantasyFootball/Injury_Status/Injury_Status_Week_", Char_Week, ".csv", sep = ""))))
