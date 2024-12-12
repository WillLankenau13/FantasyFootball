library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")



Week <- 18

###########################

no_dst <- list("SF")
no_yahoo_cup <- list("KC", "LV", "TEN", "JAC", "GB", "DET")
backup_qb <- list("BAL", "ATL", "LAR", "PHI", "ARI")
other_players <- list("Leonard Fournette")

Char_Week <- as.character(Week)  

QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week ,"_QB.csv", sep = ""))) %>% 
  filter(Start_Sit != "F") 
Flex <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week ,"_Flex.csv", sep = ""))) %>% 
  filter(Start_Sit != "F")
DST <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week ,"_DST.csv", sep = "")))

players <- rbind(QB, Flex)

QB <- QB  %>% 
  filter(!(TEAM %in% no_yahoo_cup)) %>% 
  filter(!(TEAM %in% backup_qb)) %>% 
  filter(Opponent_yah != "DEN") %>% 
  filter(!(player %in% other_players))

RB <- Flex %>% 
  filter(Position_yah == "RB")  %>% 
  filter(!(TEAM %in% no_yahoo_cup)) %>% 
  filter(!(TEAM %in% backup_qb)) %>% 
  filter(Opponent_yah != "DEN") %>% 
  filter(!(player %in% other_players))

WR <- Flex %>% 
  filter(Position_yah == "WR")  %>% 
  filter(!(TEAM %in% no_yahoo_cup)) %>% 
  filter(!(TEAM %in% backup_qb)) %>% 
  filter(Opponent_yah != "DEN") %>% 
  filter(!(player %in% other_players))

TE <- Flex %>% 
  filter(Position_yah == "TE") %>% 
  filter(!(TEAM %in% no_yahoo_cup)) %>% 
  filter(!(TEAM %in% backup_qb)) %>% 
  filter(Opponent_yah != "DEN") %>% 
  filter(!(player %in% other_players))

DST <- DST %>% 
  filter(!(TEAM %in% no_yahoo_cup)) %>% 
  filter(!(TEAM %in% no_dst)) %>% 
  filter(OPP != "DEN")


### QB ###

QB_names <- array(QB$player)
QB_points <- array(QB$combined_projection)
QB_cost <- array(QB$Salary_yah)

QB1 <- data.frame(QB_names, QB_points) %>% 
  pivot_wider(names_from = QB_names, values_from = QB_points)

QB2 <- data.frame(QB_names, QB_cost) %>% 
  pivot_wider(names_from = QB_names, values_from = QB_cost)

QB_for_opt <- rbind(QB1, QB2)

### RB ###

RB_names <- array(RB$player)
RB_points <- array(RB$combined_projection)
RB_cost <- array(RB$Salary_yah)

RB1 <- data.frame(RB_names, RB_points) %>% 
  pivot_wider(names_from = RB_names, values_from = RB_points)

RB2 <- data.frame(RB_names, RB_cost) %>% 
  pivot_wider(names_from = RB_names, values_from = RB_cost)

RB_for_opt <- rbind(RB1, RB2)

### WR ###

WR_names <- array(WR$player)
WR_points <- array(WR$combined_projection)
WR_cost <- array(WR$Salary_yah)

WR1 <- data.frame(WR_names, WR_points) %>% 
  pivot_wider(names_from = WR_names, values_from = WR_points)

WR2 <- data.frame(WR_names, WR_cost) %>% 
  pivot_wider(names_from = WR_names, values_from = WR_cost)

WR_for_opt <- rbind(WR1, WR2)

### TE ###

TE_names <- array(TE$player)
TE_points <- array(TE$combined_projection)
TE_cost <- array(TE$Salary_yah)

TE1 <- data.frame(TE_names, TE_points) %>% 
  pivot_wider(names_from = TE_names, values_from = TE_points)

TE2 <- data.frame(TE_names, TE_cost) %>% 
  pivot_wider(names_from = TE_names, values_from = TE_cost)

TE_for_opt <- rbind(TE1, TE2)


### DST ###
DST_names <- array(DST$defense)
DST_points <- array(DST$proj_points)
DST_cost <- array(DST$Salary_yah)

DST1 <- data.frame(DST_names, DST_points) %>% 
  pivot_wider(names_from = DST_names, values_from = DST_points)

DST2 <- data.frame(DST_names, DST_cost) %>% 
  pivot_wider(names_from = DST_names, values_from = DST_cost)

DST_for_opt <- rbind(DST1, DST2)


write_csv(QB_for_opt, "~/R Stuff/FantasyFootball/Optimization/QB.csv")
write_csv(RB_for_opt, "~/R Stuff/FantasyFootball/Optimization/RB.csv")
write_csv(WR_for_opt, "~/R Stuff/FantasyFootball/Optimization/WR.csv")
write_csv(TE_for_opt, "~/R Stuff/FantasyFootball/Optimization/TE.csv")
write_csv(DST_for_opt, "~/R Stuff/FantasyFootball/Optimization/DST.csv")


