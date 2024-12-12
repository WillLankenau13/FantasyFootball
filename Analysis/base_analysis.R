library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library(rlang)

library(modelr)
options(na.action = na.warn)

Week_start <- 11
Week_end <- 6

############################

Week <- Week_start

Char_Week <- as.character(Week)
Char_Week_m_1 <- as.character(Week - 1)

actual_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/week_", Char_Week, "_actual_complete.csv", sep = "")))
model_flex <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week, "_flex.csv", sep = ""))) %>% 
  mutate(week = Week)
model_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week, "_QB.csv", sep = ""))) %>% 
  mutate(week = Week)

Week <- Week - 1

while(Week >= Week_end){
  
  Char_Week <- as.character(Week)
  Char_Week_m_1 <- as.character(Week - 1)
  
  t_actual_stats <- read_csv(eval(paste("~/R Stuff/FantasyFootball/2022_Stats/Week_", Char_Week, "/week_", Char_Week, "_actual_complete.csv", sep = "")))
  t_model_flex <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week, "_flex.csv", sep = ""))) %>% 
    mutate(week = Week)
  t_model_QB <- read_csv(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, "/Combined_Model_Online_Week_", Char_Week, "_QB.csv", sep = ""))) %>% 
    mutate(week = Week)
  
  actual_stats <- rbind(actual_stats, t_actual_stats)
  model_flex <- rbind(model_flex, t_model_flex)
  model_QB <- rbind(model_QB, t_model_QB)
  
  Week <- Week - 1
}

####


actual_flex <- actual_stats %>% 
  filter(pos == "WR" | pos == "RB" | pos == "TE")

actual_QB <- actual_stats %>% 
  filter(pos == "QB")

QB <- full_join(model_QB, actual_QB, by = c("player", "week")) %>% 
  mutate(difference = expected_points - Online_proj,
         o_resid = points - Online_proj,
         m_resid = points - expected_points) %>% 
  filter(week > 7)
flex <- full_join(model_flex, actual_flex, by = c("player", "week")) %>% 
  mutate(difference = expected_points - Online_proj,
         o_resid = points - Online_proj,
         m_resid = points - expected_points) %>% 
  filter(week > 7)


flex2 <- flex %>% 
  filter(abs(difference) > 3) %>% 
  filter(abs(o_resid) < abs(m_resid)) %>% 
  filter(!is.na(points)) %>% 
  select(1:15, 57:60)


ggplot(flex2, aes(o_resid, m_resid), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(-30, 30) +
  ylim(-30, 30) +
  geom_jitter()


###

ggplot(flex, aes(expected_points, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(Online_proj, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(Salary_yah, difference), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(-15, 15) +
  geom_jitter()

mod_flex1 <- lm(points ~ expected_points, data = flex)
summary(mod_flex1)

mod_flex2 <- lm(points ~ Online_proj, data = flex)
summary(mod_flex2)

mod_flex3 <- lm(points ~ expected_points + Online_proj, data = flex)
summary(mod_flex3)

mod_flex4 <- lm(points ~ expected_points + home_away.x, data = flex)
summary(mod_flex4)

mod_flex5 <- lm(points ~ combined_projection, data = flex)
summary(mod_flex5)

###


ggplot(QB, aes(expected_points, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(QB, aes(Online_proj, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(QB, aes(Salary_yah, difference), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(-15, 15) +
  geom_jitter()

mod_flex1 <- lm(points ~ expected_points, data = QB)
summary(mod_flex1)

mod_flex2 <- lm(points ~ Online_proj, data = QB)
summary(mod_flex2)

c <- 0.001
adj_r <- 0


m_w <- 0
o_w <- 1
while(TRUE){

  
  QB <- QB %>% 
    mutate(f_combined = expected_points*m_w + Online_proj*o_w)
  
  mod_QBs <- lm(points ~ f_combined, data = QB)
  summary(mod_QBs)
  new_adj_r <- summary(mod_QBs)$adj.r.squared
  
  if(new_adj_r > adj_r){
    adj_r <- new_adj_r
  } else {
    break
  }
  
  m_w <- m_w + c
  o_w <- o_w - c
}



c <- 0.001
adj_r <- 0


m_w <- 0
o_w <- 1
while(TRUE){
  
  
  flex <- flex %>% 
    mutate(f_combined = expected_points*m_w + Online_proj*o_w)
  
  mod_flexs <- lm(points ~ f_combined, data = flex)
  summary(mod_flexs)
  new_adj_r <- summary(mod_flexs)$adj.r.squared
  
  if(new_adj_r > adj_r){
    adj_r <- new_adj_r
  } else {
    break
  }
  
  m_w <- m_w + c
  o_w <- o_w - c
}











