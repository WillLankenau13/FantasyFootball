library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library(rlang)

library(modelr)
options(na.action = na.warn)


flex <- read_csv("~/R Stuff/FantasyFootball/Analysis/week_4_for_flex.csv") %>% 
  filter(salary > 10)
QB <- read_csv("~/R Stuff/FantasyFootball/Analysis/week_4_for_QB.csv") %>% 
  filter(salary > 10)


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

ggplot(flex, aes(salary, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(expected_points, Online_proj), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(FPPG, expected_points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(FPPG, Online_proj), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(flex, aes(FPPG, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()


mod_4flex <- lm(points ~ expected_points + Online_proj + FPPG, data = flex)
summary(mod_4flex)

mod_flex <- lm(points ~ expected_points, data = flex)
summary(mod_flex)

mod2_flex <- lm(points ~ Online_proj, data = flex)
summary(mod2_flex)

mod3_flex <- lm(points ~ FPPG, data = flex)
summary(mod3_flex)


#####################


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

ggplot(QB, aes(salary, points), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

ggplot(QB, aes(expected_points, Online_proj), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()

mod_QB <- lm(points ~ expected_points + Online_proj + FPPG, data = QB)
summary(mod_QB)


####

wm <- 1
wo <- 0
wf <- 0
tot <- wo + wm + wf
wo <- wo/tot
wm <- wm/tot
wf <- wf/tot

flex <- flex %>% 
  mutate(combined = expected_points*wm + Online_proj*wo + FPPG*wf) 

mod_flex <- lm(points ~ combined, data = flex)
summary(mod_flex)

###

flex <- flex %>% 
  mutate(m_dif = expected_points - points, 
         o_dif = Online_proj - points,
         f_dif = FPPG - points)

dif1 <- flex %>% 
  select(m_dif) %>% 
  mutate(dif = "m")

dif2 <- flex %>% 
  select(o_dif) %>% 
  mutate(dif = "o")

dif3 <- flex %>% 
  select(f_dif) %>% 
  mutate(dif = "f")

names(dif1) <- c("dif", "dif_type")
names(dif2) <- c("dif", "dif_type")
names(dif3) <- c("dif", "dif_type")

differences <- rbind(dif1, dif2, dif3)

ggplot(data = differences, mapping = aes(x = reorder(dif_type, dif, FUN = median), y = dif)) +
  geom_boxplot() 

dif1 |> 
  ggplot() +
  aes(x = dif) +
  geom_histogram(bins = 80) +
  ylim(0, 25) +
  xlim(-20, 20)

dif2 |> 
  ggplot() +
  aes(x = dif) +
  geom_histogram(bins = 80) +
  ylim(0, 25) +
  xlim(-20, 20)

dif3 |> 
  ggplot() +
  aes(x = dif) +
  geom_histogram(bins = 80) +
  ylim(0, 25) +
  xlim(-20, 20)




