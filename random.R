library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library(rlang)

library(modelr)
options(na.action = na.warn)










ggplot(Combined_QB, aes(expected_points, Online_proj), position = "jitter") +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter() +  
  geom_abline(slope = 1, intercept = 0)

ggplot(Combined_Flex, aes(expected_points, Online_proj), position = "jitter") +
  geom_ref_line(h = 0) +
  xlim(0, 45) +
  ylim(0, 45) +
  geom_jitter()+  
  geom_abline(slope = 1, intercept = 0)















