library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")



Week <- 18

##

Year <- "2022_23"

###########################

Char_Week <- as.character(Week)
Char_Week_m_1 <- as.character(Week - 1)

dir.create(eval(paste("~/R Stuff/FantasyFootball/Combined_Weekly_Stats/Weeks_1_", Char_Week_m_1 ,"", sep = "")))
dir.create(eval(paste("~/R Stuff/FantasyFootball/Combined_Model_Online_Results/Week_", Char_Week, sep = "")))
dir.create(eval(paste("~/R Stuff/FantasyFootball/Middle/Week_", Char_Week, sep = "")))
dir.create(eval(paste("~/R Stuff/FantasyFootball/Model_Results/Week_", Char_Week, sep = "")))
dir.create(eval(paste("~/R Stuff/FantasyFootball/Online_Models/Week_", Char_Week, sep = "")))
dir.create(eval(paste("~/R Stuff/FantasyFootball/Prior/", Year, "/Week_", Char_Week, sep = "")))
