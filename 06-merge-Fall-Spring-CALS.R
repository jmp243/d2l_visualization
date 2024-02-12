# merging fall 2023 and spring 2024 files for CALS
# last run 2-12-2024


#library
library(tidyverse)
library(readxl)
library(purrr)
library(zoo)
library(DBI)
library(RSQLite)
library(here)
library(vroom)
library(dplyr)
library(stringr)

# get wd
getwd()
# [1] "/Users/jungmeepark/Documents/Trellis/D2L_visualization/d2l_visualization_script"

# open files from cleaned data
D2L_own_pull_Fall2023_test2b <- read_csv("~/Documents/Trellis/D2L_visualization/data/cleaned_data/D2L_own_pull_Fall2023_test2b.csv")
D2L_own_pull_Spring2024_test2b <- read_csv("~/Documents/Trellis/D2L_visualization/data/cleaned_data/D2L_own_pull_Spring2024_test2b.csv")

# combine Fall2023 and Spring2024 data files 
D2L_CALS <- rbind(D2L_own_pull_Fall2023_test2b, D2L_own_pull_Spring2024_test2b)

# keep data with d2l if cross-listed
test1 <-D2L_CALS %>% 
  group_by(Course, Instructor, CLASS_LEVEL, Term, `Total Enrollment`) %>% # Specify the columns you want to check for duplicates
  filter(!(any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()

test1_reverse <-D2L_CALS %>% 
  group_by(Course, Instructor, CLASS_LEVEL, Term, `Total Enrollment`) %>% # Specify the columns you want to check for duplicates
  filter((any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()

D2L_CALS_only <- test1 %>% 
  select(-`...1`) %>% 
  filter(COLLEGE_NAME == "Coll of Ag Life & Env Sci (formerly CALS)")

# export the data for analysis in Tableau

