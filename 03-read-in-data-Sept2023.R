# reading in Fall 2023 preliminary d2l usage data
# 2023-09-06

#library
library(tidyverse)
library(readxl)
# library(xlsx)    
library(purrr)
library(zoo)
# library(maps)

# historical data
D2L_Behaviors <- read_excel("~/Documents/Trellis/D2L_visualization/data/D2L Behaviors.xlsx")

# fall 2023 data
D2L_Behaviors_fall23 <- read_excel("~/Documents/Trellis/D2L_visualization/data/D2L Content 2234.xlsx")
# View(D2L_Behaviors)


D2L_Behaviors <- D2L_Behaviors %>%
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 21" = "2214",
             "Spring 22" = "2221",
             "Fall 22" = "2224",
             "Spring 23" = "2231"
           ))


# Create Term, rename requester_netid field, remove start_dt, and Last_update
D2L_Behaviors_fall23 <- D2L_Behaviors_fall23  %>% 
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 23" = "2234")) %>% 
  rename(REQUESTER_NETID = COURSE_REQUESTED_BY) %>% 
  select(-START_DT)
  # select(-START_DT, -LASTUP_EW_DTTM)

# names(D2L_Behaviors)
# names(D2L_Behaviors_fall23)

# append rows to the existing dataset
test <- dplyr::bind_rows(D2L_Behaviors, D2L_Behaviors_fall23)

write.csv(D2L_Behaviors_fall23, "../data/cleaned_data/D2L_Behaviors_Sept2023.csv")
