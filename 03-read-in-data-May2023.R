# Jung Mee Park
# D2L visualizations
# 2023-05-18

#library
library(tidyverse)
library(readxl)
library(xlsx)    
library(purrr)
library(zoo)
# library(maps)

D2L_Behaviors <- read_excel("~/Documents/Trellis/D2L_visualization/data/D2L Behaviors.xlsx")
View(D2L_Behaviors)

# rename spring and fall terms
decoder <- tribble(
  ~STRM, ~Term,
  "2214", "Fall 21",
  "2221", "Spring 22",
  "2224", "Fall 22",
  "2231", "Spring 23"
)

D2L_Behaviors <- D2L_Behaviors %>%
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 21" = "2214",
             "Spring 22" = "2221",
             "Fall 22" = "2224",
             "Spring 23" = "2231"
          ))
