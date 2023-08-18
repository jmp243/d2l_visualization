# Jung Mee Park
# D2L visualizations
# 2023-05-18

#library
library(tidyverse)
library(readxl)
# library(xlsx)    
library(purrr)
library(zoo)
# library(maps)

D2L_Behaviors <- read_excel("~/Documents/Trellis/D2L_visualization/data/D2L Behaviors.xlsx")
# View(D2L_Behaviors)

# rename spring and fall terms
# decoder <- tribble(
#   ~STRM, ~Term,
#   "2214", "Fall 21",
#   "2221", "Spring 22",
#   "2224", "Fall 22",
#   "2231", "Spring 23"
# )

D2L_Behaviors <- D2L_Behaviors %>%
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 21" = "2214",
             "Spring 22" = "2221",
             "Fall 22" = "2224",
             "Spring 23" = "2231"
          ))


### has d2l site
D2L_Behaviors %>% 
  group_by(HAS_D2L_SITE) %>% 
  count()

# Bank %>% mutate(Manager = recode(JobGrade, 
#                                  "1" = "Non-mgmt",
#                                  "2" = "Non-mgmt",
#                                  "3" = "Non-mgmt",
#                                  "4" = "Non-mgmt",
#                                  .default = "Mgmt"))  %>% 
#   select(Employee, JobGrade, Gender, Manager)

# mutate variables to have yes no into counts
D2L_Behaviors <- D2L_Behaviors %>%
  mutate(Has_d2l_num = factor(HAS_D2L_SITE) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_content_num = factor(HAS_CONTENT) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_Asg_num = factor(HAS_ASSIGNMENT) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_Asg_due_num = factor(HAS_ASG_DUEDATE) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_Quiz_num = factor(HAS_QUIZ) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_Quiz_due_num = factor(HAS_QUIZ_DUEDATE) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_Disc_num = factor(HAS_DISCUSSION) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) %>% 
  mutate(Has_grade_num = factor(HAS_GRADE) %>%
           fct_recode(
             "1" = "Y",
             "0" = "N")) 

# calculate percentages
table <- D2L_Behaviors %>% 
  group_by(TERM, COLLEGE_NAME) %>% 
  count(HAS_CONTENT)

D2L_Behaviors %>% 
  group_by(TERM, COLLEGE_NAME) %>% 
  count(Has_Quiz_num)
has_quiz_dd <-D2L_Behaviors %>% 
  group_by(TERM, COLLEGE_NAME) %>% 
  count(HAS_QUIZ_DUEDATE)

# ### write CSV
# write_csv(D2L_Behaviors, "../data/cleaned_data/D2L_Behaviors_May2023.csv")
# 

test <- D2L_Behaviors %>% 
  select(-CLASS_SECTION, -SSR_COMPONENT, -INSTR_NETID) %>% 
  unique()

D2L_Behaviors %>% 
  count(HAS_D2L_SITE)

test %>% 
  group_by(COLLEGE_NAME) %>% 
  count(HAS_D2L_SITE)


write_csv(test, "../data/cleaned_data/D2L_Behaviors_Aug2023.csv")

### check the n's
n_distinct(test$D2L_CRSEID)
