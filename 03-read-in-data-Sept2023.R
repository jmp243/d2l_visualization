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


# filter_data <- filtered_data %>% 
#   filter(!is.na(COURSE_REQUESTED_BY))
# filter_data <- filtered_data %>% 
#   group_by(across(c(CRSE_ID, SUBJECT, CATALOG_NBR, COLLEGE_NAME, INSTR_NETID, STRM))) %>%  # Specify the columns you want to check for duplicates
#   filter(!duplicated(CRSE_ID) | !duplicated(CRSE_ID, fromLast = TRUE)) %>%
#   ungroup()

# Add numeric variables
D2L_Behaviors <- D2L_Behaviors %>%
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 21" = "2214",
             "Spring 22" = "2221",
             "Fall 22" = "2224",
             "Spring 23" = "2231"
           ))

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


# Create Term, rename requester_netid field, remove start_dt, and Last_update
D2L_Behaviors_fall23 <- D2L_Behaviors_fall23  %>% 
  mutate(TERM = factor(STRM) %>%
           fct_recode(
             "Fall 23" = "2234")) %>% 
  rename(REQUESTER_NETID = COURSE_REQUESTED_BY) %>% 
  select(-START_DT)

# names(D2L_Behaviors)
# names(D2L_Behaviors_fall23)

# append rows to the existing dataset
test <- dplyr::bind_rows(D2L_Behaviors, D2L_Behaviors_fall23)


# test <- test %>% 
#   select(-CLASS_SECTION, -SSR_COMPONENT, -CLASS_TYPE) %>% 
#   unique()

test$REQUESTER_NETID[!is.na(test$INSTR_NETID)] <- test$INSTR_NETID[!is.na(test$INSTR_NETID)]


# remove if N when requester_netid, course id, subject, collegename, catalog nbr, class level, strm and N has_d2l_site
test <- test %>%
  select(-CLASS_SECTION, -SSR_COMPONENT, -CLASS_TYPE, INSTR_ROLE, CRSE_OFFER_NBR) %>% 
  distinct()


# test %>% 
#   group_by(COLLEGE_NAME) %>% 
#   count(HAS_D2L_SITE)
# 
test <- test %>% 
  select(-INSTR_NETID) %>% 
  distinct() # you can use distinct() and name the columns

# rename CALS
test$COLLEGE_NAME <- recode(test$COLLEGE_NAME,
               "Coll of Ag Life & Env Sci" = "Coll of Ag Life & Env Sci (formerly CALS)", 
               "College of Agric and Life Sci" = "Coll of Ag Life & Env Sci (formerly CALS)")

# filter out duplicate sites with Y in some sections and N in the other
test1 <- test %>% 
  group_by(CRSE_ID, SUBJECT, CATALOG_NBR, COLLEGE_NAME, REQUESTER_NETID, TERM) %>% # Specify the columns you want to check for duplicates
  filter(!(any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()

# test2 <- test %>% 
#   select(-CLASS_SECTION, -SSR_COMPONENT, -CLASS_TYPE) %>%
#   unique()

updates <- test2 %>%
  group_by(COLLEGE_NAME) %>%
  count(LASTUP_EW_DTTM)

# View the filtered data
View(filtered_data)

# LSCHRENK <- test %>% 
#   filter(REQUESTER_NETID == "LSCHRENK")
# write.csv(LSCHRENK, "../data/cleaned_data/LSCHRENK_Sept2023.csv")

write.csv(test1, "../data/cleaned_data/D2L_Behaviors_Sept2023.csv")

#### help ###
# Create a dataframe
fake_data <- data.frame(
  CRSE_ID = c(17568, 17568, 17568, 17569, 17569, 17569, 17568, 17568, 17568, 2333),
  SUBJECT = c("ARC", "ARC", "ARC", "ARC", "ARC", "ARC", "ARC", "ARC", "ARC", "ECON"),
  CATALOG_NBR = c(231, 231, 231, 232, 232, 232, 231, 231, 231, 101),
  CLASS_SECTION = c("002A", "2", "1", "002A", "2", "1", "1", "2", "002A","90"),
  CLASS_TYPE = c("N", "E", "E", "N", "E", "E", "E", "E", "N", "N"),
  DEPT = c("Landscape Arch", "Landscape Arch", "Landscape Arch",
                   "Landscape Arch", "Landscape Arch", "Landscape Arch",
                   "Landscape Arch", "Landscape Arch", "Landscape Arch", "Economics"),
  MY_CRSEID = c(NA, 78340, 78340, NA, 24913, 24913, 18830, 18830, NA, NA),
  HAS_MY_SITE = c("N", "Y", "Y", "N", "Y", "Y", "Y", "Y", "N", "N"),
  INSTRUCTOR = c("PARK", "PARK", "PARK", "PARK", "PARK", "PARK", "PARK", "PARK", "PARK", "DIFFER"),
  SSR_COMPONENT = c("DIS", "LEC", "LEC", "DIS", "LEC", "LEC", "LEC", "LEC", "DIS", "LEC"),
  HAS_CONTENT = c("NA", "Y", "Y", "NA", "Y", "Y", "Y", "Y", "NA", "NA"),
  TERM = c("Fall 21", "Fall 21", "Fall 21", "Spring 22", "Spring 22", "Spring 22", "Fall 23", "Fall 23", "Fall 23", "Fall 23"),
  LASTUP_EW_DTTM = c("NA", "NA", "NA", "NA", "NA", "NA", "8/29/23", "8/29/23", "NA", "NA")
)

filtered_fake_data <- fake_data %>%
  group_by(CRSE_ID, SUBJECT, CATALOG_NBR, INSTRUCTOR, TERM) %>%
  filter(!(any(HAS_MY_SITE == "Y") & HAS_MY_SITE == "N")) %>% 
  ungroup()
