# reading in D2L and peoplesoft datafiles
# using data pulled by Jung Mee using an xml file
# 2024-01-25

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
#### Start on Jan 2024 ####
# JUNG MEE's Pull
Spring2024_StuEnr_JMP  <- read_csv("~/Documents/Trellis/D2L_visualization/data/JMP_pull/Jan2024_EnrollmentAcrossCombinedSections With D2L JMP.csv")

# # filter out indep study, preceptorship, directed reading
Spring2024 <- Spring2024_StuEnr_JMP  %>%
  filter(Component != "Ind Study")

# rename instructor
Spring2024 <- Spring2024 %>% rename(Instructor = `Person Name`) 

# create a number only column for catalog numbers
Spring2024$CATALOG_NBR <- gsub("\\D+", "", Spring2024$`Cat #`)

# get D2L course ID from URL
Spring2024 <- Spring2024 %>%
  mutate(D2L_CRSEID = str_extract(`LMS URL`, "[^/]+$"))

# get instructor NetID from preferred email
Spring2024 <- Spring2024 %>%
  mutate(INSTRUCTOR_NETID = str_extract(`Preferred Email Address`, "[^@]+"))

# create a class level using case when
Spring2024 <- Spring2024 %>%
  mutate(
    CLASS_LEVEL = case_when(CATALOG_NBR >= 400 ~ 400,
                            CATALOG_NBR >= 300 ~ 300,
                            CATALOG_NBR >= 200 ~ 200,
                            TRUE ~ 100
    )
  )

# # remove high school course 13931
# Fall2023_StuEnr_FKM %>% 
#   filter(Course != 13931)
# create a binary yes no to d2l site
Spring2024$HAS_D2L_SITE <- ifelse(Spring2024$D2L_CRSEID>0, "Y", "N")

# remove the ones without instructors
# remove rows with no instructors if there is another row with an instructor
Spring2024$HAS_INSTRUCTOR <- ifelse(Spring2024$Instructor >0, "Y", "N")

# keep rows with D2L sites
test1 <- Spring2024 %>% 
  group_by(Course, Subject, CATALOG_NBR, Instructor, CLASS_LEVEL, Term) %>% # Specify the columns you want to check for duplicates
  filter(!(any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()

# # remove rows with low section enrollment
# test1 <- Fall2023_StuEnr_FKM %>% 
#   group_by(Course, Subject, CATALOG_NBR, Instructor, Term, `Section Enrollment`) %>% 
#   filter(`Section Enrollment` => 0 )
#   ungroup()
# 

test1 <- test1 %>% 
  select(Term, Campus, Subject, Component, `Total Enrollment`, `LMS URL`, D2L_CRSEID, 
         Instructor, INSTRUCTOR_NETID, `Preferred Email Address`, Section, Course, 
         HAS_D2L_SITE, HAS_INSTRUCTOR, CLASS_LEVEL,
         `Class Number`, CATALOG_NBR, `Cat #`, `Academic Organization Level 1 Desc`) %>% 
  rename(COLLEGE_NAME = `Academic Organization Level 1 Desc`) %>% 
  unique() #15220

test1a <- test1 %>% 
  filter(as.numeric(CATALOG_NBR) <= 500) %>% 
  filter(Campus != "GLBL") %>% 
  # filter(`Actual Enroll` >= 4)
  filter(`Total Enrollment` >= 4) %>% 
  distinct() #8670

test1a <- test1a %>%
  filter(Campus != "PHX") %>%
  filter(Campus != "CMNTY") %>%
  filter(Campus != "GLBD") %>% 
  distinct() #9195


test2 <- test1a %>% 
  group_by(Course, Subject, CATALOG_NBR, `Class Number`) %>%
  filter(!(any(HAS_INSTRUCTOR == "Y") & HAS_INSTRUCTOR == "N")) %>% 
  ungroup() #9059

# rename CALS
test2$COLLEGE_NAME <- recode(test2$COLLEGE_NAME,
                             "Coll of Ag Life & Env Sci" = "Coll of Ag Life & Env Sci (formerly CALS)", 
                             "College of Agric and Life Sci" = "Coll of Ag Life & Env Sci (formerly CALS)")

# rename College of Applied Sci & Tech
test2$COLLEGE_NAME <- recode(test2$COLLEGE_NAME,
                             "University of Arizona South" = "College of Applied Sci & Tech")


# college name table
table(test2$COLLEGE_NAME)

# campus table
table(test2$Campus)
# prioritize main campus values
order_of_importance <- c("MAIN", "ONLN", "DIST", "GLBD", "SOUTH")
# 
test2a <- test2 %>%
  mutate(Campus_Ord = factor(Campus, order_of_importance)) %>%
  arrange(Campus_Ord) %>%
  distinct(.keep_all = TRUE)

write.csv(test2, "../data/cleaned_data/D2L_own_pull_Jan2024_test2.csv")

# keep distinct course id
test2 %>% 
  distinct(Course) %>% 
  count() #2191

test2 %>% 
  distinct(Course, Instructor) %>% 
  count() #2191

# # Arrange data based on importance_var and then remove duplicates
result <- test2 %>%
  arrange(Course) %>%
  distinct(Course, .keep_all = TRUE)

result2 <- test2 %>%
  arrange(Course, Instructor) %>%
  distinct(Course, Instructor, .keep_all = TRUE)
# 
write.csv(result, "../data/cleaned_data/D2L_own_pull_Spring2024_distinct_Course.csv")
write.csv(result2, "../data/cleaned_data/D2L_own_pull_Spring2024_distinct_Course_Instr.csv")


# look at components
table(result$Component)
table(result2$Component)
# how to drop Class Number to create 
# seems to drop too many
# 
test2b <- test2a %>%
  distinct(Term, Course, `Cat #`, COLLEGE_NAME, D2L_CRSEID, Instructor, .keep_all= TRUE) %>%
  distinct()

write.csv(test2b, "../data/cleaned_data/D2L_own_pull_Spring2024_test2b.csv")

# if course # are the same keep the row with d2l site
# keep rows with D2L sites
test2c <- test2b %>% 
  group_by(Course, CATALOG_NBR, Instructor, Term) %>% # Specify the columns you want to check for duplicates
  filter(!(any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()


write.csv(test2c, "../data/cleaned_data/D2L_own_pull_Spring2024_test2c.csv")


# how often are d2l courses duplicated
n_occur <- data.frame(table(test2$D2L_CRSEID))

n_occur[n_occur$Freq > 1,]

duplicated_courses <- test2[test2$D2L_CRSEID %in% n_occur$Var1[n_occur$Freq > 1],]
# # main campus only?
# test2b <- test2a %>% 
#   filter(Campus == "MAIN") 
#   distinct()


# # get content objects from d2l WAY TOO BIG
# D2L_ContentObjects <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_ContentObjects.csv")
# 
# # keep only useful columns in d2l Org Units
# D2L_ContentObjects <- D2L_ContentObjects %>% 
#   select(ContentObjectId, OrgUnitId, Title) %>% 
#   distinct()

