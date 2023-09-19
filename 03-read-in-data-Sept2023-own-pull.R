# reading in D2L and peoplesoft datafiles
# September 14, 2023
# 2023-09-14

#library
library(tidyverse)
library(readxl)
# library(xlsx)    
library(purrr)
library(zoo)
# library(maps)
# https://www.michaelc-m.com/manual_posts/2022-01-27-big-CSV-SQL.html
library(DBI)
library(RSQLite)
library(here)
library(vroom)
library(dplyr)
library(stringr)
# read in data for D2L users
D2L_Users <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_Users.csv")

# keep only useful columns in d2l users
D2L_Users <- D2L_Users %>% 
  select(UserId, UserName, Organization, ExternalEmail, OrgRoleId, LastAccessed)

table(D2L_Users$OrgRoleId)

D2L_Users %>% 
  distinct(UserId, OrgRoleId) %>% 
  count(OrgRoleId)

# read chunked csv
# D2L_OrgUnits_chunked <- read_csv_chunked("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_OrganizationalUnits.csv")

# read in data for d2l Org Units
D2L_OrgUnits <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_OrganizationalUnits.csv")

# keep only useful columns in d2l Org Units
D2L_OrgUnits <- D2L_OrgUnits %>% 
  select(OrgUnitId, Organization, OrgUnitTypeId)

# get content objects from d2l WAY TOO BIG
D2L_ContentObjects <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_ContentObjects.csv")

# select key variables
D2L_ContentObjects %>% 
  select(ContentObjectId, OrgUnitId, ContentObjectType, LastModified, LastModifiedBy)
# check variables
table(D2L_ContentObjects$LastModifiedBy)

# Fall 2023 classes data from UAccess 
Fall2023_classes_orig <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/ClassSchedule Overview_Fall2023.csv")

# remove unhelpful columns
# filter out indep study, preceptorship, directed reading
Fall2023_classes <- Fall2023_classes_orig  %>% 
  select(-Campus, -`Meet #`, -`Req Desig`, -`P/F Opt`, 
         -Facility, -`Fac 1st Pref`, -`Fac 2nd Pref`, -`Fac 3rd Pref`,
         -Units, -`Min Units`,-`Max Units`, -Mode, -Start, -End,
         -`Meeting Days`, -`Max Enroll`, -`Total Enroll`, -`Rm Cap`, 
         -`Combined Section`, -`Enrollment Status`) %>% 
  filter(Component != "Ind Study")

# # filter out indep study, preceptorship, directed reading
# table(Fall2023_classes$Component)
# 
# Fall2023_classes <- Fall2023_classes %>% 
#   filter(Component != "Ind Study")

# Missing Link data from UAccess 
Fall2023_sections_orig  <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/20230919_EnrollmentAcrossCombinedSections.csv")

Fall2023_sections_orig  %>% 
  group_by(`Class  Status`) %>% 
  count()

Fall2023_sections_orig  %>% 
  group_by(Campus) %>% 
  count()

# remove cancelled classes
# remove Campus == GLBL
# filter out Section Enrollment of under 4
# take out CATALOG_NBR is over 400
Fall2023_sections_orig$CATALOG_NBR <- gsub("\\D+", "", Fall2023_sections_orig$`Cat #`)

Fall2023_sections <- Fall2023_sections_orig %>% 
  filter(as.numeric(CATALOG_NBR) <= 500) %>% 
  filter(Campus != "GLBL") %>% 
  filter(`Class  Status` != "Cancelled") %>% 
  filter(`Total Enrollment` >= 4) %>% 
  rename(INSTRUCTOR = `Person Name`) 

# remove redundant variables
Fall2023_sections <- Fall2023_sections %>% 
  select(-Session, -`Combined Section ID`)

# get URL ID, but what does it mean?
Fall2023_sections_url <- Fall2023_sections %>%
  mutate(D2L_CRSEID = str_extract(`LMS URL`, "[^/]+$"))

# create a binary yes no to d2l site
Fall2023_sections_url$HAS_D2L_SITE <- ifelse(Fall2023_sections_url$D2L_CRSEID>0, "Y", "N")
Fall2023_sections_url$HAS_INSTRUCTOR <- ifelse(Fall2023_sections_url$INSTRUCTOR >0, "Y", "N")

# remove rows with no instructors if there is another row with an instructor
Fall2023_sections_url %>%
  group_by(Course, Subject, CATALOG_NBR, `Class Number`) %>%
  filter(!(any(HAS_INSTRUCTOR == "Y") & HAS_INSTRUCTOR == "N")) %>% 
  ungroup()

# filtering data
filtered_section_data <- Fall2023_sections_url %>%
  # group_by(CRSE_ID, SUBJECT, CATALOG_NBR, INSTRUCTOR, TERM) %>%
  group_by(Course, Subject, CATALOG_NBR, INSTRUCTOR, `Class Number`) %>%
  filter(!(any(HAS_D2L_SITE == "Y") & HAS_D2L_SITE == "N")) %>% 
  ungroup()



  
# # select only useful variables from sections data
# Fall2023_sections <- Fall2023_sections %>% 
#   select(Term, Course, Section, )
#   # select(STRM, SUBJECT, CATALOG_NBR, CLASS_SECTION, COLLEGE_NAME, LMS_URL, SSR_COMP_CD)
# 
# table(Fall2023_sections$SSR_COMP_CD)


