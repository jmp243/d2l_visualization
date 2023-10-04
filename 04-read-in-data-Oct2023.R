# reading in D2L and peoplesoft datafiles
# using data pulled by Frances
# 2023-10-3

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

# Student Enrollment with D2L from UAccess FKM pull
Fall2023_StuEnr_FKM  <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/EnrollmentAcrossCombinedSections_LMS_COLLEGE_EMAIL.csv")

# Change instructor name
# rename instructor
Fall2023_StuEnr_FKM <- Fall2023_StuEnr_FKM %>% rename(Instructor = `Person Name`) 

Fall2023_StuEnr_FKM <- Fall2023_StuEnr_FKM %>% 
  filter(Component != "Ind Study")

# create a number only column for catalog numbers
Fall2023_StuEnr_FKM$CATALOG_NBR <- gsub("\\D+", "", Fall2023_StuEnr_FKM$`Cat #`)

# get D2L course ID from URL
Fall2023_StuEnr_FKM <- Fall2023_StuEnr_FKM %>%
  mutate(D2L_CRSEID = str_extract(`LMS URL`, "[^/]+$"))

# get instructor NetID from preferred email
Fall2023_StuEnr_FKM <- Fall2023_StuEnr_FKM %>%
  mutate(INSTRUCTOR_NETID = str_extract(`Preferred Email Address`, "[^@]+"))

Fall2023_StuEnr_FKM %>% 
  select(Term, Campus, Subject, Component, `Total Enrollment`, `LMS URL`, D2L_CRSEID, 
         Instructor, INSTRUCTOR_NETID, `Preferred Email Address`,
         `Class Number`, CATALOG_NBR, `Cat #`, `Academic Organization Level 1 Desc`)
