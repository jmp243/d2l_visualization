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
# read in data for D2L users
D2L_Users <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_Users.csv")

# keep only useful columns in d2l users
D2L_Users <- D2L_Users %>% 
  select(UserId, UserName, Organization, ExternalEmail, OrgRoleId, LastAccessed)

# read chunked csv
# D2L_OrgUnits_chunked <- read_csv_chunked("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_OrganizationalUnits.csv")

# read in data for d2l Org Units
D2L_OrgUnits <- read_csv("~/Documents/Trellis/D2L_visualization/data/FKM_pull/D2L_OrganizationalUnits.csv")

# keep only useful columns in d2l Org Units
D2L_OrgUnits <- D2L_OrgUnits %>% 
  select(OrgUnitId, Organization, OrgUnitTypeId)
