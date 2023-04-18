# Jung Mee Park
# D2L visualizations
# 2023-04-14

#library
library(tidyverse)
library(readxl)
library(xlsx)    
library(purrr)
library(zoo)
# library(maps)
# loading in data
f = list.files("../data")
f
my_sheet_names <- excel_sheets("../data/College Behavior Results copy.xlsx")
my_sheets <- lapply(my_sheet_names, function(x) read_excel("../data/College Behavior Results copy.xlsx", sheet = x))
names(my_sheets) <- my_sheet_names

list2env(my_sheets, envir=.GlobalEnv)

my_sheet_names2 <- excel_sheets("../data/College Behavior Results Part 2 copy.xlsx")
my_sheets2 <- lapply(my_sheet_names2, function(x) read_excel("../data/College Behavior Results Part 2 copy.xlsx", sheet = x))
names(my_sheets2) <- my_sheet_names2

list2env(my_sheets2, envir=.GlobalEnv)

# rename the terms 
# `Background Context` <- `Background Context` %>% mutate(Term = case_when(
#   STRM == "2214" ~ "Fall 21",
#   STRM == "2221" ~ "Spring 22",
#   STRM == "2224" ~ "Fall 22",
#   STRM == "2231" ~ "Spring 23"))
# 
# `Background Context Course Level` <- `Background Context Course Level` %>% mutate(Term = case_when(
#   STRM == "2214" ~ "Fall 21",
#   STRM == "2221" ~ "Spring 22",
#   STRM == "2224" ~ "Fall 22",
#   STRM == "2231" ~ "Spring 23"))

# merge background context tables
# make the two tables similar 
`Background Context`$STRM <- as.character(`Background Context`$STRM)

# `Background Context Course Level`$`No D2L Site` <- as.numeric(`Background Context Course Level`$`No D2L Site`)
# `Background Context Course Level`$`D2L Site` <- as.numeric(`Background Context Course Level`$`D2L Site`)
# `Background Context Course Level`$TOTAL <- as.numeric(`Background Context Course Level`$TOTAL)
# # `Background Context Course Level`$`No D2L Site (Perc.)` <- as.numeric(`Background Context Course Level`$`No D2L Site (Perc.)`)
# 
# background_context <- `Background Context Course Level` %>% 
#   left_join(`Background Context`)
# # rename the terms 
# `Behavior 0` <- `Behavior 0` %>% mutate(Term = case_when(
#   STRM == "2214" ~ "Fall 21",
#   STRM == "2221" ~ "Spring 22",
#   STRM == "2224" ~ "Fall 22",
#   STRM == "2231" ~ "Spring 23"))
# 
# `Behavior 2 Quizzes` <- `Behavior 2 Quizzes` %>% mutate(Term = case_when(
#   STRM == "2214" ~ "Fall 21",
#   STRM == "2221" ~ "Spring 22",
#   STRM == "2224" ~ "Fall 22",
#   STRM == "2231" ~ "Spring 23"))

# mylist=list(df1,df2)
# lapply(mylist,function(x){
#   x$newVar=x$A1
#   x$newVar[x$A3>0]=x$A2
# })
my_dfs <- list(`Background Context`, `Background Context Course Level`,
               `Behavior 0`, `Behavior 1`, `Behavior 2 Assignments`, 
               `Behavior 2 Quizzes`, `Behavior 3`,
               `Behavior 4 Assignments`, `Behavior 4 Quizzes`, `Behavior 4 Root Content Objects`,
               `Behavior 5 Assign current sem`, `Behavior 5 Assignments`, `Behavior 5 Quizzes`,
               `Behavior 5 quizzes current sem`
)

# mapped_df <- my_dfs |>
#   map(\(x) {
#     x |>
#       mutate(
#         Term = case_when(
#           STRM == "2214" ~ "Fall 21",
#           STRM == "2221" ~ "Spring 22",
#           STRM == "2224" ~ "Fall 22",
#           STRM == "2231" ~ "Spring 23"
#         ),
#         Term = factor(Term, level = c("Fall 21", "Spring 22", "Fall 22", "Spring 23"))
#       )
#   })

# mapped_df %>% 
#   map_df(as_tibble)
# my_dfs <- setNames(nm = my_dfs, c(`Behavior 0`, `Behavior 1`, `Behavior 2 Assignments`, 
#                            `Behavior 2 Quizzes`, `Behavior 3`,
#                            `Behavior 4 Assignments`, `Behavior 4 Quizzes`, `Behavior 4 Root Content Objects`,
#                            `Behavior 5 Assign current sem`, `Behavior 5 Assignments`, `Behavior 5 Quizzes`,
#                            `Behavior 5 quizzes current sem`))
# names(mapped_df) <- my_dfs
# 
# list2env(mapped_df,envir = .GlobalEnv) # error
# do.call(rbind, lapply(mapped_df, as.data.frame))
# 
# list2env(list = mapped_df(1 = `Behavior 0`, 2 = `Behavior 1`, 3 = `Behavior 2 Assignments`, 
#               4 = `Behavior 2 Quizzes`, 5 = `Behavior 3`, 6 = `Behavior 4 Assignments`,
#               7 = `Behavior 4 Quizzes`, 8 = `Behavior 4 Root Content Objects`, 9 = `Behavior 5 Assign current sem`,
#               10 = `Behavior 5 Assignments`, 11 = `Behavior 5 Quizzes`, 12 = `Behavior 5 quizzes current sem`), envir = .GlobalEnv)

# names(my_dfs) <- c(`Behavior 0`, `Behavior 1`, `Behavior 2 Assignments`, 
#                `Behavior 2 Quizzes`, `Behavior 3`,
#                `Behavior 4 Assignments`, `Behavior 4 Quizzes`, `Behavior 4 Root Content Objects`,
#                `Behavior 5 Assign current sem`, `Behavior 5 Assignments`, `Behavior 5 Quizzes`,
#                `Behavior 5 quizzes current sem`)
# 
# setNames(nm = mapped_df, c(`Behavior 0`, `Behavior 1`, `Behavior 2 Assignments`, 
#                            `Behavior 2 Quizzes`, `Behavior 3`,
#                            `Behavior 4 Assignments`, `Behavior 4 Quizzes`, `Behavior 4 Root Content Objects`,
#                            `Behavior 5 Assign current sem`, `Behavior 5 Assignments`, `Behavior 5 Quizzes`,
#                            `Behavior 5 quizzes current sem`))
# list2env(mapped_df, envir =.GlobalEnv)
decoder <- tribble(
  ~STRM, ~Term,
  "2214", "Fall 21",
  "2221", "Spring 22",
  "2224", "Fall 22",
  "2231", "Spring 23"
)

## 
out <- map(my_dfs, \(x) dplyr::left_join(x, decoder))
names(out) <- my_dfs


# list2env(setNames(out, ls(pattern = "Behavior\\d\\d\\.csv")), envir = .GlobalEnv)
# list2env(out, envir = .GlobalEnv)

names(out) <- c("Background", "Background_Level", "Behavior 0", "Behavior 1", "Behavior 2 Assignments", 
                    "Behavior 2 Quizzes", "Behavior 3",
                    "Behavior 4 Assignments", "Behavior 4 Quizzes", "Behavior 4 Root Content Objects",
                    "Behavior 5 Assign current sem", "Behavior 5 Assignments", "Behavior 5 Quizzes",
                    "Behavior 5 quizzes current sem")

list2env(out,.GlobalEnv)

# for (i in 1:length(mapped_df)) {
#   region[i] <- as.data.frame(mapped_df[[i]])
# }
# list2env(setNames(df_list, ls(pattern = "Behavior\\d\\d\\")), envir = .GlobalEnv)
# 
# names(mapped_df) <- mapped_df
# 
# list2env(mapped_df, envir=.GlobalEnv)# n <- length(mapped_df[[14]])
# DF <- structure(mapped_df, row.names = c(NA, -n), class = "data.frame")

# #method 1 to rename variables
# resultList <- lapply(my_dfs, function(x){
#   x$Term = recode(x$STRM,
#                   "2214" = "Fall 21",
#                   "2221" = "Spring 22",
#                   "2224" = "Fall 22",
#                   "2231" = "Spring 23")
# })
# Background_context <- resultList[[1]]
# 
# Background_context_course_level <- resultList[[2]]

# method 2 to rename variables
# my_function <- function(x) x %>% 
#   mutate(Term = case_when(
#     STRM == "2214" ~ "Fall 21",
#     STRM == "2221" ~ "Spring 22",
#     STRM == "2224" ~ "Fall 22",
#     STRM == "2231" ~ "Spring 23"),
#     Term = factor(
#       Term,
#       level = c("Fall 21", "Spring 22","Fall 22", "Spring 23")
#     )
#   )
# lapply(my_dfs, my_function)

# lapply(my_dfs, function(x) left_join(x, decoder))
# 
# map(my_dfs, \(x) left_join(x, decoder)) 


# out <- map(mapped_df)
# names(my_new_df) <- my_new_df

# list2env(my_new_df, envir=.GlobalEnv)

#### Behavior 2 ####
#Behavior 2 quizzes
# change values from character to numeric #
`Behavior 2 Quizzes`$HAS_CONTENT <- as.numeric(`Behavior 2 Quizzes`$HAS_CONTENT)
`Behavior 2 Quizzes`$HAS_QUIZ <- as.numeric(`Behavior 2 Quizzes`$HAS_QUIZ)
# `Behavior 2 Quizzes`$TOTAL <- as.numeric(`Behavior 2 Quizzes`$TOTAL)
`Behavior 2 Quizzes` <- `Behavior 2 Quizzes` %>% 
  mutate(Has_quiz_duedate = case_when(
    NO_QUIZ_DUEDATE == "Y" ~ "No",
    NO_QUIZ_DUEDATE == "N" ~ "Yes"))

# Behavior 2 assignments
`Behavior 2 Assignments`$HAS_CONTENT <- as.numeric(`Behavior 2 Assignments`$HAS_CONTENT)
`Behavior 2 Assignments`$HAS_ASSIGNMENT <- as.numeric(`Behavior 2 Assignments`$HAS_ASSIGNMENT)

`Behavior 2 Assignments` <- `Behavior 2 Assignments` %>% 
  mutate(Has_asg_duedate = case_when(
    NO_ASG_DUEDATE == "Y" ~ "No",
    NO_ASG_DUEDATE == "N" ~ "Yes"))

# `Behavior 2 Quizzes` %>% 
#   group_by(COLLEGE_NAME) %>%
#   count(NO_QUIZ_DUEDATE)

# merGe behavior 2
Behavior2 <- `Behavior 2 Assignments` %>% 
  left_join(`Behavior 2 Quizzes`)

# mutate illogical content flag
# mutate(Term = case_when(
#   #     STRM == "2214" ~ "Fall 21",
  #     STRM == "2221" ~ "Spring 22",


#### behavior 3 ####
# Col_of_science_beh3 <- `Behavior 3` %>% 
#   filter(COLLEGE_NAME == "College of Science")

#### Behavior 4 quizzes ####
`Behavior 4 Quizzes`$`Not In Range` <- as.numeric(`Behavior 4 Quizzes`$`Not In Range`)
`Behavior 4 Quizzes`$`In Range` <- as.numeric(`Behavior 4 Quizzes`$`In Range`)
`Behavior 4 Quizzes`$TOTAL <- as.numeric(`Behavior 4 Quizzes`$TOTAL)


# `Behavior 4 Quizzes` <- `Behavior 4 Quizzes` %>% 
#   mutate(Percentage = `Not In Range`/TOTAL)

# Behavior 4 assignments 
`Behavior 4 Assignments`$`Not In Range` <- as.numeric(`Behavior 4 Assignments`$`Not In Range`)
`Behavior 4 Assignments`$`In Range` <- as.numeric(`Behavior 4 Assignments`$`In Range`)
`Behavior 4 Assignments`$TOTAL <- as.numeric(`Behavior 4 Assignments`$TOTAL)

length(unique(unlist(`Behavior 4 Quizzes`[c("COLLEGE_NAME")])))

`Behavior 4 Quizzes` <- `Behavior 4 Quizzes` %>%
  group_by(COLLEGE_NAME) %>%
  mutate(count_not_range= sum(`Not In Range`)) %>% 
  mutate(count_total= sum(TOTAL))%>%
  mutate(college_per=paste0(round(100*count_not_range/count_total,2),'%'))


#### Save files to CSV ####
# write_csv(`Background Context`, "../data/background_context.csv")

write_csv(Background_Level, "../data/background_context_course_level.csv")

write_csv(`Behavior 0`, "../data/cleaned_data/Behavior_0.csv")

write_csv(Behavior2, "../data/cleaned_data/Behavior_2_all.csv")

write_csv(`Behavior 2 Quizzes`, "../data/cleaned_data/Behavior_2_quizzes.csv")

write_csv(Col_of_science_beh3, "../data/cleaned_data/Col_of_science_beh3.csv")

write_csv(`Behavior 4 Quizzes`, "../data/cleaned_data/Behavior_4_quizzes.csv")


