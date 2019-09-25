# David's school proficiency analysis 
library(tidyverse)
# SETUP ==========================
# here are some useful functions for turning a range into a high or low estimate. We need these to parse ed.gov proficiency data, which suppresses some records for confidentiality
lowball <- function(range){
  ifelse(!is.na(as.numeric(range)),range,
         ifelse(str_detect(range, "-"), as.numeric(str_sub(range, start = 1L, end = (str_locate(range, "-")[1] - 1))),
                ifelse(str_detect(range, pattern = "GE"), as.numeric(str_sub(range, start = str_locate(range, "GE")[2] + 1, end = nchar(range))),
                       ifelse(str_detect(range, pattern = "GT"), as.numeric(str_sub(range, start = str_locate(range, "GT")[2] + 1, end = nchar(range))),
                              ifelse(str_detect(range, pattern = "LE"), 0,
                                     ifelse(str_detect(range, pattern = "LT"), 0,
                                            ifelse(range == "PS", NA, "x")))))))
}
highball <- function(range){
  ifelse(!is.na(as.numeric(range)),range,
         ifelse(str_detect(range, "-"), as.numeric(str_sub(range, start = (str_locate(range, "-")[1] + 1), end = nchar(range))),
                ifelse(str_detect(range, pattern = "GE"), 100,
                       ifelse(str_detect(range, pattern = "GT"), 100,
                              ifelse(str_detect(range, pattern = "LE"), as.numeric(str_sub(range, start = str_locate(range, "LE")[2] + 1, end = nchar(range))),
                                     ifelse(str_detect(range, pattern = "LT"), as.numeric(str_sub(range, start = str_locate(range, "LT")[2] + 1, end = nchar(range))),
                                            ifelse(range == "PS", NA, "x")))))))
}

# download math proficiency scores from ed.gov
# schools_math <- read_csv(url("https://www2.ed.gov/about/inits/ed/edfacts/data-files/math-achievement-sch-sy2015-16.csv"))

# use updated 16-17 data, filter for CO and MI only
schools_math <- read_csv("https://www2.ed.gov/about/inits/ed/edfacts/data-files/math-achievement-sch-sy2016-17.csv")
schools_RLA <- read_csv("https://www2.ed.gov/about/inits/ed/edfacts/data-files/rla-achievement-sch-sy2016-17.csv")

# function to use lowball and highball estimate to get value
get_estimate <- function(df, subj, grade, var, year){
  # identify existing columns by names
  col_total <- paste0(var,"_",subj, grade, "NUMVALID_", year)
  col_pct <- paste0(var,"_",subj, grade, "PCTPROF_", year)
  # construct new column names
  varname_T <- paste0(var, "_",subj,"SCORE", grade, "_TOTAL")
  varname_L <- paste0(var, "_",subj,"SCORE", grade, "_passed_L")
  varname_H <- paste0(var, "_",subj,"SCORE", grade, "_passed_H")
  # get estimate
  df[[varname_T]] <- as.numeric(df[[col_total]])
  df[[varname_L]] <- df[[varname_T]] * as.numeric(map_chr(df[[col_pct]], lowball))/100
  df[[varname_H]] <- df[[varname_T]] * as.numeric(map_chr(df[[col_pct]], highball))/100
  
  return(df)

}




# math --------------

# which grades to include? (choose from "03" to "08", "HS" = high school, and "00" = all)
grades <- str_pad(seq(4,8),2,"left","0") # grades

# which variables to include? (choose from list below) -------

# ALL	All students in the school
# MAM	American Indian/Alaska Native students
# MAS	Asian/Pacific Islander students
# MHI	Hispanic students
# MBL	Black students
# MWH	White students
# MTR	Two or More Races
# CWD	Children with disabilities (IDEA)
# ECD	Economically disadvantaged students
# LEP	Limited English proficient students
# F	Female students
# M	Male students
# HOM	Homeless enrolled students
# MIG	Migrant students

vars <-  c("ALL", "MBL", "MWH", "MHI", "MAS", "MAM")

# math
for(v in vars){
  for (g in grades) {
    schools_math <- get_estimate(schools_math, subj = "MTH",var = v, grade = g, year = "1617")
  }
}

# RLA
for(v in vars){
  for (g in grades) {
    schools_RLA <- get_estimate(schools_RLA, subj = "RLA", var = v, grade = g, year = "1617")
    
  }
}

school_scores <- full_join(schools_math, schools_RLA, by = c("STNAM", "FIPST","LEAID", "ST_LEAID","ST_SCHID","LEANM", "NCESSCH", "SCHNAM", "DATE_CUR"))%>%
  select(STNAM:DATE_CUR, matches("SCORE")) %>%
  
  # wide to long so that we can split var names 
  gather(var, value, 10:ncol(.)) %>%
  # split var names to subject, grade, race
  separate(var, c("var", "other"), sep = 3) %>%
  separate(other, c("subj", "other"), sep = 9) %>%
  separate(other, c("grade", "estimate"), sep = 2) %>%
  
  # long to wide so that we can collapse data by group
  spread(estimate, value)%>%
  # collapse grade and subject
  group_by(NCESSCH, var)%>%
  summarise(TOTAL_passed_L = sum(`_passed_L`, na.rm = T),
            TOTAL_passed_H = sum(`_passed_H`, na.rm = T),
            TOTAL_total = sum(`_TOTAL`, na.rm = T))%>%
  ungroup()%>%
  mutate(TOTALSCORE_L = TOTAL_passed_L / TOTAL_total,
         TOTALSCORE_H = TOTAL_passed_H / TOTAL_total ) %>%
  
  # wide to long so that we can add race category back in
  gather(col, value, 3:ncol(.))

# long to wide for tidy format to save
final <- school_scores %>%
  unite("var",var:col) %>%
  spread(var, value)

# Load crosswalks =================================
load("../metro.data/data-raw/NCESSCH2FIPS.rda")
# join coordinates to existing school data
final <- left_join(final, school_coords, by = "NCESSCH")
save(final, file = "data/school_scores.rda")

# county level summary
co_school_scores <- school_scores %>%
  filter(!grepl("TOTALSCORE", col))%>%
  left_join(school_coords, by = "NCESSCH")%>%
  mutate(stco_code = str_sub(fips, 1,5))%>%
  group_by(stco_code, var, col) %>% 
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  # long to wide
  unite("var", var:col)%>%
  spread(var, value) %>%
  left_join(metro.data::county_cbsa_st[c("stco_code", 'stco_name')], by = "stco_code")

get_pct_pass <- function(df, race){
  varname = paste0(race,"_", "pass")
  
  High = paste0(race, "_TOTAL_passed_H")
  Low = paste0(race, "_TOTAL_passed_H")
  Total = paste0(race, "_TOTAL_total")
    
  df[[varname]] <- (df[[High]] + df[[Low]])/df[[Total]]/2
  
  df
}


for (race in vars) {
  co_school_scores <- get_pct_pass(co_school_scores, race)
  }

  
save(co_school_scores, file = "data/co_school_scores.rda")

# [DEPRECIATED] urban data api
# devtools::install_github('UrbanInstitute/education-data-package-r')
# 
# library(educationdata)
# 
# data <- get_education_data(level = "schools",
#                            source = "edfacts",
#                            topic = "assessments",
#                            filters = list(year = 2016, grade_edfacts = 9, fips = "08"),
#                            by = list("race"))


