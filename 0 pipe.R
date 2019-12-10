#####################################
## template applies to all - FUNC ###
#####################################

library(metro.data)
library(censusapi)
library(tidyverse)
library(data.table)

# SETUP =================================================

key <- Sys.getenv("CENSUS_API_KEY")

# df <- get_SBO(region = "metropolitan statistical area/micropolitan statistical area:24340")
# df <- df %>%
#   # filter(RACE_GROUP %in% race_code) %>%
#   filter(NAICS2012 %in% traded_code) %>%
#   filter(ETH_GROUP == "001" & SEX == "001")

get_SBO <- function(...){
  getCensus(name = "2012/sbo",
            vars = c( "NAICS2012","NAICS2012_TTL",
                      "RACE_GROUP_TTL","RACE_GROUP", 
                      "ETH_GROUP","ETH_GROUP_TTL",
                      "SEX","SEX_TTL", "GEO_ID","GEO_TTL","VET_GROUP", "VET_GROUP_TTL",
                      "FIRMALL", "FIRMPDEMP", "FIRMPDEMP_S", "FIRMPDEMP_F", "FIRMPDEMP_S_F"),
            ...,
            key = key)
}

create_labels <- function(df) {
  sjlabelled::get_label(df) %>%
    data.frame() %>%
    mutate(names = colnames(df)) %>%
    rename("label" = ".")
}

# Who is exluded ---------
# 1. No job
# out of work

get_oow <- function(metrocode, peercode) {
  load("../metro-dataset/out_of_work/co_oow.rda")
  code <- c(metrocode, peercode)
  
  oow <- co_oow %>%
    filter(cbsa_code %in% metrocode)
  
  df <- co_oow %>%
    filter(cbsa_code %in% code)%>%
    filter(grepl("population",population))
  
  oow_summary <- bind_cols(create_labels(co_oow), data.table::transpose(df))
  return(oow = list(peer = df, labeled = oow_summary, details = oow))
}


# 2. Bad job
# low wage
get_lww <- function(metrocode, peercode) {
  load("../metro-dataset/low_wage_worker/cbsa_low_wage_worker.rda")
  code <- c(metrocode, peercode)
  lww <- cbsa_low_wage_worker %>%
    filter(cbsa_code %in% metrocode)
  
  df <- cbsa_low_wage_worker %>%
    filter(cbsa_code %in% code)%>%
    filter(grepl("Low-wage workers|Workers",population))
  
  lww_summary <- bind_cols(create_labels(cbsa_low_wage_worker), data.table::transpose(df))
  return(lww = list(peer = df, labeled = lww_summary, details = lww))
}

# opportunity industries
get_opp <- function(metrocode, peercode) {
  # BA/2-yr but no good job
  load("../metro-dataset/opportunity industries/cbsa_oppind_race.rda")
  load("../metro-dataset/opportunity industries/cbsa_oppind.rda")
  code <- c(metrocode, peercode)
  
  opp <- cbsa_oppind_race %>%
    filter(cbsa_code %in% metrocode)
  
  df <- cbsa_oppind %>%
    filter(cbsa_code %in% code) 
  
  opp_summary <- data.table(transpose(df))
  
  return(opp = list(peer = df, labeled = opp_summary, details = opp))
}


# 3. Not able to start firm
# SBO race and gender gaps (ASE not available for Grand Rpaids MSA)
# df <- get_SBO(region = "county:081", regionin  = "state:26")

# get_SBO(region = "us:*")

get_sbo <- function(stco_code) {
  co_code <- paste0("county:",str_sub(stco_code, 3, 5))
  st_code <- paste0("state:",str_sub(stco_code, 1, 2))
  
  get_SBO(region = co_code, regionin = st_code)
  
  # traded_code <- c("31-33", "51", "52", "54")
  # race_code <- c("96", "30", "40")
  # 
  # df %>%
  #   # filter(RACE_GROUP %in% race_code) %>%
  #   filter(NAICS2012 %in% traded_code) %>%
  #   filter(SEX == "001")
  
}

summarise_sbo <- function(var, df){
  var <- rlang::enquo(var)
  
  df %>%
    # group_by(state, county, GEO_TTL, !!var) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP),
           firmall = as.numeric(FIRMALL)) %>%
    group_by(!!var)%>%
    summarise(firmdemp = sum(firmdemp),
              firmall = sum(firmall))
}


get_sbo_m <- function(stco_code){
  sbo_df <- purrr::map_df(stco_code, get_sbo)
  summarise_sbo(RACE_GROUP_TTL, sbo_df)
  return(list(race = summarise_sbo(RACE_GROUP_TTL, sbo_df),
              ethnicity = summarise_sbo(ETH_GROUP_TTL, sbo_df),
              vet = summarise_sbo(VET_GROUP_TTL, sbo_df),
              sex = summarise_sbo(SEX_TTL, sbo_df)))
}



# 4. School proficiency 

get_school <- function(code){
  load("../metro-dataset/school proficiency/co_school_proficiency.rda")
  
  df <- co_school_proficiency %>%
    filter(stco_code %in% code)
  
  return(school = list(labeled = df))
}


# 5. Flat income
# housing

get_hiratio <- function(metrocode, peercode) {
  load("../metro-dataset/housing_price/cbsa_housing_price.rda")
  code <- c(metrocode, peercode)
  
  df <- cbsa_housing_price %>%
    filter(cbsa_code %in% code)
  
  hi_summary <- bind_cols(create_labels(df), data.table::transpose(df))
  
  return(hiratio = list(peer = df, labeled = hi_summary, details = cbsa_housing_price))
  
}

