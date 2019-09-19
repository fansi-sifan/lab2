library(metro.data)
library(censusapi)
library(tidyverse)

# SETUP =================================================

key <- Sys.getenv("CENSUS_API_KEY")

get_SBO <- function(...){
  getCensus(name = "2012/sbo",
            vars = c( "NAICS2012","NAICS2012_TTL",
                      "RACE_GROUP_TTL","RACE_GROUP", 
                      "ETH_GROUP","ETH_GROUP_TTL",
                      "SEX","SEX_TTL", "GEO_ID",
                      "FIRMALL", "FIRMPDEMP"),
            ...,
            key = key)
}


create_labels <- function(df) {
  sjlabelled::get_label(df) %>%
    data.frame() %>%
    mutate(names = colnames(df)) %>%
    rename("label" = ".")
}

# DATASETS ==============================================
# define geographies
MetroDenver_cbsa <- c("22660", "19740", "14500", "24540")

# MetroDenvr_approx <- (county_cbsa_st %>%
#                         filter(cbsa_code %in% MetroDenver_cbsa))$stco_code

MetroDenvr_actual <- (county_cbsa_st %>%
                        filter(cbsa_code %in% MetroDenver_cbsa) %>%
                        filter(!stco_code %in% c("08039", "08093", "08019", "08047")))$stco_code


DV_cbsa <- c("19740")
GR_cbsa <- "24340"
GR_county <- (county_cbsa_st%>%filter(cbsa_code%in%GR_cbsa))$stco_code

# Who is exluded ---------
# 1. No job
# out of work

get_oow <- function(code) {
  load("../metro-dataset/out_of_work/co_oow.rda")

  oow <- co_oow %>%
    filter(cbsa_code %in% code)

  df <- oow %>%
    filter(population == "Sample population")

  oow_summary <- bind_cols(create_labels(co_oow), data.table::transpose(df))
  return(list(oow_summary, oow))
}

get_yoow <- function(code) {
  # young out of work
  load("../metro-dataset/out_of_work/co_oow_young.rda")

  yoow <- co_oow_young %>%
    filter(cbsa_code %in% code)

  df <- yoow %>%
    filter(population == "Out-of-work population")

  yoow_summary <- bind_cols(create_labels(co_oow_young), data.table::transpose(df))
  return(list(yoow_summary, yoow))
}


# 2. Bad job
# low wage
get_lww <- function(code) {
  load("../metro-dataset/low_wage_worker/cbsa_low_wage_worker.rda")

  lww <- cbsa_low_wage_worker %>%
    filter(cbsa_code %in% code)

  df <- lww %>%
    filter(population == "Low-wage workers")


  lww_summary <- bind_cols(create_labels(cbsa_low_wage_worker), data.table::transpose(df))
  return(list(lww_summary,lww))
}

# opportunity industries
get_opp <- function(code) {
  # BA/2-yr but no good job
  load("../metro-dataset/opportunity industries/cbsa_oppind_race.rda")
  load("../metro-dataset/opportunity industries/cbsa_oppind.rda")

  opp <- cbsa_oppind_race %>%
    filter(cbsa_code %in% code)

  opp_summary <- cbsa_oppind %>%
    filter(cbsa_code %in% code) 

  return(list(opp_summary, opp))
}


# 3. Not able to start firm
# SBO race and gender gaps (ASE not available for Grand Rpaids MSA)
# df <- get_SBO(region = "county:081", regionin  = "state:26")

# get_SBO(region = "us:*")

get_sbo <- function(stco_code) {
  co_code <- paste0("county:",str_sub(stco_code, 3, 5))
  st_code <- paste0("state:",str_sub(stco_code, 1, 2))

  df <- get_SBO(region = co_code, regionin = st_code)

  traded_code <- c("31-33", "51", "52", "54")
  race_code <- c("96", "30", "40")

  df %>%
    # filter(RACE_GROUP %in% race_code) %>%
    filter(NAICS2012 %in% traded_code) %>%
    filter(ETH_GROUP == "001" & SEX == "001")

}

get_sbo_m <- function(stco_code){
  sbo_df <- purrr::map_df(stco_code, get_sbo)
  
  sbo_summary <- sbo_df %>%
    group_by(state, county, RACE_GROUP) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP)) %>%
    summarise(firmdemp = sum(firmdemp)) %>%
    spread(RACE_GROUP, firmdemp) %>%
    rename(white = `30`, black = `40`, asian = `60`,other = `80`, all = `96`) %>%
    mutate(pct_sbo_bk = black/all, 
           pct_sbo_wh = white/all)
  
  return(list(sbo_summary, sbo_df))
}


# 4. high school compeletion


# 5. Flat income
# housing

get_hiratio <- function(code) {
  load("../../metro_data_warehouse/data_final/housing_price/cbsa_housing_price.rda")

  df <- cbsa_housing_price %>%
    filter(cbsa_code %in% code)
  
  hi_summary <- bind_cols(create_labels(df), data.table::transpose(df))
  return(list(hi_summary, cbsa_housing_price))
  
}



# Why are they excluded --------
# 1. Lack of quality job creation
# a. advanced industry trend
# b. share of jobs below living wage over time
# c. small firms and wage
# ASE


