library(metro.data)
library(dplyr)

# SETUP =================================================
source("helper.R")

# DATASETS ==============================================
# define geographies
MetroDenver_cbsa <- c("22660", "19740", "14500", "24540")
DV_cbsa <- "19740"
GR_cbsa <- "24340"

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

  sbo <- df %>%
    filter(RACE_GROUP %in% race_code) %>%
    filter(NAICS2012 %in% traded_code) %>%
    filter(ETH_GROUP == "001" & SEX == "001")

  sbo_summary <- sbo %>%
    group_by(state, county, RACE_GROUP) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP)) %>%
    summarise(firmdemp = sum(firmdemp)) %>%
    spread(RACE_GROUP, firmdemp) %>%
    rename(sbo_wh = `30`, sbo_bk = `40`, sbo_total = `96`) %>%
    mutate(pct_sbo_bk = sbo_bk / sbo_total)

  return(list(sbo_summary, sbo))
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


# GET ALL

oow <- get_oow(DV_cbsa)
yoow <- get_yoow(DV_cbsa)
lww <- get_lww(DV_cbsa)
opp <- get_opp(DV_cbsa)

DV_ct <- find_cbsa_counties("denver")$stco_code
sbo <- get_sbo(DV_ct[[7]])

hiratio <- get_hiratio(DV_cbsa)

# write result =================

openxlsx::write.xlsx(c(oow, yoow, lww, opp, sbo, hiratio), file = paste0(DV_cbsa,".xlsx"))


# Why are they excluded --------
# 1. Lack of quality job creation
# a. advanced industry trend
# b. share of jobs below living wage over time
# c. small firms and wage
# ASE


