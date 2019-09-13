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
load("../metro-dataset/out_of_work/co_oow.rda")

oow <- co_oow %>%
  filter(cbsa_code %in% DV_cbsa)

df <- oow %>%
  filter(population == "Sample population")

oww_summary <- bind_cols(create_labels(df),data.table::transpose(df))

# young out of work
load("../metro-dataset/out_of_work/co_oow_young.rda")

yoow <- co_oow_young %>%
  filter(cbsa_code %in% DV_cbsa)

df <- yoow %>%
  filter(population == "Out-of-work population")

yoww_summary <- bind_cols(create_labels(df),data.table::transpose(df))


# 2. Bad job
# low wage
load("../metro-dataset/low_wage_worker/cbsa_low_wage_worker.rda")

lww <- cbsa_low_wage_worker %>%
  filter(cbsa_code %in% GR_cbsa) 

df <- lww %>%
  filter(population == "Low-wage workers")


lww_summary <- bind_cols(create_labels(df),data.table::transpose(df))
  
# opportunity industries
# BA/2-yr but no good job
load("../metro-dataset/opportunity industries/cbsa_oppind_race.rda")
load("../metro-dataset/opportunity industries/cbsa_oppind.rda")

opp <- cbsa_oppind_race %>%
  filter(cbsa_code %in% GR_cbsa)

opp_summary <- cbsa_oppind %>%
  filter(cbsa_code %in% GR_cbsa)


#3. Not able to start firm
# SBO race and gender gaps (ASE not available for Grand Rpaids MSA)
df <- get_SBO(region = "county:081", regionin  = "state:26")
df <- get_SBO(region = "county:031", regionin  = "state:08")
#get_SBO(region = "us:*")

traded_code <- c("31-33", "51", "52", "54")
race_code <- c("96", "30", "40")

sbo <- df %>%
  filter(RACE_GROUP %in% race_code)%>%
  filter(NAICS2012 %in% traded_code)%>%
  filter(ETH_GROUP == "001"& SEX == "001")

sbo_summary <- sbo %>%
  group_by(state, county, RACE_GROUP) %>%
  mutate(firmdemp = as.numeric(FIRMPDEMP))%>%
  summarise(firmdemp  = sum(firmdemp)) %>%
  spread(RACE_GROUP, firmdemp) %>%
  rename(sbo_wh = `30`,sbo_bk = `40`, sbo_total = `96`) %>%
  mutate(pct_sbo_bk = sbo_bk/sbo_total)


#4. high school compeletion



#5. Flat income
# housing
load("../../metro_data_warehouse/data_final/housing_price/cbsa_housing_price.rda")

cbsa_housing_price %>%
  filter(cbsa_code %in% DV_cbsa)

cbsa_housing_price %>%
  filter(cbsa_code %in% GR_cbsa)


# Why are they excluded --------
# 1. Lack of quality job creation
# a. advanced industry trend
# b. share of jobs below living wage over time
# c. small firms and wage
# ASE

