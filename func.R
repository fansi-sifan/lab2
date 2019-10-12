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
                      "SEX","SEX_TTL", "GEO_ID","GEO_TTL",
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

# DATASETS ==============================================
# define geographies
MetroDenver_cbsa <- c("22660", "19740", "14500", "24540")
Denverpeer_cbsa <- c("12420", "33460", "38060","38900", "41740", "41620", "42660")
GRpeer_cbsa <- c("14260","17140","17460","24860","26900","33340","33460","34980","41620","46140","48620")

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

  df <- get_SBO(region = co_code, regionin = st_code)

  traded_code <- c("31-33", "51", "52", "54")
  race_code <- c("96", "30", "40")

  df %>%
    # filter(RACE_GROUP %in% race_code) %>%
    filter(NAICS2012 %in% traded_code) %>%
    filter(SEX == "001")

}

get_sbo_m <- function(stco_code){
  sbo_df <- purrr::map_df(stco_code, get_sbo)
  
  sbo_summary_race <- sbo_df %>%
    group_by(state, county, GEO_TTL, RACE_GROUP_TTL) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP)) %>%
    summarise(firmdemp = sum(firmdemp)) %>%
    spread(RACE_GROUP_TTL, firmdemp) 
  
  sbo_summary_ethnicity <- sbo_df %>%
    group_by(state, county, GEO_TTL, ETH_GROUP_TTL) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP)) %>%
    summarise(firmdemp = sum(firmdemp)) %>%
    spread(ETH_GROUP_TTL, firmdemp)
  
  return(sbo = list(race = sbo_summary_race, ethnicity = sbo_summary_ethnicity, details = sbo_df))
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



# Why are they excluded --------
# 1. Lack of quality job creation
# a. advanced industry/tradable trend

# readin xwalks

load("../metro.data/data/naics4_ai.rda")  
load("../metro.data/data/naics6_traded.rda")  
naics6_opp <- readxl::read_xlsx("V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/Shareable/19740 Denver CO BMPP Opportunity Industries - 2017 Job shares.xlsx", 
                                sheet = "METRO_INDUSTRY_2017") %>%
  filter(str_length(NAICS) == 6) %>%
  select(code_naics6_2017 = NAICS, pct_bad=`Other jobs`)


# Downloaded EMSI data
# peer cbsa data
emsi_peers <- read_csv("data/Emsi_2019.3_ind_data.csv") %>%
  mutate(cbsa_code = str_pad(Area,5,"left","0"))

# regional county level data 
emsi_region <- read_csv("data/Emsi_2019.4_ind_data.csv") %>%
  mutate(stco_code = str_pad(Area,5,"left","0"),
         cbsa_code = case_when(
           stco_code %in% MetroDenvr_actual ~ "Metro Denver",
           stco_code %in% GR_county ~ "Grand Rapids - Kentwood"
         )) %>%
  filter(!is.na(cbsa_code))%>%
  group_by(cbsa_code, Industry, Year)%>%
  summarise(Jobs = sum(Jobs, na.rm = T))%>% ungroup()

# merge everything together
emsi <- emsi_region %>%  
  bind_rows(emsi_peers) %>%
  mutate(code_naics6_2017 = str_pad(Industry, 6, "left","0"),
         code_naics4_2017 = str_sub(Industry,1,4)) %>%
  left_join(naics6_traded, by = "code_naics6_2017") %>%
  left_join(ai, by = "code_naics4_2017") %>%
  left_join(naics6_opp, by = "code_naics6_2017")

skimr::skim(emsi)
# EMSI 4/6 digit over time

get_ind <- function(region, peer, col){
  col = enquo(col)
  emsi %>%
    filter(cbsa_code %in% c(peer,region))%>%
    group_by(!!col, Year, cbsa_code, `Area Name`)%>%
    summarise(Jobs = sum(Jobs, na.rm = T)) %>%
    # unite("Year",!!col:Year)%>%
    spread(Year, Jobs)
  }


# b. share of new jobs below living wage over time
get_netjobs <- function(region, peer){
  emsi %>%
    ungroup()%>%
    filter(cbsa_code %in% c(peer,region))%>%
    select(cbsa_code, cbsa_name = `Area Name`, code_naics6_2017,pct_bad,Year,Jobs)%>%
    spread(Year, Jobs)%>%
    mutate(net_jobs_0810 = `2010` - `2008`,
           net_jobs_1012 = `2012` - `2010`,
           net_jobs_1214 = `2014` - `2012`,
           net_jobs_1416 = `2016` - `2014`,
           net_jobs_1618 = `2018` - `2016`
           )%>%
    group_by(cbsa_code, cbsa_name)%>%
    summarise(pct_quality_0810 = 1- weighted.mean(pct_bad,net_jobs_0810, na.rm =T),
              pct_quality_1012 = 1- weighted.mean(pct_bad,net_jobs_1012, na.rm =T),
              pct_quality_1214 = 1- weighted.mean(pct_bad,net_jobs_1214, na.rm =T),
              pct_quality_1416 = 1- weighted.mean(pct_bad,net_jobs_1416, na.rm =T),
              pct_quality_1618 = 1- weighted.mean(pct_bad,net_jobs_1618, na.rm =T))
}


# c. small firms and wage
# ASE

# 2. Lack of job preparation
# a. high school dropouts and stopouts ----------------

# source("../metro-dataset/census/clean_acs.R")
# 
# tmp <- clean_acs(geography = "county", 
#           variables = map_chr(str_pad(seq(1,29),3,"left",0), function(x)paste0("B14005_",x,"E")), 
#           county = "035", state = "CO", 
#           year = 2017, span = 5,
#           key = Sys.getenv("CENSUS_API_KEY"),
#           short = T) 
# 
# # no hs degree, not enrolled, not working 
# tmp %>%
#  select (hs_drop_unemp_m = unemployed_estimate_2,
#          hs_drop_emp_m = employed_estimate_2,
#          hs_drop_nlf_m = not_in_labor_force_estimate_2,
#          
#          hs_drop_unemp_f = unemployed_estimate_5,
#          hs_drop_emp_f = employed_estimate_5,
#          hs_drop_nlf_f = not_in_labor_force_estimate_5,
#          
#          total_1619_m = male_estimate,
#          total_1619_f = female_estimate) %>%
#   mutate()

# b. Misaligned training: demand for tech skill outpace supply (digital training)
# digitalization

get_digital <- function(metrocode, peer){
  load("../metro-dataset/digitalization/cbsa_digitalization.rda")
  cbsa_digitalization %>%
    filter(cbsa_code %in% c(metrocode,peer)) %>%
    select(cbsa_code, cbsa_name, contains("pct"))
}

# c. decline in employer-provided training

# 3. Lack of job access
# a. Job sprawl
# natalie

# b. job access difference for transit/car user
# Access Across America
load("../metro-dataset/access across america/cbsa_job_access.rda")
get_access <- function(metrocode,peer){
  cbsa_job_access%>%filter(
    cbsa_code%in%c(metrocode,peer)
  )
}
# source("access.R")

# job density
load("../metro-dataset/job_density/co_jobdensity.rda")
load("../metro-dataset/job_density/cbsa_job_density_expected.rda")

get_density <- function(metrocode,peer){
  co_jobdensity %>%
  filter(cbsa_code %in% c(metrocode,peer))%>%
  spread(year,county_jobdensity) %>%
    left_join(cbsa_job_density_expected %>%
                filter(measure == "expected jobs per square mile")%>%
                  filter(cbsa_code %in% c(metrocode,peer))%>%
                  mutate(expected_0415 = density2015/density2004-1)%>%
                  select(cbsa_code, expected_0415), by = "cbsa_code")
}

# 4. Lack of capital
# a. CDFI 
load("../Birmingham/County Cluster/Temp data/SBA_loan_cleaned.Rda")

CDFI <- loan_datafiles$TLR_matched %>%
  mutate(stco_code = str_pad(county14,5,"left","0"),
         stcotc_code = FIPS,
         program = "CDFI")%>%
  filter(between(Year,2012,2016))%>%
  filter(investeetype == "BUS") %>%
  filter(purpose %in% c("BUSFIXED", "BUSINESS", "BUSWORKCAP", "MICRO", "OTHER"))%>%
  select(stco_code,stcotc_code, program, year = Year,amount = originalamount) 

FDIC <- loan_datafiles$FDIC_matched %>%
  mutate(stco_code = str_pad(county14,5,"left","0"),
         stcotc_code = paste0(State, county, gsub("\\.","",FIPS)), 
         year = as.integer(year),
         program = "FDIC") %>%
  filter(between(year,2012,2016))%>%
  select(stco_code, stcotc_code, program, year, amount = x_tot)

# tract demography
library(tidycensus)
KEY <- Sys.getenv("CENSUS_API_KEY")
source("../metro-dataset/census/acs_var.R")

get_pov <- function(st){
  bind_cols(
    get_acs("tract", state = st,variables = pov_race_codes, output = "wide"),
    get_acs("tract", state = st,variables = pop_race_codes, output = "wide"))%>%
    calculate_pov_race() %>%
    calculate_pop_race() %>%
    mutate(is.pov = pct_belowpoverty_total >= 0.2,
           is.minority = pct_white <=0.5) %>%
    select(stcotc_code = GEOID, pop_total,is.pov, is.minority)
}

get_loan <- function(metrocode, st){
  
  tract_pov <- get_pov(st) %>%
    filter(substr(stcotc_code,1,5)%in% metrocode)
  
  a <- map_dfr(list(CDFI,FDIC), function(df){
    df%>%filter(stco_code %in% metrocode)%>%
      full_join(tract_pov, by = "stcotc_code")%>%
      group_by(is.minority)%>%
      summarise(value = sum(amount, na.rm = TRUE),
                pop = sum(pop_total,na.rm = TRUE))%>%
      mutate(pc = value/pop)
  }) 
  
  b <- map_dfr(list(CDFI,FDIC), function(df){
    df%>%filter(stco_code %in% metrocode)%>%
      full_join(tract_pov, by = "stcotc_code")%>%
      group_by(program, is.pov)%>%
      summarise(value = sum(amount, na.rm = TRUE),
                pop = sum(pop_total,na.rm = TRUE))%>%
      mutate(pc = value/pop)
  }) 
  
  bind_rows(a,b)
  
}

get_loan(GR_county,"mi")


# b. SBIR
load("../Birmingham/County Cluster/SSTR_cleaned.rda")


get_SBIR <- function(metrocode){
  
  df <- SSTR_blog %>%
    rename(stco_code = stco_fips) %>%
    filter(stco_code %in% metrocode)
  
  # companies
  details <- df %>%
    group_by(stco_code,name, Hub, gender, disadv)%>%
    summarise(count = n(),
              amt = sum(amt,na.rm = T)) %>%
    ungroup()%>%
    mutate(count_pct = count/sum(count),
           amt_pct = amt/sum(amt))%>%
    arrange(-amt_pct)%>%
    mutate(cumamt = cumsum(amt_pct))
  
  # summary
  summary <- df %>%
    group_by(stco_code)%>%
    summarise(cbsa_count = n(),
              cbsa_amt = sum(amt,na.rm = T))%>%
    left_join(county_cbsa_st[c("stco_code","co_emp")], by = "stco_code")%>%
    ungroup()%>%
    summarise(cbsa_emp = sum(co_emp, na.rm = T),
              cbsa_count_per_emp = sum(cbsa_count, na.rm = T)/cbsa_emp,
              cbsa_amt_per_emp = sum(cbsa_amt, na.rm = T)/cbsa_emp)
  
  # demography by year
  demo <- df %>%
    group_by(year) %>%
    summarise(year_count = n(),
              year_amt = sum(amt, na.rm = T),
              year_hub = mean(Hub,na.rm = T),
              year_gender = mean(gender, na.rm = T),
              year_disadv = mean(disadv, na.rm = T),
              year_hub_wt = weighted.mean(Hub,amt,na.rm = T),
              year_gender_wt = weighted.mean(gender, amt, na.rm = T),
              year_disadv_wt = weighted.mean(disadv, amt,na.rm = T))%>%
    ungroup()%>%
    mutate(year_pct_count = year_count/sum(year_count),
           year_pct_amt = year_amt/sum(year_amt))%>%
    arrange(-year)
  
  return(list(details = details, summary = summary, demo = demo))
  
}

# 5. Lack of supportive social network
# Chetty 
# Mike Lee: social capital project

# 6. Employer practices

