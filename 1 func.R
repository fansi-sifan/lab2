############################################
## template applies to all metros - FUNC and data ###
############################################

# SETUP =================================================

library(metro.data)
library(tidyverse)
library(data.table)
library(tidycensus)

source("../metro-dataset/census/SBO.R")
source("../metro-dataset/census/acs_var.R")

create_labels <- function(df) {
  sjlabelled::get_label(df) %>%
    data.frame() %>%
    mutate(names = colnames(df)) %>%
    rename("label" = ".")
}

# Who is exluded =====================================
# 1. No job
# out of work

get_oow <- function(target_cbsa, peer_cbsa) {
  load("../metro-dataset/out_of_work/co_oow.rda")
  code <- c(target_cbsa, peer_cbsa)
  
  oow <- co_oow %>%
    filter(cbsa_code %in% target_cbsa)
  
  df <- co_oow %>%
    filter(cbsa_code %in% code)%>%
    filter(grepl("population",population))
  
  oow_summary <- bind_cols(create_labels(co_oow), data.table::transpose(df))
  return(oow = list(peer = df, labeled = oow_summary, details = oow))
}


# 2. Bad job
# low wage
get_lww <- function(target_cbsa, peer_cbsa) {
  load("../metro-dataset/low_wage_worker/cbsa_low_wage_worker.rda")
  code <- c(target_cbsa, peer_cbsa)
  lww <- cbsa_low_wage_worker %>%
    filter(cbsa_code %in% target_cbsa)
  
  df <- cbsa_low_wage_worker %>%
    filter(cbsa_code %in% code)%>%
    filter(grepl("Low-wage workers|Workers",population))
  
  lww_summary <- bind_cols(create_labels(cbsa_low_wage_worker), data.table::transpose(df))
  return(lww = list(peer = df, labeled = lww_summary, details = lww))
}

# opportunity industries
get_opp <- function(target_cbsa, peer_cbsa) {
  # BA/2-yr but no good job
  load("../metro-dataset/opportunity industries/cbsa_oppind_race.rda")
  load("../metro-dataset/opportunity industries/cbsa_oppind.rda")
  code <- c(target_cbsa, peer_cbsa)
  
  opp <- cbsa_oppind_race %>%
    filter(cbsa_code %in% target_cbsa)
  
  df <- cbsa_oppind %>%
    filter(cbsa_code %in% code) 
  
  opp_summary <- opp %>%
    filter(grepl("Associate|college|Bacca", education)) %>%
    filter(race != "Total") %>%
    filter(age == "Total") %>%
    mutate(value = case_when(
      grepl("Associate|college", education) ~ as.numeric(gsub("[\\%,]", "", good_jobs)) + as.numeric(gsub("[\\%,]", "", promising_jobs)),
      grepl("Bacca", education) ~ as.numeric(gsub("[\\%,]", "", hi_good_jobs))
    )) %>%
    select(-contains("jobs"), -other) %>%
    spread(gender, value)
  
  return(opp = list(peer = df, labeled = opp_summary, details = opp))
}


# 3. Not able to start firm
# SBO race and gender gaps (ASE not available for Grand Rpaids MSA)


summarise_sbo <- function(var, df){
  var <- rlang::enquo(var)
  
  df %>%
    # group_by(state, county, GEO_TTL, !!var) %>%
    mutate(firmdemp = as.numeric(FIRMPDEMP),
           firmall = as.numeric(FIRMALL), 
           is.traded = NAICS2012 %in% traded_code) %>%
    group_by(!!var) %>%
    group_by(is.traded, add = T) %>%
    summarise(firmdemp = sum(firmdemp),
              firmall = sum(firmall))
}


get_sbo_m <- function(stco_code){
  
  # get data
  sbo_df <- purrr::map_df(stco_code, get_sbo_co)
  
  # summary table by each demographics
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
  
  sc_summary <- df %>%
    summarise_at(vars(contains("TOTAL")), sum)
  
  return(school = list(labeled = sc_summary, details = df))
}


# 5. Flat income
# housing

get_hiratio <- function(target_cbsa, peer_cbsa) {
  load("../metro-dataset/housing_price/cbsa_housing_price.rda")
  # load("../metro-dataset/housing_renter/cbsa_rental.rda")
  
  code <- c(target_cbsa, peer_cbsa)
  
  owner <- cbsa_housing_price %>%
    filter(cbsa_code %in% code)
  
  # renter <- cbsa_rental %>%
  #   filter(cbsa_code %in% code)
  
  hi_summary <- bind_cols(create_labels(df), data.table::transpose(df))
  
  return(hiratio = list(peer = df, labeled = hi_summary, details = cbsa_housing_price))
  
}


# Why they are excluded =========================

# 1. quality job creation
# a. get datasets
# read in xwalks
load("../metro.data/data/naics4_ai.rda")  
load("../metro.data/data/naics6_traded.rda")  

# read in opportunity job mix
opp_path <- "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/Shareable/"
tmp <- list.files(opp_path, full.names = T) 


path <- purrr::map(c(target_cbsa, peer_cbsa), function(x)grep(x, tmp,value = T)) %>%
  map_if(is.null, ~ NA_character_) %>%
  flatten_chr()

# denver_opp_raw <- "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/Shareable/19740 Denver CO BMPP Opportunity Industries - 2017 Job shares.xlsx"
# gr_opp_raw <-  "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/Shareable/24340 Grand Rapids MI BMPP Opportunity Industries - 2017 Job shares.xlsx"



naics6_opp <- purrr::map_dfr(path, function(x)readxl::read_xlsx(x, sheet = "METRO_INDUSTRY_2017") %>%
                               filter(str_length(NAICS) == 6) %>%
                               select(cbsa_code = CBSA, code_naics6_2017 = NAICS, pct_bad =`Other jobs`))


# Downloaded EMSI data ----------
# peer cbsa data
emsi_peers <- read_csv(path_emsi_peers) %>%
  mutate(cbsa_code = str_pad(Area,5,"left","0"))

# regional county level data 
emsi_region <- read_csv(path_emsi_region) %>%
  mutate(stco_code = str_pad(Area,5,"left","0"),
         cbsa_code = case_when(
           stco_code %in% target_co ~ target_cbsa_core)
  ) %>%
  group_by(cbsa_code, Industry, Year)%>%
  summarise(Jobs = sum(Jobs, na.rm = T))%>% 
  ungroup() %>%
  filter(!is.na(cbsa_code))

# merge everything together
emsi <- emsi_region %>%  
  bind_rows(emsi_peers) %>%
  mutate(code_naics6_2017 = str_pad(Industry, 6, "left","0"),
         code_naics4_2017 = str_sub(Industry,1,4)) %>%
  left_join(naics6_traded, by = "code_naics6_2017") %>%
  left_join(ai, by = "code_naics4_2017") %>%
  left_join(naics6_opp, by = c("code_naics6_2017", "cbsa_code"))

skimr::skim(emsi)

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
           net_jobs_1618 = `2018` - `2016`,
           quality_0810 = (1-pct_bad) *net_jobs_0810,
           quality_1012 = (1-pct_bad) *net_jobs_1012,
           quality_1214 = (1-pct_bad) *net_jobs_1214,
           quality_1416 = (1-pct_bad) *net_jobs_1416,
           quality_1618 = (1-pct_bad) *net_jobs_1618
    )%>%
    group_by(cbsa_code, cbsa_name)%>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    mutate(pct_quality_0810 = quality_0810/net_jobs_0810,
           pct_quality_1012 = quality_1012/net_jobs_1012,
           pct_quality_1214 = quality_1214/net_jobs_1214,
           pct_quality_1416 = quality_1416/net_jobs_1416,
           pct_quality_1618 = quality_1618/net_jobs_1618)
}



# EMSI 4/6 digit over time by traded, ai

get_ind <- function(region, peer, col){
  col = enquo(col)
  emsi %>%
    filter(cbsa_code %in% c(peer,region))%>%
    group_by(!!col, Year, cbsa_code, `Area Name`)%>%
    summarise(Jobs = sum(Jobs, na.rm = T)) %>%
    # unite("Year",!!col:Year)%>%
    spread(Year, Jobs)
}



# b. Misaligned training: demand for tech skill outpace supply (digital training)
# digitalization

get_digital <- function(metrocode, peer){
  load("../metro-dataset/digitalization/cbsa_digitalization.rda")
  cbsa_digitalization %>%
    filter(cbsa_code %in% c(metrocode,peer)) %>%
    select(cbsa_code, cbsa_name, contains("pct"))
}


# education fields
get_BAfields <- function(target_co){
  tidycensus::get_acs(geography = "county",
          variables = BA_field_codes,
          st = str_sub(target_co[[1]],1,2),
          key = KEY, output = "wide")%>% 
    filter(GEOID %in% target_co) %>%
    summarise_if(is.numeric, sum)
}



# b. job access difference for transit/car user
# Access Across America
load("../metro-dataset/access across america/cbsa_job_access.rda")
get_access <- function(metrocode,peer){
  cbsa_job_access%>%filter(
    cbsa_code%in%c(metrocode,peer)
  )
}


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
         year = as.integer(Year))%>%
  filter(between(year,2012,2016))%>%
  filter(investeetype == "BUS") %>%
  filter(purpose %in% c("BUSFIXED", "BUSINESS", "BUSWORKCAP", "MICRO", "OTHER"))%>%
  select(stco_code,stcotc_code,year,CDFI_amount = originalamount) 

FDIC <- loan_datafiles$FDIC_matched %>%
  mutate(stco_code = str_pad(county14,5,"left","0"),
         stcotc_code = paste0(State, county, gsub("\\.","",FIPS)), 
         year = as.integer(year)) %>%
  filter(between(year,2012,2016))%>%
  select(stco_code, stcotc_code,  year, FDIC_amount = x_tot)

# tract demography

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
  
  df <- get_pov(st) %>%
    filter(substr(stcotc_code,1,5)%in% metrocode)  %>%
    left_join(CDFI, by = "stcotc_code")%>%
    left_join(FDIC, by = c("stcotc_code", "year"))
  
  a <- df %>%
    group_by(is.minority)%>%
    summarise(CDFI_value = sum(CDFI_amount, na.rm = TRUE),
              FDIC_value = sum(FDIC_amount, na.rm = TRUE),
              pop = sum(pop_total,na.rm = TRUE))%>%
    mutate(pc_CDFI = CDFI_value/pop,
           pc_FDIC = FDIC_value/pop)
  
  b <- df %>%
    group_by(is.pov)%>%
    summarise(CDFI_value = sum(CDFI_amount, na.rm = TRUE),
              FDIC_value = sum(FDIC_amount, na.rm = TRUE),
              pop = sum(pop_total,na.rm = TRUE))%>%
    mutate(pc_CDFI = CDFI_value/pop,
           pc_FDIC = FDIC_value/pop)
  bind_rows(a,b)
  
}


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


chetty <- read_csv("V:/_metro_data_warehouse/data_raw/chetty opportunity/cz_outcomes.csv")
chetty_innovate <- read_csv("V:/_metro_data_warehouse/data_raw/chetty opportunity/patents_paper_code_and_data/code_and_data/data (public online tables)/excel/table_1a.csv")


# cost of exclusion ==================================
get_inventor <- function(peer_cz){
  chetty_innovate %>%
  # filter(par_czname %in% DV_peer_cz) %>%
  filter(par_czname %in% peer_cz) %>%
  select(par_cz, par_czname, par_state, 
         kid_count, inventor, 
         kid_count_g_m, inventor_g_m, 
         kid_count_g_f, inventor_g_f) 
}

get_upmob <- function(peer_cz){
  chetty %>%
  filter(czname %in% peer_cz)%>%
  select(cz, czname,
         kfr_top20_pooled_pooled_p75,
         kfr_top20_pooled_pooled_p25,

         kfr_black_pooled_p25,
         kfr_white_pooled_p25,
         kfr_hisp_pooled_p25,
         kfr_asian_pooled_p25,
         
         kfr_top20_black_pooled_p25,
         kfr_top20_white_pooled_p25,
         kfr_top20_hisp_pooled_p25,
         kfr_top20_asian_pooled_p25
  )
}




# edu by birth place ------------------------------

get_edu_birth <- function(target_co, peer_cbsa){
  
  
  load("../metro-dataset/census_edu_res/co_edu_res.rda")
  load("../metro-dataset/census_edu_res/cbsa_edu_res.rda")
  
  co_edu_res %>%
    filter(stco_code %in% target_co) %>%
    bind_rows(cbsa_edu_res %>%
                filter(cbsa_code %in% peer_cbsa)) %>%
    group_by(cbsa_code, cbsa_name, year)%>%
    summarise_if(is.numeric, sum) %>%
    mutate( pct_baplus_instate = baplus_instate/all_instate,
            pct_baplus_outstate = baplus_outstate/all_outstate,
            pct_baplus_fb = baplus_fb/all_fb,
            pct_baplus_total = baplus_total/all_total)
  
}


# demography trends -------------


get_age_race <- function(target_cbsa){
  load("../metro-dataset/millenials diversity/cbsa_agegroup_race.rda")
  
  cbsa_agegroup_race %>%
    filter(cbsa_code %in% target_cbsa) %>%
    group_by(race)%>%
    summarise_if(is.numeric, sum)
}
