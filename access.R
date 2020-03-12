source("func.R")
# Job access
# Access across america
# all states: https://conservancy.umn.edu/bitstream/handle/11299/200508/ALL_STATES_CSV_tr_2017_0700-0859.zip?sequence=105&isAllowed=y


# MI
mi_17 <- "https://conservancy.umn.edu/bitstream/handle/11299/200508/26_tr_2017_0700-0859.csv?sequence=128&isAllowed=y"
mi_15 <- "https://conservancy.umn.edu/bitstream/handle/11299/183801/26_tr_2015_0700-0859.csv?sequence=293&isAllowed=y"
mi_rac <- "../../LEHD LODES/rac_JT02_2017/mi_rac_JT02_2017.csv"
mi_od <- "../../LEHD LODES/od_JT02_2067/"
# CO state
co_17 <- "https://conservancy.umn.edu/bitstream/handle/11299/200508/08_tr_2017_0700-0859.csv?sequence=111&isAllowed=y"
co_15 <- "https://conservancy.umn.edu/bitstream/handle/11299/183801/08_tr_2015_0700-0859.csv?sequence=276&isAllowed=y"
co_rac <- "../../LEHD LODES/rac_JT02_2017/co_rac_JT02_2017.csv"

# merge, creat weighted average for customized region
get_access_nb <- function(county_codes, state){
  
  if (state == "co") {
    access <- read_csv(co_17)
    access_15 <- read_csv(co_15)
    rac <- read_csv(co_rac)
  } else if (state == "mi") {
    access <- read_csv(mi_17)
    access_15 <- read_csv(mi_15)
    rac <- read_csv(mi_rac)
  } else {break}
  
  tract_pov <- get_pov(state)
  
  access %>%
    left_join(access_15, by = "geoid")%>%
    mutate(geoid = as.character(geoid),
           stcobg_code = str_sub(str_pad(geoid,15,"left","0"),1,12),
           stcotc_code = str_sub(str_pad(geoid,15,"left","0"),1,11),
           stco_code = str_sub(str_pad(geoid,15,"left","0"),1,5))%>%
    left_join(rac %>% mutate(geoid = str_pad(h_geocode,15,"left","0")), by = "geoid") %>%
    filter(!is.na(C000))%>%
    # left_join(metro.data::county_cbsa_st[c("stco_code","cbsa_code")], by = "stco_code")%>%
    # group_by(cbsa_code)%>%
    # group_by(stco_code)%>%
    filter(stco_code %in% county_codes)%>%
    left_join(tract_pov, by = "stcotc_code")%>%
    group_by(is.pov, is.minority)%>%
    summarise(jobs.15 = sum(jobs.y * C000, na.rm = T)/sum(C000, na.rm = T),
              jobs.17 = sum(jobs.x * C000, na.rm = T)/sum(C000, na.rm = T))%>%
    mutate(change = jobs.17/jobs.15-1)
}
job_GR <- get_access_nb(GR_county, "mi")
job_DV <- get_access_nb(MetroDenvr_actual,"co")


# Creating aggregate measures: 
note <- 'Tract-level data are aggregated to create statistics for larger geographic areas 
like metropolitan areas, primary cities, or the nation as a whole. 
Aggregate figures represent the average number of "nearby" jobs for all tracts in the relevant area, 
weighted by tract population. Aggregate figures may also be thought of as the average number of jobs 
near the typical resident.'


# car ownership

load("../metro-dataset/acs5_2017/co_acs.rda") 
# load("../metro-dataset/acs5_2017/co_acs_raw.rda") 

load("../metro-dataset/acs5_2017/cbsa_acs.rda") 

cbsa_acs %>%
  filter(cbsa_code == GR_cbsa)%>%
  select(contains("public"), contains("commuter")) %>%
  View()

transit_race <- co_acs %>% 
  filter(stco_code %in% c(MetroDenvr_actual, GR_county))%>%
  # mutate(public_transit_black = pop_black*pct_publictrans_black,
  #        public_transit_white = pop_white*pct_publictrans_white,
  #        public_transit_latino = pop_latino*pct_publictrans_latino,
  #        public_transit_asian = pop_asian*pct_publictrans_asian,
  #        public_transit_total = pop_total*pct_publictrans_total) %>%
  select(stco_code, contains("public"), contains("commuter"))

tmp <- co_acs_raw %>%
  filter(stco_code %in% GR_county) %>%
  select(stco_code, contains("S0802_C04"),-contains("M"))

