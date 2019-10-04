# Job access
library(tidycensus)
KEY <- Sys.getenv("CENSUS_API_KEY")

# all states: https://conservancy.umn.edu/bitstream/handle/11299/200508/ALL_STATES_CSV_tr_2017_0700-0859.zip?sequence=105&isAllowed=y

# MI state
access <- read_csv("https://conservancy.umn.edu/bitstream/handle/11299/200508/26_tr_2017_0700-0859.csv?sequence=128&isAllowed=y")
access_15 <- read_csv("https://conservancy.umn.edu/bitstream/handle/11299/183801/26_tr_2015_0700-0859.csv?sequence=293&isAllowed=y")
# block group workers
mi_rac <- read_csv("../../LEHD LODES/rac_JT02_2017/mi_rac_JT02_2017.csv")

# merge, creat weighted average for customized region
access_worker <- access %>%
  left_join(access_15, by = "geoid")%>%
  mutate(stcobg_code = str_sub(str_pad(geoid,15,"left","0"),1,12),
         stco_code = str_sub(str_pad(geoid,15,"left","0"),1,5))%>%
  left_join(mi_rac, by = c("geoid"="h_geocode")) %>%
  filter(!is.na(C000))%>%
  left_join(metro.data::county_cbsa_st[c("stco_code","cbsa_code")], by = "stco_code")%>%
  group_by(cbsa_code)%>%
  # group_by(stco_code)%>%
  summarise(jobs.15 = sum(jobs.y * C000, na.rm = T)/sum(C000, na.rm = T),
            jobs.17 = sum(jobs.x * C000, na.rm = T)/sum(C000, na.rm = T))%>%
  mutate(change = jobs.17/jobs.15-1)
  
         
# Creating aggregate measures: 
note <- 'Tract-level data are aggregated to create statistics for larger geographic areas 
like metropolitan areas, primary cities, or the nation as a whole. 
Aggregate figures represent the average number of "nearby" jobs for all tracts in the relevant area, 
weighted by tract population. Aggregate figures may also be thought of as the average number of jobs 
near the typical resident.'


# 

