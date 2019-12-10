# Fayetteville

library(metro.data)
library(tidyverse)

find_cbsa_code("fayetteville")

target_cbsa <- "22220"
peer_cbsa <- c("12420","19780","20500","31540","39580")

load("../metro-dataset/metro_monitor_2019/metro_monitor_2019_allyear.rda")
load("../metro-dataset/acs5_2017/cbsa_acs.rda")

df <- cbsa_value %>%
  mutate(cbsa_code = as.character(cbsa))%>%
  left_join(cbsa_acs[c("cbsa_code", "cbsa_name", "pop_total")], by = "cbsa_code")


mm_mid <- df %>%
  filter(between(pop_total, 500000,1000000)) %>%
  # filter(year %in% c(2007, 2016, 2017)) %>%
  group_by(year)%>%
  # filter(!is.na(jobs))%>%
  summarise(
    # helper
    pop_total = sum(pop_total),
    pop_lf = sum(jobs/employment_to_population_ratio),
    annual_wage = sum(average_annual_wage*jobs),
    
    # sum
    employment_at_firms_0_5_years_old = sum(employment_at_firms_0_5_years_old, na.rm = T),
    jobs = sum(jobs, na.rm = T),
    real_gdp = sum(real_gdp, na.rm = T),
    
    # average
    average_annual_wage = annual_wage/jobs,
    output_per_job = real_gdp/jobs,
    output_per_person = real_gdp/pop_total,
    employment_to_population_ratio = jobs/pop_lf,
    
    # estimate median
    median_income = median(median_income),
    relative_income_poverty = median(relative_income_poverty),
    gap_in_employment_to_population_ratio = median(gap_in_employment_to_population_ratio),
    gap_in_median_income = median(gap_in_median_income),
    gap_in_relative_income_poverty = median(gap_in_relative_income_poverty)
  )%>%
  mutate(cbsa_name = "all mid-sized")

mm_peer <- df %>%
  filter(cbsa_code %in% c(target_cbsa,peer_cbsa)) %>%
  bind_rows(mm_mid)%>%
  filter(year %in% c(2007, 2016, 2017)) %>%
  select(cbsa_code, cbsa_name, everything(),-pop_total,-pop_lf,-annual_wage)

write.csv(mm_peer, "metro monitor_22220.csv")
write.csv(mm_mid, "metro monitor_midsize.csv")
