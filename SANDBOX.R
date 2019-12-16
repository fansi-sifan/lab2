# education outcome
pseof_co <- read_csv('data/pseof_co.csv')
# codebook "https://lehd.ces.census.gov/data/schema/V4.5.0-draft/lehd_public_use_schema.html#_pseo"
tmp <- pseof_co %>% filter(agg_level_pseo == 40)%>%
  select(-contains("status"))%>%
  group_by(agg_level_pseo, degree_level, cipcode)%>%
  summarise_if(is.numeric,sum, na.rm = T)%>%
  mutate(pct_y1_instate = y1_grads_emp_instate/y1_grads_emp,
         pct_y5_instate = y5_grads_emp_instate/y5_grads_emp,
         pct_y10_instate = y10_grads_emp_instate/y10_grads_emp)

pseoe_co <- read_csv('data/pseoe_co.csv')
skimr::skim(pseoe_co)

tmp <- pseof_co %>% filter(agg_level_pseo == 42)%>%
  select(-contains("status"))%>%
  mutate(cipcode = str_sub(cipcode,1,2))%>%
  group_by(agg_level_pseo, degree_level, cipcode)%>%
  {left_join(
    summarise_at(., vars(contains("earnings")), list(mean = mean, sd = sd), na.rm = T),
    summarise_at(., vars(contains("grads")), sum, na.rm = T)
  )} %>%
  filter(y1_grads_earn > 0)

tmp <- pseof_co %>% filter(agg_level_pseo == 38)%>%
  select(-contains("status"))%>%
  group_by(agg_level_pseo, degree_level)%>%
  {left_join(
    summarise_at(., vars(contains("earnings")), list(mean = mean, sd = sd), na.rm = T),
    summarise_at(., vars(contains("grads")), sum, na.rm = T)
  )} %>%
  filter(y1_grads_earn > 0)



# Appendix ==================================================
# GR temp workers ------------------------------------------------


tmp_worker_GR <- read_csv("data/Staffing_Patterns_9552.csv")

tmp_occ_GR <- tmp_worker_GR %>%
  mutate(soc2_code = paste0(substr(SOC,1,2),"-0000"))%>%
  group_by(soc2_code)%>%
  summarise(job_2010 = sum(`Employed in Industry (2010)`, na.rm = T),
            job_2018 = sum(`Employed in Industry (2018)`, na.rm = T)) %>%
  mutate(job_1018 = job_2018 - job_2010) %>%
  left_join(metro.data::soc2_18, by = "soc2_code")

write.csv(tmp_occ_GR,"GR_tmp occ.csv")

# race and gender industry ------------------------------------------------


library(tidyverse)
emsi <- read_csv("data/Industry_Table_8257.csv") %>%
  mutate(code_naics6_2017 = as.character(NAICS))

skimr::skim(emsi)

# denver sectors
MDEDC <- read_csv("data/MDEDC.csv") %>%
  mutate(code_naics6_2017 = as.character(`NAICS Code*`),
         MDEDC = T) %>%
  select(code_naics6_2017, name_cluster = Cluster, MDEDC)

skimr::skim(MDEDC)
summary(as.factor(MDEDC$Cluster))

load("../metro.data/data/naics4_ai.rda")  
load("../metro.data/data/naics6_traded.rda")  


# ANALYSIS ==================================================

# merge everything
ind_race <- plyr::join_all(list(emsi, MDEDC,naics6_traded), by = "code_naics6_2017") %>%
  mutate(code_naics4_2017 = substr(code_naics6_2017, 1,4)) %>%
  left_join(ai, by = "code_naics4_2017") %>%
  mutate(is.ai = (is.ai == 1),
         is.MDEDC = (MDEDC == 1),
         is.traded = (traded.naics6 == "Traded")) %>%
  # group_by(is.ai)%>%
  select(contains("code"), name_cluster, contains("is."), total = `2018 Jobs`, wage = `Avg. Earnings Per Job`,
         Males, Females, white = `White`, black = `Black or African American`, latino = `Hispanic or Latino`, asian = Asian) %>%
  mutate(wage = wage*total)%>%
  filter(wage > 0)

skimr::skim(ind_race)

# summary table
result <- ind_race %>%
  group_by(is.ai)%>%
  summarise_if(is.numeric, sum) %>%
  bind_rows(ind_race %>%
              group_by(is.traded)%>%
              summarise_if(is.numeric, sum))%>%
  bind_rows(ind_race %>%
              group_by(is.MDEDC)%>%
              summarise_if(is.numeric, sum))%>%
  bind_rows(ind_race %>%
              group_by(is.MDEDC, name_cluster)%>%
              summarise_if(is.numeric, sum))%>%
  mutate(mean_wage = wage/total) %>%
  select(is.ai, is.traded, is.MDEDC, name_cluster, total, mean_wage, everything(),-wage)

# OUTPUT ======================================================
write.csv(result, "ind_race_MetroDenver.csv")
openxlsx::write.xlsx(result,"ind_race_MetroDenver.xlsx")


# Denver SBA =====================================================


# SETUP ==============================
library(tidyverse)

# Read in raw datasets, fill first column
sba_denver_18 <- read_csv("data/sba_denver_18.csv")%>%
  # select(-contains("X")) %>%
  fill(COUNTY, .direction = "down")

# EDA =================================
skimr::skim(sba_denver_18)
summary(as.factor(sba_denver_18$COUNTY))
summary(as.factor(sba_denver_18$ETHNIC))
summary(as.factor(sba_denver_18$GENDER))
summary(as.factor(sba_denver_18$`Vet Status`))

# Take out totals
sba_summary <- sba_denver_18 %>%
  filter(is.na(ETHNIC))

sba_details <- sba_denver_18 %>%
  filter(!is.na(ETHNIC))%>%
  mutate(count = 1)

# FUNCTIONS ============================
get_breakdowns <- function(df){
  df %>%
    summarise_if(is.numeric, sum) %>%
    ungroup()%>%
    mutate(pct_loan_amt = `Loan Amount`/sum(`Loan Amount`),
           pct_loan_count = count/sum(count),
           mean_loan_amt = `Loan Amount`/count,
           
           jobs_created_per_award = `Jobs Created`/count,
           jobs_retained_per_award = `Jobs Retained`/count,
           jobs_created_per_mil = `Jobs Created`/`Loan Amount` * 1000000,
           jobs_retained_per_mil = `Jobs Retained`/`Loan Amount` * 1000000) %>%
    select(count, pct_loan_count, `Loan Amount`, pct_loan_amt, mean_loan_amt, `Jobs Created`, `Jobs Retained`,
           everything())
}  


# ANALYSIS =============================
race <- sba_details  %>%
  group_by(ETHNIC)%>%
  get_breakdowns()

gender <- sba_details %>%
  group_by(COUNTY, GENDER)%>%
  get_breakdowns() %>%
  bind_rows(sba_details %>%
              group_by(GENDER)%>%
              get_breakdowns())

vet <- sba_details %>%
  group_by(COUNTY, `Vet Status`) %>%
  get_breakdowns()%>%
  bind_rows(sba_details %>%
              group_by(`Vet Status`)%>%
              get_breakdowns())


details <- sba_details %>%
  group_by(ETHNIC, GENDER) %>%
  get_breakdowns()

openxlsx::write.xlsx(list(race = race, gender = gender, veteran = vet, all = details),"SBA.xlsx")

