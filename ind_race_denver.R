# race and gender industry

# SETUP   ==================================================
library(tidyverse)
emsi <- read_csv("data/Industry_Table_8257.csv") %>%
  mutate(code_naics6_2017 = as.character(NAICS))

skimr::skim(emsi)

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
