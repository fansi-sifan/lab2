# Summary
source("do.R")

# benchmark
load("../metro-dataset/acs5_2017/co_acs.rda")
co_bm <- co_acs %>%
  mutate(pct_pop_minority = 1-pop_white/pop_total,
         pct_pop_bk_latino = (pop_black + pop_latino)/pop_total) %>%
  select(stco_code, stco_name, contains("pct_pop"),contains("pct_edu")) 

# oow ---------------------------

# Peers by age, race, edu
tmp <- Denver$oow$peer%>%
  group_by(stco_code, pl_name, age, population)%>%
  mutate(pct_whiteNH = pwhiteNH,
         pct_bk_latino = pblackNH + platino,
         pct_somecollege_plus = psc + paa + pbaplus,
         pct_aaba = paa + pbaplus)%>%
  ungroup()%>%
  select(stco_code, population, pl_name,age, total,contains("pct")) %>%
  mutate(count_white = total*pct_whiteNH,
         count_bk_latino = total*pct_bk_latino,
         count_somecollege_plus = total * pct_somecollege_plus,
         count_aaba = total * pct_aaba)

# gaps within Denver

# use matrix to calculate peer gaps
cal <- as.matrix(tmp %>%
                   filter(population == "Out-of-work population")%>%
                   select(contains("count")))/
  as.matrix( tmp %>%
               filter(population == "Universe population")%>%
               select(contains("count"))
  )

bind_cols((tmp %>%
             filter(population == "Out-of-work population"))[,1:5],as.data.frame((cal)))

GR$oow$peer%>%
  group_by(stco_code, pl_name, age)%>%
  mutate(pct_minority = 1 - pwhiteNH,
         pct_bk_latino = pblackNH + platino,
         pct_somecollege_plus = psc + paa + pbaplus,
         pct_aaba = paa + pbaplus)%>%
  select(total,contains("pct"))

# Details
source("out of work.R")

oow_details %>%
  filter(grepl("latino|black",race))%>%
  filter(grepl("Associate|BA|college",schlcats)) %>%
  
  filter(grepl("25",agecats))%>%
  group_by(st_code, schlcats,agecats)%>%
  
  # group_by(st_code, schlcats)%>%
  summarise_if(is.numeric,sum)

# Gap

# LWW -----------------------------
# peers
Denver$lww$peer %>%
  group_by(cbsa_name) %>%
  mutate(pct_minority = 1 - pwhiteNH,
         pct_bk_latino = pblackNH + platino,
         pct_somecollege_plus = psc + paa + pbaplus,
         pct_aaba = paa + pbaplus)%>%
  select(contains("pct"))

GR$lww$peer %>%
  group_by(cbsa_name) %>%
  mutate(pct_minority = 1 - pwhiteNH,
         pct_bk_latino = pblackNH + platino,
         pct_somecollege_plus = psc + paa + pbaplus,
         pct_aaba = paa + pbaplus)%>%
  select(contains("pct"))

# Details
Denver$lww$details %>%
  select(cbsa_name, total,contains("NH"), platino, population)

GR$lww$details %>%
  select(cbsa_name, total,contains("NH"), platino, population)

# read data!!

# OPP -----------------------------

Denver$opp$peer

# details
Denver$opp$details %>%
  filter(grepl("Associate|college|Bacca",education))%>%
  filter(race !="Total") %>%
  filter(age == "Total") %>%
  mutate(value = case_when(
    grepl("Associate|college",education) ~ as.numeric(gsub("[\\%,]","",good_jobs)) + as.numeric(gsub("[\\%,]","",promising_jobs)),
    grepl("Bacca",education) ~ as.numeric(gsub("[\\%,]","",hi_good_jobs))
  )) %>%
  select(-contains("jobs"),-other)%>%
  spread(gender, value)


GR$opp$details %>%
  filter(grepl("Associate|college|Bacca",education))%>%
  filter(race !="Total") %>%
  filter(age == "Total") %>%
  mutate(value = case_when(
    grepl("Associate|college",education) ~ as.numeric(gsub("[\\%,]","",good_jobs)) + as.numeric(gsub("[\\%,]","",promising_jobs)),
    grepl("Bacca",education) ~ as.numeric(gsub("[\\%,]","",hi_good_jobs))
  )) %>%
  select(-contains("jobs"),-other)%>%
  spread(gender, value)


# SBO -----------------------------

Denver$sbo$details %>%
  # ethnicity
  # filter(grepl("Non|firms classifiable",ETH_GROUP_TTL)) %>%
  # group_by(NAICS2012_TTL, ETH_GROUP) %>%
  
  # race
  filter(grepl("Minority|firms classifiable",RACE_GROUP_TTL)) %>%
  group_by(NAICS2012_TTL, RACE_GROUP) %>%
  
  summarise(FIRMPDEMP = sum(as.numeric(FIRMPDEMP), na.rm = T)) %>%
  
  # ethnicity
  # spread(ETH_GROUP, FIRMPDEMP) %>%
  # mutate(pct_hispanic = 1- `029`/`096`)
  
  spread(RACE_GROUP, FIRMPDEMP) %>%
  mutate(pct_minority = `90`/`96`)


GR$sbo$details %>%
  # ethnicity
  # filter(grepl("Non|firms classifiable",ETH_GROUP_TTL)) %>%
  # group_by(NAICS2012_TTL, ETH_GROUP) %>%
  
  # race
  filter(grepl("Minority|firms classifiable",RACE_GROUP_TTL)) %>%
  group_by(NAICS2012_TTL, RACE_GROUP) %>%
  
  summarise(FIRMPDEMP = sum(as.numeric(FIRMPDEMP), na.rm = T)) %>%
  
  # ethnicity
  # spread(ETH_GROUP, FIRMPDEMP) %>%
  # mutate(pct_hispanic = 1- `029`/`096`)
  
  spread(RACE_GROUP, FIRMPDEMP) %>%
  mutate(pct_minority = `90`/`96`)
