# Summary
source("do.R")
source("out of work.R")

# benchmark
load("../metro-dataset/acs5_2017/co_acs.rda")
co_acs %>%
  # filter(stco_code %in% (MetroDenvr_actual))%>%
  filter(stco_code %in% (GR_county)) %>%
  summarise_at(vars(contains("pop")), sum) %>%
  mutate(
    pct_pop_minority = 1 - pop_white / pop_total,
    pct_pop_bk_latino = (pop_black + pop_latino) / pop_total
  )

load("../metro-dataset/acs5_2017/cbsa_acs.rda")

cbsa_acs %>%
  # filter(cbsa_code %in% MetroDenver_cbsa)%>%
  filter(cbsa_code %in% GR_cbsa) %>%
  select(cbsa_name, contains("pct_edu_baplus"), contains("pop")) %>%
  summarise(pct_edu_bklt = sum((pct_edu_baplus_black * pop_black + pct_edu_baplus_latino * pop_latino)) / sum((pop_black + pop_latino)))

# oow ================================
# details ---------------------------
oow_de <- oow_details %>%
  filter(grepl("latino|black", race)) %>%
  filter(grepl("Associate|BA|college", schlcats)) %>%
  filter(population == "Out-of-work population") %>%

  # filter(grepl("25",agecats))%>%
  # group_by(st_code, schlcats,agecats)%>%
  group_by(st_code, schlcats) %>%
  summarise_if(is.numeric, sum) %>%
  group_by(st_code, schlcats, count) %>%
  summarise_each(funs(. / count))

# Peers by age, race, edu ---------------------------
gap_oow_peers <- function(df) {
  tmp <- df$oow$peer %>%
    group_by(stco_code, pl_name, age, population) %>%
    mutate(
      pct_whiteNH = pwhiteNH,
      pct_blackNH = pblackNH,
      pct_latino = platino,
      pct_somecollege_plus = psc + paa + pbaplus,
      pct_aaba = paa + pbaplus
    ) %>%
    ungroup() %>%
    select(stco_code, population, pl_name, age, total, contains("pct")) %>%
    mutate(
      count_white = total * pct_whiteNH,
      count_black = total * pct_blackNH,
      count_latino = total * pct_latino,
      count_somecollege_plus = total * pct_somecollege_plus,
      count_aaba = total * pct_aaba
    )

  # use matrix to calculate peer gaps
  cal <- as.matrix(tmp %>%
    filter(population == "Out-of-work population") %>%
    select(contains("count"))) /
    as.matrix(tmp %>%
      filter(population == "Universe population") %>%
      select(contains("count")))

  bind_cols((tmp %>%
    filter(population == "Out-of-work population")), as.data.frame((cal)))
}

cal_gap <- function(df, co_codes, age) {
  t <- gap_oow_peers(df) %>%
    filter(age == !!age)

  A <- as.matrix(t %>% filter(stco_code %in% co_codes) %>%
    select(contains("1")))
  
  B <- as.matrix(t %>% select(contains("1")))

  df <- as.data.frame(matrix(rep(A, each = nrow(t)), nrow = nrow(t)) - B)

  bind_cols(t, df) 
  
  # bind_cols(pct %>%
  #   group_by(stco_code, population, pl_name, age, total) %>%
  #   mutate_each(funs(. * total)), 
  # as.data.frame(B))
}

gap_oow_peers(Denver)

gap_oow_peer_denver <- bind_rows(
  cal_gap(Denver, MetroDenvr_actual, "18-24"),
  cal_gap(Denver, MetroDenvr_actual, "25-64")
) %>%
  arrange(stco_code, pl_name)

gap_oow_peer_GR <- cal_gap(GR, GR_county, "25-64")

# gaps within ---------------------------------

gap_oow_self <- bind_cols(
  oow_summary %>%
    filter(population == "Out-of-work population")%>%
    group_by(st_code, population, a1824, count)%>%
    mutate_each(funs(.*count)),
    
  as.data.frame(oow_summary[5:8, 5:28] - oow_summary[1:4, 5:28])
)

# LWW -----------------------------
# peers
gap_lww_peers <- function(df) {
  tmp <- df$lww$peer %>%
    group_by(cbsa_name, population) %>%
    mutate(
      pct_white = pwhiteNH,
      pct_black = pblackNH,
      pct_latino = platino,
      pct_somecollege_plus = psc + paa + pbaplus,
      pct_aaba = paa + pbaplus
    ) %>%
    select(total, contains("pct")) %>%
    mutate_each(funs(. * total), -total)

  # # use matrix to calculate peer gaps
  A <- as.matrix(tmp %>% ungroup() %>%
    filter(population == "Low-wage workers") %>%
    select(contains("pct")))

  B <- as.matrix(tmp %>% ungroup() %>%
    filter(population == "Workers") %>%
    select(contains("pct")))

  bind_cols((tmp %>%
    filter(population == "Low-wage workers")), as.data.frame((A / B)))
}

gap_lww_Denver <- gap_lww_peers(Denver)
gap_lww_GR <- gap_lww_peers(GR)

# Details
# read data!!


# OPP -----------------------------

Denver$opp$peer

# details
opp_DV <- Denver$opp$details %>%
  filter(grepl("Associate|college|Bacca", education)) %>%
  filter(race != "Total") %>%
  filter(age == "Total") %>%
  mutate(value = case_when(
    grepl("Associate|college", education) ~ as.numeric(gsub("[\\%,]", "", good_jobs)) + as.numeric(gsub("[\\%,]", "", promising_jobs)),
    grepl("Bacca", education) ~ as.numeric(gsub("[\\%,]", "", hi_good_jobs))
  )) %>%
  select(-contains("jobs"), -other) %>%
  spread(gender, value)


opp_GR <- GR$opp$details %>%
  filter(grepl("Associate|college|Bacca", education)) %>%
  filter(race != "Total") %>%
  filter(age == "Total") %>%
  mutate(value = case_when(
    grepl("Associate|college", education) ~ as.numeric(gsub("[\\%,]", "", good_jobs)) + as.numeric(gsub("[\\%,]", "", promising_jobs)),
    grepl("Bacca", education) ~ as.numeric(gsub("[\\%,]", "", hi_good_jobs))
  )) %>%
  select(-contains("jobs"), -other) %>%
  spread(gender, value)


# SBO -----------------------------

sbo_DV <- Denver$sbo$details %>%
  # ethnicity
  # filter(grepl("Non|firms classifiable",ETH_GROUP_TTL)) %>%
  # group_by(NAICS2012_TTL, ETH_GROUP) %>%

  # race
  filter(grepl("Minority|firms classifiable", RACE_GROUP_TTL)) %>%
  group_by(NAICS2012_TTL, RACE_GROUP) %>%
  summarise(FIRMPDEMP = sum(as.numeric(FIRMPDEMP), na.rm = T)) %>%

  # ethnicity
  # spread(ETH_GROUP, FIRMPDEMP) %>%
  # mutate(pct_hispanic = 1- `029`/`096`)

  spread(RACE_GROUP, FIRMPDEMP) %>%
  mutate(pct_minority = `90` / `96`)


sbo_GR <- GR$sbo$details %>%
  # ethnicity
  # filter(grepl("Non|firms classifiable",ETH_GROUP_TTL)) %>%
  # group_by(NAICS2012_TTL, ETH_GROUP) %>%

  # race
  filter(grepl("Minority|firms classifiable", RACE_GROUP_TTL)) %>%
  group_by(NAICS2012_TTL, RACE_GROUP) %>%
  summarise(FIRMPDEMP = sum(as.numeric(FIRMPDEMP), na.rm = T)) %>%

  # ethnicity
  # spread(ETH_GROUP, FIRMPDEMP) %>%
  # mutate(pct_hispanic = 1- `029`/`096`)

  spread(RACE_GROUP, FIRMPDEMP) %>%
  mutate(pct_minority = `90` / `96`)

# OUTPUT ==========================================================
df <- list(
  oow_details = oow_de,
  gap_oow_peer_denver = gap_oow_peer_denver,
  gap_oow_peer_GR = gap_oow_peer_GR,
  gap_oow_self = gap_oow_self,
  gap_lww_Denver = gap_lww_Denver,
  gap_lww_GR = gap_lww_GR
)

openxlsx::write.xlsx(df, file = "lab_gap.xlsx")

df2 <- list(
  opp_DV = opp_DV,
  opp_GR = opp_GR,
  sbo_DV = sbo_DV,
  sbo_GR = sbo_GR
)
openxlsx::write.xlsx(df2, file = "lab_gaps2.xlsx")
