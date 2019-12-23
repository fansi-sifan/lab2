# Summary

# source("out of work.R")

# benchmark ------------------------

get_benchmark <- function(target_co){
  
  load("../metro-dataset/acs5_2017/co_acs.rda")
  load("../metro-dataset/acs5_2017/co_acs_raw.rda")
  
  bind_cols(
    co_acs %>%
      filter(stco_code %in% (target_co)) %>%
      summarise_at(vars(contains("pop")), sum) %>%
      mutate(
        pct_pop_minority = 1 - pop_white / pop_total,
        pct_pop_bk_latino = (pop_black + pop_latino) / pop_total
      ),
    co_acs_raw %>%
      filter(stco_code %in% c(target_co))%>%
      select(contains("S1501")) %>%
      select(contains("E"))%>%
      summarise_if(is.numeric, sum)%>%
      mutate(edu_baplus_nonwhite = (S1501_C01_036E+S1501_C01_054E+S1501_C01_042E),
             edu_all_nonwhite = (S1501_C01_034E+S1501_C01_052E+S1501_C01_040E),
             pct_edu_baplus_nonwhite = edu_baplus_nonwhite/edu_all_nonwhite,
             edu_baplus = S1501_C01_033E + edu_baplus_nonwhite,
             edu_all = S1501_C01_031E + edu_all_nonwhite,
             pct_edu = edu_baplus/edu_all)%>%
      select(contains("edu")) 
  )}


# Peers by age, race, edu ---------------------------
get_gap_oow_peers <- function(df) {
  tmp <- df$oow.peer %>%
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
  t <- get_gap_oow_peers(df) %>%
    filter(age == !!age)

  A <- as.matrix(t %>% filter(stco_code %in% co_codes) %>%
    select(contains("1")))
  
  B <- as.matrix(t %>% select(contains("1")))

  tmp <- as.data.frame(matrix(rep(A, each = nrow(t)), nrow = nrow(t)) - B)
 
  bind_cols(t, tmp)
  
  # bind_cols(pct %>%
  #   group_by(stco_code, population, pl_name, age, total) %>%
  #   mutate_each(funs(. * total)), 
  # as.data.frame(B))
}


# LWW -----------------------------
# peers
gap_lww_peers <- function(df) {
  tmp <- df$lww.peer %>%
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




