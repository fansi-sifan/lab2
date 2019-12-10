library(tidyverse)

# numbers and shares by category

# Summary
source("out of work.R")
skimr::skim(oow_sample)

get_matrix <- function(var){
  
  var <- enquo(var)
  name <- ensym(var)
  
  oow_sample %>%
    group_by(SAMPLE_POP, st_code, nilf, !!var)%>%
    summarise(count = sum(pwgtp_adj)) %>%
    mutate(!!name:= paste0(name, "_", make.names(!!var)))%>%
    spread(!!var, count)
}

oow_matrix <- get_matrix(race) %>%
  left_join(get_matrix(employed))%>%
  left_join(get_matrix(male))%>%
  left_join(get_matrix(schlcats)) %>%
  left_join(get_matrix(agecats)) %>%
  left_join(get_matrix(lep))%>%
  left_join(get_matrix(vet))%>%
  left_join(get_matrix(youngchild))



load("../metro-dataset/opportunity industries/cbsa_oppind_race.rda")
number_opp <- readxl::read_xlsx(denver_opp_raw, 
                                sheet = "DEMO2_OFCOL")
names(number_opp) <- tolower(names(number_opp))

# opp

get_opp_matrix <- function(df){
  bind_rows(
    df %>%
      filter(gender == "Total") %>%
      filter(age == "Total") %>%
      filter(education == "Total") %>%
      filter(race == "Total"),
    df %>%
      filter(gender != "Total") %>%
      filter(age == "Total") %>%
      filter(education == "Total") %>%
      filter(race == "Total"),
    df %>%
      filter(gender == "Total") %>%
      filter(age != "Total") %>%
      filter(education == "Total") %>%
      filter(race == "Total"),
    df %>%
      filter(gender == "Total") %>%
      filter(age == "Total") %>%
      filter(education != "Total") %>%
      filter(race == "Total"),
    df %>%
      filter(gender == "Total") %>%
      filter(age == "Total") %>%
      filter(education == "Total") %>%
      filter(race != "Total")
  )
}

test <- function(df, cond){
  df %>%
    filter((gender == "Total")== cond[[1]]) %>%
    filter((education == "Total") == cond[[2]]) %>%
    filter((age == "Total") == cond[[3]]) %>%
    filter((race == "Total" )== cond[[4]])
}

# set which filtering variables to TRUE
conditions = as.list(as.data.frame(cbind(rep(1,4),diag(x = 1, 4, 4)==0)))

opp_matrix_row <- purrr::map_dfr(conditions, function(cond)test(number_opp, cond))

opp_matrix_col <- purrr::map_dfr(conditions, function(cond)test(cbsa_oppind_race%>%
                                                                  filter(cbsa_code %in% DV_cbsa), cond))



# business owner

sbo_dv <- get_sbo_m(MetroDenvr_actual)
openxlsx::write.xlsx(c(sbo_dv, list(oow_matrix, opp_matrix_row, opp_matrix_col)), file = "Denvermatrix.xlsx")

sbo_gr <- get_sbo_m(GR_county)
openxlsx::write.xlsx(sbo_gr, file = "sbo_gr.xlsx")

