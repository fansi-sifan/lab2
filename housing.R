load("../metro-dataset/housing_renter/cbsa_rental.rda")

library(tidyverse)




install.packages('googlesheets4')
library(googlesheets4)

homeownership <- read_sheet("https://docs.google.com/spreadsheets/d/1BXVPZXA_R1ejffXIgnOTJrGbK5VbgZERCB0maL1hdNE/edit#gid=1343127264")
cost_burden <- read_sheet("https://docs.google.com/spreadsheets/d/1FroaeijtlKq2Gmgoy0exkk67AeYixT9qdgzhX7KsN4c/edit#gid=1946300338")
low_rent <- read_sheet("https://docs.google.com/spreadsheets/d/12qk05RW5qBvoDqg-BCXdvl1dor6DpE-jHqlORq1RO74/edit#gid=1826847862")


df <- list(
homeownership = homeownership %>%
  mutate(cbsa_code = str_pad(CBSA, 5, "left", "0")) %>% 
  filter(cbsa_code %in% c(peer_cbsa,target_cbsa)), 
  
low_rent = low_rent %>%
  mutate(cbsa_code = str_pad(GEOID, 5, "left", "0")) %>%
  filter(cbsa_code %in% c(peer_cbsa,target_cbsa)),

cost_burden = cost_burden %>%
  mutate(cbsa_code = str_pad(GEOID, 5, "left", "0")) %>%
  filter(cbsa_code %in% c(peer_cbsa,target_cbsa)),

rental_race = cbsa_rental %>%
  filter(cbsa_code %in% c(peer_cbsa,target_cbsa)))

openxlsx::write.xlsx(df, file = paste0("result/", name,"_housing.xlsx"))
