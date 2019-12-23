# stuffs that require manual edit/downloads

library(tidyverse)
library(metro.data)

# define geographies ==============================================

# targets  ----------------------
# name <- "Metro Denver"
name <- "Grand Rapids"
# MetroDenver
# target_cbsa <- c("22660", "19740", "14500", "24540")
target_cbsa_core <- target_cbsa <- "24340"
# if multiple cbsa, select a core
# target_cbsa_core <- "19740"
# GR cbsa
# target_cbsa <- "24340"

target_co <- (county_cbsa_st %>% filter(cbsa_code %in% target_cbsa))$stco_code

# customized counties
# target_co <- (county_cbsa_st %>%
#   filter(cbsa_code %in% target_cbsa) %>%
#   filter(!stco_code %in% c("08039", "08093", "08019", "08047")))$stco_code
# 

# peers -------------------------

# Denver peers
# peer_cbsa <- c("12420", "33460", "38060", "38900", "41740", "41620", "42660")

# Grand Rapids peers
peer_cbsa <- c("14260","17140","17460","24860","26900","33340","33460","34980","41620","46140","48620")

peer_co <- (county_cbsa_st %>% filter(cbsa_code %in% peer_cbsa))$stco_code

# communting zones
# peer_cz <- c("Denver", "Austin","Boulder","Fort Collins","Greeley","Minneapolis","Phoenix","Portland","Salt Lake City","San Diego","Seattle")

peer_cz <- c("Grand Rapids","Wichita", "Tulsa","Greenville", "Salt Lake City",
                "Cincinnati", "Nashville", "Boise City", "Cleveland","Milwaukee", "Indianapolis", "Minneapolis")

# download datasets ------------------

# EMSI 6-digit industry employment tables

path_emsi_peers <- "data/Emsi_2019.3_ind_data.csv"

# if customized county: 
path_emsi_region <- "data/Emsi_2019.4_ind_data.csv"

# access
# source("access.R")
