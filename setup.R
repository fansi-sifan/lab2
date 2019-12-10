library(tidyverse)
library(metro.data)

# define geographies ==============================================

# metro regions ----------------------
# MetroDenver
target_cbsa <- c("22660", "19740", "14500", "24540") 
# GR cbsa
# target_cbsa <- "24340"
# Denver peers
peer_cbsa <- c("12420", "33460", "38060","38900", "41740", "41620", "42660") 

# Grand Rapids peers
# peer_cbsa <- c("14260","17140","17460","24860","26900","33340","33460","34980","41620","46140","48620")


# counties -------------------------
target_co <- (county_cbsa_st%>%filter(cbsa_code%in%target_cbsa))$stco_code
peer_co <- (county_cbsa_st%>%filter(cbsa_code%in%peer_cbsa))$stco_code

# customized counties
target_co <- (county_cbsa_st %>%
                        filter(cbsa_code %in% target_cbsa) %>%
                        filter(!stco_code %in% c("08039", "08093", "08019", "08047")))$stco_code

