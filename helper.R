library(censusapi)
library(tidyverse)

key <- Sys.getenv("CENSUS_API_KEY")

get_SBO <- function(...){
  getCensus(name = "2012/sbo",
            vars = c( "NAICS2012","NAICS2012_TTL",
                      "RACE_GROUP_TTL","RACE_GROUP", 
                      "ETH_GROUP","ETH_GROUP_TTL",
                      "SEX","SEX_TTL", "GEO_ID",
                      "FIRMALL", "FIRMPDEMP"),
            ...,
            key = key)
}


create_labels <- function(df) {
  sjlabelled::get_label(df) %>%
    data.frame() %>%
    mutate(names = colnames(df)) %>%
    rename("label" = ".")
}
