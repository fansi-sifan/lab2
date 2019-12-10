
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
