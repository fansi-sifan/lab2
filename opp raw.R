library(tidyverse)

# read in opportunity job mix --------

# raw data 
opp_path <- "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/"
# opp_path <- "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/24340 Grand Rapids MI/24340 Grand Rapids MI 3digit flows.csv"

opp_raw <- read_csv(paste0(opp_path,"19740 Denver CO/19740 3digit flows.csv"))

skimr::skim(opp_raw)

# summary(as.factor(opp_raw$gender))
# summary(as.factor(opp_raw$race))
# summary(as.factor(opp_raw$education))
# summary(as.factor(opp_raw$age))
# summary(as.factor(opp_raw$titlea))
summary(as.factor(opp_raw$occ_a))


# wage by age and race
opp_sp <- opp_raw %>%
  filter(age == "Total") %>%
  filter(gender == "Total") %>%
  # mutate(edu = ifelse(education %in% c("Associate's degree","Some college or certificate", "High school diploma"),"High School",education))%>%
  group_by(race, education) %>%
  summarise(p_a_wage = weighted.mean(p_a_wage, weight),
            f_a_wage = weighted.mean(f_a_wage, weight),
            total = sum(weight)) 

skimr::skim(opp_sp)

metro.data::bbplot(opp_sp, aes(x = education, y = p_a_wage, fill = race))+
  geom_col(position = "dodge")

load("../metro-dataset/acs5_2017/co_acs_raw.rda")

co_acs_raw %>%
  filter(stco_code %in% c(target_co))%>%
  select(contains("S1501")) %>%
  select(contains("E"))%>%
  summarise_if(is.numeric, sum)%>%
  mutate(white_asian_tl = S1501_C01_031E + S1501_C01_040E,
         white_asian_ba = S1501_C01_033E + S1501_C01_042E,
         white_asian_hs = S1501_C01_032E + S1501_C01_041E - white_asian_ba,
         white_asian_no = white_asian_tl- (S1501_C01_032E + S1501_C01_041E),
         
         black_tl = S1501_C01_034E,
         black_ba = S1501_C01_036E,
         black_hs = S1501_C01_035E - black_ba,
         black_no = black_tl- S1501_C01_035E,
         
         latino_tl = S1501_C01_052E,
         latino_ba = S1501_C01_054E,
         latino_hs = S1501_C01_053E - latino_ba,
         latino_no = latino_tl- S1501_C01_053E) %>%
  select(-contains("S1501")) %>%
  View()


# read demo files =============

demo_col <- readxl::read_excel(paste0(opp_path, "19740 Denver CO/19740 Denver CO BMPP Opportunity Industries - 2017 Job shares.xlsx"), sheet = "DEMO2_OFCOL")
