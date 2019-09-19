source("func.R")

# NATALIE's raw data files
# V:\Human Capital\Higgins unemployment\DATA\full_dataset.dta
# export CO and MI samples

# UNIVERSE: UNEMPLOYED & NOT IN LABOR FORCE, 18 - 64

# NOT INCLUDED: 
# students: graduate students, college students in dorm, high school stduents at home
# stay home parents: married, has kids & ftotincome > 2* fpl
# retired or not able to work: receive retirement or benefits income

# LOAD datasets
oow <- read_csv("data/GRDV.csv")
oow_label <- read_csv("data/var.csv")

load("../metro.data/data/puma2county.rda")
# determine if PUMA falls into customized geography
puma2county <- puma2county %>%
  mutate(stpuma = paste0(st_code, puma_code))

xwalk <- oow %>%
  select(stpuma)%>%
  distinct()%>%
  left_join(puma2county, by = "stpuma") %>%
  filter(stco_code %in% MetroDenvr_actual | stco_code %in% GR_county) %>%
  group_by(stpuma)%>%
  mutate(n = sum(afact1))%>%
  filter(n > 0.5)

# breakdowns
oow_sample <- oow %>%
  filter(SAMPLE_POP == 1)%>%
  filter(stpuma %in% xwalk$stpuma) %>%
  mutate(st_code = substr(stpuma, 1,2),
         count = 1,
         nocar = 1-car) 

oow_details <- oow_sample %>%
  select(st_code, agecats, schlcats, race,count,pwgtp_adj, unemployed, male, married, children, youngchild, insch, lep, fb, noncitizen, refugee, dis, poor, hcost50p, nocar)%>%
  group_by(st_code, agecats, schlcats, race) %>%
  summarise_each(funs(sum(.*pwgtp_adj)), -pwgtp_adj) %>%
  mutate_if(is.numeric, as.integer)
  
write.csv(oow_details, "oow_details.csv")

# summaries
oow_summary <- oow_sample %>%
  select(st_code,a1824, whiteNH, blackNH, latino, asianNH, otherNH, lths,hs, sc, aa, baplus, pwgtp_adj, unemployed, male, married, children, youngchild, insch, lep, fb, noncitizen, refugee, dis, poor, hcost50p, nocar)%>%
  group_by(st_code, a1824) %>%
  summarise_each(funs(weighted.mean(x=.,w=pwgtp_adj, na.rm = T)), -pwgtp_adj) 

t <- data.table::setDT(as.data.frame(t(oow_summary)), keep.rownames = TRUE)[] %>%
  left_join(oow_label[c("name", "varlab")], by = c("rn" = "name"))

write.csv(t, "oow_summary.csv")


# [DEPRECIATED] ipums download

# puma <- read_csv("data/usa_00014.csv")
# 
# skimr::skim(puma)
# 
# # GEOGRAPHY
# DV_puma <- puma %>%
#   filter(STATEFIP == "8") %>%
#   filter(COUNTYFIP %in% as.numeric(substr(MetroDenvr_actual,3,5)))
# 
# GR_puma <- puma %>%
#   filter(STATEFIP == "26") %>%
#   filter(COUNTYFIP %in% as.numeric(substr(GR_county,3,5)))
# 
# # CHECK
# summary(as.factor(puma$COUNTYFIP))
# 
# # APPLY FILTERS ===
# t <- DV_puma %>%
#   filter(AGE >=18 & AGE <=64) %>%
#   filter(LABFORCE == 1)%>%
#   filter(SCHOOL == 2)
# 
# nrow(t)  
# summary(as.factor(t$RACED))
# summary(as.factor(t$EDUCD))
# summary(as.factor(t$GRADEATTD))
# 
# skimr::skim(t)