source("func.R")
# do

# GET ALL

get_all <- function(metrocode, peercode, stco_code, name){
  oow <- get_oow(metrocode, peercode)
  yoow <- get_yoow(metrocode, peercode)
  lww <- get_lww(metrocode, peercode)
  opp <- get_opp(metrocode, peercode)
  
  sbo <- get_sbo_m(stco_code)
  school <- get_school(stco_code)
  hiratio <- get_hiratio(metrocode, peercode)
  
  openxlsx::write.xlsx(c(oow, yoow, lww, opp, sbo, school, hiratio), file = paste0(name,".xlsx"))
  
}

# denver data
get_all(MetroDenver_cbsa,Denverpeer_cbsa, MetroDenvr_actual, "Metro Denver")

# GR data
get_all(GR_cbsa,GRpeer_cbsa,GR_county, "Grand Rapids")


df <- get_sbo_m(MetroDenvr_actual)

