source("func.R")
# do

# GET ALL

get_all <- function(metrocode, peercode, stco_code, name){
  oow <- get_oow(metrocode, peercode)
  lww <- get_lww(metrocode, peercode)
  opp <- get_opp(metrocode, peercode)
  
  sbo <- get_sbo_m(stco_code)
  school <- get_school(stco_code)
  hiratio <- get_hiratio(metrocode, peercode)
  
  df <- list(oow = oow, lww = lww,opp = opp, sbo = sbo, school = school, hiratio = hiratio)
  # openxlsx::write.xlsx(df, file = paste0(name,".xlsx"))
  
  return(df)
  
}

# denver data
Denver <- get_all(MetroDenver_cbsa,Denverpeer_cbsa, MetroDenvr_actual, "Metro Denver")

# GR data
GR <- get_all(GR_cbsa,GRpeer_cbsa,GR_county, "Grand Rapids")


# df <- get_sbo_m(MetroDenvr_actual)

