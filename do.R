# do

# GET ALL

get_all <- function(code, stco_code, name){
  oow <- get_oow(code)
  yoow <- get_yoow(code)
  lww <- get_lww(code)
  opp <- get_opp(code)
  
  sbo <- get_sbo_m(stco_code)
  
  hiratio <- get_hiratio(code)
  
  openxlsx::write.xlsx(c(oow, yoow, lww, opp, sbo, hiratio), file = paste0(name,".xlsx"))
  
}

get_all(MetroDenver_cbsa, MetroDenvr_actual, "Metro Denver")
get_all(GR_cbsa, GR_county, "Grand Rapids")


df <- get_sbo_m(MetroDenvr_actual)
