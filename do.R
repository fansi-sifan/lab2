source("func.R")
# do

# Assignment 2

get_all <- function(metrocode, peercode, stco_code, name){
  oow <- get_oow(metrocode, peercode)
  lww <- get_lww(metrocode, peercode)
  opp <- get_opp(metrocode, peercode)
  
  sbo <- get_sbo_m(stco_code)
  school <- get_school(stco_code)
  hiratio <- get_hiratio(metrocode, peercode)
  
  df <- list(oow = oow, lww = lww,opp = opp, sbo = sbo, school = school, hiratio = hiratio)
  openxlsx::write.xlsx(df, file = paste0(name,".xlsx"))
  
  return(df)
  
}

# denver data
Denver <- get_all(MetroDenver_cbsa,Denverpeer_cbsa, MetroDenvr_actual, "Metro Denver")

# GR data
GR <- get_all(GR_cbsa,GRpeer_cbsa,GR_county, "Grand Rapids")

GR$school <- NULL
openxlsx::write.xlsx(Denver$oow, file = "Denver_oow.xlsx")

# df <- get_sbo_m(MetroDenvr_actual)

# assignment 3

get_all2 <- function(county, cbsa, peer, st,name){
  
  ai <- get_ind(name, peer, is.ai)
  traded <- get_ind(name, peer, traded.naics6)
  netjobs <- get_netjobs(name, peer)
  
  access <- get_access(cbsa, peer)
  density <- get_density(cbsa, peer)
  
  digital <- get_digital(cbsa, peer)

  SBIR <- get_SBIR(county)
  loan <- get_loan(county,st)

  df <- list(ai = ai,traded = traded,netjobs = netjobs, 
             access = access, density = density,
             digital = digital,
             sbir_details = SBIR$details, sbir_summary = SBIR$demo, 
             loan = loan)
  return(df)
}

get_netjobs("Grand Rapids - Kentwood", GRpeer_cbsa) %>%
  View()

get_netjobs("Metro Denver", GRpeer_cbsa) %>%
  View()

DV2 <- get_all2(county = MetroDenvr_actual, 
                cbsa = MetroDenver_cbsa,
                peer = Denverpeer_cbsa,
                st = "co", 
                name = "Metro Denver")

GR2 <- get_all2(county = GR_county, 
                cbsa = GR_cbsa,
                peer = GRpeer_cbsa,
                st = "mi",
                "Grand Rapids - Kentwood")

openxlsx::write.xlsx(DV2, file = "Denver3.xlsx")
openxlsx::write.xlsx(GR2, file = "Grand Rapids3.xlsx")

