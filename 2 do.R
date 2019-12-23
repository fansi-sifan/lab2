# source("func.R")
source("0 setup.R")
source("1 func.R")

# Who is excluded --------------------------------------------------
get_who <- function(target_cbsa, peer_cbsa, target_co, name){
  oow <- get_oow(target_cbsa, peer_cbsa)
  lww <- get_lww(target_cbsa, peer_cbsa)
  opp <- get_opp(target_cbsa, peer_cbsa)
  
  sbo <- get_sbo_m(target_co)
  school <- get_school(target_co)
  hiratio <- get_hiratio(target_cbsa, peer_cbsa)
  
  df <- c(oow = oow, lww = lww,opp = opp, sbo = sbo, school = school, hiratio = hiratio)
  
  return(df)
  
}

who <- get_who(target_cbsa, peer_cbsa, target_co, name)
openxlsx::write.xlsx(who, file = paste0("result/", name,"_who.xlsx"))


# Why they are excluded --------------------------------------------------
get_why <- function(county, cbsa, peer, st,name){
  
  ai <- get_ind(cbsa, peer, is.ai)
  traded <- get_ind(cbsa, peer, traded.naics6)
  netjobs <- get_netjobs(cbsa, peer)
  
  access <- get_access(cbsa, peer)
  density <- get_density(cbsa, peer)
  
  digital <- get_digital(cbsa, peer)
  bafields <- get_BAfields(county) %>%
    calculate_BA_field() %>%
    select(-contains("C15010"))

  SBIR <- get_SBIR(county)
  loan <- get_loan(county,substr(county[[1]],1,2))

  df <- list(ai = ai,traded = traded,netjobs = netjobs, 
             access = access, density = density,
             digital = digital,bafields = bafields,
             sbir_details = SBIR$details, sbir_summary = SBIR$demo, 
             loan = loan)
  return(df)
}

why <- get_why(county = target_co, 
               cbsa = target_cbsa,
               peer = peer_cbsa)

openxlsx::write.xlsx(why, file =  paste0("result/", name,"_why.xlsx"))


# gaps ==========================================================

source("gaps.R")

gaps <- list(
  benchmark = get_benchmark(target_co),
  
  gap_oow =  bind_rows(
    
    # 18-24 not available for grand rapids
    
    # cal_gap(who, target_co, "18-24"),
    cal_gap(who, target_co, "25-64")
  ) %>%
    arrange(stco_code, pl_name),

  gap_lww = gap_lww_peers(who)
  
)

openxlsx::write.xlsx(gaps, file = paste0("result/", name,"_gaps.xlsx"))


# Cost of exculsion and others -------------------------------------------

costs <- list(
  upmobility = get_upmob(peer_cz),
  inventor = get_inventor(peer_cz),
  edu_birth = get_edu_birth(target_co, peer_cbsa),
  age_race = get_age_race(target_cbsa)
)

openxlsx::write.xlsx(costs, file = paste0("result/", name,"_cost.xlsx"))
