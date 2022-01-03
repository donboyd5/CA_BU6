#.. utility functions ----
gaip <- function(p, i, n, g, end = FALSE){
  # graduated annuity (growing at rate g) initial payment
  # p=principal, i=interest rate, n=periods, g=growth rate in payments
  # calculating gaip directly
  # end: , if TRUE, payment at the end of period. 
  # examples:
  # gaip(100, 0.08, 10, 0.04)
  # gaip(100, 0.08, 10, 0.02, end = TRUE)
  if(end) p <- p*(1 + i) 
  k <- (1 + g)/(1 + i)
  a_sn <- (1 - k^n )/(1 - k)
  pmt <- p/a_sn  # the initial payment in the graduated annuity
  return(pmt)
}


#.. base data functions ----
fbase <- function(w){
  # w is wdata, data for a single worker -- one row
  # build the base data for each year from aoe (age of entry) to aod (age of death)
  base <- tibble(age=w$aoe:w$aod) %>%
    mutate(year=w$cyoe + row_number() - 1,
           yos=ifelse(age < w$aor, age - w$aoe + 1, 0),  # actual years of service, 0 in retirement
           yor=ifelse(age >= w$aor, age - w$aor + 1, 0),
           # FOR NOW: backcast wages from fas -- improve this when practical
           wage=ifelse(age < w$aor, 
                       w$fas / ((1 + nomwage_pch)^(w$fyos - yos)),
                       0)) %>%
    select(year, age, yos, yor, wage)
  base
}


#.. function to get previously saved cpi historical data and construct forecast ----
get_cpifc <- function(){
  # note:
  #   I do not use the 5.9% for 2021 estimated by ssa
  #   I really should use cpi-w for Soc Sec but keep this for now
  cpi <- readRDS(here::here("data", "cpi.rds"))
  cpifc <- cpi %>%
    select(year, cpi) %>%
    bind_rows(tibble(year=2021:2200)) %>%
    mutate(cpi=ifelse(year > 2020,
                      cpi[year==2020] * (1 + cpi_pch)^(year - 2020),
                      cpi),
           cpi_pch=cpi / cpi[match(year - 1, year)] - 1)
  cpifc
}


