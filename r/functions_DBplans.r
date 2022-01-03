# nomenclature:
#   "yo" variables are "year of" variables relative to zero, answering "how many" -- for example:
#      yos - year of service
#      yor - year of retirement
#   "cy" variables are calendar year variables
#      cyob - calendar year of birth
#      cyor - calendar year of retirement

pension <- function(db_covered, stabbr, tier, w, b, p){
  # w: worker data
  # b: base data, which is a function of w; it has:
       # one record for each year from aoe (age of entry) to aod (age of death), with
       # year, age, yos, yor, wage
  # p: params
  # TODO:
  if(!db_covered) return(NULL)
  
  # define interim-calc variables we don't need to keep in the output
  max_early_years <- p$db_aor_normal - p$db_aor_min
  early_penalty_per_year <- ifelse(max_early_years > 0,
                                   (p$db_benfactor_normal - p$db_benfactor_min) / max_early_years,
                                   0)
  
  eec <- function(stabbr, tier, wage, p) {
    p$db_eec_rate1 * pmin(wage, p$db_eec_ceiling1) +
      p$db_eec_rate2 * pmax(wage - p$db_eec_ceiling1, 0)
  }
  
  cola <- function(ipension, age, aor, cola_ceil1, cola_rate1, compound) {
    # CAUTION: does not implement the 2nd cola rate a la OR
    ipension <- ipension[1]  # this will come in as a vector
    cola1_0 <- pmin(ipension, cola_ceil1)
    nyears <- length(age)
    pension_year <- pmax(age - aor + 1, 0)
    db_noncola_part <- ifelse(ipension > cola1_0, ipension - cola1_0, 0) * (pension_year > 0)
    if(compound) {
      db_cola_part <- cola1_0 * (1 + cola_rate1)^(pension_year - 1) * (pension_year > 0) 
    } else db_cola_part <- (cola1_0 + cola1_0 * cola_rate1 * (pension_year - 1)) * (pension_year > 0)
    db_pension <- db_noncola_part + db_cola_part
    # tibble(age, noncola_part, cola_part, pension)
    tibble(db_noncola_part, db_cola_part, db_pension)
    }

  # test data  
  # cola(ipension, age, aor, cola_ceil1, cola_rate1, compound=FALSE)
  # age <- 50:65
  # aor <- 55
  # cola_ceil1 <- 10e3
  # cola_rate1 <- .02
  # cola_ceil2 <- 55e3
  # cola_rate2 <- .0
  # ipension <- 50e3
  
  # compute employee contributions and benefits by year
  results <- unnest(b, cols = c()) %>%
    # determine career contributions
    mutate(db_eec=ifelse(yos > 0,
                         eec(stabbr, tier, wage, p),
                         0)) %>%
    # now determine retirement benefits
    mutate(early_years=pmax(p$db_aor_normal - w$aor, 0),
           early_penalty=pmin(early_years * early_penalty_per_year, p$db_benfactor_normal),
           db_benfactor=p$db_benfactor_normal - early_penalty,
           db_pctfas_uncapped=db_benfactor * w$fyos,
           db_pctfas=pmin(db_pctfas_uncapped, p$db_max_benpct),
           db_ipension=w$fas * db_pctfas, # initial pension
           cola(db_ipension, age, aor=w$aor, cola_ceil1=p$db_cola_ceiling1, cola_rate1=p$db_cola_rate1, compound=p$db_cola_compound) # returns a data frame
    ) %>%
    select(-names(b))
  results
}
