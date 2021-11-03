
pension <- function(db_covered, stabbr, tier, w, b, p){
  # w: worker data
  # b: base
  # p: params
  # TODO:
  if(!db_covered) return(NULL)
  
  # define interim-calc variables we don't need to keep in the output
  max_early_years <- p$db_aor_normal - p$db_aor_min
  early_penalty_per_year <- ifelse(max_early_years > 0,
                                   (p$db_benfactor_normal - p$db_benfactor_min) / max_early_years,
                                   0)
  
  # eec <- function(stabbr, tier, wage, p) {
  #   case_when(stabbr=="CA" ~ 
  #               pmax(wage - p$db_eec_exclude, 0) * p$db_eec_rate,
  #             stabbr=="MA" ~ wage * .09 + pmax(wage - 30000, 0) * .02,
  #             TRUE ~ pmax(wage - p$db_eec_exclude, 0) * p$db_eec_rate
  #             )
  # }
  
  eec <- function(stabbr, tier, wage, p) {
    p$db_eec_rate1 * pmin(wage, p$db_eec_base1) +
      p$db_eec_rate2 * pmax(wage - p$db_eec_base1, 0)
  }
  
  cola <- function(stabbr, tier, p) {
    
  }
  
  # compute employee contributions and benefits by year
  results <- unnest(b, cols = c()) %>%
    # determine career contributions
    mutate(db_eec=ifelse(yos > 0,
                         eec(stabbr, tier, wage, p),
                         0)) %>%
    # now determine retirement benefits
    mutate(early_years=pmax(p$db_aor_normal - w$aor, 0),
           early_penalty=early_years * early_penalty_per_year,
           db_benfactor=p$db_benfactor_normal - early_penalty,
           db_pctfas_uncapped=db_benfactor * w$fyos,
           db_pctfas=pmin(db_pctfas_uncapped, p$db_max_benpct),
           ipension=w$fas * db_pctfas, # initial pension
           db_pension2=ifelse(age >= w$aor,
                              ipension * (1 + p$db_cola_rate)^(b$yor - 1),
                              0),
           db_pension_colagrown=ifelse(age >= w$aor,
                                       p$db_cola_base * (1 + p$db_cola_rate)^(b$yor - 1),
                                       0),
           db_pension_base=case_when(age < w$aor ~ 0,
                                     p$db_cola_base==0 ~
                                       ipension * (1 + p$db_cola_rate)^(b$yor - 1),
                                     p$db_cola_base > 0 ~ ipension,
                                     TRUE ~ -1e99),
           db_pension=db_pension_colagrown + db_pension_base
    ) %>%
    select(-names(b), -ipension)
  results
}
