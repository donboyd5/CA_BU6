
dcvalues <- function(dc_covered, w, b, p){
  # w: worker data
  # b: base
  # p: params
  if(!dc_covered) return(NULL)
  
  # get dc_annuity_factor for $1 of assets
  # iannuity <- gaip(1, p$dc_ror, n=w$aod - w$aor, g=cpi_pch)
 
  dcvalues <- unnest(b, cols = c()) %>%
    mutate(dc_assets_boy=0,
           dc_eec=p$dc_eec_rate * wage,
           dc_erc=p$dc_erc_rate * wage,
           dc_annuity=0,
           dc_invassets=0,
           dc_invinc=0,
           dc_assets_eoy=0)
  
  
  # calculate the first year
  dcvalues$dc_invassets[1] <- dcvalues$dc_eec[1] + dcvalues$dc_erc[1]
  dcvalues$dc_invinc[1] <- dcvalues$dc_invassets[1] * p$dc_ror
  dcvalues$dc_assets_eoy[1] <- dcvalues$dc_assets_boy[1] + 
    dcvalues$dc_eec[1] + dcvalues$dc_erc[1] +
    dcvalues$dc_invinc[1]
  
  # loop through the rest of the years
  for(i in 2:nrow(dcvalues)){
    dcvalues$dc_assets_boy[i] <- dcvalues$dc_assets_eoy[i - 1]
    dcvalues$dc_annuity[i] <- ifelse(b$age[i] >= w$aor,
                                     gaip(dcvalues$dc_assets_boy[i], p$dc_ror, n=w$aod - b$age[i] + 1, g=cpi_pch),
                                     0)
    dcvalues$dc_invassets[i] <- dcvalues$dc_assets_boy[i] + 
      dcvalues$dc_eec[i] + dcvalues$dc_erc[i] -
      dcvalues$dc_annuity[i]
    
    dcvalues$dc_invinc[i] <- dcvalues$dc_invassets[i] * p$dc_ror
    
    dcvalues$dc_assets_eoy[i] <- dcvalues$dc_assets_boy[i] + 
      dcvalues$dc_eec[i] + dcvalues$dc_erc[i] +
      dcvalues$dc_invinc[i] -
      dcvalues$dc_annuity[i]
  }
  
  dcvalues %>%
    select(-names(b))
}
