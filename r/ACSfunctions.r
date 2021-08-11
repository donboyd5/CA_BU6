# Don Boyd
# 10/31/2015
# ACSfunctions.r


#****************************************************************************************************
#                Geographies ####
#****************************************************************************************************
f_puma_county <- function(PUMA00, PUMA10, ST) {
  # pumacnty = f_puma_county(PUMA00, PUMA10, ST)
  # PUMA codes are 5 digits
  pumacnty <- rep("other", length(PUMA00))
  # PUMA00 Census 2000
  # PUMA10 Census 2010
  # key fips
  # NYC Bronx 36005 Kings 36047 New York 36061 Queens 36081 Richmond 36085 
  # Nassau 36059 
  # Suffolk 36103 
  # Westchester 36119 
  # 005 047 061 081 085 059 103 119
  # use at http://mcdc.missouri.edu/websas/geocorr12.html
  
  pvec <- function(nvec) return(str_pad(nvec, 5, "left", "0"))
  
  # PUMA 2000 codes per MCDC
  # 3400	3400		Westchester
  # 3501	3506		Westchester
  # 
  # 4201	4212		Nassau
  # 4301	4312		Suffolk
  # 
  # 3701	3710		Bronx
  # 3801	3810		Manhattan
  # 3901	3903		Staten Island
  # 4001	4018		Brooklyn
  # 4101	4114		Queens
  westchester00 <- expression(ST=="36" & PUMA00 %in% pvec(c(3400, 3501:3506)))
  
  nassau00 <- expression(ST=="36" & PUMA00 %in% pvec(4201:4212))
  suffolk00 <- expression(ST=="36" & PUMA00 %in% pvec(4301:4312))
  
  bronx00 <- expression(ST=="36" & PUMA00 %in% pvec(3701:3710))
  manhattan00 <- expression(ST=="36" & PUMA00 %in% pvec(3801:3810))
  statenisland00 <- expression(ST=="36" & PUMA00 %in% pvec(3901:3903))
  brooklyn00 <- expression(ST=="36" & PUMA00 %in% pvec(4001:4018))
  queens00 <- expression(ST=="36" & PUMA00 %in% pvec(4101:4114))
  
  # 2010 PUMA codes per MABLE (mcdc)
  # 3101	3107		Westchester
  # 
  # 3201	3212		Nassau
  # 3301	3313		Suffolk
  # 
  # 3701	3710		Bronx
  # 3801	3810		Manhattan
  # 3901	3903		Staten Island
  # 4001	4018		Brooklyn
  # 4101	4114		Queens
  westchester10 <- expression(ST=="36" & PUMA10 %in% pvec(c(3101:3107)))
  
  nassau10 <- expression(ST=="36" & PUMA10 %in% pvec(3201:3212))
  suffolk10 <- expression(ST=="36" & PUMA10 %in% pvec(3301:3313))
  
  bronx10 <- expression(ST=="36" & PUMA10 %in% pvec(3701:3710))
  manhattan10 <- expression(ST=="36" & PUMA10 %in% pvec(3801:3810))
  statenisland10 <- expression(ST=="36" & PUMA10 %in% pvec(3901:3903))
  brooklyn10 <- expression(ST=="36" & PUMA10 %in% pvec(4001:4018))
  queens10 <- expression(ST=="36" & PUMA10 %in% pvec(4101:4114))
  # RECODE PUMA (3701 thru 3710=1) (3801 thru 3810=1) (3901 thru 3903=1) (4001 thru 4018=1) (4101 thru 4114=1) (ELSE=0) INTO NYCResident.

  pumacnty <- ifelse(eval(westchester00) | eval(westchester10), "westchester", pumacnty)
  pumacnty <- ifelse(eval(nassau00) | eval(nassau10), "nassau", pumacnty)
  pumacnty <- ifelse(eval(suffolk00) | eval(suffolk10), "suffolk", pumacnty)
  
  pumacnty <- ifelse(eval(bronx00) | eval(bronx10), "bronx", pumacnty)
  pumacnty <- ifelse(eval(manhattan00) | eval(manhattan10), "manhattan", pumacnty)
  pumacnty <- ifelse(eval(statenisland00) | eval(statenisland10), "statenisland", pumacnty)
  pumacnty <- ifelse(eval(brooklyn00) | eval(brooklyn10), "brooklyn", pumacnty)
  pumacnty <- ifelse(eval(queens00) | eval(queens10), "queens", pumacnty)
  return(pumacnty)
}

# f_puma_county("03100", "", "36")
# f_puma_county("03701", "", "36")
# f_puma_county("03701", "", "35")
# f_puma_county("", "03701", "36")
# f_puma_county("03502", "", "36")
# f_puma_county("", "03502", "36")
# f_puma_county("", "03103", "36")




#****************************************************************************************************
#                Race and ethnicity codes ####
#****************************************************************************************************
# RAC1P 1
# Recoded detailed race code
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or American
# .Indian or Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races
f_rac1p <- function(RAC1P) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  labs <- c("White alone", "Black alone", "Asian alone", "Am-Indian alone", "Alaska Native alone",
            "Am-Ind or AK native tribe", "Hawaiian or PacIsland alone", "Other race alone", "Two or more races")
  #factor(rac1p, levels=levs, labels=labs, ordered=TRUE)
  x <- ordered(RAC1P, levels=levs, labels=labs)
  return(x)
}

f_race <- function(RAC1P) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  x <- factor(RAC1P, levels=levs, ordered = TRUE)
  levels(x) <- list("White alone"=1, 
                    "Black alone"=2, 
                    "Asian alone"=6, 
                    "Other race alone"=c(3:5, 7:8), 
                    "Two or more races"=9)
  return(x)
}
# table(f_race(c(1:9, 3, 5)))


f_race_hisp <- function(RAC1P, HISP) {
  y <- ifelse(HISP!=1, 0, RAC1P)
  levs <- c(1, 2, 3:9)
  y <- factor(y)
  levels(y) <- list("White alone, not Hispanic"=1, 
                    "Black alone, not Hispanic"=2,
                    "Hispanic, any race"=0,
                    "Other"=c(3:9))
  return(y)
}

f_race_hisp_old <- function(RAC1P, HISP) {
  y <- ifelse(HISP!=1, 0, RAC1P)
  levs <- c(1, 2, 6, 3:5, 7:9)
  y <- factor(y)
  levels(y) <- list("White alone, not Hispanic"=1, 
                    "Black alone, not Hispanic"=2,
                    "Hispanic, any race"=0,
                    "Asian alone, not Hispanic"=6,
                    "Other or multiple race, not Hispanic"=c(3:5, 7:9))
  return(y)
}


#****************************************************************************************************
#                Occupation codes ####
#****************************************************************************************************
# (tmp <- f_race_hisp(rac1p=c(1, 1, 2, 2, 6), hisp=c(1, 2, 2, 1, 1)))
# table(tmp)

f_occgrp <- function(OCCP02, OCCP10, OCCP12) {
  # OCCP02 2009
  # OCCP10 2010, 2011
  # OCCP12 2012, 2013
  # 6200-6940	47-0000	.	.	.	Construction and Extraction Occupations -- all 3 sets of codes
  # create numeric versions of codes as they are easier to work with
  noccp02 <- as.numeric(OCCP02) # will generate warning for non-numerics
  noccp10 <- as.numeric(OCCP10) # will generate warning for non-numerics
  noccp12 <- as.numeric(OCCP12) # will generate warning for non-numerics
  
  # use this next function to deal with NA logical values
  is.true <- function(x) {!is.na(x) & x}
  
  carpenter <- expression((noccp02==6230) | (noccp10==6230) | (noccp12==6230))
  carpenter <- is.true(eval(carpenter))
  
  construction <- expression((noccp02 %in% 6200:6940) |
                               (noccp10 %in% 6200:6940) |
                               (noccp12 %in% 6200:6940))
  construction <- is.true(eval(construction))  
  
  service <- expression((noccp02 %in% 3600:4650) |
                               (noccp10 %in% 3600:4650) |
                               (noccp12 %in% 3600:4650))
  service <- is.true(eval(service))  
  
  occgrp <- ifelse(carpenter, "carpenter", "other")
  occgrp <- ifelse(construction & !carpenter, "otherconstruction", occgrp)
  occgrp <- ifelse(service, "service", occgrp)
  return(occgrp)
}

# f_occgrp("6230", "N.A", "N.A")
# OCCP02 <- "4720"; OCCP10 <- "N.A."; OCCP12 <- "N.A."

# f_occgrp("6230", "", "")
# f_occgrp("", "6230", "")
# f_occgrp("", "", "6230")
# 
# f_occgrp("6210", "", "")
# f_occgrp("", "6210", "")
# f_occgrp("", "", "6210")
# 
# f_occgrp("3700", "", "")
# f_occgrp("", "3700", "")
# f_occgrp("", "", "3700")
# 
# f_occgrp("3500", "", "")
# f_occgrp("", "3500", "")
# f_occgrp("", "", "3500")
# 
# occa <- c("6230", "6210", "3700", "3500")
# occb <- c("", "", "", "")
# occc <- c("", "", "", "")
# f_occgrp(occa, occb, occc)
# f_occgrp(occb, occa, occc)
# f_occgrp(occb, occc, occa)




