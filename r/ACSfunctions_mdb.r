# Don Boyd
# 11/20/2015
# ACSfunctions_mdb.r


#****************************************************************************************************
#                Geographies ####
#****************************************************************************************************

f_puma_county <- function(puma00, puma10, st) {
  # pumacnty = f_puma_county(PUMA00, PUMA10, ST)
  # PUMA codes are 5 digits

  # PUMA00 Census 2000
  # PUMA10 Census 2010
  # key fips
  # NYC Bronx 36005 Kings 36047 New York 36061 Queens 36081 Richmond 36085 
  # Nassau 36059 
  # Suffolk 36103 
  # Westchester 36119 
  # 005 047 061 081 085 059 103 119
  # use at http://mcdc.missouri.edu/websas/geocorr12.html
  
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
  westchester00 <- st==36 & puma00 %in% c(3400, 3501:3506)
  
  nassau00 <- st==36 & puma00 %in% 4201:4212
  suffolk00 <- st==36 & puma00 %in% 4301:4312
  
  bronx00 <- st==36 & puma00 %in% 3701:3710
  manhattan00 <- st==36 & puma00 %in% 3801:3810
  statenisland00 <- st==36 & puma00 %in% 3901:3903
  brooklyn00 <- st==36 & puma00 %in% 4001:4018
  queens00 <- st==36 & puma00 %in% 4101:4114
  
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
  westchester10 <- st==36 & puma10 %in% 3101:3107
  
  nassau10 <- st==36 & puma10 %in% 3201:3212
  suffolk10 <- st==36 & puma10 %in% 3301:3313
  
  bronx10 <-  st==36 & puma10 %in% 3701:3710
  manhattan10 <- st==36 & puma10 %in% 3801:3810
  statenisland10 <- st==36 & puma10 %in% 3901:3903
  brooklyn10 <- st==36 & puma10 %in% 4001:4018
  queens10 <- st==36 & puma10 %in% 4101:4114
  # RECODE PUMA (3701 thru 3710=1) (3801 thru 3810=1) (3901 thru 3903=1) (4001 thru 4018=1) (4101 thru 4114=1) (ELSE=0) INTO NYCResident.

  pumacnty <- rep("other", length(puma00))
  pumacnty[westchester00 | westchester10] <- "westchester"
  pumacnty[nassau00 | nassau10] <- "nassau"
  pumacnty[suffolk00 | suffolk10] <- "suffolk"
  pumacnty[bronx00 | bronx10] <- "bronx"
  pumacnty[manhattan00 | manhattan10] <- "manhattan"
  pumacnty[statenisland00 | statenisland10] <- "statenisland"
  pumacnty[brooklyn00 | brooklyn10] <- "brooklyn"
  pumacnty[queens00 | queens10] <- "queens"
  return(pumacnty)
}

# f_puma_county(03100, 0, 36)
# f_puma_county(03701, 0, 36)
# f_puma_county(03701, 0, 35)
# f_puma_county(0, 03701, 36)
# f_puma_county(03502, 0, 36)
# f_puma_county(0, 03502, 36)
# f_puma_county(0, 03103, 36)


f_puma_powcounty <- function(powpuma00, powpuma10, powsp05, powsp12) {
  # powpumas (place of work public use microdata areas) are more aggregated than puma codes
  # they are unique within state
  
  # POWPUMA00 5 digits Place of work PUMA based on Census 2000 definition for data collected prior to 2012
  # POWPUMA10 5 difits Place of work PUMA based on 2010 Census definition for data collected in 2012 or later
  
  # POWSP05 3 Place of work for data collected prior to 2012 - State or foreign country recode
  # POWSP12 3 Place of work for data collected in 2012 or later - State or foreign country recode
  
  # powpuma00 must be used with powsp05, and powpuma10 with powsp12
  
  
  # I determined the powpumas for key counties by examining the
  # worksheet "POWPUMA-2009-2013-lookup" in the Census file "ACSPUMS2009_2013CodeLists.xlsx" (avail online)
  # There may be a better source, but I have not found it.
  
  # key counties		powpuma00	powpuma10
 
  # 119	Westchester County	 3400:3590	3100
   
  # 59	Nassau County	 4207:4291	3200
  # 103	Suffolk County	 4390:4392	3300

  # 005	Bronx	3700	3700
  # 47	Kings County	04000	04000
  # 61	New York County	03800	03800
  # 81	Queens County	04100	04100
  # 85	Richmond County	03900	03900
  
  # first, 2000
  westchester00 <- powsp05==36 & powpuma00 %in% 3400:3590
  
  nassau00 <- powsp05==36 & powpuma00 %in% 4207:4291
  suffolk00 <- powsp05==36 & powpuma00 %in% 4390:4392
  
  bronx00 <- powsp05==36 & powpuma00 %in% 3700
  brooklyn00 <- powsp05==36 & powpuma00 %in% 4000
  manhattan00 <- powsp05==36 & powpuma00 %in% 3800
  queens00 <- powsp05==36 & powpuma00 %in% 4100
  statenisland00 <- powsp05==36 & powpuma00 %in% 3900

  # now 2010
  westchester10 <- powsp12==36 & powpuma10 %in% 3100
  
  nassau10 <- powsp12==36 & powpuma10 %in% 3200
  suffolk10 <- powsp12==36 & powpuma10 %in% 3300
  
  bronx10 <- powsp12==36 & powpuma10 %in% 3700
  brooklyn10 <- powsp12==36 & powpuma10 %in% 4000
  manhattan10 <- powsp12==36 & powpuma10 %in% 3800
  queens10 <- powsp12==36 & powpuma10 %in% 4100
  statenisland10 <- powsp12==36 & powpuma10 %in% 3900

    
  pumapowcnty <- rep("other", length(powpuma00)) # default pumapowcounty is "other"
  pumapowcnty[westchester00 | westchester10] <- "westchester"
  pumapowcnty[nassau00 | nassau10] <- "nassau"
  pumapowcnty[suffolk00 | suffolk10] <- "suffolk"
  pumapowcnty[bronx00 | bronx10] <- "bronx"
  pumapowcnty[manhattan00 | manhattan10] <- "manhattan"
  pumapowcnty[statenisland00 | statenisland10] <- "statenisland"
  pumapowcnty[brooklyn00 | brooklyn10] <- "brooklyn"
  pumapowcnty[queens00 | queens10] <- "queens"
  return(pumapowcnty)
}




# MIGPUMA00 5 Migration PUMA based on Census 2000 definition for data collected prior to 2012
# bbbbb .N/A (person less than 1 year old/lived in same house 1 year ago)
# 00001 .Did not live in the United States or in Puerto Rico one year ago
# 00002 .Lived in Puerto Rico one year ago and current residence is in the U.S.
# 00100..08200 .Assigned Migration PUMA. Use with MIGSP05.
# -0009 .Code classification is Not Applicable because data collected in 2012 or later
# 
# MIGPUMA10 5 Migration PUMA based on 2010 Census definition for data collected in 2012 or later
# bbbbb .N/A (person less than 1 year old/lived in same house 1 year ago)
# 00001 .Did not live in the United States or in Puerto Rico one year ago
# 00002 .Lived in Puerto Rico one year ago and current residence is in the U.S.
# 00100..70100 .Assigned Migration PUMA. Use with MIGSP12.

# MIGSP05 3 Migration recode for data collected prior to 2012 - State or foreign country code
# MIGSP12 3 Migration recode for data collected in 2012 or later - State or foreign country code



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
f_rac1p <- function(rac1p) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  labs <- c("White alone", "Black alone", "Asian alone", "Am-Indian alone", "Alaska Native alone",
            "Am-Ind or AK native tribe", "Hawaiian or PacIsland alone", "Other race alone", "Two or more races")
  #factor(rac1p, levels=levs, labels=labs, ordered=TRUE)
  x <- ordered(rac1p, levels=levs, labels=labs)
  return(x)
}

f_race <- function(rac1p) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  x <- factor(rac1p, levels=levs, ordered = TRUE)
  levels(x) <- list("White alone"=1, 
                    "Black alone"=2, 
                    "Asian alone"=6, 
                    "Other race alone"=c(3:5, 7:8), 
                    "Two or more races"=9)
  return(x)
}
# table(f_race(c(1:9, 3, 5)))


f_race_hisp <- function(rac1p, hisp) {
  y <- ifelse(hisp==1, rac1p, 0) # hisp==1 means NOT hispanic, hisp>1 is hispanic
  y <- factor(y)
  levels(y) <- list("White alone, not Hispanic"=1, 
                    "Black alone, not Hispanic"=2,
                    "Hispanic, any race"=0,
                    "Other"=c(3:9))
  return(y)
}

f_race_hisp_old <- function(rac1p, hisp) {
  y <- ifelse(hisp!=1, 0, rac1p)
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
f_occgrp <- function(occp02, occp10, occp12) {
  # codes are defined in Census file ACSPUMS2009_2013CodeLists.xlsx
  # Use different variables for differnt data years as follows:
  #   occp02 2009
  #   occp10 2010, 2011
  #   occp12 2012, 2013
  # codes usually are the same in each year, but they MAY be different so check the Census doc
  
  # 6230 carpenter, all 3 variables
  # 6200-6940	47-0000	.	.	.	Construction and Extraction Occupations -- all 3 variables
  
  # define code groups for each occupation of interest
  # if they differ across occp02, occp10, occp12, create multiple variables for each kind (e.g., hlth.support.c02, hlth.support.c10, ...)
  
  # construction as a whole, and then subcategories
  construction.c <- as.character(6200:6940)
  supervisor.construction.c <- "6200"
  carpenter.c <- "6230"
  
  # services as a whole, and then subcategories
  service.c <- as.character(3600:4650)
  
  hlth.support.c02 <- as.character(3600:3650)
  hlth.support.c10 <- as.character(3600:3655)
  hlth.support.c12 <- as.character(3600:3655)
  
  foodprep.c <- as.character(4000:4160)
  bldg.grounds.c <- as.character(4200:4250)
  personal.care.etc.c <- as.character(4300:4650)
  
  
  # now create a single variable for each major category
  construction <- (occp02 %in% construction.c) |
                  (occp10 %in% construction.c) |
                  (occp12 %in% construction.c)
  
  consuper <- (occp02 %in% supervisor.construction.c) |
              (occp10 %in% supervisor.construction.c) |
              (occp12 %in% supervisor.construction.c)
  
  carpenter <- (occp02 %in% carpenter.c) |
               (occp10 %in% carpenter.c) |
               (occp12 %in% carpenter.c)
  
  service <- (occp02 %in% service.c) |
             (occp10 %in% service.c) |
             (occp12 %in% service.c)

  hlth.support <- (occp02 %in% hlth.support.c02) |
                  (occp10 %in% hlth.support.c10) |
                  (occp12 %in% hlth.support.c12)

  foodprep <- (occp02 %in% foodprep.c) |
              (occp10 %in% foodprep.c) |
              (occp12 %in% foodprep.c)
  
  bldg.grounds <- (occp02 %in% bldg.grounds.c) |
                  (occp10 %in% bldg.grounds.c) |
                  (occp12 %in% bldg.grounds.c)
    
  personal.care.etc <- (occp02 %in% personal.care.etc.c) |
                       (occp10 %in% personal.care.etc.c) |
                       (occp12 %in% personal.care.etc.c)
  
  
  occgrp <- rep("_other.or.NILF", length(occp02)) # default
  
  occgrp[carpenter] <- "construction.carpenter"
  occgrp[consuper] <- "construction.trade.supervisor"
  occgrp[construction & !carpenter & !consuper] <- "construction._other"
  
  occgrp[hlth.support] <- "service.health.support"
  occgrp[foodprep] <- "service.foodprep"
  occgrp[bldg.grounds] <- "service.bldg.grounds"
  occgrp[personal.care.etc] <- "service.personal.care.and.othrservice"
  occgrp[service & !hlth.support & !foodprep & !bldg.grounds & !personal.care.etc] <- "service._other"
  
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


# Other work variables ####
# WKHP 2 Usual hours worked per week past 12 months
# bb .N/A (less than 16 years old/did not work during the past 12 months)
# 01..98 .1 to 98 usual hours
# 99 .99 or more usual hours
# 
# WKL 1 When last worked
# b .N/A (less than 16 years old)
# 1 .Within the past 12 months
# 2 .1-5 years ago
# 3 .Over 5 years ago or never worked
# 
# WKW 1 Weeks worked during past 12 months
# b .N/A (less than 16 years old/did not work during the past 12 months)
# 1 .50 to 52 weeks
# 2 .48 to 49 weeks
# 3 .40 to 47 weeks
# 4 .27 to 39 weeks
# 5 .14 to 26 weeks
# 6 .13 weeks or less
# 
# WRK 1 Worked last week
# b .N/A (not reported)
# 1 .Worked
# 2 .Did not work

# ESR 1 Employment status recode
# b .N/A (less than 16 years old)
# 1 .Civilian employed, at work
# 2 .Civilian employed, with a job but not at work
# 3 .Unemployed
# 4 .Armed forces, at work
# 5 .Armed forces, with a job but not at work
# 6 .Not in labor force


#****************************************************************************************************
#                Class of worker (COW) code ####
#****************************************************************************************************
# COW 1 Class of worker
# b .Not in universe (less than 16 years old/NILF who last worked more than 5 years ago or never worked)
# 1 .Employee of a private for-profit company or business, or of an individual, for wages, salary, or commissions
# 2 .Employee of a private not-for-profit, tax-exempt, or charitable organization
# 3 .Local government employee (city, county, etc.)
# 4 .State government employee
# 5 .Federal government employee
# 6 .Self-employed in own not incorporated business, professional practice, or farm
# 7 .Self-employed in own incorporated business, professional practice or farm
# 8 .Working without pay in family business or farm
# 9 .Unemployed and last worked 5 years ago or earlier or never worked

f_cow <- function(cow) {
  # cowf <- factor(cow)
  cowf <- factor(cow, exclude=NULL) # exclude=NULL allows NA to be one of the factor levels
  levels(cowf) <- list("Employed, private" = 1:2,
                       "Employed, govt" = 3:5,
                       "Self-employed" = 6:7,
                       "Unemployed" = 9,
                       "Family business" = 8,
                       "NILF"=NA) # Not In Labor Force
  return(cowf)
}

# f_cow(1:10)
# f_cow(c(NA, 1:10))
# table(f_cow(c(NA, 1:10)))
# count(data.frame(fc=f_cow(c(NA, 1:10))), fc)


#****************************************************************************************************
#                Nativity and immigration ####
#****************************************************************************************************
# CIT 1 Citizenship status
# 1 .Born in the U.S.
# 2 .Born in Puerto Rico, Guam, the U.S. Virgin Islands, or the Northern Marianas
# 3 .Born abroad of American parent(s)
# 4 .U.S. citizen by naturalization
# 5 .Not a citizen of the U.S.

# CITWP05 4 Year of naturalization write-in for data collected prior to 2012
# CITWP12 4 Year of naturalization write-in for data collected in 2012 or later
 
# ENG 1 Ability to speak English
# b .N/A (less than 5 years old/speaks only English)
# 1 .Very well
# 2 .Well
# 3 .Not well
# 4 .Not at all

# MIG 1 Mobility status (lived here 1 year ago)
# b .N/A (less than 1 year old)
# 1 .Yes, same house (nonmovers)
# 2 .No, outside US and Puerto Rico
# 3 .No, different house in US or Puerto Rico



# NATIVITY 1 Nativity
# 1 .Native
# 2 .Foreign born

# WAOB 1 World area of birth ****
# 1 .US state (POB = 001-059)
# 2 .PR and US Island Areas (POB = 060-099)
# 3 .Latin America (POB = 303,310-399)
# 4 .Asia (POB = 158-159,161,200-299)
# 5 .Europe (POB = 100-157,160,162-199)
# 6 .Africa (POB = 400-499)
# 7 .Northern America (POB = 300-302,304-309)
# 8 .Oceania and at Sea (POB = 500-554)



#****************************************************************************************************
#                Health insurance ####
#****************************************************************************************************
# HICOV 1 Health insurance coverage recode
# 1 .With health insurance coverage
# 2 .No health insurance coverage

# PRIVCOV 1 Private health insurance coverage recode
# 1 .With private health insurance coverage
# 2 .Without private health insurance coverage
# 
# PUBCOV 1 Public health coverage recode
# 1 .With public health coverage
# 2 .Without public health coverage

# HINS1 1 Insurance through a current or former employer or union
# 1 .Yes
# 2 .No
# HINS2 1 Insurance purchased directly from an insurance company
# 1 .Yes
# 2 .No
# HINS3 1 Medicare, for people 65 and older, or people with certain disabilities
# 1 .Yes
# 2 .No
# HINS4 1 Medicaid, Medical Assistance, or any kind of government-assistance plan for those with low incomes or a disability
# 1 .Yes
# 2 .No
# HINS5 1 TRICARE or other military health care
# 1 .Yes
# 2 .No
# HINS6 1 VA (including those who have ever used or enrolled for VA health care)
# 1 .Yes
# 2 .No
# HINS7 1 Indian Health Service
# 1 .Yes
# 2 .No

# hidf <- getall %>%
#   # define desired vars
#   select(serialno, pwgtp, hicov, privcov, pubcov, starts_with("hins"))
# system.time(hidf <- collect(hidf)) # first time is slow, later is fast 6 secs vs 100 secs rsqlite
# ht(hidf)
# hidf %>% mutate(hins.sum=hins1+hins2+hins3+hins4+hins5+hins6+hins7) %>%
#   group_by(hicov) %>%
#   summarise(hins.sum.min=min(hins.sum), hins.sum.max=max(hins.sum))
# 
# tmp <- hidf %>% mutate(hins.sum=hins1+hins2+hins3+hins4+hins5+hins6+hins7) %>%
#   filter(hicov==2, hins.sum==13)
# looks like 38k people have hins7==1 and yet have hicov==2, i.e., Indian Health Service does not count as hicov



#****************************************************************************************************
#                Marital status ####
#****************************************************************************************************
# MAR 1 Marital status
# 1 .Married
# 2 .Widowed
# 3 .Divorced
# 4 .Separated
# 5 .Never married or under 15 years old


# MSP 1 Married, spouse present/spouse absent
# b .N/A (age less than 15 years)
# 1 .Now married, spouse present
# 2 .Now married, spouse absent
# 3 .Widowed
# 4 .Divorced
# 5 .Separated
# 6 .Never married


#****************************************************************************************************
#                Educational attainment ####
#****************************************************************************************************
# SCHL 2 Educational attainment
# bb .N/A (less than 3 years old)
# 01 .No schooling completed
# 02 .Nursery school, preschool
# 03 .Kindergarten
# 04 .Grade 1
# 05 .Grade 2
# 06 .Grade 3
# 07 .Grade 4
# 08 .Grade 5
# 09 .Grade 6
# 10 .Grade 7
# 11 .Grade 8
# 12 .Grade 9
# 13 .Grade 10
# 14 .Grade 11
# 15 .12th grade - no diploma
# 16 .Regular high school diploma
# 17 .GED or alternative credential
# 18 .Some college, but less than 1 year
# 19 .1 or more years of college credit, no degree
# 20 .Associate's degree
# 21 .Bachelor's degree
# 22 .Master's degree
# 23 .Professional degree beyond a bachelor's degree
# 24 .Doctorate degree


# RELP 2 Relationship
# 00 .Reference person
# 01 .Husband/wife
# 02 .Biological son or daughter
# 03 .Adopted son or daughter
# 04 .Stepson or stepdaughter
# 05 .Brother or sister
# 06 .Father or mother
# 07 .Grandchild
# 08 .Parent-in-law
# 09 .Son-in-law or daughter-in-law
# 10 .Other relative
# 11 .Roomer or boarder
# 12 .Housemate or roommate
# 13 .Unmarried partner



