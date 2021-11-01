
# https://apps.urban.org/features/SLEPP/data.html


# libraries ---------------------------------------------------------------
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(scales)
library(lubridate)
library(purrr)
library(purrrlyr)
library(readxl)
library(vroom)
library(haven)
library(RSQLite)

library(Hmisc)

library(kableExtra)
library(gt)
library(knitr)
library(arrow)

library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)
library(gridExtra)
library(RcppRoll)
library(ggbreak)
library(patchwork)
library(RColorBrewer)

library(fixest)
library(cgwtools)  # for resave

library(fredr)

library(btools)
library(bdata)


# constants ---------------------------------------------------------------

# API keys
bea_apikey <- "21F782AD-56A6-439D-B3D5-9A592F020E26"
bls_apikey <- "e1a32c87a90f4d889f5342174e275470"
brbls_apikey <- "2ec4ce7e7f5b4934a8477539715adbae"
census_apikey <- "b27cb41e46ffe3488af186dd80c64dce66bd5e87"
fred_apikey <- "a5e1199baac333154cbffcba3b263c28"

# usethis::edit_r_environ() # setting the environment variable didn't work
# fredr_get_key()
# fredr_has_key()


# https://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/urban-institute-state-and-local-employee-pension-plan-database
# https://www.urban.org/sites/default/files/2019/01/02/slepp_database_2018_for_posting_december_update.xlsx
sleppd <- r"(E:\data\SLEPP_database\)"
sleppfn <- "slepp_database_2018_for_posting_december_update.xlsx"

xlfn <- "Boyd_CABU6(1).xlsx"


# ONETIME: Get and save cpi -----------------------------------------------
fredr_set_key(fred_apikey)
# get cpi for the same years so we can compute real awi historically
cpi1 <- fredr(
  series_id = "USACPIALLAINMEI",
  observation_start = as.Date("1949-01-01"),
  observation_end = as.Date("2020-01-01"),
  frequency = "a", # quarterly
  units = "lin" # lin (default) is level; pch is change over previous value
  )

cpi <- cpi1 %>%
  filter(!is.na(value)) %>%
  rename(cpi=value) %>%
  mutate(year=year(date),
         cpipch=cpi / lag(cpi) -1) %>%
  select(year, cpi, cpipch)
cpi

saveRDS(cpi, here::here("data", "cpi.rds"))


# create worker data -------------------------------------------------------------
workers <- read_csv("
aoe, aor, aod, fas
25, 50, 85, 100e3
25, 55, 85, 100e3
25, 57, 85, 100e3
25, 60, 75, 100e3
25, 57, 82, 100e3
") %>%
  mutate(worker=row_number(),
         yoe=1990,
         yob=yoe - aoe,
         yod=yob + aod,
         fyos=aor - aoe) %>%  # final years of service
  select(worker, yob, yoe, yod, aoe, aor, aod, fyos, fas)
workers


# get retirement plan parameters ----
params1 <- read_excel(here::here("boyd", xlfn), sheet="retplan_parameters")
params1
ns(params1)

params <- params1 %>%
  filter(!is.na(stabbr)) %>%
  mutate(across(c(db_covered, dc_covered, ss_covered, db_cola_compound), as.logical))
params


# join workers and parameters ---------------------------------------------
names(workers)
names(params)
data <- 
  full_join(workers, params %>% select(-db_source), by = character()) %>%  # cartesian product
  nest(wdata=c(yob:fas)) %>%  # worker data
  nest(params=dc_eec_rate:db_eec_exclude) %>%  # retirement plan parameters
  mutate(id=row_number()) %>%
  select(id, worker, stabbr, tier, tname, db_covered, ss_covered, dc_covered, everything())
data

# data <- expand_grid(params, workers)  # same thing as full join by character()
data %>%
  unnest(wdata)

data %>%
  unnest(params)


#.. inflation constants ---------------------------------------------------------------
realwage_pch <-  0.009007175  # average real growth in wage index, 1950-ish to 2020
cpi_pch <- .02
nomwage_pch <- realwage_pch + cpi_pch


#.. Social Security parameters ----
ss_eecrate <- .062


#.. Social Security normal retirement age ----
# https://www.ssa.gov/oact/ProgData/nra.html
# Normal Retirement Age
# Year of birth	Age
# 1937 and prior	65
# 1938	65 and 2 months
# 1939	65 and 4 months
# 1940	65 and 6 months
# 1941	65 and 8 months
# 1942	65 and 10 months
# 1943-54	66
# 1955	66 and 2 months
# 1956	66 and 4 months
# 1957	66 and 6 months
# 1958	66 and 8 months
# 1959	66 and 10 months
# 1960 and later	67

ss_nra <- tibble(yob=1930:2030) %>%
  mutate(nra = case_when(yob <= 1937 ~ 65,
                         yob %in% 1938:1942 ~ 65 + 2/12 * (yob - 1937),
                         yob %in% 1943:1954 ~ 66,
                         yob %in% 1955:1959 ~ 66 + 2/12 * (yob - 1954),
                         yob >= 1960 ~ 67,
                         TRUE ~ -Inf))
ss_nra

#.. get previously saved cpi historical data and construct forecast ----
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


#.. Social Security benefit parameters ----

#.. SS wage-related parameters ----
#.... SS average wage index series and awi ----
awi1 <- read_excel(here::here("boyd", xlfn), sheet="awiseries", range="A2:B72")
awi <- awi1 %>%
  mutate(awi=awiseries[year==max(year)] / awiseries,
         awipch=awiseries / awiseries[match(year - 1, year)] - 1)
awi

# compute a forecasted wage index series
awifc <- awi %>%
  select(year, awiseries) %>%
  bind_rows(tibble(year=2021:2200)) %>%
  mutate(awiseries=ifelse(year > 2020,
                          awiseries[year==2020] * (1 + realwage_pch + cpi_pch)^(year - 2020),
                          awiseries),
         awi=awiseries[year==2020] / awiseries,
         awi_pch=awiseries / awiseries[match(year - 1, year)] - 1)
awifc


#.. SS bends for the Social Security formula ----
bends1 <- read_excel(here::here("boyd", xlfn), sheet="bends", range="A4:C48")
bends1

# create a forecasted series
bendsfc <- bends1 %>%
  bind_rows(tibble(year=2023:2200)) %>%
  mutate(across(c(bend1, bend2), 
                function(x) ifelse(year > 2022,
                                   x[year==2022] * (1 + nomwage_pch)^(year - 2022),
                                   x)))
bendsfc


# functions ---------------------------------------------------------------
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
    mutate(year=w$yoe + row_number() - 1,
           yos=ifelse(age < w$aor, age - w$aoe + 1, 0),  # actual years of service, 0 in retirement
           yor=ifelse(age >= w$aor, age - w$aor + 1, 0),
           # FOR NOW: backcast wages from fas -- improve this when practical
           wage=ifelse(age < w$aor, 
                       w$fas / ((1 + nomwage_pch)^(w$fyos - yos)),
                       0)) %>%
    select(year, age, yos, yor, wage)
  base
}


#.. Social Security functions ----
fiwage <- function(yob, year, wage){
  # indexed (real) wage for each year
  # based on iwage factors, assumed to be the same for every year
  
  # inputs:
  #        nomwage_pch -- constant real growth rate for wages
  # output: iwage -- indexed wage series
  
  # https://www.ssa.gov/oact/COLA/awifactors.html
  
  # An individual's earnings are always indexed to the average wage level two
  # years prior to the year of first eligibility. Thus, for a person retiring at
  # age 62 in 2022, the person's earnings would be indexed to the average wage
  # index for 2020
  
  # A factor will always equal one for the year in which the person attains age
  # 60 and all later years. The indexing factor for a prior year Y is the result
  # of dividing the average wage index for the year in which the person attains
  # age 60 by the average wage index for year Y.
  
  # awifc -- year, awiseries, awi, pch
  # we want to construct a wage index that is 1 for age 60 and later, indexed to age 60 in earlier years
  iwagedf <- tibble(year, wage) %>%
    left_join(awifc %>% select(year, awiseries, awi), by="year") %>%
    mutate(idx=awiseries[year==(yob + 60)] / awiseries,
           idx=ifelse(year > (yob + 60), 1, idx),
           iwage=wage * idx)
  iwagedf$iwage
}


faime <- function(iwage){
  # aime -- average indexed monthly earnings
  
  # iwage -- vector of indexed wages
  
  # https://www.ssa.gov/oact/COLA/Benefits.html#aime
  
  # Up to 35 years of earnings are needed to compute average indexed monthly
  # earnings. After we determine the number of years, we choose those years with
  # the highest indexed earnings, sum such indexed earnings, and divide the
  # total amount by the total number of months in those years.
  
  # the sums for the highest 35 years of indexed earnings and the corresponding
  # average monthly amounts of such earnings. (The average is the result of
  # dividing the sum of the 35 highest amounts by the number of months in 35
  # years.) Such an average is called an "average indexed monthly earnings"
  # (AIME).
  
  best_wages <- sort(iwage[iwage > 0], decreasing = TRUE)
  if(length(best_wages) > 35) length(best_wages) <- 35 # only count the 35 best years
  aime <- sum(best_wages) / (length(best_wages) * 12) # get average, per month
  aime
}


fpia <- function(aime, yofe, yossnra){
  # The "primary insurance amount" (PIA) is the benefit (before rounding down to
  # next lower whole dollar) a person would receive if he/she elects to begin
  # receiving retirement benefits at his/her normal retirement age. At this age,
  # the benefit is neither reduced for early retirement nor increased for
  # delayed retirement.
  
  # STEPS:
  # - get wages through yor - 1, (https://www.ssa.gov/oact/ProgData/retirebenefit1.html)
  #     I assume SS yor is yossnra -- year of SS normal retirement age
  # - calc wages indexed to yoage 60 (yofe - 2), index=1 afterward (same), using nominal wage
  #     index values (https://www.ssa.gov/oact/COLA/AWI.html and https://www.ssa.gov/oact/COLA/awidevelop.html)
  # - calc aime based on highest (up to) 35 years (again https://www.ssa.gov/oact/ProgData/retirebenefit1.html)
  # - apply pia formula using bend points for age=62 (https://www.ssa.gov/oact/ProgData/retirebenefit2.html)
  #     bend points: (https://www.ssa.gov/oact/COLA/bendpoints.html)
  # - index this tentative pia from year of first eligibility, (yoage62) to yo SS retirement, which
  #     I assume to be yossnra for everyone (all retire for SS purposes at SS nra)
  #     using cpiw cola (same - https://www.ssa.gov/oact/ProgData/retirebenefit2.html)
  # - the result is pia, the initial benefit person would get if retires at ssnra
  # - (initial benefit could be adjusted up or down for early or late retirement, but I assume claim SS at nra)
  # - benefits in later years adjusted by cpiw cola
  
  # aime - average indexed monthly earnings using data through yor - 1, indexed to age 60 (index=1 after 60)
  # yofe - year of first eligibility -- yo age 62
  
  # pia -- primary insurance amount
  
  # ASSUMES the following are in the global environment:
  #   bendsfc -- forecast of bends cut points
  #   cpifc -- cpi forecast
  
  # inputs receive a vector of years and wages, 
  # return pia, primary insurance amount
  
  # https://www.ssa.gov/oact/COLA/Benefits.html
  # https://www.ssa.gov/oact/ProgData/retirebenefit2.html
  
  # The PIA is the sum of three separate percentages of portions of the AIME.
  # While the percentages of this PIA formula are fixed by law, the dollar
  # amounts in the formula change annually with changes in the national average
  # wage index. These dollar amounts, called "bend points," govern the portions
  # of the AIME.
  
  # The bend points in the year 2022 PIA formula, $1,024 and $6,172, apply for
  # workers becoming eligible in 2022. See the table of bend points for the bend
  # points applicable in past years.
  
  # For example, a person who had maximum-taxable earnings in each year since
  # age 22, and who retires at age 62 in 2022, would have an AIME equal to
  # $11,430. Based on this AIME amount and the bend points $1,024 and $6,172,
  # the PIA would equal $3,357.60. This person would receive a reduced benefit
  # based on the $3,357.60 PIA. The first COLA this individual could receive is
  # the one effective for December 2022.
  
  # bends <- c(NA_real_, NA_real_)
  bends <- c(bendsfc$bend1[bendsfc$year==yofe],
             bendsfc$bend2[bendsfc$year==yofe])
  
  # djb: the pia rates are fixed at:
  pia_rates <- c(.9, .32, .15)
  
  # examples from ss site:  aime --> pia
  #   4643, 2079.68: 	$1,024	$6,172	.9(1024) + .32(4643 - 1024) = $2,079.68
  #  10141, 2957.74:  $10,141	895	5,397	.9(895) + .32(5397 - 895) + .15(10141 - 5397) = $2,957.74
  # aime <- 4643; bends <- c(1024, 6172)
  # aime <- 10141; bends <- c(895, 5397)
  
  excess1 <- pmin(bends[1], aime)
  excess2 <- ifelse(aime > bends[1],
                    pmin(aime, bends[2]) - bends[1],
                    0)
  excess3 <- ifelse(aime > bends[2],
                    aime - bends[2],
                    0)
  pia_preCOLA <- 
    pia_rates[1] * excess1 +
    pia_rates[2] * excess2 +
    pia_rates[3] * excess3
  
  # now add cola, if any, from yofe to normal retirement age - 1
  cpi_yofe_minus1 <- cpifc$cpi[cpifc$year==(yofe - 1)]  # I think this is right, but triple check
  cpi_nra_minus1 <- cpifc$cpi[cpifc$year==(round(yossnra) - 1)]
  # note that I don't use the 5.9% cola for 2021, I use 2% (for convenience)
  
  pia <- pia_preCOLA * cpi_nra_minus1 / cpi_yofe_minus1
  pia
}


fsocsec <- function(ss_covered, w, b, p){
  # w: worker data (non-time-varying)
  # b: base data for the worker, per year
  # p: params for retirement benefit calculations
  
  if(!ss_covered){
    # socsec <- tibble(sstax=rep(0, nrow(b)), iwage=0, pia=0, ssbenefit=0)
    # return(socsec)
    return(NULL)
  }

  socsec <- unnest(b, cols = c()) %>%
    mutate(sstax=ifelse(yos > 0,
                        wage * ss_eecrate,
                        wage * 0)) %>%
    mutate(iwage=fiwage(w$yob, year, wage))  # indexed wage
  
  aime <- faime(socsec$iwage)  # average indexed monthly earnings, scalar
  
  yofe <- 62 - w$aoe + w$yoe # year of first eligibility -- year a person attains age 62
  # ss_nra is df with yob, nra
  ssnra <- ss_nra$nra[ss_nra$yob==w$yob]
  yossnra <- w$yob + ssnra # I assume everyone claims SS at ss normal retirement age, NOT at govt retirement
  
  pia <- fpia(aime, yofe, yossnra) # primary insurance amount, scalar
  
  socsec <- socsec %>%
    left_join(cpifc %>% select(year, cpi),
              by="year") %>%
    mutate(pia=pia,
           # determine annual benefits, with cola if relevant (now, ALWAYS -- djb)
           ssbenefit=ifelse(age < round(ssnra),
                            0,
                            pia * 12 * cpi / cpi[year==round(yossnra)])) %>%
    select(-names(b), -cpi)  # don't duplicate names

  socsec
}


# .. DB pension functions ----------------------------------------------------

pension <- function(db_covered, w, b, p){
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
  
  # compute employee contributions and benefits by year
  results <- unnest(b, cols = c()) %>%
    # determine career contributions
    mutate(db_eec=ifelse(yos > 0,
                         pmax(wage - p$db_eec_exclude, 0) * p$db_eec_rate,
                         0)) %>%
    # now determine retirement benefits
    mutate(early_years=pmax(p$db_aor_normal - w$aor, 0),
           early_penalty=early_years * early_penalty_per_year,
           db_benfactor=p$db_benfactor_normal - early_penalty,
           db_pctfas=db_benfactor * w$fyos,
           ipension=w$fas * db_pctfas, # initial pensoin
           pension=ifelse(age >= w$aor,
                          ipension * (1 + p$db_cola)^(b$yor - 1),
                          0)
    ) %>%
    select(-names(b))
  results
}



#.. DC defined contribution functions ---------------------------------------
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



# calculate results -------------------------------------------------------

df <- data %>%
  filter(stabbr != "MA") %>%
  rowwise() %>%
  # build a series of list-columns each of which has a dataframe from age of entry to age of death
  mutate(base=list(fbase(wdata)),
         socsec=list(fsocsec(ss_covered, wdata, base, params)),
         pension=list(pension(db_covered, wdata, base, params)),
         dcvalues=list(dcvalues(dc_covered, wdata, base, params)))
df
# glimpse(df)

df2 <- df %>%
  # filter(id==1) %>%
  # filter(tname=="peprass") %>%
  filter(id==1 |
           tname=="peprass") %>%
  # unnest(cols=c(wdata)) # params, wdata, base, socsec, pension
  unnest(cols=c(params, wdata, base, socsec, pension, dcvalues))

tmp <- df %>%
  filter(tier=="private") %>%
  # filter(id==1 |
  #          tname=="peprass") %>%
  unnest(cols=c(params, wdata, base, socsec, pension, dcvalues))



# analyze results ---------------------------------------------------------

dr <- .04
age0 <- 50

pvage0 <- function(value, dr, age, age0) {
  # pv in a given year
  value / {(1 + dr)^(age - age0)}
  }

df3 <- df %>%
  unnest(cols = c(wdata, base, socsec, pension)) %>%
  mutate(tier=factor(tier, levels=c("major", "prior", "newhire"))) %>%
  group_by(stabbr, tier, tname, worker) %>%
  arrange(age) %>%
  mutate(across(.cols=c(eec, benefit, sstax, ssbenefit), list(pv=~pvage0(.x, dr, age, age0)))) %>%
  ungroup %>%
  arrange(stabbr, tier, tname, worker) 
summary(df3)  # make sure there are no NA values

df3 %>%
  group_by(worker, stabbr, tier, tname) %>%
  summarise(fyos=first(fyos),
            aor=first(aor),
            pctfas=first(pctfas),
            eec_pv=sum(eec_pv),
            benefit_pv=sum(benefit_pv),
            sstax_pv=sum(sstax_pv),
            ssbenefit_pv=sum(ssbenefit_pv),
            .groups="drop") %>%
  mutate(net_penpv=benefit_pv - eec_pv,
         net_sspv=ssbenefit_pv - sstax_pv,
         net_retpv=net_penpv + net_sspv) %>%
  filter(worker==2) %>%
  arrange(worker, stabbr, tier) # %>%
  # mutate(eec_ch=eec_pv - eec_pv[tier=="major" & tname=="poffa"],
  #        ben_ch=ben_pv - ben_pv[tier=="major" & tname=="poffa"],
  #        net_ch=net_pv - net_pv[tier=="major" & tname=="poffa"],
  #        ben_pch=ben_pv / ben_pv[tier=="major" & tname=="poffa"] - 1,
  #        net_pch=net_pv / net_pv[tier=="major" & tname=="poffa"] - 1)


  
# notes to self about nesting and passing data ----------------------------

# passing . passes the ENTIRE data frame (ALL rows)
# passing .data passes the row, but NOT as a data frame
# passing a named list passes just that last  
df <- data %>%
  rowwise() %>%
  mutate(pension=list(pen(.data))) %>%
  ungroup

df2 <- df %>%
  # filter(row_number()==1) %>%
  filter(tname=="peprass") %>%
  unnest(cols = c(pension))


df %>%
  filter(row_number()==1, worker==1) %>%
  select(stabbr, tier, tname, worker, fas, aoe, aor, pension) %>%
  unnest(cols = c(pension))


df %>%
  unnest(cols = c(pension)) %>%
  filter(benefit > 0, id==2) %>%
  ggplot(aes(age, benefit, colour=tier)) +
  geom_line() +
  geom_point()



# summarize results -------------------------------------------------------


dr <- .04
age0 <- 50
df3 <- df %>%
  unnest(cols = c(pension)) %>%
  mutate(tier=factor(tier, levels=c("major", "prior", "newhire"))) %>%
  group_by(stabbr, tier, tname, worker) %>%
  arrange(age) %>%
  mutate(eec_pv=eec / {(1 + dr)^(age - age0)},
         ben_pv=benefit / {(1 + dr)^(age - age0)}) %>%
  ungroup %>%
  arrange(stabbr, tier, tname, worker) 

df3 %>%
  group_by(worker, stabbr, tier, tname) %>%
  summarise(fyos=first(fyos),
            aor=first(aor),
            pctfas=first(pctfas),
            eec_pv=sum(eec_pv),
            ben_pv=sum(ben_pv)) %>%
  mutate(net_pv=ben_pv - eec_pv) %>%
  ungroup %>%
  filter(worker==2) %>%
  arrange(tier) %>%
  mutate(eec_ch=eec_pv - eec_pv[tier=="major"],
         ben_ch=ben_pv - ben_pv[tier=="major"],
         net_ch=net_pv - net_pv[tier=="major"],
         ben_pch=ben_pv / ben_pv[tier=="major"] - 1,
         net_pch=net_pv / net_pv[tier=="major"] - 1)



# analysis ----------------------------------------------------------------

f <- function(df){
  print(df)
  # tibble(age=df$aoe:df$aod)
}

# . passes the ENTIRE dataframe to the function
# .data passes a "pronoun"
tmp <- data %>%
  group_by(stabbr, tier, tname, worker) %>%
  nest() %>%
  mutate(pen=list(f(.data)))



tmp <- data %>%
  group_by(select(everything())) %>%
  nest() %>%
  mutate(pen=list(f(.data)))

tmp <- data %>%
  rowwise() %>%
  nest(data = everything()) %>%
  mutate(pen=list(f(.)))


f <- function(df){
  # print(df$tier)
  tibble(age=df$aoe:df$aod) %>%
    mutate(age2=age^2)
}
tmp <- data %>%
  rowwise() %>%
  mutate(pen=list(f(.data)))
tmp

tmp2 <- tmp %>%
  unnest(pen)
tmp %>%
  filter(worker==1) %>%
  select(stabbr, tier, tname, worker, pen) %>%
  unnest(cols = c(pen))



# slepp prep --------------------------------------------------------------
vnames <- read_csv(
"vdesc, vname
order, order
Last updated, updated
State, state
Occupation, occ
Tier, tier
Plan name, pname
Plan type (FAS=final average salary; DC = defined contribution; CB = cash balance), ptype
Social Security coverage, socsec
2012 employee contribution rate, eec2012
2018 employee contribution rate, eec2018
Vesting (years), vestyears
Retirement eligibility: Normal (age/years of service), elig_normal
Retirement eligibility: Early (age/years of service), elig_early
Formula (Benefit = …), formula
Final average salary (FAS), fas
Multiplier, benfactor
Benefit supplement, bensupp
Early retirement penalty, erpenalty
Spreadsheet for details on early retirement penalty, details
Early retirement formula, erformula
Employer contribution rate to defined contribution plan, ercdc
Interest rate specified for defined contribution account balance, dcirate
Employee contribution to defined contribution plan, eecdc
Interest rate earned on cash balance plan, cbirate
Cost-of-living adjustment (COLA), cola
Notes, notes
Deferred vested start date, dveststart
Deferred vested increases, dvestincrease
Covers new hires? (0 = no; 1 = yes), newhires
Mandatory retirement? (0 = no; 1 = yes), mandret
Deferred retirement option plan (DROP)? (0 = no; 1 = yes), drop
2016 ratio of operating cash flow to assets1, opcfassets2016
2016 state fiscal grade1, grade2016
State operating cash flow ratio 3 year average (2014-2016)1, opcfassets3year
2016 state funded ratio @ reported discount rate1, fr2016planrate
2016 state funded ratio @ 6.5% discount rate1, fr2016dr6p5
State net amortization as a share of payroll 3 year average (2014-2016)1, netamortpct3year
...38, junk
Main Source, source1
Additional Sources, source2
Miscellaneous notes, source3
Plan ID, planid
") %>% mutate(vorder=row_number())


# get slepp ---------------------------------------------------------------
slepp1 <- read_excel(paste0(sleppd, sleppfn), sheet="State plans", skip=1)

glimpse(slepp1)
names(slepp1)

slepp2 <- slepp1 %>%
  setNames(vnames$vname)
glimpse(slepp2)

caslepp <- slepp2 %>%
  filter(str_detect(state, "California"), 
         str_detect(pname, "CalPERS"),
         !str_detect(occ, "General"),
         !str_detect(tier, "Local")) %>%
  select(updated, state, occ, tier, planid, pname, ptype, socsec, eec2018, eec2018, vestyears,
         elig_normal, elig_early, formula, fas, benfactor, bensupp,
         erpenalty, details, erformula, cbirate, cola, notes, newhires, mandret, drop, 
         source1, source2, source3)



# OLD stuff ---------------------------------------------------------------
