
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
  units = "pch" # change over previous value
  )

cpi <- cpi1 %>%
  filter(!is.na(value)) %>%
  mutate(year=year(date),
         cpipch=value / 100) %>%
  select(year, cpipch)
cpi

saveRDS(cpi, here::here("data", "cpi.rds"))


# get model parameters ----------------------------------------------------


# inflation ---------------------------------------------------------------
cpi_pch <- .02

#.. Social Security parameters ----
ss_eecrate <- .062


#.. wage-related parameters ----
# average wage index series and awi
awi1 <- read_excel(here::here("boyd", xlfn), sheet="awiseries", range="A2:B72")
awi <- awi1 %>%
  mutate(awi=awiseries[year==max(year)] / awiseries,
         awipch=awiseries / awiseries[match(year - 1, year)] - 1)
awi


# calculate average change in real wage index
real <- awi %>%
  left_join(cpi, by = "year") %>%
  filter(!is.na(awipch), !is.na(cpipch)) %>%
  mutate(realpch=awipch - cpipch)
summary(real)
real %>%
  select(year, contains("pch")) %>%
  pivot_longer(cols=-year) %>%
  ggplot(aes(year, value, colour=name)) +
  geom_line()

real_pch <- real %>% filter(year > 1980) %>% .$realpch %>% mean()
real_pch  # 0.009007175

salgrow <- real_pch + cpi_pch


#.. Pension parameters ----
params1 <- read_excel(here::here("boyd", xlfn), sheet="pension_parameters")
params1

params <- params1 %>%
  filter(!is.na(benfactor_normal)) %>%
  mutate(compound=as.logical(compound),
         socsec=as.logical(socsec))
params


# create data -------------------------------------------------------------
workers <- read_csv("
aoe, aor, aod, fas
25, 50, 85, 100e3
25, 55, 85, 100e3
25, 57, 85, 100e3
25, 60, 75, 100e3
25, 57, 82, 100e3
") %>%
  mutate(worker=row_number()) %>%
  select(worker, aoe, aor, aod, fas)
workers

# params <- read_csv(
# "stabbr, tier, name, benfactor, rpypp, cola, cola_compound, aor_normal, aor_min, fas, eec, socsec, source
# CA, newhire, pepra,.025, .02, TRUE, 57, 55,.071,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# CA, prior, poffb,.025, .02, TRUE, 57, 55,.071,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# CA, major, poffa,.03, .02, TRUE, 50, 50,.0,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# MA, newhire, ,.02, .02, TRUE, 57, 55,,,,,abc
# NY, newhire, ,.015, .02, TRUE, 57, 55,,,,,def
# ")
# params


# VERIFY that FAS=1 year for poffa
# COLA for all 3 is % starting in 2nd year after retirement. Cumulative adjustment can't be more than cumulative change in CPI since retirem
# =HYPERLINK("https://www.calpers.ca.gov/page/newsroom/calpers-news/2017/board-adopts-new-rates-state-school-employers","https://www.calpers.ca.gov/page/newsroom/calpers-news/2017/board-adopts-new-rates-state-school-employers")


data <- 
  full_join(workers, params %>% select(stabbr:socsec), by = character()) %>%  # cartesian product
  nest(wdata=c(aoe, aor, aod, fas)) %>%
  nest(params=c(benfactor_min:socsec)) %>%
  mutate(id=row_number()) %>%
  select(id, everything())
data

# data <- expand_grid(params, workers)  # same thing as full join by character()


# functions ---------------------------------------------------------------

#.. base data functions ----
fbase <- function(w){
  # w is wdata, data for a single worker -- one row
  # build the base data for years
  base <- tibble(age=w$aoe:w$aod) %>%
    mutate(year=row_number(),
           fyos=w$aor - w$aoe,  # final years of service
           yos=ifelse(age < w$aor, age - w$aoe + 1, NA_real_),  # actual years of service
           wage=ifelse(age < w$aor, 
                       w$fas / ((1 + salgrow)^(fyos - yos)),
                       0))
  base
}


#.. Social Security functions ----
fiwage <- function(age, wage, salgrow){
  # indexed (real) wage for each year
  # based on iwage factors, assumed to be the same for every year
  
  # inputs: wdf -- worker data frame (one worker)
  #         salgrow -- constant real growth rate for wages
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
  
  n <- age - 60
  awi <- 1 / (1 + salgrow)^n
  awi <- ifelse(age > 60, 1, awi)
  iwage <- wage * awi
  iwage
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
  if(length(best_wages) > 35) length(best_wages) <- 35
  aime <- sum(best_wages) / (length(best_wages) * 12)
  aime
}


fpia <- function(aime, bends){
  # pia -- primary insurance amount
  
  # inputsreceive a vector of years and wages, 
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
  
  # djb: the weights are fixed at:
  weights <- c(.9, .32, .15)
  
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
  pia <- 
    weights[1] * excess1 +
    weights[2] * excess2 +
    weights[3] * excess3
  
  # now add cola, if any
  pia
}


fsocsec <- function(w, b, p){
  # w: worker data
  # b: base data for years
  # p: params
  
  bends <- c(1024, 6172)
  # bends <- c(895, 5397)
  
  socsec <- tibble(sstax=ifelse(p$socsec &   # the person participates in Soc Sec
                          (b$yos > 0),
                        b$wage * ss_eecrate,
                        b$wage * 0)) %>%
    mutate(iwage=fiwage(b$age, b$wage, salgrow),  # indexed wage
           aime=faime(iwage),  # average indexed monthly earnings)
           pia=fpia(aime, bends)  # primary insurance amount
    )
  socsec
}


# .. pension functions ----------------------------------------------------

pension <- function(w, b, p){
  # w: worker data
  # b: base
  # p: params
  # TODO:
  
  # define interim-calc variables we don't need to keep in the output
  max_early_years <- p$aor_normal - p$aor_min
  early_penalty_per_year <- ifelse(max_early_years > 0,
                                   (p$benfactor_normal - p$benfactor_min) / max_early_years,
                                   0)
  
  # compute employee contributions and benefits by year
  results <- unnest(b, cols = c()) %>%
    # determine career wage and contributions
    mutate(eec=ifelse(yos > 0,
                      pmax(wage - p$eec_exclude, 0) * p$eec_rate,
                      0)) %>%
    # now determine retirement benefits
    mutate(early_years=pmax(p$aor_normal - w$aor, 0),
           early_penalty=early_years * early_penalty_per_year,
           benfactor=p$benfactor_normal - early_penalty,
           pctfas=benfactor * fyos,
           ibenefit=w$fas * pctfas,
           yor=ifelse(age >= w$aor, age - w$aor + 1, 0),
           benefit=ifelse(age >= w$aor,
                          ibenefit * (1 + p$cola)^(yor - 1),
                          0)
    ) %>%
    select(-names(b))
  results
}


# calculate results -------------------------------------------------------

df <- data %>%
  rowwise() %>%
  # build a series of list-columns each of which has a dataframe from age of entry to age of death
  mutate(base=list(fbase(wdata)),
         socsec=list(fsocsec(wdata, base, params)),
         pension=list(pension(wdata, base, params)))
df
glimpse(df)

df2 <- df %>%
  # filter(row_number()==1) %>%
  filter(tname=="peprass") %>%
  unnest(cols=c(base, pension))


# analyze results ---------------------------------------------------------

dr <- .04
age0 <- 50
df3 <- df %>%
  unnest(cols = c(wdata, base, socsec, pension)) %>%
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

# pen <- function(worker, params) {
#   # note that we cannot treat worker like a dataframe when it is passed as .data
#   # but we can access individual elements with the $ notation
#   # we must NOT return any variables in the return dataframe that have the same
#   # name as those passed in via worker (using the .data call)
#   print(unlist(worker))
#   worker_df <- params %>%
#     mutate(yos=worker$aor - worker$aoe + 1,
#            benfactor_adj=ifelse(worker$aor < aor_normal,
#                                 benfactor - rpypp * (aor_normal - worker$aor),
#                                 benfactor),
#            fyos=worker$aor - worker$aoe + 1,
#            pctfas=benfactor_adj * fyos,
#            ibenefit=worker$fas * pctfas)
# 
#   results <- expand_grid(age=worker$aoe:worker$aod, worker_df) %>%
#     mutate(
#       year=row_number(),
#       yos=ifelse(age < worker$aor, age - worker$aoe + 1, fyos),
#       yor=ifelse(age >= worker$aor, age - worker$aor + 1, 0),
#       benefit=ifelse(age >= worker$aor, ibenefit * (1 + cola)^(yor - 1), 0)
#     )
#   results
# }
