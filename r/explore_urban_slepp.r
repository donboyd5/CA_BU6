

# notes -------------------------------------------------------------------

# https://apps.urban.org/features/SLEPP/data.html



# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(scales)
library(lubridate)
library(purrr)
library(purrrlyr)
library(readxl)

# constants ---------------------------------------------------------------

# https://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/urban-institute-state-and-local-employee-pension-plan-database
# https://www.urban.org/sites/default/files/2019/01/02/slepp_database_2018_for_posting_december_update.xlsx
sleppd <- r"(E:\data\SLEPP_database\)"
sleppfn <- "slepp_database_2018_for_posting_december_update.xlsx"


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
