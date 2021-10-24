
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

library(btools)
library(bdata)


# constants ---------------------------------------------------------------



# https://www.urban.org/policy-centers/cross-center-initiatives/program-retirement-policy/projects/urban-institute-state-and-local-employee-pension-plan-database
# https://www.urban.org/sites/default/files/2019/01/02/slepp_database_2018_for_posting_december_update.xlsx
sleppd <- r"(E:\data\SLEPP_database\)"
sleppfn <- "slepp_database_2018_for_posting_december_update.xlsx"



# get model parameters ----------------------------------------------------
fn <- "Boyd_CABU6(1).xlsx"
sheet <- "pension_parameters"

params <- read_excel(here::here("boyd", fn), sheet=sheet)
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
  mutate(id=row_number())
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
  full_join(params %>% select(stabbr:socsec), workers, by = character()) 

# data <- expand_grid(params, workers)  # same thing


# functions ---------------------------------------------------------------
pen <- function(worker){
  # define variables we don't need to keep in the output
  max_early_years <- worker$aor_normal - worker$aor_min
  early_penalty_per_year <- ifelse(max_early_years > 0,
                                   (worker$benfactor_normal - worker$benfactor_min) / max_early_years,
                                   0)
  
  # compute benefits by year
  results <- tibble(age=worker$aoe:worker$aod) %>%
    mutate(fyos=worker$aor - worker$aoe + 1,
           early_years=pmax(worker$aor_normal - worker$aor, 0),
           early_penalty=early_years * early_penalty_per_year,
           benfactor=worker$benfactor_normal - early_penalty,
           pctfas=benfactor * fyos,
           ibenefit=worker$fas * pctfas,
           year=row_number(),
           yos=ifelse(age < worker$aor, age - worker$aoe + 1, fyos),
           yor=ifelse(age >= worker$aor, age - worker$aor + 1, 0),
           benefit=ifelse(age >= worker$aor, ibenefit * (1 + worker$cola)^(yor - 1), 0)
           )
  results
}


# passing . passes the ENTIRE data frame
# passing .data passes the row, but NOT as a data frame
df <- data %>%
  rowwise() %>%
  mutate(pension=list(pen(.data))) %>%
  ungroup

df2 <- df %>%
  filter(row_number()==1) %>%
  unnest(cols = c(pension))


df %>%
  unnest(cols = c(pension)) %>%
  filter(benefit > 0, id==2) %>%
  ggplot(aes(age, benefit, colour=tier)) +
  geom_line() +
  geom_point()


dr <- .04
age0 <- 50
df3 <- df %>%
  unnest(cols = c(pension)) %>%
  mutate(tier=factor(tier, levels=c("major", "prior", "newhire"))) %>%
  group_by(stabbr, tier, tname, id) %>%
  arrange(age) %>%
  mutate(pv=benefit / {(1 + dr)^(age - age0)}) %>%
  ungroup %>%
  arrange(stabbr, tier, tname, id) 

df3 %>%
  group_by(id, stabbr, tier, tname) %>%
  summarise(fyos=first(fyos),
            aor=first(aor),
            pctfas=first(pctfas),
            pv=sum(pv)) %>%
  ungroup %>%
  filter(id==2) %>%
  arrange(tier) %>%
  mutate(pchange=pv / pv[tier=="major"] - 1)


# analysis ----------------------------------------------------------------



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
