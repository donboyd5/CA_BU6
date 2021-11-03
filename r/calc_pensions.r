# Don Boyd
# 11/3/2021

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
# library(cgwtools)  # for resave

library(btools)
library(bdata)


# constants ---------------------------------------------------------------


xlfn <- "Boyd_CABU6(1).xlsx"


# create worker data -------------------------------------------------------------
workers <- read_csv("
aoe, aor, aod, fas
25, 50, 85, 100e3
25, 55, 85, 100e3
25, 57, 85, 100e3
25, 60, 85, 100e3
") %>%
  mutate(worker=row_number(),
         yoe=1990,
         yob=yoe - aoe,
         yod=yob + aod,
         fyos=aor - aoe) %>%  # final years of service
  select(worker, yob, yoe, yod, aoe, aor, aod, fyos, fas)
workers


# get retirement plan parameters and make merged file ----------
params1 <- read_excel(here::here("boyd", xlfn), sheet="retplan_parameters", skip = 1)
params1
ns(params1)

params <- params1 %>%
  mutate(across(c(db_max_benpct, db_cola_base), as.numeric),
         across(c(include, db_covered, dc_covered, ss_covered, db_cola_compound), as.logical)) %>%
  filter(include)
params
glimpse(params)


# join workers and parameters
names(workers)
names(params)
data <- 
  full_join(workers, params %>% select(-db_source), by = character()) %>%  # cartesian product
  nest(wdata=c(yob:fas)) %>%  # worker data
  nest(params=dc_eec_rate:db_eec_rate2) %>%  # retirement plan parameters
  mutate(id=row_number()) %>%
  select(id, worker, stabbr, tier, tname, db_covered, ss_covered, dc_covered, everything())
data

# data <- expand_grid(params, workers)  # same thing as full join by character()
data %>%
  unnest(wdata)

data %>%
  unnest(params)


# Define additional parameters and functions ------------------------------

#.. ONETIME get and save cpi: only do this when data are updated ----
# source(here::here("r", "ONETIME_get_and_save_cpi.r"))
#.. END ONETIME get and save cpi ----

# inflation constants
realwage_pch <-  0.009007175  # average real growth in wage index, 1950-ish to 2020
cpi_pch <- .02
nomwage_pch <- realwage_pch + cpi_pch

# Social Security parameters
ss_eecrate <- .062
source(here::here("r", "functions_socsec.r"))

source(here::here("r", "functions_DBplans.r"))
source(here::here("r", "functions_DCplans.r"))


# calculate results -------------------------------------------------------

data %>% unnest(cols=params)

df <- data %>%
  # filter(stabbr != "MA") %>%
  rowwise() %>%
  # build a series of list-columns each of which has a dataframe from age of entry to age of death
  mutate(base=list(fbase(wdata)),
         socsec=list(fsocsec(ss_covered, wdata, base, params)),
         pension=list(pension(db_covered, stabbr, tier, wdata, base, params)),
         dcvalues=list(dcvalues(dc_covered, wdata, base, params)))
df
# glimpse(df)


# summarize results ---------------------------------------------------------

dr <- .04
age0 <- 50
year0 <- 2021

# pvage0 <- function(value, dr, age, age0) {
#   # pv in a given year
#   value / {(1 + dr)^(age - age0)}
# }

pvyear0 <- function(value, dr, year, year0) {
  # pv in a given year
  value / {(1 + dr)^(year - year0)}
}
# pv=~pvage0(.x, dr, age, age0))
# pv=~pvyear0(.x, dr, year, year0))

df3 <- df %>%
  unnest(cols = c(wdata, base, socsec, pension, dcvalues)) %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%
  mutate(tier=factor(tier, levels=c("major", "prior", "newhire", "private"))) %>%
  group_by(stabbr, tier, tname, worker) %>%
  arrange(age) %>%
  mutate(across(.cols=c(db_eec, db_pension,
                        sstax, ssbenefit,
                        dc_eec, dc_annuity),
                list(pv=~pvyear0(.x, dr, year, year0)))) %>%
  ungroup %>%
  arrange(stabbr, tier, tname, worker) 
summary(df3)  # make sure there are no NA values
ns(df3)

tmp <- df3 %>%
  filter(db_pctfas != db_pctfas_uncapped)

results <- df3 %>%
  group_by(worker, stabbr, tier, tname) %>%
  summarise(fyos=first(fyos),
            aoe=first(aoe),
            aor=first(aor),
            aod=first(aod),
            db_pctfas=first(db_pctfas),
            db_eec_pv=sum(db_eec_pv),
            db_pension_pv=sum(db_pension_pv),
            sstax_pv=sum(sstax_pv),
            ssbenefit_pv=sum(ssbenefit_pv),
            dc_eec_pv=sum(dc_eec_pv),
            dc_annuity_pv=sum(dc_annuity_pv),
            .groups="drop") %>%
  mutate(net_penpv=db_pension_pv - db_eec_pv,
         net_sspv=ssbenefit_pv - sstax_pv,
         net_dcpv=dc_annuity_pv - dc_eec_pv,
         net_dbdcpv=net_penpv + net_dcpv,
         net_retpv=net_penpv + net_sspv + net_dcpv) %>%
  arrange(worker, stabbr, tier) 


# examine results ---------------------------------------------------------
results %>%
  select(worker, stabbr, tier, tname, aoe, aor, aod, contains("net"))

# all retirement income, including SS
results %>%
  select(worker, stabbr, tier, tname, aoe, aor, aod, net_retpv) %>%
  pivot_wider(names_from = c(tier, tname), values_from = net_retpv)

# just net pension benefits (not SS)
results %>%
  select(worker, stabbr, tier, tname, aoe, aor, aod, net_dbdcpv) %>%
  pivot_wider(names_from = c(tier, tname), values_from = net_dbdcpv)


tmp <- df3 %>%
  filter(worker==1, stabbr=="CA", tier=="major", tname=="poffa") %>%
  unnest(cols=params)
# why does net pv at age 50 fall for those who retire later? I get it for the DB plans
# but why the dc plans?
  


# OLD BELOW HERE ----------------------------------------------------------

#.. notes to self about nesting and passing data ----------------------------

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
