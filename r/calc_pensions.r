

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


# create data -------------------------------------------------------------
workers_df <- read_csv("
aoe, aor, aod, fas
25, 50, 85, 100e3
25, 55, 85, 100e3
25, 57, 85, 100e3
25, 60, 75, 100e3
25, 57, 82, 100e3
") %>%
  mutate(id=row_number())
workers_df

# params <- read_csv(
# "stabbr, tier, name, benfactor, rpypp, cola, cola_compound, aor_normal, aor_min, fas, eec, socsec, source
# CA, newhire, pepra,.025, .02, TRUE, 57, 55,.071,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# CA, prior, poffb,.025, .02, TRUE, 57, 55,.071,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# CA, major, poffa,.03, .02, TRUE, 50, 50,.0,3,,, https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
# MA, newhire, ,.02, .02, TRUE, 57, 55,,,,,abc
# NY, newhire, ,.015, .02, TRUE, 57, 55,,,,,def
# ")
# params

params_df <- read_csv(
"stabbr, tier, tname, benfactor, aor_normal, aor_min, rpypp, maxpct, cola, compound, fasperiod, eec, socsec, source
CA, newhire, pepra, .025, 57, 50, .00071, ,.02, TRUE, 3, , , https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
CA, prior, poffb, .025, 55, 50, .001, ,.02, TRUE, 3, , , https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
CA, major, poffa, .03, 50, 50, 0, ,.02, TRUE, 3, , , https://www.calpers.ca.gov/docs/forms-publications/new-member-state-guide.pdf
"  
)
params_df
# rpypp is reduction per year for early retirement, as percentage points


# California --------------------------------------------------------------

# POFFA, POFFB, PEPRA benefit factors
# 50	3.000	2.000	2.000
# 51	N/A	2.100	2.071
# 52	N/A	2.200	2.143
# 53	N/A	2.300	2.214
# 54	N/A	2.400	2.286
# 55	N/A	2.500	2.357
# 56	N/A	N/A	2.429
# 57	N/A	N/A	2.500



# functions ---------------------------------------------------------------
pen <- function(worker, params) {
  worker_df <- params %>%
    mutate(yos=worker$aor - worker$aoe + 1,
           benfactor_adj=ifelse(worker$aor < aor_normal,
                                benfactor - rpypp * (aor_normal - worker$aor),
                                benfactor),
           fyos=worker$aor - worker$aoe + 1,
           pctfas=benfactor_adj * fyos,
           ibenefit=worker$fas * pctfas)

  results <- expand_grid(age=worker$aoe:worker$aod, worker_df) %>%
    mutate(
      year=row_number(),
      yos=ifelse(age < worker$aor, age - worker$aoe + 1, fyos),
      yor=ifelse(age >= worker$aor, age - worker$aor + 1, 0),
      benefit=ifelse(age >= worker$aor, ibenefit * (1 + cola)^(yor - 1), 0)
    )
  results
}

# worker <- workers_df %>% filter(id==1)
# params <- params_df %>% filter(stabbr=="CA", tier=="newhire")

pen(worker, params)

pen(workers_df %>%
      filter(id==1), params_df %>% filter(stabbr=="CA", tier=="newhire"))

list(pen(workers_df %>%
           filter(id==1), params_df %>% filter(stabbr=="CA", tier=="newhire")))



check <- workers_df %>%
  # filter(id==1) %>%
  rowwise() %>%
  mutate(pension=list(pen(.data, params_df %>% filter(stabbr=="CA", tier=="newhire"))))

check
check %>%
  unnest(cols = pension)

workers_df %>%
  rowwise() %>%
  mutate(pension=list(pen(.data, params_df %>% filter(stabbr=="CA", tier=="prior")))) %>%
  unnest(cols = pension)

df <- workers_df %>%
  rowwise() %>%
  mutate(penca_newhire=list(pen(.data, params_df %>% filter(stabbr=="CA", tier=="newhire"))),
         penca_prior=list(pen(.data, params_df %>% filter(stabbr=="CA", tier=="prior"))),
         penca_major=list(pen(.data, params_df %>% filter(stabbr=="CA", tier=="major")))
         )
df         

df2 <- df %>%
  pivot_longer(cols=starts_with("pen")) %>%
  unnest(cols = c(value))

df2 %>%
  filter(benefit > 0, id==1) %>%
  ggplot(aes(age, benefit, colour=tier)) +
  geom_line() +
  geom_point()

df2 %>%
  filter(benefit > 0, id==1)  %>%
  select(age, tier, benefit) %>%
  pivot_wider(names_from = tier, values_from = benefit)

dr <- .04
age0 <- 50
df3 <- df2 %>%
  mutate(pv=benefit / {(1 + dr)^(age - age0)})

df3 %>%
  group_by(id, stabbr, tier) %>%
  summarise(pv=sum(pv)) %>%
  filter(id==1)


# analysis ----------------------------------------------------------------



