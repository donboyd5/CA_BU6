

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
workers <- read_csv("
id, yob, aoe, aor, aod, fas
1, 1980, 25, 55, 85, 129032.258
2, 1980, 25, 60, 75, 100e3
3, 1980, 25, 57, 82, 100e3
")
workers


# functions ---------------------------------------------------------------
#  return df with ages, benefits

make_params <- function(stabbr, benfactor, cola, minaor=55, tier="newhire") {
  params <- list()
  params$stabbr <- stabbr
  params$tier <- tier
  params$benfactor <- benfactor
  params$cola <- cola
  params$minaor <- minaor
  params
}

params_ma <- make_params("MA", benfactor=.02, cola=.02)
params_ma
# $1,809,053
params_ny <- make_params("NY", benfactor=.015, cola=.02)
params_ny

params_ca <- make_params("CA", benfactor=.025, cola=.02)
params_ca

pen <- function(row, params) {
  
  worker <- tibble(stabbr=params$stabbr,
                   benfactor=params$benfactor,
                   cola=params$cola,
                   fyos=row$aor - row$aoe + 1,
                   ibenefit=fyos * benfactor * row$fas)
  
  results <- expand_grid(age=row$aoe:row$aod, worker) %>%
    mutate(
      year=row_number(),
      yos=ifelse(age < row$aor, age - row$aoe + 1, fyos),
      yor=ifelse(age >= row$aor, age - row$aor + 1, 0),
      benefit=ifelse(age >= row$aor, ibenefit * (1 + cola)^(yor - 1), 0)
    ) %>%
    select(fyos, stabbr, year, 
           age, yos, yor,
           benfactor, cola,
           ibenefit, benefit)
  results
}

check <- workers %>%
  rowwise() %>%
  mutate(pension=list(pen(.data, params_ma)))
check
check %>%
  unnest(cols = pension)

df <- workers %>%
  rowwise() %>%
  mutate(penny=list(pen(.data, params_ny)),
         penma=list(pen(.data, params_ma)),
         penca=list(pen(.data, params_ca)))
df         

df2 <- df %>%
  pivot_longer(cols=starts_with("pen")) %>%
  unnest(cols = c(value))

df2 %>%
  filter(benefit > 0, id==1) %>%
  ggplot(aes(age, benefit, colour=stabbr)) +
  geom_line() +
  geom_point()

dr <- .04
age0 <- 55
df3 <- df2 %>%
  mutate(pv=benefit / {(1 + dr)^(age - age0)})

df3 %>%
  group_by(id, stabbr) %>%
  summarise(pv=sum(pv))


# analysis ----------------------------------------------------------------



