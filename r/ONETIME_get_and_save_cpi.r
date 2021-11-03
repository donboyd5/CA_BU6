
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(scales)
library(lubridate)
library(fredr)


fred_apikey <- "a5e1199baac333154cbffcba3b263c28"

# usethis::edit_r_environ() # setting the environment variable didn't work
# fredr_get_key()
# fredr_has_key()
# therefore set the key

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