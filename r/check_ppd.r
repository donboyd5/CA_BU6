


# libraries ---------------------------------------------------------------

library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(scales)
library(lubridate)
library(purrr)
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
library(pdata)


# constants ---------------------------------------------------------------



# analysis ----------------------------------------------------------------
glimpse(ppd)
count(ppd, PlanType, planf)

ppd %>%
  rename(stabbr=StateAbbrev,
         stname=StateName) %>%
  filter(stabbr=="CA") %>%
  count(ppd_id, PlanName)

ppd %>%
  rename(stabbr=StateAbbrev,
         stname=StateName) %>%
  filter(PlanType==3) %>%
  count(stabbr, ppd_id, PlanName)





