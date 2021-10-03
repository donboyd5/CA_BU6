
# libraries ----
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(purrr)
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

library(btools)
library(bdata)


# locations ----
acsdir <- r"(E:\data\acs\)"
pumsdir <- paste0(acsdir, "pums/")


# get pums dictionary ----

fn <- paste0(pumsdir, "PUMS_Data_Dictionary_2015-2019.csv")
dict <- read_csv((fn))



