
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


# constants ----
acsdir <- r"(E:\data\acs\pums\sqlite\)"
acsfn <- "acs2019_5year"
acsdb <- paste0(acsdir, acsfn, ".sql3")


# connect and get all California records ----
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=acsdb)

vars <- "serialno, st, adjinc, pwgtp, agep, cow, wagp, occp, powsp, powpuma"
sqlcmd <- paste0("SELECT ", vars, " FROM pus WHERE st=6")  # California
system.time(cadf <- collect(tbl(db, sql(sqlcmd))))  # 2 secs  1.9m records  California
glimpse(cadf)
summary(cadf)
dbDisconnect(db)

# get California teachers employed by local government ----
cateach <- cadf %>%
  filter(occp %in% 2205:2555, # Educational Instruction, and Library Occupations
         cow %in% 3, # class of worker: employed by local government
         powsp==6,  # place of work: California
         !is.na(powpuma), # place of work public use microdata area is not missing
         wagp > 0, # earned wages last year
         wagp >= 10e3,  # make sure they were really earning money
         agep >= 20) %>%  # were at least 20 years old
  mutate(adjinc=adjinc / 1e6,  # adjustment factor to put all years in the 5-year acs on same basis
         awagp=wagp * adjinc,   # adjusted wages
         sanfran=powpuma==7500, # place of work San Francisco
         agegroup=cut(agep, breaks=seq(0, 99, 5), right=FALSE),
         agegroupn=as.numeric(agegroup),  # sortable
         agegroup=as.character(agegroup))
summary(cateach)
count(cateach, occp)
count(cateach, sanfran)
count(cateach, agep)
count(cateach, agegroupn, agegroup)
glimpse(cateach)


# produce table and data file for CA in total, and San Francisco ----
catab <- cateach %>%
  group_by(agegroupn, agegroup) %>%
  summarise(n=n(), wtdn=sum(pwgtp), wagesum=sum(awagp * pwgtp)) %>%
  mutate(geo="ca") %>%
  ungroup
catab

sftab <- cateach %>%
  filter(sanfran) %>%
  group_by(agegroupn, agegroup) %>%
  summarise(n=n(), wtdn=sum(pwgtp), wagesum=sum(awagp * pwgtp)) %>%
  mutate(geo="sf") %>%
  ungroup
sftab

# combine, add group totals, save ----
casf <- bind_rows(catab, sftab)
tots <- casf %>% 
  group_by(geo) %>% 
  summarise(across(c(n, wtdn, wagesum), sum)) %>%
  mutate(agegroupn=99, agegroup="All ages")

casftots <- bind_rows(casf, tots) %>%
  arrange(geo, agegroupn) %>%
  group_by(geo) %>%
  mutate(avgwage=wagesum / wtdn,
         pctwtdn=wtdn / wtdn[agegroupn==99] * 100,
         wageratio=avgwage / avgwage[agegroupn==99]) %>%
  ungroup

casftots %>% 
  write_csv(here::here("casftots.csv"))


# show tables ----

casftots %>%
  select(geo, agegroupn, agegroup, pctwtdn) %>%
  pivot_wider(names_from = geo, values_from = pctwtdn) %>%
  mutate(sfca=sf / ca)

casftots %>%
  select(geo, agegroupn, agegroup, avgwage) %>%
  pivot_wider(names_from = geo, values_from = avgwage) %>%
  mutate(sfca=sf / ca)


# occupation codes ----
# 2205-2555	25-0000	Educational Instruction, and Library Occupations:
#   
#   2205	25-1000	Postsecondary teachers
# 2300	25-2010 	Preschool and kindergarten teachers
# 2310	25-2020 	Elementary and middle school teachers
# 2320	25-2030 	Secondary school teachers
# 2330	25-2050 	Special education teachers
# 2350	25-3041	Tutors
# 2360	25-30XX	Other teachers and instructors
# Combines: 	
#       25-3011	Adult basic education, adult secondary education, and English as a second language instructors
#       25-3021	Self-enrichment teachers
#       25-3031	Substitute teachers, short-term 
#       25-3099	Teachers and instructors, all other 
# 2400	25-4010	Archivists, curators, and museum technicians
# 2435	25-4022	Librarians and media collections specialists
# 2440	25-4031	Library technicians
# 2545	25-9040	Teaching assistants

