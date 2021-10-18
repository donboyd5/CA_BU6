# https://download.bls.gov/pub/time.series/oe/

# libraries ----
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(purrr)
library(haven)
library(readxl)
library(RSQLite)
library(lubridate)

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


# misc test ----
url <- 'ftp://speedtest.tele2.net/'
filenames = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

starwars <- read_html("https://rvest.tidyverse.org/articles/starwars.html")


url <- "https://download.bls.gov/pub/time.series/oe/"
tmp <- read_html(url)

# BLS OES data ----
# https://www.bls.gov/oes/current/oes_research_estimates.htm
# https://www.bls.gov/oes/oes_emp.htm
# https://www.bls.gov/oes/oes_ques.htm#qf1


#.. BLS state OES data -- all owner codes combined ----
# landing page https://www.bls.gov/oes/tables.htm
urlbase <- "https://www.bls.gov/oes/special.requests/"
# 1997-2002 state file names oes97st.zip
# 2003 May November to -- 2020 May
#   May https://www.bls.gov/oes/special.requests/oesm03st.zip
#   November https://www.bls.gov/oes/special.requests/oesn03st.zip

fnames1 <- paste0("oes", str_sub(1997:2002, 3, 4), "st.zip")
fnames2 <- paste0("oesm", str_sub(2003:2020, 3, 4), "st.zip")  # focus just on May for now??
fnames <- c(fnames1, fnames2)
fdir <- r"(E:\data\oesw\states\)"

for(fname in fnames){
  print(fname)
  download.file(paste0(urlbase, fname), paste0(fdir, fname), mode="wb")
}

for(fname in fnames){
  fpath <- paste0(fdir, fname)
  unzip(fpath)
  # unzip(fpath, exdir=fdir)
}


#.. BLS state research estimates -- provides owner breakdown by industry ----
# https://www.bls.gov/oes/current/oes_research_estimates.htm
# The OEWS program provides wage and employment estimates by state and industry
# beginning with the May 2012 reference period. These estimates are intended for
# research purposes, and users should be aware of the limitations of the data.
# Estimates prior to May 2012 are not available. Estimates are only available
# for states; industry-specific estimates for metropolitan areas are not
# available. Estimates are only available for detail and major occupation
# groups; industry-specific estimates for broad and minor occupation groups are
# not available.

urlbase <- "https://www.bls.gov/oes/special.requests/"
resdir <- r"(E:\data\oesw\oes_research\)"

stcodes
# Add Guam to stcodes
stcodes2 <- bind_rows(stcodes,
                      tibble(stabbr="GU", stfips="66", stname="Guam"))

#.. All NAICS sectors for 2020 ----
# we can go back to 2019 if we want
f1 <- function(year){
  print(year)
  fname <- paste0("oes_research_", year, "_allsectors.xlsx")
  upath <- paste0(urlbase, fname)
  dlpath <- paste0(resdir, fname)
  print(dlpath)
  download.file(upath, dlpath, mode = "wb")
}
# purrr::map(2016:2020, f1)
# 2016 area	area_title	naics	naics_title	occ code	occ title	group	i_group	tot_emp	emp_prse	pct_total	h_mean	a_mean	mean_prse	h_pct10	h_pct25	h_median	h_pct75	h_pct90	a_pct10	a_pct25	a_median	a_pct75	a_pct90	annual	hourly
# 2020 AREA	AREA_TITLE	NAICS	NAICS_TITLE	I_GROUP	OCC_CODE	OCC_TITLE	O_GROUP	TOT_EMP	EMP_PRSE	PCT_TOTAL	H_MEAN	A_MEAN	MEAN_PRSE	H_PCT10	H_PCT25	H_MEDIAN	H_PCT75	H_PCT90	A_PCT10	A_PCT25	A_MEDIAN	A_PCT75	A_PCT90	ANNUAL	HOURLY

(fnames <- paste0("oes_research_", 2016:2020, "_allsectors.xlsx"))
(fpaths <- paste0(resdir, fnames))

df <- read_excel(fpaths[1], sheet="data")

# make one big file
g <- function(x){
  x <- str_replace_all(x, "`", "")
  str_replace_all(x, " ", "_")
}
# g("`occ code`")
f1a <- function(year){
  print(year)
  fpath <- str_subset(fpaths, as.character(year))
  df <- read_excel(fpath, sheet=1, col_type="text") %>%
    setNames(str_to_lower(names(.))) %>%
    rename_with(.fn=g) %>%
    mutate(year=!!year)
  df
}
df <- purrr::map_dfr(2016:2020, f1a)
glimpse(df)
count(df, year)

df2 <- df %>%
  mutate(across(c(tot_emp:hourly, hourly), as.numeric))
glimpse(df2)

df3 <- df2 %>%
  left_join(stcodes2 %>% select(area=stfips, stabbr), by="area") %>%
  select(stabbr, year, occ=occ_code, occname=occ_title, everything())
count(df3, stabbr)

df3 %>%
  filter(stabbr=="CA", year==2020, occ=="33-3012")

saveRDS(df3, paste0(resdir, "oesres_allsectors.rds"))

oesres <- readRDS(paste0(resdir, "oesres_allsectors.rds"))
glimpse(oesres)



#.. NAICS 99 has federal, state, and local govt separately ----
# https://www.bls.gov/oes/special.requests/oes_research_2012_sec_99.xlsx
f2 <- function(year){
  print(year)
  fname <- paste0("oes_research_", year, "_sec_99.xlsx")
  upath <- paste0(urlbase, fname)
  dlpath <- paste0(resdir, fname)
  print(dlpath)
  download.file(upath, dlpath, mode = "wb")
}

purrr::map(2012:2020, f2)

(fnames <- paste0("oes_research_", 2012:2020, "_sec_99.xlsx"))
(fpaths <- paste0(resdir, fnames))

df <- read_excel(fpaths[1], sheet="data")

year <- 2012

str_subset(fpaths, "2012")

f <- function(year){
  print(year)
  fpath <- str_subset(fpaths, as.character(year))
  df <- read_excel(fpath, sheet=1, col_type="text") %>%
    setNames(str_to_lower(names(.))) %>%
    mutate(year=!!year)
  df
}

df <- purrr::map_dfr(2012:2020, f)
glimpse(df)

df2 <- df %>%
  mutate(occ=ifelse(is.na(`occ code`), occ_code, `occ code`),
         occname=ifelse(is.na(`occ title`), occ_title, `occ title`)) %>%
  mutate(across(c(tot_emp:hourly, pct_total), as.numeric)) %>%
  select(-c(occ_code, `occ code`, occ_title, `occ title`))

glimpse(df2)
count(df2, occ, occname)
count(df2, area, area_title)  # 54 -- states + DC, Guam, Puerto Rico, Virgin Islands
count(df2, year)


df3 <- df2 %>%
  left_join(stcodes2 %>% select(area=stfips, stabbr), by="area") %>%
  select(stabbr, year, occ, occname, everything())
count(df3, stabbr)

df3 %>%
  filter(stabbr=="CA", year==2012, occ=="33-3012")

saveRDS(df3, paste0(resdir, "oesres_sec99.rds"))

# saveRDS(df3, here::here("data", "oesresearch.rds"))


#.... look at oes research ----

oes <- readRDS(paste0(resdir, "oesres_sec99.rds"))
glimpse(oes)
count(oes, naics, naics_title)

oes2 <- oes %>%
  select(stabbr, year, naics, occ, occname, wage=a_mean)

tmp <- oes2 %>%
  filter(naics=="999200")  # ,str_sub(occ, 1, 2)=="33"
summary(co)
co %>%
  filter(is.na(wage))
# 2012: CO, IL -- both
# 2017 RI 3012

co %>%
  group_by(year, occ) %>%
  mutate(iwage=wage / median(wage, na.rm=TRUE) * 100) %>%
  ungroup %>%
  filter(stabbr=="CA") %>%
  ggplot(aes(year, iwage, colour=occ)) +
  geom_line() +
  geom_point()


occnum <- "33-1012"  # "33-3012", 
occnum <- "35-0000"
dfp %>%
  mutate(iwage=wage / median(wage, na.rm=TRUE) * 100) %>%
  ungroup %>%
  filter(occ=="33-3012", stabbr %in% c("CA", "MA", "NY", "TX", "FL", "NV", "OR", "WA")) %>%
  ggplot(aes(year, iwage, colour=stabbr)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100) +
  theme_bw()

dfp <- tmp %>%
  group_by(year, occ) %>%
  mutate(iwage=wage / median(wage, na.rm=TRUE) * 100,
         mwage=median(wage, na.rm=TRUE)) %>%
  ungroup

occnum <- "33-1012"  # "33-3012", 
occnum <- "25-0000"
dfp %>%
  filter(occ==occnum, stabbr %in% c("CA", "MA", "NY", "TX", "FL", "NV", "OR", "WA")) %>%
  ggplot(aes(year, iwage, colour=stabbr)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100) +
  theme_bw() +
  ggtitle(occnum)



occnum <- "33-1011"  # "33-3012", 
occnum <- "49-0000"
dfp %>%
  filter(occ==occnum, stabbr %in% c("CA", "MA", "NY", "TX", "FL", "NV", "OR", "WA")) %>%
  ggplot(aes(year, wage, colour=stabbr)) +
  geom_line() +
  geom_line(aes(year, mwage), colour="black", size=2,
            data=dfp %>% filter(occ==occnum, stabbr=="AL")) +
  geom_point() +
  theme_bw() +
  ggtitle(occnum)

# 33-0000	Protective Service Occupations
# 33-1011	First-Line Supervisors of Correctional Officers
# 33-1012	First-Line Supervisors of Police and Detectives
# 33-1021	First-Line Supervisors of Firefighting and Prevention Workers
# 33-1090	Miscellaneous First-Line Supervisors, Protective Service Workers
# 33-2011	Firefighters
# 33-2021	Fire Inspectors and Investigators
# 33-2022	Forest Fire Inspectors and Prevention Specialists
# 33-3011	Bailiffs
# 33-3012	Correctional Officers and Jailers
# 33-3021	Detectives and Criminal Investigators
# 33-3041	Parking Enforcement Workers
# 33-3051	Police and Sheriff's Patrol Officers
# 33-9011	Animal Control Workers
# 33-9032	Security Guards
# 33-9091	Crossing Guards and Flaggers
# 33-9092	Lifeguards, Ski Patrol, and Other Recreational Protective Service Workers
# 33-9093	Transportation Security Screeners
# 33-9098	School Bus Monitors and Protective Service Workers, All Other


# California OES data ----

# https://data.edd.ca.gov/w/geww-ef3u/98fh-2xv7?cur=WqyoLG-9JVV&from=root
# https://data.edd.ca.gov/Wages/Occupational-Employment-and-Wage-Statistics-OEWS-/pwxn-y2g5
# E:\data\CA_BU6_project\Occupational_Employment_and_Wage_Statistics__OEWS_.csv
capath <- r"(E:\data\CA_BU6_project\Occupational_Employment_and_Wage_Statistics__OEWS_.csv)"
df <- read_csv(capath)
glimpse(df)

cnames <- c("atype", "aname", "year", "qtr", "indname", "soc", "occname", "wtype", 
            "emp", "wage", "wp10", "wp25", "wp50", "wp75", "wp90", "wmrse")
df2 <- df %>%
  setNames(cnames)
glimpse(df2)
count(df2, wtype)
# wtype                      n
# <chr>                  <int>
#   1 Annual wage or salary 209192
# 2 Hourly wage           206016
levels(factor(df2$wtype))
count(df2, atype)
# 1 California-Statewide     1598
# 2 California - Statewide  19438
# 3 Metropolitan Area      355428
# 4 OES Survey Region       38744
count(df2, aname)  # 42 MSAs
count(df2, qtr)  # all 1st
count(df2, year)
cal_oescodes <- count(df2, soc, occname)
cal_oescodes %>%
  filter(str_sub(soc, 1, 2)=="33")

# soc occname                                                                       n
# <dbl> <chr>                                                                     <int>
#   1 330000 Protective Service Occupations                                              882
# 2 331011 First-Line Supervisors of Correctional Officers                              80
# 3 331011 First-Line Supervisors/Managers of Correctional Officers                    154
# 4 331012 First-Line Supervisors of Police and Detectives                             266
# 5 331012 First-Line Supervisors/Managers of Police and Detectives                    560
# 6 331021 First-Line Supervisors of Fire Fighting and Prevention Workers              258
# 7 331021 First-Line Supervisors/Managers of Fire Fighting and Prevention Workers     540
# 8 331090 Miscellaneous First-Line Supervisors, Protective Service Workers            124
# 9 331099 First-Line Supervisors of Protective Service Workers, All Other             126
# 10 331099 First-Line Supervisors/Managers, Protective Service Workers, All Other      550
# 11 332011 Fire Fighters                                                               560
# 12 332011 Firefighters                                                                254
# 13 332021 Fire Inspectors and Investigators                                           422
# 14 332022 Forest Fire Inspectors and Prevention Specialists                           142
# 15 333011 Bailiffs                                                                     30
# 16 333012 Correctional Officers and Jailers                                           360
# 17 333021 Detectives and Criminal Investigators                                       636
# 18 333031 Fish and Game Wardens                                                        58
# 19 333041 Parking Enforcement Workers                                                 366
# 20 333051 Police and Sheriff's Patrol Officers                                        838
# 21 333052 Transit and Railroad Police                                                  32
# 22 339011 Animal Control Workers                                                      492
# 23 339021 Private Detectives and Investigators                                        350
# 24 339031 Gambling Surveillance Officers and Gambling Investigators                    24
# 25 339031 Gaming Surveillance Officers and Gaming Investigators                       106
# 26 339032 Security Guards                                                             882
# 27 339091 Crossing Guards                                                             390
# 28 339091 Crossing Guards and Flaggers                                                 60
# 29 339092 Lifeguards, Ski Patrol, and Other Recreational Protective Service Workers   646
# 30 339093 Transportation Security Screeners                                           354
# 31 339098 School Bus Monitors and Protective Service Workers, All Other               132
# 32 339099 Protective Service Workers, All Other                                       734

count(df2, atype)

cal_oes <- df2 %>%
  select(-qtr) %>%
  mutate(wtype=factor(wtype, labels=c("annual", "hourly")),
         atype=case_when(str_detect(atype, "Statewide") ~ "state",
                         str_detect(atype, "Metro") ~ "msa",
                         str_detect(atype, "Region") ~ "oesregion",
                         TRUE ~ "ERROR"))

save(cal_oes, cal_oescodes, file=here::here("data", "caloes.rdata"))


load(file=here::here("data", "caloes.rdata"), verbose=TRUE)
count(cal_oes, wtype)
count(cal_oes, atype)


cal_oes %>%
  filter(soc=="333012", wtype=="annual", atype=="state") %>%
  ggplot(aes(year, wage)) +
  geom_line() +
  geom_point()

cal_oes %>%
  filter(soc=="333012", wtype=="annual", atype=="state") %>%
  pivot_longer(cols=wage:wp90) %>%
  group_by(name) %>%
  mutate(ivalue=value / value[year==2009] * 100 - 100) %>%
  ggplot(aes(year, ivalue, colour=name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks=seq(-50, 50, 5)) +
  geom_hline(yintercept = 0)
  


# codes ----
# First-Line Supervisors of Correctional Officers Totals	33-1011				3636	0		3636
# CAPTAIN, YOUTH AUTHORITY                                                                                                	WU50	9569	2	0		2
# CORRECTIONAL LIEUTENANT                                                                                                 	WY30	9656	1070	0		1070
# CORRECTIONAL SERGEANT                                                                                                   	WY40	9659	2523	0		2523
# LIEUTENANT, YOUTH AUTHORITY                                                                                             	WU70	9574	16	0		16
# SERGEANT, YOUTH AUTHORITY                                                                                               	WU80	9577	25	0		25

# Correctional Officers and Jailers Totals	33-3012				22062	319		22381
# CORRECTIONAL OFFICER                                                                                                    	WY50	9662	21852	318		22170
# YOUTH CORRECTIONAL OFFICER   WU90	9579	210	1		211


#.. oes codes from research estimates ----
# 33-0000	Protective Service Occupations
# 33-1011	First-Line Supervisors of Correctional Officers
# 33-1012	First-Line Supervisors of Police and Detectives
# 33-1021	First-Line Supervisors of Firefighting and Prevention Workers
# 33-1090	Miscellaneous First-Line Supervisors, Protective Service Workers
# 33-2011	Firefighters
# 33-2021	Fire Inspectors and Investigators
# 33-2022	Forest Fire Inspectors and Prevention Specialists
# 33-3011	Bailiffs
# 33-3012	Correctional Officers and Jailers
# 33-3021	Detectives and Criminal Investigators
# 33-3041	Parking Enforcement Workers
# 33-3051	Police and Sheriff's Patrol Officers
# 33-9011	Animal Control Workers
# 33-9032	Security Guards
# 33-9091	Crossing Guards and Flaggers
# 33-9092	Lifeguards, Ski Patrol, and Other Recreational Protective Service Workers
# 33-9093	Transportation Security Screeners
# 33-9098	School Bus Monitors and Protective Service Workers, All Other



