# https://download.bls.gov/pub/time.series/oe/

# libraries ----
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(purrr)
library(haven)
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


#.. BLS state OES data ----
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
# YOUTH CORRECTIONAL OFFICER                                                                                              	WU90	9579	210	1		211

