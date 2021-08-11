# Don Boyd
# 10/28/2015

# http://www2.census.gov/acs2013_1yr/pums/
# ftp://ftp.census.gov/programs-surveys/acs/data/pums/2013/1-Year/csv_pny.zip
# http://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.2014.html


# http://www.census.gov/programs-surveys/acs/technical-documentation/pums/subjects.html
# http://guides.newman.baruch.cuny.edu/nyc_data/nbhoods
# http://cdrpc.org/data/census-data/american-community-survey/
# https://sdcclearinghouse.wordpress.com/category/sharing/tools/




#****************************************************************************************************
#                adjinc and inflation adjustment ####
#****************************************************************************************************

# see http://www.sdcbidc.iupui.edu/sharing/documents/PUMSInflationAdjustmentdocumentation.pdf

# Note: The value of ADJINC inflation-adjusts reported income to 2013 dollars.
# ADJINC applies to variables FINCP and HINCP in the housing record, and
# variables INTP, OIP, PAP, PERNP, PINCP, RETP, SEMP, SSIP, SSP, and WAGP in
# the person record. 

# "..Data users will need to multiply the microdata by the ADJUST variable on the PUMS files for each
# individual year..."

# Adjusting Income for Inflation – ACS Income amounts are reported for the 12 months
# preceding the interview month. Monthly Consumer Price Indices (CPIs) are used to inflationadjust
# these components to a reference calendar year (January through December). For
# example, a household interviewed in March 2006 reports their income for March 2005
# through February 2006. Their income is inflation-adjusted to the 2006 reference calendar
# year by multiplying their reported income by the 2006 average annual CPI (January-December 2006)
# and then dividing by the average CPI for the March 2005-February 2006
# income reference period. Since there are twelve different income reference periods
# throughout the interview year, there are twelve different income inflation adjustments that
# take place. To maintain respondent confidentiality, none of the twelve inflation-adjustment
# factors are on the PUMS files. Instead, a variable, “ADJUST,” is on the file and is the
# average of the twelve inflation adjustment factors.
# In order to inflation adjust income amounts from previous years to the current reference
# calendar year, dollar values on individual records are multiplied by the average annual CPIU-RS
# for the current reference year then divided by the average annual CPI-U-RS for the
# earlier income amount year. 



#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("ggplot2")
library("scales") # so we can use scales with ggplot2
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("reshape2")
library("magrittr")
library("tidyr")
library("dplyr") # always load AFTER plyr
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library("knitr")
library("lubridate")
library("stringr")
library("grDevices")
library("readr")
library("readxl")

library("haven")
library("Hmisc")

library("survey")

library("btools") # library that I created (install from github)
library("bdata")
library("pdata")
library("fof")
library("apitools")

# library("RSQLite")

# devtools::install_github("hadley/haven")
# devtools::install_github("hadley/ggplot2")
# if (packageVersion("devtools") < 1.6) {
#   install.packages("devtools")
# }
# devtools::install_github("hadley/lazyeval")
# devtools::install_github("hadley/dplyr")
# devtools::install_github("hadley/readr")
# devtools::install_github("hadley/readxl")

# my packages:
# devtools::install_github("donboyd5/btools")
# devtools::install_github("donboyd5/bdata")
# devtools::install_github("donboyd5/ptools")
# devtools::install_github("donboyd5/pdata")
# devtools::install_github("donboyd5/apitools")
# devtools::install_github("donboyd5/fof")



#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
acs2013.1year <- "E:/Data/CensusACS/2013/"
acs2013.5year <- "E:/Data/CensusACS/20135year/"


ny1yrd <- "E:/Data/CensusACS/NY/1-year-files/"
ny3yrd <- "E:/Data/CensusACS/NY/3-year-files/"
ny5yrd <- "E:/Data/CensusACS/NY/5-year-files/"


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************



#****************************************************************************************************
#                Get ACS occupation codes ####
#****************************************************************************************************
fn <- "2010 Occupation Index 08132013.xls"
ocdf <- read_excel(paste0(acs2013.1year, fn))
glimpse(ocdf)
names(ocdf) <- c("occ.census", "occ.soc", "occtitle", "cenind", "naicsind")

ocdf %>% filter(str_detect(occtitle, coll("carpenter", ignore_case = TRUE))) %>%
  arrange(occ.census, occ.soc)

# Construction and Extraction Occupations: 6200-6940
# Carpenter 6230


#****************************************************************************************************
#                Download ACS files ####
#****************************************************************************************************
 # ONLY RUN THIS WHEN NEW DATA ARE NEEDED!!!
acsftp <- "ftp://ftp.census.gov/programs-surveys/acs/data/pums/"
acsweb <- "http://www2.census.gov/acs"

# get the 1-year files
for(year in 2007:2014) {
  fn <- "unix_pny.zip"
  #acsdir <- paste0(acsftp, year, "/1-Year/")
  acsdir <- paste0(acsweb, year, "_1yr/pums/")
  # print(acsdir)
  fnget <- paste0(acsdir, fn)
  fnput <- paste0(ny1yrd, year, "_", fn)
  print(fnput)
  download.file(fnget, fnput, mode="wb")
}

# now get 3-year files
# get the 1-year files
for(year in c(2007, 2013)) {
  fn <- "unix_pny.zip"
  acsdir <- paste0(acsweb, year, "_3yr/pums/")
  fnget <- paste0(acsdir, fn)
  fnput <- paste0(ny3yrd, year, "_", fn)
  print(fnput)
  download.file(fnget, fnput, mode="wb")
}


for(year in c(2009, 2013)) {
  fn <- "unix_pny.zip"
  acsdir <- paste0(acsweb, year, "_5yr/pums/")
  fnget <- paste0(acsdir, fn)
  fnput <- paste0(ny5yrd, year, "_", fn)
  print(fnput)
  download.file(fnget, fnput, mode="wb")
}


#****************************************************************************************************
#                Convert ACS files to R ####
#****************************************************************************************************
# ONLY RUN THIS WHEN NEW DATA ARE NEEDED!!!

convert_file <- function(year, dir) {
  fnz <- paste0(year, "_unix_pny.zip")
  fnsas <- "psam_p36.sas7bdat"
  unzip(paste0(dir, fnz), fnsas, exdir = tempdir())
  system.time(tmp <- read_sas(file.path(tempdir(), fnsas)))
  names(tmp) <- tolower(names(tmp))
  saveRDS(tmp, paste0(dir, "pny", year, ".rds"))
}

l_ply(c(2009, 2013), convert_file, ny5yrd, .progress="text")


getclass <- function(year, dir) {
  df <- readRDS(paste0(dir, "pny", year, ".rds"))
  tmp <- as.data.frame(sapply(df, class))
  names(tmp)[1] <- "class"
  tmp <- tmp %>% mutate(vname=row.names(.), year=year)
  return(tmp)
}


# 1-year 2007:2014
# 3-year 2007, 2013
# 5-year 2009, 2013

vvals <- ldply(c(2009, 2013), getclass, ny5yrd, .progress="text")
vvals2 <- vvals %>% spread(year, class)
vvals2
saveRDS(vvals2, "./data/vvals5year.rds")
# vvals <- readRDS("./data/vvals.rds")

vvals2 %>% filter((vname %in% c("adjust", "adjinc", "agep", "puma", "rac1p", "hisp", "occp")) |
                    str_detect(vname, fixed("occ")) |
                    str_detect(vname, fixed("puma")))



#****************************************************************************************************
#                Geographies ####
#****************************************************************************************************
# PUMA codes are 5 digits
# NYC PUMAs start with 037-041
# Nassau starts 032
# Suffolk starts 033
f_puma <- function(puma) {
  pumaf <- rep("Other", length(puma))
  pumaf <- ifelse(str_sub(puma, 1, 3)=="032", "Nassau", pumaf)
  pumaf <- ifelse(str_sub(puma, 1, 3)=="033", "Suffolk", pumaf)
  pumaf <- ifelse(str_sub(puma, 1, 3) %in% str_pad(37:41, 3, "left", "0"), "NYC", pumaf)
  return(pumaf)
}
# f_puma(c("03100", "03299", "03344", "03700", "04199", "04200"))



#****************************************************************************************************
#                Define factors for selected codes ####
#****************************************************************************************************
# RAC1P 1
# Recoded detailed race code
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or American
# .Indian or Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races
f_rac1p <- function(rac1p) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  labs <- c("White alone", "Black alone", "Asian alone", "Am-Indian alone", "Alaska Native alone",
            "Am-Ind or AK native tribe", "Hawaiian or PacIsland alone", "Other race alone", "Two or more races")
  #factor(rac1p, levels=levs, labels=labs, ordered=TRUE)
  ordered(rac1p, levels=levs, labels=labs)
}

f_race <- function(rac1p) {
  levs <- c(1, 2, 6, 3:5, 7:9)
  x <- factor(rac1p, levels=levs, ordered = TRUE)
  levels(x) <- list("White alone"=1, 
                    "Black alone"=2, 
                    "Asian alone"=6, 
                    "Other race alone"=c(3:5, 7:8), 
                    "Two or more races"=9)
  return(x)
}
# table(f_race(c(1:9, 3, 5)))

f_race_hisp <- function(rac1p, hisp) {
  y <- ifelse(hisp!=1, 0, rac1p)
  levs <- c(1, 2, 6, 3:5, 7:9)
  y <- factor(y)
  levels(y) <- list("White alone, not Hispanic"=1, 
                    "Black alone, not Hispanic"=2,
                    "Hispanic, any race"=0,
                    "Asian alone, not Hispanic"=6,
                    "Other or multiple race, not Hispanic"=c(3:5, 7:9))
  return(y)
}

# (tmp <- f_race_hisp(rac1p=c(1, 1, 2, 2, 6), hisp=c(1, 2, 2, 1, 1)))
# table(tmp)

f_occgrp <- function(occp) {
  occgrp <- ifelse(occp=="6230", "carpenter", "other")
  occgrp <- ifelse(occp >= "6200" & occp <= "6940" & occgrp=="other", "construction", occgrp)
  occgrp <- ifelse(occp >= "3600" & occp <= "4650" & occgrp=="other", "service", occgrp)
  return(occgrp)
}

# f_occgrp(c("6230", "6231", "3610", "9000"))

# 6600	47-3012

#****************************************************************************************************
#                Speed comparisons, an entire file ####
#****************************************************************************************************

# dat.laf <- laf_open_csv(paste0(acs2013, files[1]), column_names=cnames, column_types=rep("string", length(cnames)), skip=1)
# system.time(dfl <- dat.laf[,]) # 398 secs
# 
# system.time(dfr <- read_csv(paste0(acs2013, files[1]))) # 255 secs
# system.time(saveRDS(dfr, "e:/temp/acs2013a.rds")) # 458 secs
# system.time(dfrin <- readRDS("e:/temp/acs2013a.rds")) # 178 secs
# 
# fnz <- "unix_pus.zip" 
# fnsas <- "psam_pusa.sas7bdat"
# system.time(dfsas <- read_sas(unzip(paste0(acs2013.5year, fnz), fnsas))) # 650
# system.time(dfsas2 <- read_sas(paste0(acs2013.5year, fnsas))) # 684 secs

# system.time(dfrin2 <- load(paste0(acs2013.5year, "psam_pusa.rdata")))

# using stattransfer to convert from sas to r takes about 9 or 10 mins per file
# but the resulting rdata files take a long time to load

#****************************************************************************************************
#                Read sas files, save as rds files ####
#****************************************************************************************************

sasfiles <- c("psam_pusa.sas7bdat", "psam_pusb.sas7bdat", "psam_pusc.sas7bdat", "psam_pusd.sas7bdat")
for(fnsas in sasfiles) {
  print(fnsas)
  fnz <- "unix_pus.zip"
  print(system.time(dfsas <- read_sas(unzip(paste0(acs2013.5year, fnz), fnsas)))) # 10 mins
  print(system.time(saveRDS(dfsas, paste0(acs2013.5year, fnsas, ".rds")))) # 8 mins, 700mb
}


#****************************************************************************************************
#                Read rds files, save as SQLite database ####
#****************************************************************************************************

library("RSQLite")

dbfn <- "acs2013_5year"
dbfile <- paste0(acs2013.5year, dbfn, ".sql3")
tblname <- dbfn
# unlink(dbfile) # DANGER!!! delete database file if it exists
dbDisconnect(db) # in case it is connected - will throw error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)

rfiles <- c("psam_pusa.sas7bdat.rds", "psam_pusb.sas7bdat.rds", "psam_pusc.sas7bdat.rds", "psam_pusd.sas7bdat.rds")
# rfiles <- c("psam_pusb.sas7bdat.rds", "psam_pusc.sas7bdat.rds", "psam_pusd.sas7bdat.rds")

for(rfile in rfiles){
  t1 <- proc.time()
  print(rfile)
  print("reading r file...")
  system.time(tmp <- readRDS(paste0(acs2013.5year, rfile)))
  tmp <- as.data.frame(tmp)
  
  print("writing db table")
  if(!dbExistsTable(db, tblname)) dbWriteTable(db, tblname, tmp, row.names=FALSE) else  # create table and associated indexes the first time through
    dbWriteTable(db, tblname, tmp, row.names=FALSE, append=TRUE) # indexes will automatically be updated  
  rm(tmp)
  t2 <- proc.time()
  print(t2 - t1)
}

dbDisconnect(db)


print("Now building state index...")
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)
sqlcmd <- "CREATE INDEX iST ON acs2013_5year(ST)" # 94 secs!
sqlcmd <- "CREATE INDEX iOCCP02 ON acs2013_5year(OCCP02)" # 94 secs!
sqlcmd <- "CREATE INDEX iOCCP10 ON acs2013_5year(OCCP10)" # 94 secs!
system.time(dbGetQuery(db, sqlcmd))
dbDisconnect(db)

# retrieve some data


#****************************************************************************************************
#                mess around with the db  ####
#****************************************************************************************************
library("RSQLite")

dbfn <- "acs2013_5year"
dbfile <- paste0(acs2013.5year, dbfn, ".sql3")

# get info about the db ####
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)
dbListTables(db)
dbGetInfo(db)

sqlcmd <- "PRAGMA table_info(acs2013_5year)" # get info about columns in a table
dbGetQuery(db, sqlcmd)

sqlcmd <- "SELECT serialno, ST, ADJINC, PWGTP, AGEP, COW FROM acs2013_5year" # 93 secs

vars <- "serialno, PUMA00, PUMA10, ST, ADJINC, PWGTP, AGEP, COW, MAR, SCHL, SEX, WAGP, 
  HISP, RAC1P, OCCP02, OCCP10, OCCP12"
sqlcmdall <- paste0("SELECT ", vars, " FROM acs2013_5year") # 151 secs
sqlcmdny <- paste0("SELECT ", vars, " FROM acs2013_5year WHERE ST=36") # 10 secs

my_db <- src_sqlite(dbfile)
df.dplyr <- tbl(my_db, sql(sqlcmdall))
df.dplyr
count(df.dplyr, ST)

system.time(dfny <- collect(tbl(my_db, sql(sqlcmdny))))

system.time(dfall <- collect(tbl(my_db, sql(sqlcmdall)) %>% filter(ST=="36")))

myq <- df.dplyr %>% group_by(RAC1P) %>% summarise(n=n()) # create a query but don't execute it
tmp <- collect(myq) # this executes the query

dfny <- tbl(my_db, sql(sqlcmdny))
count(dfny, RAC1P)
memory()


system.time(df.dplyr <- tbl(db, sql(sqlcmdall)))

system.time(df <- dbGetQuery(db, sqlcmdall))
glimpse(df)

system.time(dfny <- dbGetQuery(db, sqlcmdny))
glimpse(dfny)

# are these all numeric? 
count(df, ADJINC)
count(df, COW)
count(df, MAR)
count(df, SCHL)
count(df, SEX)
count(df, HISP)
count(df, RAC1P)
# yes

df2 <- df %>% mutate_each(funs(as.numeric), ADJINC, COW, MAR, SCHL, SEX, HISP, RAC1P) %>%
  mutate(ADJINC=ADJINC / 1e6,
         stabbr=stcodes$stabbr[match(ST, stcodes$stfips)])
glimpse(df2)
saveRDS(df2, paste0(acs2013.5year, "acs2013x.rds"))

system.time(df2 <- readRDS(paste0(acs2013.5year, "acs2013x.rds"))) # ~60 secs
glimpse(df2)
count(df2, ST, stabbr)

# The 2009-2013 PUMS data contain 6,671,272 housing unit records and 14,988,864 person records
# from households and 629,248 person records from GQs. The GQ person records include some
# imputed records.
14988864 + 629248 # = 15,618,112
# VS 15,450,262 in our data


sum(df2$PWGTP) / 1e6

dfny <- df2 %>% filter(stabbr=="NY")
sum(dfny$PWGTP) / 1e6

dfny2 <- dfny %>% mutate(area=f_puma(PUMA00)) %>%
  filter(area %in% c("NYC")) %>%
  group_by(occgrp=f_occgrp(OCCP10),
           race=f_race_hisp(RAC1P, HISP)) %>%
  summarise(n=n(), n.wtd=sum(PWGTP)) %>%
  group_by(occgrp) %>%
  mutate(racpct=n.wtd / sum(n.wtd) * 100)

dfny2 %>% select(occgrp, race, racpct) %>%
  spread(occgrp, racpct) %>%
  kable(digits=2)

dfny2 %>% select(occgrp, race, n) %>%
  spread(occgrp, n) %>%
  kable(digits=2)

sum(dfny2$n)



#****************************************************************************************************
#                Read selected columns with LaF ####
#****************************************************************************************************
library("LaF") # large Ascii files - we need this because some files have 10k+ columns and we may only want to read a few

# acs2011 <- "E:/Data/CensusACS/20115year/"
acs2013 <- "E:/Data/CensusACS/20135year/"

files <- c("ss13pusa.csv", "ss13pusb.csv", "ss13pusc.csv", "ss13pusd.csv")

# read and save each file individually, then put them together
cnames <- names(read_csv(paste0(acs2013, files[1]), n_max=1))
cnames

keepvars <- c("serialno", "PUMA00", "PUMA10", "ST", "ADJINC", "PWGTP", "AGEP", "COW", "MAR", 
              "SCHL", "SEX", "WAGP", "HISP", "RAC1P",
              "OCCP02", "OCCP10", "OCCP12")

for(file in files) {
  print(file)
  dat.laf <- laf_open_csv(paste0(acs2013, file), column_names=cnames, column_types=rep("string", length(cnames)), skip=1)
  print(system.time(df <- dat.laf[, keepvars]))
  saveRDS(df, paste0(acs2013, file, ".rds"))
}

# combine the files and save one big file
getrds <- function(file) {
  print(system.time(df <- readRDS(paste0(acs2013, file, ".rds"))))
  return(df)
}
dfall <- ldply(files, getrds, .progress="text")

glimpse(dfall)

dfall2 <- dfall %>% mutate_each(funs(as.numeric), -serialno, -PUMA00, -PUMA10) %>%
  mutate(ADJINC=ADJINC / 1e6)
glimpse(dfall2)

system.time(saveRDS(dfall2, paste0(acs2013, "acs2013x.rds")))

dfall2 <- readRDS(paste0(acs2013, "acs2013x.rds")))

dfall2 %>% filter(OCCP10==6230) %>% summarise(n=n(), n.wtd=sum(PWGTP, na.rm=TRUE))

dfall2 %>% filter(OCCP10==6230) %>%
  mutate(stabbr=stcodes$stabbr[match(ST, stcodes$stfips)]) %>%
  group_by(stabbr) %>%
  summarise(n=n(), n.wtd=sum(PWGTP, na.rm=TRUE), WAGP=wtd.mean(WAGP*ADJINC, PWGTP)) %>%
  arrange(-WAGP)

dfall2 %>% filter(OCCP10==6230) %>%
  mutate(stabbr=stcodes$stabbr[match(ST, stcodes$stfips)],
         race=f_race_hisp(RAC1P, HISP)) %>%
  group_by(stabbr, race) %>%
  summarise(n=n(), n.wtd=sum(PWGTP, na.rm=TRUE), WAGP=wtd.mean(WAGP*ADJINC, PWGTP)) %>%
  select(stabbr, race, n) %>%
  spread(race, n) %>%
  ungroup %>%
  arrange(as.character(stabbr))

count(dfall2, ADJINC)


#****************************************************************************************************
#                Make one big multi-year file with a limited set of variables ####
#****************************************************************************************************
getfile <- function(year, dir) {
  # create an extract (drop the replication weights) and do minor transformations
  pny <- readRDS(paste0(dir, "pny", year, ".rds")) %>% 
    select(serialno, starts_with("adj"), pwgtp, agep, hisp, starts_with("occp"), starts_with("puma"), rac1p, schl, sex, wagp)
  if("adjust" %in% names(pny)) pny <- pny %>% rename(adjinc=adjust)
  if(year==2013) {
    pny <- pny %>% mutate(occp=ifelse(is.na(occp12), occp10, occp12),
                          puma=ifelse(is.na(puma10), puma00, puma10))
  }
  # convert selected variables to numeric
  pny <- pny %>%  mutate_each(funs(as.numeric), adjinc, hisp, rac1p, schl, sex) %>%
    mutate(adjinc=adjinc / 1e6,
           area=f_puma(puma),
           year=year)
  return(pny)
}


pnybase <- ldply(c(2009, 2013), getfile, ny5yrd, .progress="text")
saveRDS(pnybase, "./data/pnybase5year.rds")

glimpse(pnybase)
count(pnybase, year)
count(pnybase, area)
count(pnybase, area, year) %>% spread(area, n)
tmp <- count(pnybase, puma, year) %>% spread(year, n)
# CAUTION!! This suggests that the coding may have changed for Nassau and Suffolk counties between 2011 and 2012!! ####
nchar(tmp$puma)



#****************************************************************************************************
#                Enhance the big multi-year file ####
#****************************************************************************************************
pny <- readRDS("./data/pnybase.rds")
pny <- readRDS("./data/pnybase3year.rds")
pny <- readRDS("./data/pnybase5year.rds")
glimpse(pny)
count(pny, area)

pny %>% group_by(year, area) %>%
  summarise(n=n()) %>%
  spread(year, n)

df <- pny %>% filter(area %in% c("NYC")) %>%
  group_by(year, occgrp=f_occgrp(occp), race=f_race_hisp(rac1p, hisp)) %>%
  summarise(n=n(), n.wtd=sum(pwgtp)) %>%
  group_by(year, occgrp) %>%
  mutate(racpct=n.wtd / sum(n.wtd) * 100)

df %>% filter(occgrp=="carpenter") %>%
  select(occgrp, race, n) %>%
  spread(year, n)

df %>% filter(occgrp=="carpenter") %>%
  select(occgrp, race, n.wtd) %>%
  spread(year, n.wtd)

df %>% filter(occgrp=="carpenter") %>%
  select(occgrp, race, racpct) %>%
  spread(year, racpct) %>%
  kable(digits=2)

df %>% filter(year==2013) %>%
  select(occgrp, race, racpct) %>%
  spread(occgrp, racpct) %>%
  kable(digits=2)



#****************************************************************************************************
#                Get the ACS ####
#****************************************************************************************************
# use the SAS file rather than csv as it has variable types the way Census thinks makes sense and variable labels
# but we do have to convert some values
fnz <- "unix_pny.zip"
fnsas <- "psam_p36.sas7bdat"
# df2 <- read_sas(unz(paste0(acs2013.1year, fnz), fn)) # can't seem to read SAS from zip
# unzip then read
unzip(paste0(acs2013.1year, fnz), fnsas, exdir = tempdir())
system.time(tmp <- read_sas(file.path(tempdir(), fnsas)))
glimpse(tmp)
names(tmp) <- tolower(names(tmp))
saveRDS(tmp, "./data/pny2013.rds")

# get the csv version
# fnz <- "csv_pny.zip"
# fnc <- "ss13pny.csv"
# system.time(df2 <- read_csv(unz(paste0(acs2013.1year, fnz), fnc))) # 11 secs
# glimpse(df2)
# memory()
# names(df2) <- tolower(names(df2))
# saveRDS(df2, "./data/pny2013.rds")


#****************************************************************************************************
#                Analyze ####
#****************************************************************************************************

pny <- readRDS("./data/pny2013.rds")
glimpse(pny)

# create an extract (drop the replication weights) and do minor transformations
pny2 <- pny %>% select(-one_of(paste0("pwgtp", 1:80))) %>%
  # convert selected variables to numeric
  mutate_each(funs(as.numeric), adjinc, hisp, rac1p, schl, sex) %>%
  mutate(adjinc=adjinc / 1e6,
         area=f_puma(puma))
glimpse(pny2)

count(pny2, mar)
count(pny2, occp)
count(pny2, hisp)
tmp <- count(pny2, occp)


pny2 %>% filter(area %in% c("NYC")) %>%
  group_by(occgrp=f_occgrp(occp), race=f_race_hisp(rac1p, hisp)) %>%
  summarise(n.wtd=sum(pwgtp)) %>%
  group_by(occgrp) %>%
  mutate(racpct=n.wtd / sum(n.wtd) * 100) %>%
  select(occgrp, race, racpct) %>%
  spread(occgrp, racpct) %>%
  kable(digits=2)

 
# constr <- expression(occp >= "6200" & occp <= "6940")  # construction worker
df <- pny2 %>% filter(occp >= "6200" & occp <= "6940")  # construction worker 5k obs, 813 carpenters
count(df, occp)

df %>% group_by(carp=occp=="6230") %>% summarise(wagp=wtd.mean(wagp*adjinc, pwgtp))
# df %>% group_by(carp=occp=="6230") %>% summarise(wagp=mean(wagp))

df %>% group_by(carp=occp=="6230") %>% summarise(agep=wtd.mean(agep, pwgtp))

df %>% filter(area %in% c("NYC")) %>%
  group_by(carp=occp=="6230", race=f_race_hisp(rac1p, hisp)) %>%
  summarise(n.wtd=sum(pwgtp)) %>%
  group_by(carp) %>%
  mutate(racpct=n.wtd / sum(n.wtd) * 100) %>%
  select(carp, race, racpct) %>%
  spread(carp, racpct) %>%
  kable(digits=2)


#****************************************************************************************************
#                Survey package ####
#****************************************************************************************************

library(survey)
> data(api)
> dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)



