# Don Boyd
# 11/11/2015

# I have found that the best approach is to:
# 1) Download and save on local computer zipped archives of ACS files in SAS format
# 2) Read them directly from the archives (without unzipping) using haven and unz, and save as R files
# 3) Read the R files and combine into one BIG RSQLite database from which we'll pull data as needed
# The rsqlite database is big but access is fast. Adding indexes for geography (e.g., state codes) seems
# to make it a little bit faster still.

# So far step 3 does not exceed the memory available on my pc, but if it does, it should be possible
# to read instead from csv files using LaF and write to rsqlite without ever pulling much data into R
# at any one time. That would change all of the steps above. There is sample LaF code below.


# some useful urls:

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

library(RSQLite)

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
# library("RSQLite")
# 
# dbfn <- "acs2013_5year"
# dbfile <- paste0(acs2013.5year, dbfn, ".sql3")
# 
# # get info about the db ####
# dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
# db <- dbConnect(SQLite(), dbname=dbfile)
# dbListTables(db)
# dbGetInfo(db)
# 
# sqlcmd <- "PRAGMA table_info(acs2013_5year)" # get info about columns in a table
# dbGetQuery(db, sqlcmd)
# 
# sqlcmd <- "SELECT serialno, ST, ADJINC, PWGTP, AGEP, COW FROM acs2013_5year" # 93 secs
# 
# vars <- "serialno, PUMA00, PUMA10, ST, ADJINC, PWGTP, AGEP, COW, MAR, SCHL, SEX, WAGP, 
#   HISP, RAC1P, OCCP02, OCCP10, OCCP12"
# sqlcmdall <- paste0("SELECT ", vars, " FROM acs2013_5year") # 151 secs
# sqlcmdny <- paste0("SELECT ", vars, " FROM acs2013_5year WHERE ST=36") # 10 secs
# 
# my_db <- src_sqlite(dbfile)
# df.dplyr <- tbl(my_db, sql(sqlcmdall))
# df.dplyr
# count(df.dplyr, ST)
# 
# system.time(dfny <- collect(tbl(my_db, sql(sqlcmdny))))
# 
# system.time(dfall <- collect(tbl(my_db, sql(sqlcmdall)) %>% filter(ST=="36")))
# 
# myq <- df.dplyr %>% group_by(RAC1P) %>% summarise(n=n()) # create a query but don't execute it
# tmp <- collect(myq) # this executes the query
# 
# dfny <- tbl(my_db, sql(sqlcmdny))
# count(dfny, RAC1P)
# memory()
# 
# 
# system.time(df.dplyr <- tbl(db, sql(sqlcmdall)))
# 
# system.time(df <- dbGetQuery(db, sqlcmdall))
# glimpse(df)
# 
# system.time(dfny <- dbGetQuery(db, sqlcmdny))
# glimpse(dfny)
# 
# # are these all numeric? 
# count(df, ADJINC)
# count(df, COW)
# count(df, MAR)
# count(df, SCHL)
# count(df, SEX)
# count(df, HISP)
# count(df, RAC1P)
# # yes
# 
# df2 <- df %>% mutate_each(funs(as.numeric), ADJINC, COW, MAR, SCHL, SEX, HISP, RAC1P) %>%
#   mutate(ADJINC=ADJINC / 1e6,
#          stabbr=stcodes$stabbr[match(ST, stcodes$stfips)])
# glimpse(df2)
# saveRDS(df2, paste0(acs2013.5year, "acs2013x.rds"))
# 
# system.time(df2 <- readRDS(paste0(acs2013.5year, "acs2013x.rds"))) # ~60 secs
# glimpse(df2)
# count(df2, ST, stabbr)
# 
# # The 2009-2013 PUMS data contain 6,671,272 housing unit records and 14,988,864 person records
# # from households and 629,248 person records from GQs. The GQ person records include some
# # imputed records.
# 14988864 + 629248 # = 15,618,112
# # VS 15,450,262 in our data
# 
# 
# sum(df2$PWGTP) / 1e6
# 
# dfny <- df2 %>% filter(stabbr=="NY")
# sum(dfny$PWGTP) / 1e6
# 
# dfny2 <- dfny %>% mutate(area=f_puma(PUMA00)) %>%
#   filter(area %in% c("NYC")) %>%
#   group_by(occgrp=f_occgrp(OCCP10),
#            race=f_race_hisp(RAC1P, HISP)) %>%
#   summarise(n=n(), n.wtd=sum(PWGTP)) %>%
#   group_by(occgrp) %>%
#   mutate(racpct=n.wtd / sum(n.wtd) * 100)
# 
# dfny2 %>% select(occgrp, race, racpct) %>%
#   spread(occgrp, racpct) %>%
#   kable(digits=2)
# 
# dfny2 %>% select(occgrp, race, n) %>%
#   spread(occgrp, n) %>%
#   kable(digits=2)
# 
# sum(dfny2$n)



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

dfall2 <- readRDS(paste0(acs2013, "acs2013x.rds"))

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


