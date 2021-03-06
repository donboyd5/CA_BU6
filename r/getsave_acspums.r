

# 5-year pums ending 2019 ----

# install.packages("arrow", repos = "https://arrow-r-nightly.s3.amazonaws.com")

# links ----
# https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/  # data landing page
# https://data.census.gov/mdat/#/search?ds=ACSPUMS5Y2019   # data and documentation
# https://www.census.gov/programs-surveys/acs/microdata/documentation.html
# https://www2.census.gov/programs-surveys/acs/data/pums/2019/5-Year/csv_hus.zip  # data file

# 3802 Correctional officers and jailers 33-3012   # occ code 2018


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
csvdir <- paste0(pumsdir, "csv/")

fna <- "psam_pusa.csv"
fnb <- "psam_pusb.csv"

fnha <- "psam_husa.csv"
fnhb <- "psam_husb.csv"
fnhc <- "psam_husc.csv"
fnhc <- "psam_husd.csv"

# pzpath <- paste0(pumsdir, "csv_pus.zip")
# hzpath <- paste0(pumsdir, "csv_hus.zip")

# household data 5-year estimates ----
tblname <- "hus"
# sasfiles <- c("psam_pusa.sas7bdat", "psam_pusb.sas7bdat", "psam_pusc.sas7bdat", "psam_pusd.sas7bdat")
csvfiles <- c("psam_husa.csv", "psam_husb.csv", "psam_husc.csv", "psam_husd.csv")
dir <- r"(E:\data\acs\pums\5year\)"

dbdir <- r"(E:\data\acs\pums\sqlite\)"
dbfn <- "acs2019_5year"
dbfile <- paste0(dbdir, dbfn, ".sql3")
# unlink(dbfile) # DANGER!!! delete database file if it exists

dbDisconnect(db) # in case it is connected - will throw error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)

#.. do a quick test before doing everything! ----
# tmp <- read_csv(paste0(dir, csvfiles[1]),
#                 col_types = cols(RT="c",
#                                  SERIALNO="c",
#                                  .default = "i"),
#                 n_max=1000)
# glimpse(tmp)


#..now do everything ----
nmax <- Inf
for(file in csvfiles){
  t1 <- proc.time()
  print(file)
  print("reading file...")
  a <- proc.time()
  tmp <- read_csv(paste0(dir, file),
                  col_types = cols(RT="c",
                                   SERIALNO="c",
                                   .default = "i"),
                  n_max=nmax)
  # tmp <- as.data.frame(tmp)
  tmp <- tmp %>%
    setNames(str_to_lower(names(.)))
  b <- proc.time()
  print(b - a)
  
  print("writing db table")
  if(!dbExistsTable(db, tblname)) {
    # create table and associated indexes the first time through
    dbWriteTable(db, tblname, tmp, row.names=FALSE)
  } else {
    dbWriteTable(db, tblname, tmp, row.names=FALSE, append=TRUE) # indexes will automatically be updated  
  }  
  
  rm(tmp)
  t2 <- proc.time()
  print(t2 - t1)
}


# quick checks
dbListTables(db)
dbGetInfo(db)

sqlcmd <- "PRAGMA table_info(hus)" # get info about columns in a table
dbGetQuery(db, sqlcmd)

dbGetQuery(db, paste0("SELECT COUNT(*) FROM ", "hus")) # much faster than nrow

dbDisconnect(db)



# person data 5-year estimates ----
tblname <- "pus"
# sasfiles <- c("psam_pusa.sas7bdat", "psam_pusb.sas7bdat", "psam_pusc.sas7bdat", "psam_pusd.sas7bdat")
csvfiles <- c("psam_pusa.csv", "psam_pusb.csv", "psam_pusc.csv", "psam_pusd.csv")
dir <- r"(E:\data\acs\pums\5year\)"

dbdir <- r"(E:\data\acs\pums\sqlite\)"
dbfn <- "acs2019_5year"
dbfile <- paste0(dbdir, dbfn, ".sql3")
tblname <- "pus"
# unlink(dbfile) # DANGER!!! delete database file if it exists

dbDisconnect(db) # in case it is connected - will throw error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)

nmax <- Inf
for(file in csvfiles){
  t1 <- proc.time()
  print(file)
  print("reading file...")
  a <- proc.time()
  tmp <- read_csv(paste0(dir, file),
                  col_types = cols(RT="c",
                                   SERIALNO="c",
                                   NAICSP="c",
                                   SOCP="c",
                                   .default = "i"),
                  n_max=nmax)
  # tmp <- as.data.frame(tmp)
  tmp <- tmp %>%
    setNames(str_to_lower(names(.)))
  b <- proc.time()
  print(b - a)
  
  print("writing db table")
  if(!dbExistsTable(db, tblname)) {
    # create table and associated indexes the first time through
    dbWriteTable(db, tblname, tmp, row.names=FALSE)
    } else {
      dbWriteTable(db, tblname, tmp, row.names=FALSE, append=TRUE) # indexes will automatically be updated  
    }  
    
  rm(tmp)
  t2 <- proc.time()
  print(t2 - t1)
}

# quick checks
dbListTables(db)
dbGetInfo(db)

sqlcmd <- "PRAGMA table_info(pus)" # get info about columns in a table
dbGetQuery(db, sqlcmd)
# tmp <- dbGetQuery(db, sqlcmd)

dbGetQuery(db, paste0("SELECT COUNT(*) FROM ", "pus")) # much faster than nrow

dbDisconnect(db)





# ALL DONE ----
# check out the data ----
# get info about the db
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)
dbListTables(db)
dbGetInfo(db)

sqlcmd <- "PRAGMA table_info(pus)" # get info about columns in a table
dbGetQuery(db, sqlcmd)

dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", "pus")) # much faster than nrow

getall <- tbl(db, sql(paste0("SELECT * FROM ", "pus")))
glimpse(getall)
count(getall, st)
getall %>% summarise(pop=sum(pwgtp)) # 311,536,599 is target OLD target

unionall <- function(vars, years, where) {
  # do NOT include year in the input vector vars - it is a reserved word
  # build a union all query in pieces
  part1 <- paste0('SELECT "year", ', toString(vars))
  part2 <- paste0(" FROM acs", years)
  part3 <- paste0(" WHERE ", where)
  queryvec <- paste0(part1, part2, part3)
  unionallqry <- paste(queryvec, collapse=" UNION ALL ")
  return(unionallqry)
}

vars <- c("serialno", "st", "pwgtp")
rules <- "st=36"
rules <- "st>=0"
# rules <- "size_code=0 AND (agglvl_code<='28' OR (agglvl_code>='58' AND agglvl_code<='64')) " # 2 secs
fullqry <- unionall(vars, c(2007:2014), rules)



# set indexes ----
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)
sqlcmd <- "CREATE INDEX iST ON pus(ST)" # 94 secs!
sqlcmd <- "CREATE INDEX iOCCP ON pus(OCCP)" # 94 secs!
system.time(dbExecute(db, sqlcmd))
dbDisconnect(db)


# get an extract ----
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)

# vars <- "serialno, PUMA00, PUMA10, ST, ADJINC, PWGTP, AGEP, COW, MAR, SCHL, SEX, WAGP, 
#   HISP, RAC1P, OCCP02, OCCP10, OCCP12"

vars <- "serialno, st, adjinc, pwgtp, agep, cow, mar, schl, sex, wagp, hisp, rac1p, occp"

# sqlcmdall <- paste0("SELECT ", vars, " FROM acs2013_5year") # 151 secs
# sqlcmdca <- paste0("SELECT ", vars, " FROM pus WHERE st=6") # 10 secs
# system.time(dfca <- collect(tbl(db, sql(sqlcmdca))))  # 3 secs
# count(dfca, st)
# count(dfca, st)count(dfca, st)


sqlcmd_corr <- paste0("SELECT ", vars, " FROM pus WHERE occp=3802") # 10 secs
system.time(dfcorr <- collect(tbl(db, sql(sqlcmd_corr))))  # 3 secs

glimpse(dfcorr)

count(dfcorr, st)
df2 <- dfcorr %>%
  left_join(stcodes %>% select(st=stfips, stabbr) %>% mutate(st=as.integer(st)),
            by="st") %>%
  filter(wagp > 0, agep >= 18) %>%
  mutate(adjinc=adjinc / 1e6)

sts <- c("CA", "MA", "RI", "CT", "NY", "FL", "NV", "TX") %>% sort
storder <- c("other", sts)
df3 <- df2 %>%
  filter(cow==4) %>%
  mutate(stgroup=ifelse(stabbr %in% sts, stabbr, "other"),
         stgroup=factor(stgroup, levels=storder),
         agep2=agep^2,
         exp=agep - 18,
         exp2=exp^2,
         lwage=log(wagp),
         sex=factor(sex, levels=1:2, labels=c("male", "female")),
         edattain=case_when(schl <= 15 ~ "notHSgrad",
                            schl %in% 16:19 ~ "HSgrad",
                            schl == 20 ~ "associate",
                            schl == 21 ~ "BA",
                            schl %in% 22:24 ~ "MA+",
                            TRUE ~ "other"),
         edattain=factor(edattain, levels=c("notHSgrad", "HSgrad", "associate", "BA", "MA+", "other")),
         marstat=factor(mar, levels=1:5, labels=c("married", "widowed", "divorced", "separated", "neverM")),
         marstat=factor(mar, levels=1:5, labels=c("marwid", "marwid", rep("notmarwid", 3))))
# count(df3, stgroup, stabbr)
count(df3, stgroup, edattain)


mod <- lm(lwage ~ sex + stgroup + exp + exp2 + edattain + marstat, data=df3)
summary(mod)


# MAR Character 1
# Marital status
# 1 .Married
# 2 .Widowed
# 3 .Divorced
# 4 .Separated
# 5 .Never married or under 15 years old




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

# Adjusting Income for Inflation ??? ACS Income amounts are reported for the 12 months
# preceding the interview month. Monthly Consumer Price Indices (CPIs) are used to inflationadjust
# these components to a reference calendar year (January through December). For
# example, a household interviewed in March 2006 reports their income for March 2005
# through February 2006. Their income is inflation-adjusted to the 2006 reference calendar
# year by multiplying their reported income by the 2006 average annual CPI (January-December 2006)
# and then dividing by the average CPI for the March 2005-February 2006
# income reference period. Since there are twelve different income reference periods
# throughout the interview year, there are twelve different income inflation adjustments that
# take place. To maintain respondent confidentiality, none of the twelve inflation-adjustment
# factors are on the PUMS files. Instead, a variable, ???ADJUST,??? is on the file and is the
# average of the twelve inflation adjustment factors.
# In order to inflation adjust income amounts from previous years to the current reference
# calendar year, dollar values on individual records are multiplied by the average annual CPIU-RS
# for the current reference year then divided by the average annual CPI-U-RS for the
# earlier income amount year. 




# old stuff ----
# fnz <- "unix_pus.zip"
# sdir <- r"(E:\data\acs\pums\5year\)"
# spath <- paste0(sdir, sasfiles[1])
df <- read_csv(paste0(dir, csvfiles[1]),
               col_types = cols(RT="c",
                                SERIALNO="c",
                                NAICSP="c",
                                SOCP="c",
                                .default = "i"),
               n_max=10)
glimpse(df)
names(df)
ns(df)
problems()







sasfiles <- c("psam_pusa.sas7bdat", "psam_pusb.sas7bdat", "psam_pusc.sas7bdat", "psam_pusd.sas7bdat")
fnz <- "unix_pus.zip"
sdir <- r"(E:\data\acs\pums\5year\)"
spath <- paste0(sdir, sasfiles[1])



a <- proc.time()
dfa <- read_sas(
  data_file=spath,
  catalog_file = NULL,
  encoding = NULL,
  col_select = c(RT:FYOEP),
  skip = 0L,
  n_max = Inf,
  .name_repair = "unique")
b <- proc.time()
b - a # 5.5 mins

glimpse(dfa)
ns(dfa)


# df <- read_sas(
#   data_file,
#   catalog_file = NULL,
#   encoding = NULL,
#   catalog_encoding = encoding,
#   col_select = NULL,
#   skip = 0L,
#   n_max = Inf,
#   cols_only = "DEPRECATED",
#   .name_repair = "unique"
# )


for(fnsas in sasfiles) {
  print(fnsas)
  fnz <- "unix_pus.zip"
  print(system.time(dfsas <- read_sas(unzip(paste0(acs2013.5year, fnz), fnsas)))) # 10 mins
  print(system.time(saveRDS(dfsas, paste0(acs2013.5year, fnsas, ".rds")))) # 8 mins, 700mb
}


# old parquet ----

# read and save ----
# con <- unz(zpath, fna) # read directly from zip file
cona <- paste0(csvdir, fna)
dfa <- read_csv(cona, col_types = cols(RT="c", 
                                       SERIALNO="c",
                                       NAICSP="c",
                                       SOCP="c",
                                       .default = "i"), n_max=-1)
problems()
# close(con=con)
glimpse(dfa)
cnames <- names(dfa)
cnames[129]

conb <- paste0(csvdir, fnb)
dfb <- read_csv(conb, col_types = cols(RT="c", 
                                       SERIALNO="c",
                                       NAICSP="c",
                                       SOCP="c",
                                       .default = "i"), n_max=-1)
problems()

df <- bind_rows(dfa, dfb) %>%
  select(RT:FYOEP) %>%
  setNames(str_to_lower(names(.)))
memory()
rm(dfa, dfb)
summary(df)

a <- proc.time()
write_parquet(df, paste0(pumsdir, "acsp2019.parquet"))
b <- proc.time()
b - a


# get selected variables ----
df <- read_parquet(paste0(pumsdir, "acsp2019.parquet"),
                   col_select = c(rt, serialno, sporder, st, adjinc, pwgtp, sex, agep, cow, wagp, occp)) %>%
  left_join(stcodes %>% select(st=stfips, stabbr) %>% mutate(st=as.integer(st)),
            by="st")

count(df, st)
count(df, sex)

corr <- df %>%
  filter(occp==3802, 
         agep >= 18,
         wagp > 0) %>%
  mutate(lwage=log(wagp),
         sex=factor(sex, levels=1:2, labels=c("male", "female")),
         agep2=agep^2)
count(corr, stabbr)
summary(corr)

sts <- c("CA", "NY", "FL", "NV", "TX")
storder <- c("other", sts)
corr2 <- corr %>%
  filter(cow==4) %>%
  mutate(stgroup=ifelse(stabbr %in% sts, stabbr, "other"),
         stgroup=factor(stgroup, levels=storder))
glimpse(corr2)
count(corr2, stgroup, stabbr)


mod <- lm(lwage ~ sex + stgroup + agep + agep2, data=corr2)
summary(mod)


corr %>%
  group_by(stabbr) %>%
  summarise(n=n(), 
            wtdn=sum(pwgtp), 
            wagp=sum(wagp * pwgtp),
            agep=wtd.mean(agep, weights = pwgtp)) %>%
  mutate(avg=wagp / wtdn) %>%
  left_join(spop.a %>% filter(year==2010) %>% select(stabbr, pop=value), by=c("stabbr")) %>%
  mutate(per100k=wtdn / pop * 1e5) %>%
  arrange(desc(avg))






