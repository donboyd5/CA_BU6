# install.packages("arrow", repos = "https://arrow-r-nightly.s3.amazonaws.com")

# links ----
# https://www.census.gov/programs-surveys/acs/microdata/documentation.html



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

zpath <- paste0(pumsdir, "csv_pus.zip")

files <- unzip(zpath, list=TRUE)
files

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
  

# 5-year estimates ----
sasfiles <- c("psam_pusa.sas7bdat", "psam_pusb.sas7bdat", "psam_pusc.sas7bdat", "psam_pusd.sas7bdat")
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

dbDisconnect(db)


# check out the data ----
# get info about the db
dbDisconnect(db) # in case it is connected - will throw an error if not, but that's ok
db <- dbConnect(SQLite(), dbname=dbfile)
dbListTables(db)
dbGetInfo(db)

sqlcmd <- "PRAGMA table_info(pus)" # get info about columns in a table
dbGetQuery(db, sqlcmd)


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







