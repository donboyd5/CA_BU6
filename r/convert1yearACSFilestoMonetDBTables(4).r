# 11/30/2015

# Create a MonetDB database with a table for each ACS 1-year person file for the United States (ALL records)
# TODO: add a table for each 1-year housing file

# As of now, I have person files for 2007-2014 so the db will have 8 tables (eventually 16)

# Approach:
#   Define schema of database
#   Use super-fast COPY INTO to get data from csv to mdb without going through memory

# monetdb or its r interface is a little buggy. The approach below seems to be best. 
# I tried many that didn't work, or not as well. For example, COPY INTO should be faster
# if we provide a slight overestimate of # of records to read and write, but in many attempts to make that
# work, either it didn't, or was slower.

# https://www.monetdb.org/Documentation/Cookbooks/SQLrecipes/LoadingBulkData
# https://r-forge.r-project.org/scm/viewvc.php/pkg/MonetDB.R/R/monetdb.R?root=monetr&r1=36&r2=39&pathrev=129
# https://www.monetdb.org/pipermail/users-list/2015-November/thread.html#start

# For control totals, see:
#  https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html

#****************************************************************************************************
#                Notes on the ACS 1-year files and how to convert them ####
#****************************************************************************************************
# http://www2.census.gov/programs-surveys/acs/data/pums for data
# I am using the csv files so that I can easily control the data type for each variable
# Each year has 2 csv files inside a zip archive, named "a" and "b" as follows:
#    ss07pusa.csv  and  ss07pusb.csv  for 2007, ...
#    ss14pusa.csv  and  ss14pusb.csv  for 2014

# The zip file is always named csv_pus.zip
# It will always be in a location named after the year. For example:
#    D:\Data\CensusACS\2011

# unzip files in advance of reading (a section below will do this, if desired)

# I convert all variable names to lower case

# I use COPY INTO sql to avoid reading an entire file into memory, although it would be possible:
# about 2.5gb memory needed for a single csv file (2 per year)


#****************************************************************************************************
#                System-dependent section: Define locations ####
#****************************************************************************************************
acsbase <- "D:/Data/CensusACS/"
mdb <- paste0(acsbase, "acs1yearMonetDB/")
zname <- "csv_pus.zip"

# a few useful snippets
# unzip(zip, exdir=str_sub(csvdir, 1, -2)) # remove the trailing / from the dirname
# fn <- "D:/Data/CensusACS/2011/csv_pus.zip:ss11pusa.csv" # referencing a file within a zip archive
# z <- unzip( tf , exdir = td ) # extract a temp file to temp directory


#****************************************************************************************************
#                Loads ####
#****************************************************************************************************
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("reshape2")
library("magrittr")
library("tidyr")
library("dplyr") # always load AFTER plyr
options(dplyr.print_min = 60) # default is 10
options(dplyr.print_max = 60) # default is 20
library("lubridate")
library("stringr")
library("readr")
library("readxl")
library("btools")
library("MonetDBLite")
library("MonetDB.R")


#****************************************************************************************************
#                Unzip all needed csv files (delete later) ####
#****************************************************************************************************
# years <- 2010:2011
# for(year in years) { # about 20 secs per year
#   a <- proc.time()
#   zipdir <- paste0(acsbase, year)
#   zip <- paste0(zipdir, "/", zname)
#   print(paste0("Unzipping: ", zip))
#   unzip(zip, exdir=zipdir)
#   b <- proc.time()
#   print(b - a)
# }



#****************************************************************************************************
#                Functions that will read a single year and write as monetDB table ####
#****************************************************************************************************

dummydf <- function(year) {
  # create a dummy dataframe with desired column types, to be used to create table structure
  csvdir <- paste0(acsbase, year, "/")
  files <- paste0("ss", str_sub(year, 3, 4), "pus", c("a", "b"), ".csv")
  varsdf <- read_csv(paste0(csvdir, files[1]), n_max=0) # default: read the a file
  vnames <- names(varsdf)
  
  # make integer our default value, then override for certain vars
  ctypes.def <- rep("i", length(vnames))
  
  # double precision variables
  dvars <- c("ADJINC", "ADJUST", "INTP", "RETP", "SEMP", "SSIP", "SSP", "WAGP", "PERNP", "PINCP",
             vnames[str_detect(vnames, "PWGT")], vnames[str_detect(vnames, "pwgt")])
  # character variables
  cvars <- c("serialno", "RT", "NAICSP", "OCCP", "OCCP02", "OCCP10", "OCCP12", "SOCP", "SOCP00", "SOCP10", "SOCP12")
  
  ctypes <- ctypes.def
  ctypes[which(vnames %in% dvars)] <- "d"
  ctypes[which(vnames %in% cvars)] <- "c"
  ctypes <- paste0(ctypes, collapse="")
  ctypes
  
  # now read the file for real, giving us a data frame with the structure we want
  dummydf <- read_csv(paste0(csvdir, files[1]), n_max=0, col_types = ctypes)
  names(dummydf) <- tolower(names(dummydf))
  retlist <- list(dummydf=dummydf, ctypes=ctypes)
  return(retlist)
}


writeyear <- function(year, con, dummydf, test=FALSE, nrecs=1000) {
  # ASSUMES we are already connected to the database with connection con
  # CAUTION: deletes existing table
  
  csvdir <- paste0(acsbase, year, "/")
  files <- paste0(csvdir, "ss", str_sub(year, 3, 4), "pus", c("a", "b"), ".csv")
  tbl <- paste0("acs", year)
  
  if(dbExistsTable(con, tbl)) dbRemoveTable(con, tbl)
  dbWriteTable(con, tbl, dummydf[FALSE, ], transaction = FALSE)
  
  dbBegin(con)
  for (file in files) {
    thefile <- normalizePath(file)
    print(thefile) 
    
    a <- proc.time()
    # use the following fillcmd for test purposes
    if(test) {
      fillcmd <- paste0("COPY ", format(nrecs, scientific=FALSE), " OFFSET 2 RECORDS INTO ", tbl, " FROM ",
                        "'", thefile, "'",
                        " USING DELIMITERS ',', '\\n', '\"' NULL as '' ")
    } else {
      # use the following fillcmd to fill the entire database
      fillcmd <- paste0("COPY OFFSET 2 INTO ", tbl, " FROM ",
                        "'", thefile, "'",
                        " USING DELIMITERS ',', '\\n', '\"' NULL as '' ")
    }
    print(fillcmd)
    dbSendUpdate(con, fillcmd)
    b <- proc.time()
    print(b - a)
  }
  dbCommit(con)
  # now add a year variable to the file
  addcmd <- paste0("ALTER TABLE ", tbl, " ADD \"year\" INT NOT NULL DEFAULT ", year)
  dbSendUpdate(con, addcmd)
}


writeyeardf <- function(year, con, nrecs=-1) {
  ctypes <- dummydf(year)$ctypes
  csvdir <- paste0(acsbase, year, "/")
  files <- paste0(csvdir, "ss", str_sub(year, 3, 4), "pus", c("a", "b"), ".csv")
  tbl <- paste0("acs", year)
  if(dbExistsTable(con, tbl)) dbRemoveTable(con, tbl)
  
  a1 <- proc.time()
  for(file in files) {
    print(file)
    a <- proc.time()
    df <- read_csv(paste0(file), n_max=nrecs, col_types = ctypes)
    names(df) <- tolower(names(df))
    df$year <- as.integer(year)
    dbBegin(con)
    dbWriteTable(con, tbl, df, append=TRUE) # another 4 secs
    dbCommit(con)
    b <- proc.time()
    print(b - a)
  }
  print("Total time")
  print(b - a1)
  return()
}


#****************************************************************************************************
#                Test-read a file if needed ####
#****************************************************************************************************
year <- 2014
csvdir <- paste0(acsbase, year, "/")
files <- paste0("ss", str_sub(year, 3, 4), "pus", c("a", "b"), ".csv")
df <- read_csv(paste0(csvdir, files[1]), n_max=1) # default: read the a file
glimpse(df[, 1:18])
count(df, CITWP)

data.frame(a=c(NA, "abc", "def"))
data.frame(a=c("NA", "abc", "def"))



#****************************************************************************************************
#                Connect to database and write years ####
#****************************************************************************************************
# dbRemoveTable(con, "acs2014")

con <- dbConnect(MonetDB.R(), embedded=str_sub(mdb, 1, -2), wait=TRUE)

year <- 2013
ddf <- dummydf(year)$dummydf
glimpse(ddf)
names(ddf)
writeyear(year, con, ddf, test=TRUE, nrecs=1000) # oddly, I had trouble with this for 2014

writeyear(year, con, ddf) # once the test works properly do a full copy; takes about 3 mins per year

# IF COPY INTO doesn't work (as it doesn't for 2014) instead read full files and do dbWriteTable
# writeyeardf(year, con, nrecs=100) # test
# writeyeardf(year, con) # full, about 4-5 mins

glimpse(df)


# SUCCESS!

# take a look before disconnecting
# if problems # dbRollback(con)

dbGetInfo(con)
dbListTables(con)

tbl <- paste0("acs", year)
dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", tbl)) # much faster than nrow

acsdb <- src_monetdb(mdb, con=con)
getall <- tbl(acsdb, sql(paste0("SELECT * FROM ", tbl)))
glimpse(getall)
count(getall, st)
getall %>% summarise(pop=sum(pwgtp)) # 311,536,599 is target

dbDisconnect(con)
rm(con)



#****************************************************************************************************
#                Check out the data with unionall ####
#****************************************************************************************************

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


con <- dbConnect(MonetDB.R(), embedded=str_sub(mdb, 1, -2), wait=TRUE)
tbl <- paste0("acs", year)
dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", tbl)) # much faster than nrow

acsdb <- src_monetdb(mdb, con=con)
getall <- tbl(acsdb, sql(paste0("SELECT * FROM ", "acs2009")))
glimpse(getall)


vars <- c("serialno", "st", "pwgtp")
rules <- "st=36"
rules <- "st>=0"
# rules <- "size_code=0 AND (agglvl_code<='28' OR (agglvl_code>='58' AND agglvl_code<='64')) " # 2 secs
fullqry <- unionall(vars, c(2007:2014), rules)
fullqry

system.time(tmp <- tbl(acsdb, sql(fullqry)))
system.time(tmp2 <- collect(tmp, n=-1))
glimpse(tmp2)
count(tmp2, year)

tmp2 %>% group_by(year) %>%
  summarise(pop=sum(pwgtp))





#****************************************************************************************************
#                Check to see if var names are same across files ####
#****************************************************************************************************
# ASSUME that the a file and b file in each year will have the same names
getnames <- function(year) {
  csvdir <- paste0(acsbase, year, "/")
  files <- paste0("ss", str_sub(year, 3, 4), "pus", c("a", "b"), ".csv")
  df <- read_csv(paste0(csvdir, files[1]), n_max=0)
  return(names(df))
}

years <- 2007:2011
nlist <- llply(years, getnames)
names(nlist) <- years
str(nlist)
length(nlist)
lapply(nlist, length)
nlist[[as.character(2008)]]

year1 <- 2007
year2 <- 2008
f <- function(year1, year2) {
  cy1 <- as.character(year1)
  cy2 <- as.character(year2)
  if(setequal(nlist[[cy1]], nlist[[cy2]])) print(paste0(year1, " and ", year2, " are equal.")) else{
    print(paste0(year1, " and ", year2, " are NOT equal.")); print("")
    print(paste0("Vars in ", year1, " that are not in ", year2, ": "))
    print(setdiff(nlist[[cy1]], nlist[[cy2]])); print("")
    print(paste0("Vars in ", year2, " that are not in ", year1, ": "))
    print(setdiff(nlist[[cy2]], nlist[[cy1]])); print("")
  }
  return()
}

f(2007, 2008)
f(2008, 2009)
f(2009, 2010)
f(2010, 2011)
f(2011, 2012)
f(2012, 2013)
f(2013, 2014)

f(2007, 2011)

# see final section for noted variable differences


#****************************************************************************************************
#                Variable differences across years ####
#****************************************************************************************************

# > f(2007, 2008)
# [1] "2007 and 2008 are NOT equal."
# [1] ""
# [1] "Vars in 2007 that are not in 2008: "
# [1] "ADJUST" "DWRK"   "MILY"   "DS"     "FDWRKP" "FMILYP"
# [1] ""
# [1] "Vars in 2008 that are not in 2007: "
# [1] "ADJINC"  "CITWP"   "DEAR"    "DRAT"    "DRATX"   "HINS1"   "HINS2"   "HINS3"   "HINS4"   "HINS5"   "HINS6"   "HINS7"   "MARHD"   "MARHM"  
# [15] "MARHT"   "MARHW"   "MARHYP"  "DIS"     "HICOV"   "PRIVCOV" "PUBCOV"  "FCITWP"  "FDEARP"  "FDRATP"  "FDRATXP" "FHINS1P" "FHINS2P" "FHINS3P"
# [29] "FHINS4P" "FHINS5P" "FHINS6P" "FHINS7P" "FMARHDP" "FMARHMP" "FMARHTP" "FMARHWP" "FMARHYP"
# [1] ""
# NULL
# > f(2008, 2009)
# [1] "2008 and 2009 are NOT equal."
# [1] ""
# [1] "Vars in 2008 that are not in 2009: "
# [1] "UWRK"
# [1] ""
# [1] "Vars in 2009 that are not in 2008: "
# [1] "WRK"       "FOD1P"     "FOD2P"     "SCIENGP"   "SCIENGRLP" "FFODP"     "FHINS3C"   "FHINS4C"   "FHINS5C"   "FWRKP"    
# [1] ""
# NULL
# > f(2009, 2010)
# [1] "2009 and 2010 are NOT equal."
# [1] ""
# [1] "Vars in 2009 that are not in 2010: "
# [1] "REL"
# [1] ""
# [1] "Vars in 2010 that are not in 2009: "
# [1] "RELP"
# [1] ""
# NULL
# > f(2010, 2011)
# [1] "2010 and 2011 are NOT equal."
# [1] ""
# [1] "Vars in 2010 that are not in 2011: "
# character(0)
# [1] ""
# [1] "Vars in 2011 that are not in 2010: "
# [1] "FDISP"     "FPERNP"    "FPINCP"    "FPRIVCOVP" "FPUBCOVP" 
# [1] ""
# NULL


# > f(2007, 2011)
# [1] "2007 and 2011 are NOT equal."
# [1] ""
# [1] "Vars in 2007 that are not in 2011: "
# [1] "ADJUST" "DWRK"   "MILY"   "REL"    "UWRK"   "DS"     "FDWRKP" "FMILYP"
# [1] ""
# [1] "Vars in 2011 that are not in 2007: "
# [1] "ADJINC"    "CITWP"     "DEAR"      "DRAT"      "DRATX"     "HINS1"     "HINS2"     "HINS3"     "HINS4"     "HINS5"     "HINS6"    
# [12] "HINS7"     "MARHD"     "MARHM"     "MARHT"     "MARHW"     "MARHYP"    "RELP"      "WRK"       "DIS"       "FOD1P"     "FOD2P"    
# [23] "HICOV"     "PRIVCOV"   "PUBCOV"    "SCIENGP"   "SCIENGRLP" "FCITWP"    "FDEARP"    "FDISP"     "FDRATP"    "FDRATXP"   "FFODP"    
# [34] "FHINS1P"   "FHINS2P"   "FHINS3C"   "FHINS3P"   "FHINS4C"   "FHINS4P"   "FHINS5C"   "FHINS5P"   "FHINS6P"   "FHINS7P"   "FMARHDP"  
# [45] "FMARHMP"   "FMARHTP"   "FMARHWP"   "FMARHYP"   "FPERNP"    "FPINCP"    "FPRIVCOVP" "FPUBCOVP"  "FWRKP"    
# [1] ""
# NULL



