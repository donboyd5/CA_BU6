# 11/23/2015

# note that the ORDER BY, LIMIT and OFFSET keywords are not supported in the query when using
# tbl on a connection to a MonetDB database


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
datd <- "./data/"
resd <- "./results/"

acs2013.1year <- "D:/Data/CensusACS/2013/"

acs2013.5year <- "D:/Data/CensusACS/20135year/"
acsfn <- "acs2013_5year"
acsdbfile <- paste0(acs2013.5year, acsfn, ".sql3")
acsxtract <- paste0(acs2013.5year, "acs2013x.rds")

mdbdir <- "D:/Data/CensusACS/20135year/acsMonetDB/"

 
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
library("survey")

# library("RSQLite")
library("MonetDBLite")
library("MonetDB.R")



#****************************************************************************************************
#                Functions ####
#****************************************************************************************************

sb_df <- function(sb) {
  # receive a svyby object, return a long data frame with the two groups and a value column with stat, se
  atsb <- attributes(sb)
  stat <- str_replace(atsb$svyby$statistic, "svy", "")
  rowgrp <- str_replace(deparse(atsb$call[[3]]), "~", "")
  colgrp <- str_replace(deparse(atsb$call[[2]]), "~", "")
  rowstubs <- atsb$row.names
  colstubs <- str_replace(atsb$svyby$variables, colgrp, "")
  df <- as.data.frame(sb)
  names(df) <- c(rowgrp, paste0(colstubs, "_", stat), paste0(colstubs, "_se"))
  df <- df %>% gather(variable, value, -1) %>%
    separate(variable, c(colgrp, "stat"), sep="_")
  return(df)
}


rcsums <- function(df) {
  # take a data frame and add a column with rowsums
  # and then a row of column sums
  # limited to the numeric variables
  numcols <- sapply(df, is.numeric)
  df2 <- df %>% mutate(rowsum=rowSums(.[, numcols]))
  numcols <- c(numcols, rowsum=TRUE)
  sumrow <- df2 %>% summarise_each(funs(colsum=sum(., na.rm=TRUE)), which(numcols))
  df3 <- bind_rows(df2, sumrow)
  ccols <- sapply(df3, is.character)
  fcols <- sapply(df3, is.factor)
  df3[nrow(df3), ccols] <- "colsum"
  return(df3)
}

# df <- data.frame(stub=c("a", "b", "c", "d"), x1=1:4, x2=3:6, x3=7:10)
# df$stub <- as.character(df$stub)
# df
# rcsums(df)





#****************************************************************************************************
#                Create extract of data ####
#****************************************************************************************************
tbl <- "acs2013_5year"

con <- dbConnect(MonetDB.R(), embedded=str_sub(mdbdir, 1, -2)) # run if needed
acsdb <- src_monetdb(mdbd, con=con)
show(acsdb)

dbGetInfo(con)
dbListTables(con)
dbGetQuery(con, paste0("SELECT COUNT(*) FROM ", tbl)) # much faster than nrow
# str(tbl(acsdb, sql(paste0("SELECT COUNT(*) FROM ", tbl))))
tbl(acsdb, sql(paste0("SELECT COUNT(*) FROM ", tbl)))


getall <- tbl(acsdb, sql(paste0("SELECT * FROM ", tbl))) # for test purposes
glimpse(getall) # to help decide what variables to get

count(getall, st)
getall %>% summarise(pop=sum(pwgtp))
# target 311,536,599
# pop
# (dbl)
# 1  311536599
getall %>% group_by(st) %>% summarise(pop=sum(pwgtp))
getall %>% summarise(max(pwgtp))
summary(getall)


df <- getall %>%
  filter(st==36 | powsp05==036 | powsp12==036) %>% # live or work in NYS
  # define desired vars
  select(serialno, puma00, puma10, st, 
         powpuma00, powpuma10, powsp05, powsp12, # use POWPUMA00 and POWSP05 together; POWPUMA10 and POWSP12 together
         adjinc, pwgtp, agep, cow, mar, relp, schl, sex, wagp, semp, hisp, rac1p,
         occp02, occp10, occp12, starts_with("pwgtp"))
system.time(df2 <- collect(df)) # first time is slow, later is fast 6 secs vs 100 secs rsqlite
ht(df2)
max(df2$pwgtp)

dbDisconnect(con)

saveRDS(df2, paste0(acs2013.5year, "acsextract.rds")) # useful to keep a raw extract around


#****************************************************************************************************
#                Modify data as needed before creating design object; source functions ####
#****************************************************************************************************
df2 <- readRDS(paste0(acs2013.5year, "acsextract.rds"))

agebrks <- c(-1e9, -1, 4, 9, 14, 19, 24, 34, 44, 54, 59, 64, 74, 84, 120, 1e9)

source("ACSfunctions_mdb.r")

a <- proc.time()
df3 <- df2 %>% 
  # mutate_each(funs(as.numeric), 
  #             -c(serialno, puma00, puma10, st, powpuma00, powpuma10, powsp05, powsp12, occp02, occp10, occp12)) %>%
  mutate(adjinc=adjinc / 1e6,
         one=1,
         sex=factor(sex, levels=1:2, labels=c("male", "female")),
         occgrp=f_occgrp(occp02, occp10, occp12),
         cowf=f_cow(cow),
         race=f_race_hisp(rac1p, hisp),
         agecut=cut(agep, agebrks),
         pumacnty=f_puma_county(puma00, puma10, st),
         pumapowcnty=f_puma_powcounty(powpuma00, powpuma10, powsp05, powsp12),
         nyc.res=pumacnty %in% c("bronx", "brooklyn", "manhattan", "queens", "statenisland"),
         nyc.pow=pumapowcnty %in% c("bronx", "brooklyn", "manhattan", "queens", "statenisland"),
         stabbr=stcodes$stabbr[match(st, as.integer(stcodes$stfips))],
         year=as.numeric(str_sub(serialno, 1, 4)))
b <- proc.time()
b - a # 8 secs

# do some checks on the result
glimpse(df3)
str(df3)
count(df3, sex)
count(df3, occgrp)
count(df3, race)
count(df3, pumacnty)
count(df3, nyc.res, nyc.pow)
count(df3, nyc.res, pumacnty)
count(df3, nyc.pow, pumapowcnty)
count(df3, agecut)
sum(df3$pwgtp)
sum(df3$pwgtp37)

df3 %>% filter(nyc.res) %>% 
  group_by(occgrp, race) %>%
  summarise(n=n()) %>%
  filter(!is.na(occgrp)) %>%
  spread(occgrp, n)

df3 %>% filter(nyc.res, !nyc.pow, occgrp=="carpenter") %>% group_by(pumacnty, pumapowcnty) %>% summarise(n=n())

tmp <- df3 %>% filter(nyc.res, !nyc.pow, occgrp=="carpenter", pumacnty=="bronx") %>%
  select(serialno, puma00, puma10, st, powpuma00, powpuma10, powsp05, powsp12, pumacnty, pumapowcnty, nyc.res, nyc.pow)

nrow(df3 %>% filter(nyc.res | nyc.pow, occgrp=="carpenter"))


df3 %>% filter(nyc.res | nyc.pow, occgrp=="carpenter", cow %in% 1:2) %>% 
  select(pwgtp, wagp, semp) %>% 
  summarise(n=n(), pwgtp=sum(pwgtp), 
            wagp=mean(ifelse(wagp>0, wagp, NA), na.rm=TRUE), 
            semp=mean(ifelse(semp>0, semp, NA), na.rm=TRUE))

df3 %>% filter(nyc.res | nyc.pow, occgrp=="carpenter", cow %in% 3:5) %>% 
  select(pwgtp, wagp, semp) %>% 
  summarise(n=n(), pwgtp=sum(pwgtp), 
            wagp=mean(ifelse(wagp>0, wagp, NA), na.rm=TRUE), 
            semp=mean(ifelse(semp>0, semp, NA), na.rm=TRUE))

df3 %>% filter(nyc.res | nyc.pow, occgrp=="carpenter", cow %in% 6:7) %>% 
  select(pwgtp, wagp, semp) %>% 
  summarise(n=n(), pwgtp=sum(pwgtp), 
            wagp=mean(ifelse(wagp>0, wagp, NA), na.rm=TRUE), 
            semp=mean(ifelse(semp>0, semp, NA), na.rm=TRUE))

count(filter(df3, nyc.pow | nyc.res, occgrp=="carpenter"), cowf)

count(filter(df3, nyc.pow | nyc.res), occgrp)
count(filter(df3, nyc.pow), occgrp)

df3 %>% filter(nyc.pow) %>% group_by(occgrp, cowf) %>% summarise(n=n()) %>% spread(cowf, n)
df3 %>% filter(nyc.pow) %>% group_by(occgrp, year) %>% summarise(n=n()) %>% spread(year, n)

df3 %>% filter(nyc.pow) %>%
  group_by(occgrp, year) %>%
  summarise(n=n()) %>% 
  spread(year, n) %>% 
  mutate(total=rowSums(.[, sapply(., is.numeric)])) 

f <- function(df) {
  df2 <- df %>% summarise_each(funs(colsum=sum(., na.rm=TRUE)), -1)
  df3 <- bind_rows(df, df2)
  return(df3)
}
  

df3 %>% filter(nyc.pow) %>%
  group_by(occgrp, year) %>%
  summarise(n=n()) %>% 
  spread(year, n) %>% 
  #mutate(total=rowSums(.[, sapply(., is.numeric)])) %>%  # works
  mutate(total=rowSums(.[] %>% select(-one_of("occgrp")))) %>% # works
  # mutate(total=rowSums(.[] %>% select(-matches("occgrp")))) %>% # works
  do(f(.))

#  mutate(sumrow = rowSums(.[, -"occgrp"]))
# mutate(sumrow = rowSums(select(., -occgrp)))
# mutate(total=rowSums(.[, sapply(., is.numeric)]))
#  summarise_each(funs(colsum=sum(., na.rm=TRUE)), -1)

col_grep <- "Sepal"
iris %>% mutate(tmp_name = rowSums(.[grep(col_grep,names(.))]))
iris %>% mutate(tmp_name = rowSums(.[] %>% select(matches(col_grep)) )) 

 #****************************************************************************************************
#                Create survey design object ####
#****************************************************************************************************

df3 <- as.data.frame(df3) # this is CRUCIAL - must use data frame

rwts <- paste0("pwgtp", 1:80)

options( "survey.replicates.mse" = TRUE) # THIS IS IMPORTANT - NOT identical to setting mse=TRUE in svrepdesign

system.time(ny.svm <- svrepdesign(repweights = df3[, rwts], 
                                 weights = ~pwgtp,
                                 combined.weights = TRUE, 
                                 type = "JK1", 
                                 scale = 4/80, rscales = rep(1, 80),
                                 # MSE=TRUE, # this is important
                                 data = df3)) # 18 secs
summary(ny.svm)
str(ny.svm)
system.time(saveRDS(ny.svm, paste0(acs2013.5year, "ny.svm"))) # 90 secs



#****************************************************************************************************
#                Get saved data and reproduce some published PUMA summaries ####
#****************************************************************************************************
system.time(ny.svm <- readRDS(paste0(acs2013.5year, "ny.svm"))) # 15 secs
summary(ny.svm)
str(ny.svm)
glimpse(ny.svm$variables)

# Housing unit population (RELP=0-15)
# GQ population (RELP=16-17)
# GQ institutional population (RELP=16)
# GQ noninstitutional population (RELP=17)

# create a nys subset
nys.sv <- subset(ny.svm, st==36)

svytotal(~one, ny.svm) # this includes commuters to ny -- note that se's are much larger than for the NY-only subset
svytotal(~one, nys.sv) # subsetting is slow
# total     SE
# one 19487053 49.864
# Total population	19,487,053	 50

svytotal(~one, nys.sv) # note how much higher the SE is than for NY residents
# total     SE
# one 564574 4769.7
# My theory: One sampling stratum for the ACS is state, and it is designed to hit the "known" total (I presume).
# When we use the stratum in its entirety (e.g., NY residents), the sampling error is very low.
# But when we use a sub-stratum (or set of sub-strata), such as non-NYers where pow==ny, we don't have control total
# I thought maybe the PWGT vars would be more spread out for the non-nyers, but they are not

# Let's investigate
# chk <- as.data.frame(ny.sv$variables) %>%
#   select(ST, starts_with("PWGT"))
# chk %>% group_by(ny=ST=="36") %>%
#   do(qtiledf(.$PWGTP)) # not all that different
# 
# chk <- as.data.frame(ny.sv$variables) %>%
#   select(serialno, ST, starts_with("PWGT")) %>%
#   select(-PWGTP) %>%
#   gather(variable, repweight, -ST, -serialno) %>%
#   group_by(ny=ST=="36", serialno) %>%
#   summarise(rwsd=sd(repweight, na.rm=TRUE))
# chk %>% group_by(ny) %>%
#   summarise(rwsd=median(rwsd, na.rm=TRUE))



# two different ways of producing totals by factor category
# system.time(tmp <- svytotal(~sex, ny.svm)); tmp # 1.7 secs
system.time(tmp <- svytotal(~sex, subset(ny.svm, st==36))); tmp # 23 secs - using svytotal is faster than svyby

system.time(nys <- subset(ny.svm, st==36))
system.time(print(svytotal(~sex, nys)))

system.time(tmp <- svyby(~one, ~sex, subset(ny.svm, st==36), svytotal)); tmp # 38 secs
# SEX      one       se
# male     male  9443600 2273.859
# female female 10043453 2285.688
# 36	New York	Total males (SEX=1)	9,443,600	 2,274 	 3,740 
# 36	New York	Total females (SEX=2)	10,043,453	 2,286 	 3,760 

# fast way to get unweighted counts
ny.svm$variables %>% filter(nyc.pow) %>% count(occgrp, sex) %>% spread(sex, n)

# two different ways for age
system.time(tmp <- svytotal(~agecut, nys)) # 12 secs
tmp
system.time(tmp <- svyby(~one, ~agecut, ny.svm, unwtd.count)) # 27 secs
tmp
ny.svm$variables %>% filter(st==36) %>% count(agecut) %>% spread(sex, n)
system.time(tmp <- svyby(~one, ~agecut, ny.svm, svytotal)) # 28 secs
tmp



#****************************************************************************************************
#                Analyze ####
#****************************************************************************************************
str(ny.svm)
str(subset(ny.svm, nyc.res)) # 341k recs
glimpse(filter(ny.svm$variables, nyc.res))

# simple nyc counts
svyby(~one, ~sex+occgrp, subset(ny.svm, nyc.res), unwtd.count)
ny.svm$variables %>% filter(nyc.res) %>% count(sex, occgrp) %>% spread(sex, n) # much faster
# svyby(~SEX, ~occgrp, subset(ny.sv, nyc), unwtd.count) # unexpected result - counts by occgrp only
# svyby(~one, ~SEX+occgrp, subset(ny.sv, nyc), svy.mean) # no good
# svyby(~SEX+occgrp, ~one, subset(ny.sv, nyc), svy.mean) # no good
# svyby(~SEX, ~occgrp+one, subset(ny.sv, nyc), svy.mean) # no good

# gender
tmp <- svyby(~sex, ~occgrp, ny.svm, svymean)
tmp
confint(tmp)

tmp.res <- svyby(~sex, ~occgrp, subset(ny.svm, nyc.res), svymean)
tmp.res
confint(tmp.res)

tmp.pow <- svyby(~sex, ~occgrp, subset(ny.svm, nyc.pow), svymean)
tmp.pow
confint(tmp.pow)

# incommuters
tmp.incom <- svyby(~sex, ~occgrp, subset(ny.svm, nyc.pow & !nyc.res), svymean)
tmp.incom
confint(tmp.incom)
svyby(~sex, ~occgrp, subset(ny.svm, nyc.pow & !nyc.res), unwtd.count)


# race
tmp <- svyby(~race, ~occgrp, subset(ny.svm, nyc.res), svymean)
tmp
confint(tmp)
as.data.frame(confint(tmp)) %>% mutate(type=row.names(.)) %>%
  separate(type, c("occgrp", "race"), ":") %>%
  mutate(race=str_sub(race, 5, length(race))) %>%
  select(occgrp, race, everything()) %>%
  arrange(occgrp, race)

names(tmp)
# before gathering, fix the names
vnames <- names(tmp)
vnames[7:11] <- paste0(vnames[2:6], ".se")
vnames[2:6] <- paste0(vnames[2:6], ".occ_raceshare")
vnames[2:11] <- str_replace(vnames[2:11], "race", "")
vnames
tmp2 <- tmp
names(tmp2) <- vnames
tmp3 <- tmp2 %>% gather(variable, value, -occgrp) %>%
  separate(variable, c("race", "type"), "\\.") %>%
  spread(type, value)
tmp3 %>% select(-se) %>% spread(occgrp, occ_raceshare) %>% arrange(-carpenter) %>% kable(digits=4)


#****************************************************************************************************
#                nyc.pow: Occupation and race ####
#****************************************************************************************************
system.time(ny.svm <- readRDS(paste0(acs2013.5year, "ny.svm"))) # 15 secs
system.time(nyc.pow.sv <- subset(ny.svm, nyc.pow)) # 4 secs takes a while to create, so keep it handy

# svytable(~race+occgrp, nyc.pow.sv)
# svychisq(~race+occgrp, nyc.pow.sv, statistic="adjWald") # takes a long time

# a <- svymean(~interaction(race, occgrp, sep=":", lex.order=TRUE), design = nyc.pow.sv)
# a <- svymean(~race:occgrp, design = nyc.pow.sv)
# a <- svymean(~interaction(race, occgrp), design = nyc.pow.sv)
# a
# str(a)
# ftable(a)
# a %>% as.data.frame %>% 
#   mutate(group=row.names(.), 
#          group=str_sub(group, str_locate(group, "\\)")[1]+1, -1)) %>%
#   separate(group, c("race", "occgrp"),  sep=":")%>%
#   gather(variable, value, mean, SE) %>%
#   #filter(variable=="mean") %>%
#   spread(occgrp, value) %>%
#   arrange(variable, race) %>%
#   write_csv("d:/temp/meanse.csv")


# str_sub("abc)def", str_locate("abc)def", "\\)")$start, -1)
# 
# str(str_locate("abc)def", "\\)"))
# str_locate("abc)def", "\\)")[1]


tmp <- svyby(~race, ~occgrp, nyc.pow.sv, svymean)
str(tmp)

sb_df(tmp) %>% spread(occgrp, value) %>%
  arrange(race, stat) %>%
  write_csv("d:/temp/check.csv")
  # kable(digits=2)

nyc.pow.sv$variables %>% group_by(race, occgrp) %>%
  summarise(n=n()) %>%
  spread(occgrp, n) %>%
  write_csv("d:/temp/check_unwtd.csv")

tmp <- svyby(~race, ~occgrp, nyc.pow.sv, svytotal)

tmp %>% sb_df %>% filter(stat!="se") %>%
  spread(occgrp, value) %>%
  do(rcsums(.)) %>%
  write_csv("d:/temp/rcsums.csv")

nyc.pow.sv$variables %>%
  group_by(occgrp, race) %>%
  summarise(n=n()) %>%
  spread(occgrp, n) %>%
  do(rcsums(.)) %>%
  write_csv("d:/temp/rcsums.csv")

# ftable(tmp) # triggers error!

# confint(tmp) %>% as.data.frame %>%
#   mutate(type=row.names(.)) %>%
#   separate(type, c("occgrp", "race"), ":") %>%
#   mutate(race=str_sub(race, 5, length(race))) %>%
#   select(occgrp, race, everything()) %>%
#   arrange(occgrp, race) %>%
#   gather(lbub, value, -occgrp, -race) %>%
#   mutate(lbub=str_replace_all(lbub, " ", "")) %>%
#   unite(occgrp_lbub, occgrp, lbub) %>%
#   spread(occgrp_lbub, value) %>%
#   write_csv("d:/temp/lbub.csv")

#****************************************************************************************************
#                END OF ANALYSIS ####
#****************************************************************************************************


#****************************************************************************************************
#                Census-published NY person totals, se, and moe from pums_estimates_9_13.xlsx ####
#****************************************************************************************************
# st	state	characteristic	pums_est_09_to_13	 pums_se_09_to_13 	 pums_moe_09_to_13 
# 36	New York	Total population	19,487,053	 50 	 82 
# 36	New York	Housing unit population (RELP=0-15)	18,901,364	 50 	 82 
# 36	New York	GQ population (RELP=16-17)	585,689	 -   	 -   
# 36	New York	GQ institutional population (RELP=16)	229,765	 28 	 47 
# 36	New York	GQ noninstitutional population (RELP=17)	355,924	 28 	 47 
# 36	New York	Total males (SEX=1)	9,443,600	 2,274 	 3,740 
# 36	New York	Total females (SEX=2)	10,043,453	 2,286 	 3,760 
# 36	New York	Age 0-4	1,162,235	 1,500 	 2,468 
# 36	New York	Age 5-9	1,155,121	 4,001 	 6,581 
# 36	New York	Age 10-14	1,201,418	 4,016 	 6,607 
# 36	New York	Age 15-19	1,334,213	 2,973 	 4,891 
# 36	New York	Age 20-24	1,420,373	 3,428 	 5,639 
# 36	New York	Age 25-34	2,712,114	 3,074 	 5,056 
# 36	New York	Age 35-44	2,584,838	 2,891 	 4,756 
# 36	New York	Age 45-54	2,857,187	 2,407 	 3,960 
# 36	New York	Age 55-59	1,266,213	 4,280 	 7,041 
# 36	New York	Age 60-64	1,099,146	 4,224 	 6,948 
# 36	New York	Age 65-74	1,427,812	 1,702 	 2,799 
# 36	New York	Age 75-84	863,334	 2,839 	 4,671 
# 36	New York	Age 85 and over	403,049	 2,749 	 4,522 


#****************************************************************************************************
#                Census PUMA code info ####
#****************************************************************************************************
# POWPUMA00 POWPUMA10 POWSP05 POWSP12
# POWPUMA00 5
# Place of work PUMA based on Census 2000 definition for data collected prior
# to 2012
# bbbbb .N/A (not a worker--not in the labor force,
#             .including persons under 16 years;
#             .unemployed; civilian employed, with a job not
#             .at work; Armed Forces, with a job but not at work)
# 00001 .Did not work in the United States or in Puerto Rico
# 00100..08200 .Assigned Place of work PUMA. Use with POWSP05.
# 22777 .Combination of 01801, 01802, and 01905 in Louisiana
# -0009 .Code classification is Not Applicable because data
# .collected in 2012 or later
# 
# POWPUMA10 5
# Place of work PUMA based on 2010 Census definition for data collected in
# 2012 or later
# bbbbb .N/A (not a worker--not in the labor force,
#             .including persons under 16 years;
#             .unemployed; civilian employed, with a job not
#             .at work; Armed Forces, with a job but not at work)
# 00001 .Did not work in the United States or in Puerto
# .Rico
# 00100..70100 .Assigned Place of work PUMA. Use with POWSP12.
# -0009 .Code classification is Not Applicable because data
# .collected prior to 2012

# POWSP05 3
# Place of work for data collected prior to 2012 - State or foreign country recode

# POWSP12 3
# Place of work for data collected in 2012 or later - State or foreign country
# recode
# bbb .N/A (not a worker--not in the labor force,
#           .including persons under 16 years; unemployed;
#           .employed, with a job not at work; Armed Forces,
#           .with a job but not at work)
# 001 .Alabama/AL


#****************************************************************************************************
#                Survey design and package information ####
#****************************************************************************************************
# From svrepdesign documentation:
# The successive difference weights in the American Community Survey use 
#   scale = 4/ncol(repweights) and 
#   rscales=rep(1,   ncol(repweights)) 
# 
# JK2 weights use scale=1, rscales=rep(1, ncol(repweights))

# https://usa.ipums.org/usa/repwt.shtml (re Stata)
# the replicate standard errors contained in the IPUMS-USA data are calculated using the successive
# difference replication method, which is different from the types of replicate weights that most 
# statistical software packages can handle...The sample should be treated as a single stratum 
# (the weights contain the relevant information from the sample design), so no PSU should be specified.
# ...Correspondence with StataCorp statisticians and IPUMS testing revealed that successive difference 
# replicate weights can be treated as jackknife replicate weights if the options are specified correctly.

# Can I simply divide the full sample into 80 random subsamples from the full sample and calculate 
# replicate standard errors manually? 
# No. Replicate weights contain full information about the complex sample design of the ACS/PRCS, and 
# this information would be lost when drawing random subsamples. Furthermore, replicate samples incorporate 
# information from all cases in the full sample. In contrast, random subsamples would each be 1/80th 
# the size of a single replicate subsample.

# How are the ACS/PRCS replicate weights calculated?
# As mentioned, replicate weights in the ACS and PRCS are constructed using the successive difference
# replication method. This involves creating a k x k Hadamard matrix (where k is the number of 
# replicate weights desired), assigning sample cases to rows in the matrix and calculating a replicate 
# factor from the row values, and finally multiplying the full-sample weight by these replicate factors.
# For more details, see the Census Bureau's "Estimating ASEC Variances with Replicate Weights" 
# document, written for the CPS

# http://www.ats.ucla.edu/stat/stata/library/replicate_weights.htm good reading
# https://github.com/ajdamico/asdfree/blob/master/Confidentiality/how%20to%20create%20de-identified%20replicate%20weights.R useful



