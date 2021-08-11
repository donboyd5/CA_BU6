
# links ----
# https://www.nber.org/research/data/current-population-survey-cps-merged-outgoing-rotation-group-earnings-data

# https://cps.ipums.org/cps/outgoing_rotation_notes.shtml
# https://forum.ipums.org/t/merging-nber-morg-and-may-files-with-cps-ipums-help/2579
# https://ceprdata.org/cps-uniform-data-extracts/cps-outgoing-rotation-group/
# http://ceprdata.org/cps-uniform-data-extracts/cps-basic-programs/cps-basic-monthly-programs/

# dta files
# morg79..morg99
# morg00..morg20


# 3802 Correctional officers and jailers 33-3012

# libraries ----
library(tidyverse)
options(tibble.print_max = 80, tibble.print_min = 80) # if more than 60 rows, print 60 - enough for states
library(purrr)
library(haven)
library(arrow)
library(kableExtra)
library(btools)
library(gt)
library(knitr)

library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)
library(gridExtra)
library(RcppRoll)
library(ggbreak)
library(patchwork)
library(RColorBrewer)


# locations ----
urlbase <- "https://data.nber.org/morg/annual/"

cpsdir <- r"(E:\data\cpsmorg\)"
statadir <- paste0(cpsdir, "stata/")



# functions ----



# download data ----

f <- function(year){
  print(year)
  y2 <- str_sub(year, 3, 4)
  fn <- paste0("morg", y2, ".dta")
  upath <- paste0(urlbase, fn)
  dlpath <- paste0(statadir, fn)
  download.file(upath, dlpath, mode="wb")
}

purrr::map(1979:2020, f)


# save data files as arrow format ----
path <- paste0(statadir, "morg20.dta")
df <- read_dta(path)
glimpse(df) # 271k records
# look at labeled variables
count(df, stfips) # 51 - includes DC
as_factor(df$stfips)
length(unique(df$hhid)) # 105k households
count(df, intmonth)
count(df, hrhtype)  # marital status, we'll want to drop group_quarters
count(df, penatvty)
count(df, pemntvty)
count(df, pefntvty)
count(df, prcitshp)
count(df, prcitflg)
count(df, peinusyr)
count(df, lfsr94)  # 1=employed at work
count(df, reason94)
count(df, absent94)
count(df, why3594)
count(df, ftpt94)  # 2= FT 35+
count(df, class94)  # 2=govstate, 3=govlocal
count(df, eligible)
count(df, otc)
count(df, paidhre)
count(df, unionmme)
count(df, unioncov)
count(df, studftpt)
count(df, occ2012)
unique(df$occ2012) %>% sort
ns(df)





df2 <- df %>%
  mutate(stabbr=as_factor(stfips) %>% as.character) %>%
  select(stabbr, intmonth, earnwt, age, sex, race, ethnic, earnhre, earnwke, lfsr94, ftpt94, class94, occ2012)
glimpse(df2)

df2 %>%
  filter(lfsr94 %in% 1:2, ftpt94==2, class94==2) %>%
  group_by(stabbr) %>%
  summarise(n=n()) # 405 state gov, employed full time

ca <- df2 %>% filter(stabbr=="CA")
count(ca, lfsr94)  # 12.2k 1,2 employed
count(ca, ftpt94)  # 8.7k FT
count(ca %>% filter(lfsr94 %in% 1:2), ftpt94) # 8.7k FT
count(ca, class94)  # 614 state gov
count(ca, occ2012)  # 614 state gov

ca %>% filter(occ2012==3802)  # 12 people!


# read arrow and save a longitudinal extract ----



# analysis ----


# selected documentation ----

# Interview Month
# Month in sample
# State
# MSA/PMSA FIPS code
# PMSA ranking
# CMSA/MSA ranking
# MSA/CMSA size
# CMSA code
# Metropolitan status code
# Central city code
# Household ID
# Sex
# Veteran
# Highest grade attended
# Whether completed highest grade
# What was doing most of last week
# How many hours last all jobs
# Usually works >= 35 hrs at this job
# Why not at least 35 hours last week
# Class of worker
# Usual hours
# Paid by the hour
# Earnings per hour
# Usual earnings per week
# Union member
# Covered by a union contract
# Enrolled as a student full/part time
# relationship to reference person	Age
# Marital status
# Race
# How many hours last week?
#   Reason less than or equal to 35 hours last w
# Why absent from work last week?
#   3-digit industry code
# 3-digit occupation code
# Class of worker
# Usual hours
# Paid by the hour
# Union member
# Ethnicity
# Labor force status recode
# Full-time or part-time status
# Detailed industry code
# Detailed occupation code
# Earnings eligibility flag
# Class of worker 2
# Earnings per hour
# Earnings per week
# Final weight
# Earnings weight for all races
# Usual hours (I25a) allocation flag
# Paid by hour (I25b) allocation flag
# Earnings/hr (I25c) allocation flag
# Usl Earn/hr (I25d) allocation flag


# older items ----

# C:\Users\DonAMD\Documents\Data\CPS MORG\Data

morg<-"C:\\Users\\DonAMD\\Documents\\Data\\CPS MORG\\Data\\"

# load(paste(morg,"morg10.rdata",sep=""))

# lm(y~x+z+factor(firmid))

# head(morg10)
# str(morg10)

# create a useful subset
count(morg10,"minsamp")

keepvars<-c("year","earnwke","class94","age","sex","grade92","marital","ownchild","unionmme","unioncov","race","ethnic","occ00","centcity","stfips")

getmorg<-function(y4){
  y2<-substr(y4,3,4)
  dfname<-paste("morg",y2,sep="")
  load(paste(morg,dfname,".rdata",sep=""))  
  df<-subset(get(dfname),minsamp==4 & age>=17 & class94 %in% c(2,3,4),select=keepvars)
  return(df)
}
for(year in 2008:2010){
  print(paste("getting year: ",year,sep=""))
  assign(paste("morg",year,sep=""),getmorg(year))
}
morgall<-rbind(morg2008,morg2009,morg2010)

morgall$stabbr<-factor(morgall$stfips,levels=as.numeric(stfips),labels=stabbr)
morgall$exp<-morgall$age-17
morgall$exp2<-morgall$exp*morgall$exp
morgall$lnearnwke<-ifelse(morgall$earnwke>0,log(morgall$earnwke),NA) # when earnwke=0 we get Inf rather than NA, which causes lm error
morgall$stabbr2<-ifelse(morgall$stabbr %in% c("CA","IL","NJ","NY","TX","VA"),as.character(morgall$stabbr),"_RON")
head(morgall); tail(morgall)
str(morgall)

sum(is.na(morgall$centcity))
sum(is.na(morgall$race)) 
sum(is.na(morgall$marital))
sum(is.na(morgall$ownchild))
sum(is.na(morgall$grade92))
sum(is.na(morgall$occ00))
sum(is.na(morgall$ethnic)) # too many missing to use

# state and local
reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(marital)+factor(centcity)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=subset(morgall, class94 %in% c(2,3)))
summary(reg)

# private
reg<-lm(lnearnwke~exp+exp2+factor(sex)+factor(marital)+factor(centcity)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=subset(morgall, class94 %in% c(4)))
summary(reg)

# state gov
reg<-lm(lnearnwke~exp+exp2+factor(sex)+factor(marital)+factor(centcity)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=subset(morgall, class94 %in% c(2)))
summary(reg)

# local gov
reg<-lm(lnearnwke~exp+exp2+factor(sex)+factor(marital)+factor(centcity)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=subset(morgall, class94 %in% c(3)))
summary(reg)



reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(marital)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=subset(morgall,!is.na(centcity)))
summary(reg)


reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(marital)+factor(race)+factor(grade92)+factor(occ00)+factor(year)+factor(stabbr2),data=morgall)
summary(reg)

count(morgall,"stabbr2")


lm(lnearnwke~exp+exp2,data=morgall)
reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(race)+factor(year)+factor(occ00)+factor(stabbr2),data=morgall)
reg
summary(reg)


reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(race)+factor(occ00)+interaction(factor(year),factor(stabbr2)),data=morgall)
summary(reg)
reg







morg2010<-getmorg(2010)


sum(is.na(morg2$race)) 
sum(is.na(morg2$ethnic)) # too many missing to use


lm(lnearnwke~exp+exp2,data=morg2)
reg<-lm(lnearnwke~exp+exp2+factor(class94)+factor(sex)+factor(race)+factor(stabbr2),data=morg2)
reg
summary(reg)




lm(lnearnwke~exp+exp2+factor(class94),data=morg2[1:1150,])


morg2[1100:1150,]


# ,na.action=na.omit

model lnearnwke=exp exp2 class94 stabbr grade92 year centcity occ00 sex / noint solution;
morg10$lnearnwke<-log(morg10$earnwke)


