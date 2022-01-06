
# acs factors

#.. COW Character 1 ----
# Class of worker
# b .Not in universe (less than 16 years old/NILF who last worked .more than 5 years ago or never worked)
# 1 .Employee of a private for-profit company or business, or of an .individual, for wages, salary, or commissions
# 2 .Employee of a private not-for-profit, tax-exempt, or .charitable organization
# 3 .Local government employee (city, county, etc.)
# 4 .State government employee
# 5 .Federal government employee
# 6 .Self-employed in own not incorporated business, professional .practice, or farm
# 7 .Self-employed in own incorporated business, professional .practice or farm
# 8 .Working without pay in family business or farm
# 9 .Unemployed and last worked 5 years ago or earlier or never .worked

f_cow <- function(cow){
  levs <- 1:9
  labs <- c("Private, for profit",
            "Private, not for profit",
            "Local government",
            "State government",
            "Federal government",
            "Self-employed, unincorporated",
            "Self-employed, incorporated",
            "Unpaid family or farm",
            "Unemployed 5+ years")
  # cbind(levs, labs)
  factor(cow, levels=levs, labels=labs)
}

#.. MAR Character 1 ----
# Marital status
# 1 .Married
# 2 .Widowed
# 3 .Divorced
# 4 .Separated
# 5 .Never married or under 15 years old

f_marstat <- function(mar){
  factor(mar,
         levels=1:5,
         labels=c("married", "widowed", "divorced", "separated", "neverM"))
}


#.. RAC1P Character 1 ----
# Recoded detailed race code
# 1 .White alone
# 2 .Black or African American alone
# 3 .American Indian alone
# 4 .Alaska Native alone
# 5 .American Indian and Alaska Native tribes specified; or .American Indian or
# Alaska Native, not specified and no other races
# 6 .Asian alone
# 7 .Native Hawaiian and Other Pacific Islander alone
# 8 .Some Other Race alone
# 9 .Two or More Races

f_race_hisp <- function(rac1p, hisp) {
  # hisp==1 is NOT hispanic
  y <- ifelse(hisp==1, rac1p, 0)
  y <- factor(y)
  levels(y) <- list("White alone, not Hispanic"=1, 
                    "Black alone, not Hispanic"=2,
                    "Hispanic, any race"=0,
                    "Asian alone, not Hispanic"=6,
                    "Other or multiple race, not Hispanic"=c(3:5, 7:9))
  return(y)
}

#.. SCHL Character 2 ----
# Educational attainment
# bb .N/A (less than 3 years old)
# 01 .No schooling completed
# 02 .Nursery school, preschool
# 03 .Kindergarten
# 04 .Grade 1
# 05 .Grade 2
# 06 .Grade 3
# 07 .Grade 4
# 08 .Grade 5
# 09 .Grade 6
# 10 .Grade 7
# 11 .Grade 8
# 12 .Grade 9
# 13 .Grade 10
# 14 .Grade 11
# 15 .12th grade - no diploma
# 16 .Regular high school diploma
# 17 .GED or alternative credential

# 18 .Some college, but less than 1 year
# 19 .1 or more years of college credit, no degree

# 20 .Associate's degree
# 21 .Bachelor's degree
# 22 .Master's degree
# 23 .Professional degree beyond a bachelor's degree
# 24 .Doctorate degree

f_yoschool <- function(schl, agep){
  # estimate years of schooling based on educational attainment
  yoschool <- case_when(schl <= 14 ~ 12,  # no higher than 11th grade -- give them 11 years
                        schl %in% 15:17 ~ 12,  # 12th grade, HS diploma or alternative
                        schl %in% 18:19 ~ 13,  # +/- another year
                        schl == 20 ~ 14,  # associate's
                        schl == 21 ~ 16,  # bachelor's
                        schl == 22 ~ 18,  # master's -- 2 years?
                        schl == 23 ~ 19,  # professional degree
                        schl == 24 ~ 20,  # doctorate
                        TRUE ~ NA_real_)
  ifelse(yoschool > agep - 6,
         agep - 6,
         yoschool)
}


f_edattain <- function(schl){
  edattain <- case_when(schl <= 15 ~ "notHSgrad",
                        schl %in% 16:17 ~ "HSgrad",
                        schl %in% 18:19 ~ "HSplus",
                        schl == 20 ~ "associate",
                        schl == 21 ~ "BA",
                        schl %in% 22:24 ~ "MAplus",
                        TRUE ~ "other")
  # put edattain values in desired order
  edattain <- factor(
    edattain,
    levels=c("HSgrad", "HSplus", "associate", "BA", "MAplus", "notHSgrad", "other"))
  # make hsgrad first in sorts??
  edattain
}
# f_edattain(11)

f_create_vars <- function(df){
  # create multiple new variables for df
  # df has acs variables
  df %>%
    mutate(year=str_sub(serialno, 1, 4) %>% as.integer,
           adjinc=adjinc / 1e6,
           rwagp=wagp * adjinc,
           yoschool=f_yoschool(schl, agep),
           ltassoc=schl < 20,
           ltba=schl < 21,
           ltma=schl < 22,
           bornus=cit==1,
           agep2=agep^2,
           exp=agep - yoschool - 6,
           exp2=exp^2,
           lrwagp=log(rwagp),
           powpumaf=paste0(st, powpuma),
           sex=factor(sex, levels=1:2, labels=c("male", "female")),
           rachisp=f_race_hisp(rac1p, hisp),
           edattain=f_edattain(schl),
           marstat=f_marstat(mar))
}
