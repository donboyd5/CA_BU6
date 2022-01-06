
get_ss_nra <- function(){
  #.. Social Security normal retirement age ----
  # https://www.ssa.gov/oact/ProgData/nra.html
  # Normal Retirement Age
  # Year of birth	Age
  # 1937 and prior	65
  # 1938	65 and 2 months
  # 1939	65 and 4 months
  # 1940	65 and 6 months
  # 1941	65 and 8 months
  # 1942	65 and 10 months
  # 1943-54	66
  # 1955	66 and 2 months
  # 1956	66 and 4 months
  # 1957	66 and 6 months
  # 1958	66 and 8 months
  # 1959	66 and 10 months
  # 1960 and later	67
  
  ss_nra <- tibble(cyob=1930:2030) %>%
    mutate(nra = case_when(cyob <= 1937 ~ 65,
                           cyob %in% 1938:1942 ~ 65 + 2/12 * (cyob - 1937),
                           cyob %in% 1943:1954 ~ 66,
                           cyob %in% 1955:1959 ~ 66 + 2/12 * (cyob - 1954),
                           cyob >= 1960 ~ 67,
                           TRUE ~ -Inf))
  ss_nra
}


get_awifc <- function(){
  #.... SS average wage index series and awi including forecast (i.e., awifc) ----
  awi1 <- read_excel(static, sheet="awiseries", range="A2:B72")
  awi <- awi1 %>%
    mutate(awi=awiseries[year==max(year)] / awiseries,
           awipch=awiseries / awiseries[match(year - 1, year)] - 1)
  
  # compute a forecasted wage index series
  awifc <- awi %>%
    select(year, awiseries) %>%
    bind_rows(tibble(year=2021:2200)) %>%
    mutate(awiseries=ifelse(year > 2020,
                            awiseries[year==2020] * (1 + realwage_pch + cpi_pch)^(year - 2020),
                            awiseries),
           awi=awiseries[year==2020] / awiseries,
           awi_pch=awiseries / awiseries[match(year - 1, year)] - 1)
  awifc
}


get_bendsfc <- function(){
  #.. SS bends for the Social Security formula ----
  bends1 <- read_excel(static, sheet="bends", range="A4:C48")
  
  # create a forecasted series
  bendsfc <- bends1 %>%
    bind_rows(tibble(year=2023:2200)) %>%
    mutate(across(c(bend1, bend2), 
                  function(x) ifelse(year > 2022,
                                     x[year==2022] * (1 + nomwage_pch)^(year - 2022),
                                     x)))
  bendsfc
}


fiwage <- function(cyob, year, wage){
  # indexed (real) wage for each year ----
  # based on iwage factors, assumed to be the same for every year
  
  # inputs:
  #        nomwage_pch -- constant real growth rate for wages
  # output: iwage -- indexed wage series
  
  # https://www.ssa.gov/oact/COLA/awifactors.html
  
  # An individual's earnings are always indexed to the average wage level two
  # years prior to the year of first eligibility. Thus, for a person retiring at
  # age 62 in 2022, the person's earnings would be indexed to the average wage
  # index for 2020
  
  # A factor will always equal one for the year in which the person attains age
  # 60 and all later years. The indexing factor for a prior year Y is the result
  # of dividing the average wage index for the year in which the person attains
  # age 60 by the average wage index for year Y.
  
  # awifc -- year, awiseries, awi, pch
  # we want to construct a wage index that is 1 for age 60 and later, indexed to age 60 in earlier years
  iwagedf <- tibble(year, wage) %>%
    left_join(awifc %>% select(year, awiseries, awi), by="year") %>%
    mutate(idx=awiseries[year==(cyob + 60)] / awiseries,
           idx=ifelse(year > (cyob + 60), 1, idx),
           iwage=wage * idx)
  iwagedf$iwage
}


faime <- function(iwage){
  # aime -- average indexed monthly earnings ----
  
  # iwage -- vector of indexed wages
  
  # https://www.ssa.gov/oact/COLA/Benefits.html#aime
  
  # Up to 35 years of earnings are needed to compute average indexed monthly
  # earnings. After we determine the number of years, we choose those years with
  # the highest indexed earnings, sum such indexed earnings, and divide the
  # total amount by the total number of months in those years.
  
  # the sums for the highest 35 years of indexed earnings and the corresponding
  # average monthly amounts of such earnings. (The average is the result of
  # dividing the sum of the 35 highest amounts by the number of months in 35
  # years.) Such an average is called an "average indexed monthly earnings"
  # (AIME).
  
  best_wages <- sort(iwage[iwage > 0], decreasing = TRUE)
  if(length(best_wages) > 35) length(best_wages) <- 35 # only count the 35 best years
  aime <- sum(best_wages) / (length(best_wages) * 12) # get average, per month
  aime
}


fpia <- function(aime, cyofe, cyossnra){
  # The "primary insurance amount" (PIA) ----
  # pia is the benefit (before rounding down to
  # next lower whole dollar) a person would receive if he/she elects to begin
  # receiving retirement benefits at his/her normal retirement age. At this age,
  # the benefit is neither reduced for early retirement nor increased for
  # delayed retirement.
  
  # STEPS:
  # - get wages through yor - 1, (https://www.ssa.gov/oact/ProgData/retirebenefit1.html)
  #     I assume SS yor is cyossnra -- year of SS normal retirement age
  # - calc wages indexed to yoage 60 (cyofe - 2), index=1 afterward (same), using nominal wage
  #     index values (https://www.ssa.gov/oact/COLA/AWI.html and https://www.ssa.gov/oact/COLA/awidevelop.html)
  # - calc aime based on highest (up to) 35 years (again https://www.ssa.gov/oact/ProgData/retirebenefit1.html)
  # - apply pia formula using bend points for age=62 (https://www.ssa.gov/oact/ProgData/retirebenefit2.html)
  #     bend points: (https://www.ssa.gov/oact/COLA/bendpoints.html)
  # - index this tentative pia from year of first eligibility, (yoage62) to yo SS retirement, which
  #     I assume to be cyossnra for everyone (all retire for SS purposes at SS nra)
  #     using cpiw cola (same - https://www.ssa.gov/oact/ProgData/retirebenefit2.html)
  # - the result is pia, the initial benefit person would get if retires at ssnra
  # - (initial benefit could be adjusted up or down for early or late retirement, but I assume claim SS at nra)
  # - benefits in later years adjusted by cpiw cola
  
  # aime - average indexed monthly earnings using data through yor - 1, indexed to age 60 (index=1 after 60)
  # cyofe - year of first eligibility -- cyo age 62
  
  # pia -- primary insurance amount
  
  # ASSUMES the following are in the global environment:
  #   bendsfc -- forecast of bends cut points
  #   cpifc -- cpi forecast
  
  # inputs receive a vector of years and wages, 
  # return pia, primary insurance amount
  
  # https://www.ssa.gov/oact/COLA/Benefits.html
  # https://www.ssa.gov/oact/ProgData/retirebenefit2.html
  
  # The PIA is the sum of three separate percentages of portions of the AIME.
  # While the percentages of this PIA formula are fixed by law, the dollar
  # amounts in the formula change annually with changes in the national average
  # wage index. These dollar amounts, called "bend points," govern the portions
  # of the AIME.
  
  # The bend points in the year 2022 PIA formula, $1,024 and $6,172, apply for
  # workers becoming eligible in 2022. See the table of bend points for the bend
  # points applicable in past years.
  
  # For example, a person who had maximum-taxable earnings in each year since
  # age 22, and who retires at age 62 in 2022, would have an AIME equal to
  # $11,430. Based on this AIME amount and the bend points $1,024 and $6,172,
  # the PIA would equal $3,357.60. This person would receive a reduced benefit
  # based on the $3,357.60 PIA. The first COLA this individual could receive is
  # the one effective for December 2022.
  
  # bends <- c(NA_real_, NA_real_)
  bends <- c(bendsfc$bend1[bendsfc$year==cyofe],
             bendsfc$bend2[bendsfc$year==cyofe])
  
  # djb: the pia rates are fixed at:
  pia_rates <- c(.9, .32, .15)
  
  # examples from ss site:  aime --> pia
  #   4643, 2079.68: 	$1,024	$6,172	.9(1024) + .32(4643 - 1024) = $2,079.68
  #  10141, 2957.74:  $10,141	895	5,397	.9(895) + .32(5397 - 895) + .15(10141 - 5397) = $2,957.74
  # aime <- 4643; bends <- c(1024, 6172)
  # aime <- 10141; bends <- c(895, 5397)
  
  excess1 <- pmin(bends[1], aime)
  excess2 <- ifelse(aime > bends[1],
                    pmin(aime, bends[2]) - bends[1],
                    0)
  excess3 <- ifelse(aime > bends[2],
                    aime - bends[2],
                    0)
  pia_preCOLA <- 
    pia_rates[1] * excess1 +
    pia_rates[2] * excess2 +
    pia_rates[3] * excess3
  
  # now add cola, if any, from cyofe to normal retirement age - 1
  cpi_cyofe_minus1 <- cpifc$cpi[cpifc$year==(cyofe - 1)]  # I think this is right, but triple check
  cpi_nra_minus1 <- cpifc$cpi[cpifc$year==(round(cyossnra) - 1)]
  # note that I don't use the 5.9% cola for 2021, I use 2% (for convenience)
  
  pia <- pia_preCOLA * cpi_nra_minus1 / cpi_cyofe_minus1
  pia
}


# ONETIME SETUP of data and parameters for Social Security ----
ss_nra <- get_ss_nra()
awifc <- get_awifc()
bendsfc <- get_bendsfc()
# END ONETIME SETUP ----

fsocsec <- function(ss_covered, w, b, p, cpifc){
  # main Social Security function ----
  # w: worker data (non-time-varying)
  # b: base data for the worker, per year
  # p: params for retirement benefit calculations
  
  if(!ss_covered){
    socsec <- tibble(sstax=0, iwage=0, pia=0, ssbenefit=0)
    # socsec <- tibble()
    # tibble(sstax=rep(0, nrow(b)), iwage=0, pia=0, ssbenefit=0)
    return(socsec)
    # return(NULL)
  }
  
  socsec <- unnest(b, cols = c()) %>%
    mutate(sstax=ifelse(yos > 0,
                        wage * ss_eecrate,
                        wage * 0)) %>%
    mutate(iwage=fiwage(w$cyob, year, wage))  # indexed wage
  
  aime <- faime(socsec$iwage)  # average indexed monthly earnings, scalar
  
  cyofe <- 62 - w$aoe + w$cyoe # year of first eligibility -- year a person attains age 62
  # ss_nra is df with cyob, nra
  ssnra <- ss_nra$nra[ss_nra$cyob==w$cyob]
  cyossnra <- w$cyob + ssnra # I assume everyone claims SS at ss normal retirement age, NOT at govt retirement
  
  pia <- fpia(aime, cyofe, cyossnra) # primary insurance amount, scalar
  
  socsec <- socsec %>%
    left_join(cpifc %>% select(year, cpi),
              by="year") %>%
    mutate(pia=pia,
           # determine annual benefits, with cola if relevant (now, ALWAYS -- djb)
           ssbenefit=ifelse(age < round(ssnra),
                            0,
                            pia * 12 * cpi / cpi[year==round(cyossnra)])) %>%
    select(-names(b), -cpi)  # don't duplicate names
  
  socsec
}

