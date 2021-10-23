
# https://stat.ethz.ch/pipermail/r-help/2010-May/238556.html

# if the plm function only puts out one r-squared, it should be the within
# r-squared, but I could be wrong. Stata, for example, gives you a within, a
# between, and an overall r-squared. Here is what they do.

library(tidyverse)
library(plm)
library(fixest)

set.seed(1)
x <- rnorm(100)
fe <- rep(rnorm(10), each=10)
id <- rep(1:10, each=10)
ti <- rep(1:10, 10)
e <- rnorm(100)
y <- x + fe + e

data <- data.frame(y, x, id, ti)

reg <- plm(y ~ x, model="within", index=c("id", "ti"), data=data)
summary(reg)
# RSS 83.908, TSS 178.5
# R-Squared:      0.52994
# Adj. R-Squared: 0.47712  # feols does not seem to report this


cat("R-squared: ", 1 - 83.908/178.5)  #  0.5299272

regfix <- feols(y ~ x | id, data = data)
summary(regfix)
# coef is the same as reg plm but se is clustered on id and is larger than in plm
# RMSE: 0.916014     Adj. R2: 0.718695
# Within R2: 0.529939
r2(regfix, type = "all", full_names = TRUE)  # gives everything
# Squared Correlation                        R2               Adjusted R2                 Pseudo R2        Adjusted Pseudo R2                 Within R2 
# 0.7471097                 0.7471097                 0.7186951                 0.3405304                 0.2909915                 0.5299392 
# Adjusted Within R2          Within Pseudo R2 Adjusted Within Pseudo R2 
# 0.5246576                 0.2209019                 0.2150494 
data %>%
  mutate(fitted=fitted(regfix),
         resid=y - fitted,
         ymeandiff=y - mean(y)) %>%
  summarise(tss=sum(ymeandiff^2),
            rss=sum(resid^2)) %>%
  mutate(ess=tss - rss,
         r2=ess / tss) # this is what fixest reports as the R2 from the model, plm does not report
# tss      rss      ess        r2
# 1 331.7968 83.90818 247.8886 0.7471097  



regpool <- feols(y ~ x, data = data)
summary(regpool)
# Adj. R2: 0.329955
r2(regpool, type = "all", full_names = TRUE)  # gives everything
# Squared Correlation                        R2               Adjusted R2                 Pseudo R2        Adjusted Pseudo R2                 Within R2 
# 0.33672330                0.33672330                0.32995517                0.10169425                0.09674036                        NA 
# Adjusted Within R2          Within Pseudo R2 Adjusted Within Pseudo R2 


#Let's compute the squared residuals of this regression
SSR <- sum(residuals(reg)^2)

#let's compute the total squares of the ys
SS0 <- sum((y-mean(y))^2)
SS0 #Note that this is not the TSS given by plm

#Now, let's demean y and x for each individual separately
y.dem <- y-tapply(y, id, mean)[id]
x.dem <- x-tapply(x, id, mean)[id]

#and regress them
#note that we do not estimate the intercept because we have demeaned the
data
reg.fe <- lm(y.dem ~ -1 + x.dem)
summary(reg.fe)
#The coefficient is correct, i.e., the same as in plm
#Note that the standard error is wrong, however. We would need to account for
#that we are losing degrees of freedom by taking out the fixed effects.


#now let's look at the sum of squares after demeaning y
SSR.y.dem <- sum((y.dem-mean(y.dem))^2)
SSR.y.dem #Note, this is the Total sum of squares given by plm

#Now, we know that the total sum of squares
#not accounting for fixed effects is

# TSS = SS0 = 331.7986

#However, we know that after taking out the fixed effects (demeaning y)
# the total sum of squares is
# SSR.y.dem=178.5050

#The within R-squared is then the variance explained by x AFTER having taken out the fixed effects
# So the R-squared computable from the plm output is in fact the within R-squared
cat("Within R-squared: ", 1-SSR/SSR.y.dem)

#which is identical to the r-squared in our hand-computed FE regression
summary(reg.fe)$r.squared


#The two other R-squareds Stata would give you are:

#The overall r-squared
#which is the r-squared of a pooled OLS of y on x WITHOUT accounting for the
fixed effects

#Pooled OLS
reg1 <- lm(y ~ x)
summary(reg1)

#This is what Stata shows as overall R-squared
summary(reg1)$r.squared

#The second R-squared Stata shows is the between R-squared
# which is the R-squared of regressing the mean of the individual y(i)
# on the mean(s) of the individual X(i)

#Get the means of y and x for each individual
y.means <- tapply(y, id, mean)[id]
x.means <- tapply(x, id, mean)[id]

#Regress them on each other
reg2 <- lm(y.means ~ x.means)

#This is what Stata shows as between R-squared
summary(reg2)$r.squared


# So you see that the R-squared computable from the plm output is indeed the
# within R-squared.
# 
# For comparison, look at the Stata output:
#   
#   Fixed-effects (within) regression               Number of obs      =      
#   100
# Group variable: id                              Number of groups   =       
#   10
# 
# R-sq:  within  = 0.5299                         Obs per group: min =       
#   10
# between = 0.1744                                        avg =     
#   10.0
# overall = 0.3367                                        max =       
#   10
# 
# F(1,89)            =   
#   100.34
# corr(u_i, Xb)  = 0.0547                         Prob > F           =   
#   0.0000
# 
# ------------------------------------------------------------------------------
#   y |      Coef.   Std. Err.      t    P>|t|     [95% Conf.
#                                                   Interval]
# -------------+----------------------------------------------------------------
#   x |   1.111187   .1109319    10.02   0.000     .8907682   
# 1.331607
# _cons |   .3155321   .0978457     3.22   0.002     .1211147   
# .5099495
# -------------+----------------------------------------------------------------
#   sigma_u |  1.2318621
# sigma_e |  .97097297
# rho |  .61679513   (fraction of variance due to u_i)
# ------------------------------------------------------------------------------
#   F test that all u_i=0:     F(9, 89) =    16.05               Prob > F =
#   0.0000
# 
# HTH,
# Daniel

sigma_u <- 1.2318621
sigma_e <- .97097297
var <- (sigma_u^2 + sigma_e^2)
rho <- sigma_u^2 / var
rho  # .61679513

summary(femod)
summary(mod_fsl)
summary(camod_cluster)
summary(camod_educ)
