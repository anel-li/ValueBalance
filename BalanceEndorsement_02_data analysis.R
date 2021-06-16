                    ##########################################
                    # Part III: DATA ANALYSIS  (Steps 9-13) 
                    #########################################

######################################XXXXX####
# (Step 9)  SETTING UP DATA FOR FINAL ANALYSIS
###############################################
#installing packages
# install.packages("jtools")
# install.packages("Matrix")
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("survey")
# install.packages("summarytools")
# install.packages("huxtable")                    
# loading packages
library(tidyverse)
library(psych)
library(jtools)
library(survey)
library(Matrix)
library(summarytools)
library(DataExplorer)
                    

rm(list = ls())             # used to delete objects from the memory/global environment. Will reload needed dataset on line 24
setwd(".")                  # setting the working directory, the directory, where the R project is saved and dataset "E_raw.Rdata" was saved to
#getwd()

# loading dataset
load("E_completeCases.Rdata")  # alternatively loading "E_prepared.Rdata", which is cleaned and prepared but including all available cases, not only complete cases
#load("E_prepared.Rdata")      # in this data set there is no res variable included, so this variable should be deleted from the analysis in the case of using this data set

##########################################
# (Step 10)  DESCRIPTIVE ANALYSES
##########################################
# (10.1) Check Product Moments of All Variables
# defining a vector of variables in order to compute the descriptive summaries for the selected vars
vars <- c("healthR", "swb", "soctrst", "trstprl", "DBS9", "SDSo", "STR", "SEN", "gndr", "agea", "eduyrs", "rlgdgr","vote1", "essround")   
nE <- E[vars] 
ov <- descr(nE, style = 'rmarkdown')
print(ov, digits = 3)
freq(nE, plain.ascii = FALSE, style = "rmarkdown")

#first correlation matrix to gain insight 
library(DataExplorer)
plot_correlation(
  nE,
  ggtheme = theme_minimal(),
  cor_args = list("use" = "pairwise.complete.obs")
)

# (10.2) Histograms
hist(E$healthR)
hist(E$swb)
hist(E$soctrst)
hist(E$trstprl)
hist(E$SDSo)
hist(E$DBS9)
hist(E$SEN)
hist(E$STR)

### (10.2.1) Based on our observations, we want to adjust for response tendencies
### We examined the zero correlations between ST dim and ST-SE-VB and then partial 
### correlations to examine how ST-SE-VB relates to  ST and SE after holding the ST dim constan

# computing partial correlations controlling for SEN and STR
partial.r(data=E,x=c("DBS9","SDSo"),y="SEN")
partial.r(data=E,x=c("DBS9","SDSo"),y="STR")

### Results indicate ST-SE-VB would better reflect "balance" (without regard for level of ST or SE
### endorsement) if controlling for ST dim
## We create this adjusted Value Balance Scale

r <- lm(DBS9 ~ SDSo,data=E)
summary(r$residuals)

# are residuals normally distributed?
hist(r$residuals)
qqnorm(r$residuals)

# storing the residuals in a variable for an ADJUSTED VALUE BALANCE MEASURE
E$res <- r$residuals
summary(E$res)

# (10.3) Relationships Among Study Variables

# Aiming to detect relevant relations within our data set we calculated correlations based on unweighted 
# as well as weighted data

# (10.3.1) Unweighted correlations of all study variables
cu <- cor(E[c("essround", "rlgdgr", "eduyrs" , "agea" , "SDSo", "DBS9", "res", "healthR", "swb" , "soctrst" , "trstprl")], method="pearson", use = "complete.obs")
ccu <- as.data.frame(cu)
ccu <- round(ccu, digits = 3)
ccu
#write.csv2(ccu, "correlation_all_vars_unweighted_Pearson.csv")


# (10.3.2) defining the design for the weighted analysis
# design is called ESS design, with fweight taking taking the variables pspwght and pweight from the ESS data into account
ESSdesign <- svydesign(~1, weights = ~fweight, data = E)

# Weighted correlations of all study variables
cw <-
  svycor(
    ~ essround + rlgdgr + eduyrs + agea + SDSo + DBS9 + res + healthR + swb + soctrst + trstprl,
    design = ESSdesign,
    digits = 3,
    na.rm = T
  )
ccw <- as.data.frame(cw$cors)
ccw <- round(ccw, digits = 3)
ccw
#write.csv2(ccw, "correlation_all_vars_weighted_Pearson.csv")


##### (10.3.3) Examination of categorical variables

chisq.test(svytable(~vote1+gndr, ESSdesign))

svyttest(agea   ~gndr,ESSdesign)
svyttest(eduyrs ~gndr,ESSdesign)
svyttest(rlgdgr ~gndr,ESSdesign)
svyttest(SDSo   ~gndr,ESSdesign)
svyttest(DBS9   ~gndr,ESSdesign)
svyttest(res    ~gndr,ESSdesign)
svyttest(healthR~gndr,ESSdesign)
svyttest(swb    ~gndr,ESSdesign)
svyttest(soctrst~gndr,ESSdesign)
svyttest(trstprl~gndr,ESSdesign)


svyttest(agea   ~vote1,ESSdesign)
svyttest(eduyrs ~vote1,ESSdesign)
svyttest(rlgdgr ~vote1,ESSdesign)
svyttest(SDSo   ~vote1,ESSdesign)
svyttest(DBS9   ~vote1,ESSdesign)
svyttest(res    ~vote1,ESSdesign)
svyttest(healthR~vote1,ESSdesign)
svyttest(swb    ~vote1,ESSdesign)
svyttest(soctrst~vote1,ESSdesign)
svyttest(trstprl~vote1,ESSdesign)


##########################################
# (Step 11)  HYPOTHESIS TESTING
##########################################
# Regression models controlling for sex, age, education, religiosity, ESS round, country 
# Main hypotheses: Is value balance positively associated with SWB, self-rated health,
# social trust, political trust, and voting behavior?
# We conduct separate analyses for our ADJUSTED ST-SE-VB score, the original ST-SE-VB
# score which compensates for ST>SE rating patterns, and the ST dim score which captures the
# ST>SE rating pattern

# description of different regression models, that were run for all the exploratory variables
# model 1: demographic variables only
# model 2: demographic and adjusted ST-SE-VB
# model 3: demographic and original ST-SE-VB
# model 4: demographic and ST dim

### (11.1) healtR
h1 <- svyglm(healthR ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
h2 <- svyglm(healthR ~ res  + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
h3 <- svyglm(healthR ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
h4 <- svyglm(healthR ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)

#the results from the regression models, including model info and model fit
summ(h1, confint = TRUE, digits = 3)
summ(h2, confint = TRUE, digits = 3)
summ(h3, confint = TRUE, digits = 3)
summ(h4, confint = TRUE, digits = 3)

# calculationg the WALD statistics for the regression models
regTermTest(h2,"res")
regTermTest(h3,"DBS9")
regTermTest(h4,"SDSo")

# comparing the regression models, this works best with R markdown 
export_summs(h1, h2, h3, h4, model.names = c("demographics", "res", "DBS9", "SDSo"),
  scale = TRUE, error_format = "[{conf.low}, {conf.high}], p = {p.value}")


### (11.2) SWB
w1 <- svyglm(swb ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
w2 <- svyglm(swb ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
w3 <- svyglm(swb ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
w4 <- svyglm(swb ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)

#the results from the regression models, including model info and model fit
summ(w1, confint = TRUE, digits = 3)
summ(w2, confint = TRUE, digits = 3)
summ(w3, confint = TRUE, digits = 3)
summ(w4, confint = TRUE, digits = 3)

# calculationg the WALD statistics for the regression models
regTermTest(w2,"res")
regTermTest(w3,"DBS9")
regTermTest(w4,"SDSo")

# comparing the regression models, this works best with R markdown 
export_summs(w1, w2, w3, w4, model.names = c("demographics", "res", "DBS9", "SDSo"),
             scale = TRUE, error_format = "[{conf.low}, {conf.high}], p = {p.value}")

### (11.3) SOCIAL TRUST
s1 <- svyglm(soctrst ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
s2 <- svyglm(soctrst ~  res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
s3 <- svyglm(soctrst ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
s4 <- svyglm(soctrst ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)

#the results from the regression models, including model info and model fit
summ(s1, confint = TRUE, digits = 3)
summ(s2, confint = TRUE, digits = 3)
summ(s3, confint = TRUE, digits = 3)
summ(s4, confint = TRUE, digits = 3)

# calculationg the WALD statistics for the regression models
regTermTest(s2,"res")
regTermTest(s3,"DBS9")
regTermTest(s4,"SDSo")

# comparing the regression models, this works best with R markdown 
export_summs(s1, s2, s3, s4, model.names = c("demographics", "res", "DBS9", "SDSo"),
             scale = TRUE, error_format = "[{conf.low}, {conf.high}], p = {p.value}")

### (11.4) POLITICAL TRUST
p1 <- svyglm(trstprl ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
p2 <- svyglm(trstprl ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
p3 <- svyglm(trstprl ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)
p4 <- svyglm(trstprl ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESSdesign)

#the results from the regression models, including model info and model fit
summ(p1, confint = TRUE, digits = 3)
summ(p2, confint = TRUE, digits = 3)
summ(p3, confint = TRUE, digits = 3)
summ(p4, confint = TRUE, digits = 3)

# calculating the WALD statistics for the regression models
regTermTest(p2,"res")
regTermTest(p3,"DBS9")
regTermTest(p4,"SDSo")

# comparing the regression models, this works best with R markdown 
export_summs(p1, p2, p3, p4, model.names = c("demographics", "res", "DBS9", "SDSo"),
             scale = TRUE, error_format = "[{conf.low}, {conf.high}], p = {p.value}")

### (11.5) VOTING
v1 <- svyglm(vote1 ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESSdesign, maxit=100)
v2 <- svyglm(vote1 ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESSdesign, maxit=100)
v3 <- svyglm(vote1 ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESSdesign, maxit=100)
v4 <- svyglm(vote1 ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESSdesign, maxit=100)

#the results from the regression models, including model info and model fit
summ(v1, confint = TRUE, digits = 3)
summ(v2, confint = TRUE, digits = 3)
summ(v3, confint = TRUE, digits = 3)
summ(v4, confint = TRUE, digits = 3)

# calculating the WALD statistics for the regression models
regTermTest(v2,"res")
regTermTest(v3,"DBS9")
regTermTest(v4,"SDSo")

# comparing the regression models, this works best with R markdown 
export_summs(v1, v2, v3, v4, model.names = c("demographics", "res", "DBS9", "SDSo"),
             scale = TRUE, error_format = "[{conf.low}, {conf.high}], p = {p.value}")


#####################################################
# (Step 12)  Extra Analyses: ESS Round 9 Data
#####################################################

# We replicated our hypothesis testing in Round 9
# We sought to examine whether controlling for strata and cluster altered results
# Round 9 data included 24 countries

# (12.1 Regressions using weighted data)
E9 <- E %>% filter(essround==9)

# defining the design for weights
ESS9design <- svydesign(~1, weights = ~fweight, data = E9)
# defining the design for complex data analysis
ESS9complex <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight,
                         data = E9, nest=T)

# 1 Stratum with only 1 PSU was removed
options(survey.lonely.psu="remove")

#weighted analysis
h1w <- svyglm(healthR ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
h2w <- svyglm(healthR ~ res  + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
h3w <- svyglm(healthR ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
h4w <- svyglm(healthR ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)

summ(h1w, confint = TRUE, digits = 3)
summ(h2w, confint = TRUE, digits = 3)
summ(h3w, confint = TRUE, digits = 3)
summ(h4w, confint = TRUE, digits = 3)

w1w <- svyglm(swb ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
w2w <- svyglm(swb ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
w3w <- svyglm(swb ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
w4w <- svyglm(swb ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)

summ(w1w, confint = TRUE, digits = 3)
summ(w2w, confint = TRUE, digits = 3)
summ(w3w, confint = TRUE, digits = 3)
summ(w4w, confint = TRUE, digits = 3)

s1w <- svyglm(soctrst ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
s2w <- svyglm(soctrst ~  res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
s3w <- svyglm(soctrst ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
s4w <- svyglm(soctrst ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)

summ(s1w, confint = TRUE, digits = 3)
summ(s2w, confint = TRUE, digits = 3)
summ(s3w, confint = TRUE, digits = 3)
summ(s4w, confint = TRUE, digits = 3)

p1w <- svyglm(trstprl ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
p2w <- svyglm(trstprl ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
p3w <- svyglm(trstprl ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)
p4w <- svyglm(trstprl ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9design)

summ(p1w, confint = TRUE, digits = 3)
summ(p2w, confint = TRUE, digits = 3)
summ(p3w, confint = TRUE, digits = 3)
summ(p4w, confint = TRUE, digits = 3)

v1w <- svyglm(vote1 ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESS9design, maxit=100)
v2w <- svyglm(vote1 ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESS9design, maxit=100)
v3w <- svyglm(vote1 ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESS9design, maxit=100)
v4w <- svyglm(vote1 ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESS9design, maxit=100)

summ(v1w, confint = TRUE, digits = 3)
summ(v2w, confint = TRUE, digits = 3)
summ(v3w, confint = TRUE, digits = 3)
summ(v4w, confint = TRUE, digits = 3)

# (12.2 Regressions using complex design analysis)
h1c <- svyglm(healthR ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
h2c <- svyglm(healthR ~ res  + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
h3c <- svyglm(healthR ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
h4c <- svyglm(healthR ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)

summ(h1c, confint = TRUE, digits = 3)
summ(h2c, confint = TRUE, digits = 3)
summ(h3c, confint = TRUE, digits = 3)
summ(h4c, confint = TRUE, digits = 3)

w1c <- svyglm(swb ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
w2c <- svyglm(swb ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
w3c <- svyglm(swb ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
w4c <- svyglm(swb ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)

summ(w1c, confint = TRUE, digits = 3)
summ(w2c, confint = TRUE, digits = 3)
summ(w3c, confint = TRUE, digits = 3)
summ(w4c, confint = TRUE, digits = 3)

s1c <- svyglm(soctrst ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
s2c <- svyglm(soctrst ~  res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
s3c <- svyglm(soctrst ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
s4c <- svyglm(soctrst ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)

summ(s1c, confint = TRUE, digits = 3)
summ(s2c, confint = TRUE, digits = 3)
summ(s3c, confint = TRUE, digits = 3)
summ(s4c, confint = TRUE, digits = 3)

p1c <- svyglm(trstprl ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
p2c <- svyglm(trstprl ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
p3c <- svyglm(trstprl ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)
p4c <- svyglm(trstprl ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), ESS9complex)

summ(p1c, confint = TRUE, digits = 3)
summ(p2c, confint = TRUE, digits = 3)
summ(p3c, confint = TRUE, digits = 3)
summ(p4c, confint = TRUE, digits = 3)

v1c <- svyglm(vote1 ~ gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESS9complex, maxit=100)
v2c <- svyglm(vote1 ~ res + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial,  design=ESS9complex, maxit=100)
v3c <- svyglm(vote1 ~ DBS9 + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESS9complex, maxit=100)
v4c <- svyglm(vote1 ~ SDSo + gndr + agea + eduyrs + rlgdgr + essround + factor(cntry), family=quasibinomial, design=ESS9complex, maxit=100)

summ(v1c, confint = TRUE, digits = 3)
summ(v2c, confint = TRUE, digits = 3)
summ(v3c, confint = TRUE, digits = 3)
summ(v4c, confint = TRUE, digits = 3)

##########################################
# (Step 13)  Extra Analysis: Country Level
##########################################

# the following analyses were used to see results on country level
# defining the design for weights
library(srvyr)
ESSdesign_svyr <- as_survey_design(.data=E, weights=fweight)


# calculating the mean of self-enhancement scores across countries
Qsen <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanSEN = mean(SEN,na.rm=T))
Qsen

# calculating the mean of self-transcendence scores across countries
Qstr <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanSTR = mean(STR,na.rm=T))
Qstr

# calculating the mean of ST-SE-VB (original) scores across countries
Qdbs <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanDBS9 = mean(DBS9,na.rm=T))
Qdbs

# calculating the mean of ST-SE-VB (adjusted) scores across countries
Qres <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanRES = mean(res,na.rm=T))
Qres

# calculating the mean of ST dim across countries
Qsds <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanSDSo = mean(SDSo,na.rm=T))
Qsds

# calculating the mean of health across countries
Qh <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanHEALTH = mean(healthR,na.rm=T))
Qh

# calculating the mean of swb across countries
Qw <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanSWB = mean(swb,na.rm=T))
Qw

# calculating the mean of social trust across countries
Qs <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanSOCTRST = mean(soctrst,na.rm=T))
Qs

# calculating the mean of political trust across countries
Qp <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanTRSTPRL = mean(trstprl,na.rm=T))
Qp

# calculating the mean of voting behavior across countries
Qv <- ESSdesign_svyr %>% group_by(cntry) %>% summarise(meanVOTE = mean(vote1,na.rm=T))
Qv


# the following results table was used to visualize the results on country level per year (maps)
E$year <- E$essround*2+2000

FORMAPS <- ESSdesign_svyr %>% group_by(cntry,year) %>%
  summarize(STSEVB = survey_mean(DBS9),
            STSEVBadj = survey_mean(res),
            ST = survey_mean(STR),
            SE = survey_mean(SEN),
            STdim =survey_mean(SDSo),
            health = survey_mean(healthR),
            swb = survey_mean(swb),
            soctrst =survey_mean(soctrst),
            trstprl = survey_mean(trstprl),
            vote = survey_mean(vote1))

#write.csv2(FORMAPS, "country level results_weighted.csv")
