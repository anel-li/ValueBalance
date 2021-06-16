
##########################################
# R CODE (aligned with Data Analysis Plan)
##########################################
# Can A Balanced Endorsement of Self-Enhancement and Self-Transcendence Values Be Measured? 
# An Investigation Using European Social Survey Data
# Heidi A. Wayment, Elisabeth Linek, Annamária Tátrai

# Theoretical models outlining the basic tenets of human flourishing argue for the importance of both self- and other-related values.
# One of the primary dimensions of the widely cited and used Schwartz model of universal values situates self-enhancement (SE) 
# and self-transcendence (ST) values in oppositional motivational space. We conducted our analysis as a secondary data analyses based 
# on nine rounds (2002-2018) of the European Social Survey (ESS). We examined the extent to which individuals reported balanced 
# endorsement of ST and SE values, developed a new way to create a measure for value balance, and explored the relationships between  expected outcomes. 
# value balance and hypothesized outcomes: SWB, self-rated health, social trust, political trust, and voting behavior.


                          ######################################
                          # Part I: DATA ASSEMBLY (Steps 1 & 2)
                          ######################################

#########################################
# (STEP 1) Assemble Initial Raw Data Files
##########################################

# Integrated data files for nine ESS rounds are available for downloaded from the ESS Website (https://www.europeansocialsurvey.org/data/round-index.html). 
# (doi:10.21338/NSD-ESS1-2002 through doi:10.21339/NSD=ESS9-2018)

##########################################
# (STEP 2) Create Single Raw Data File
##########################################

# Data from ESS rounds 1-9 were combined to form a single data set called E.Rdata. 
# R package expss used to read in the data and the R package tidyverse, 
# specifically dplyr, was used to bind the single sets to one overall dataset. 
# The variables selected for this dataset are included in attached OSF file (Overview of Study Variables.pdf)
# The data set created is available in the OSF repository (E.Rdata)
#
# installing needed packages
install.packages("expss")
install.packages("tidyverse")

# loading relevant packages
library(expss)  
library(tidyverse)  

setwd(".")                 #current directory, should be the directory, where the R project is saved
getwd                      #check wd
setwd("./original")        #this wd refers to the sub-folder in the directory, where the original ESS files are saved and unzipped

w1 <- expss::read_spss("ESS1e06_6.sav")
w2 <- expss::read_spss("ESS2e03_6.sav")
w3 <- expss::read_spss("ESS3e03_7.sav")
w4 <- expss::read_spss("ESS4e04_5.sav")
w5 <- expss::read_spss("ESS5e03_4.sav")
w6 <- expss::read_spss("ESS6e02_4.sav")
w7 <- expss::read_spss("ESS7e02_2.sav")
w8 <- expss::read_spss("ESS8e02_2.sav")
w9 <- expss::read_spss("ESS9e03.sav")  

# selecting variables, we decided to keep for further analysis, loading the selected ones into one vector
ALLvars <- c("dweight", "pspwght", "pweight", "anweight", "nwspol", "netusoft", "netustm", "ppltrst", "pplfair", "pplhlp", "psppsgva", 
             "actrolga", "psppipla", "cptppola", "trstprl", "trstlgl", "trstplc", "trstplt", "trstprt", "trstep", "trstun", "vote", "lrscale", 
             "stflife", "stfeco", "stfgov", "stfdem", "stfedu", "stfhlth", "gincdif", "freehms", "hmsfmlsh", "hmsacld", "happy", "sclmeet", "inprdsc", 
             "sclact", "health", "hlthhmp", "atchctr", "atcherp", "rlgblg", "rlgdgr", "hhmmb", "gndr", "agea", "yrbrn", "rshpsts", "rshpsgb", 
             "marsts", "marstgb", "chldhhe", "domicil", "edulvlb", "eisced", "edulvlpb", "eiscedp", "crpdwk", "pdjobev", "emplrel", "tporgwk", 
             "hincsrca", "hincfel", "iincsrc", "atncrse", "region", "regunit", "ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", 
             "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl", "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
             "impenv", "imptrad", "impfun", "name", "essround", "edition", "proddate", "idno", "inwtm", "prob", "stratum", "psu", "cntry", "eduyrs")

# checking if the data set from different ESS rounds show any differences regarding the selected set of variables:
setdiff(ALLvars,names(w1))
setdiff(ALLvars,names(w2))
setdiff(ALLvars,names(w3))
setdiff(ALLvars,names(w4))
setdiff(ALLvars,names(w5))
setdiff(ALLvars,names(w6))
setdiff(ALLvars,names(w7))
setdiff(ALLvars,names(w8))
setdiff(ALLvars,names(w9))

# for the selected columns keep all the selected variables:
w1 <- w1[intersect(names(w1),ALLvars)]
w2 <- w2[intersect(names(w2),ALLvars)]
w3 <- w3[intersect(names(w3),ALLvars)]
w4 <- w4[intersect(names(w4),ALLvars)]
w5 <- w5[intersect(names(w5),ALLvars)]
w6 <- w6[intersect(names(w6),ALLvars)]
w7 <- w7[intersect(names(w7),ALLvars)]
w8 <- w8[intersect(names(w8),ALLvars)]
w9 <- w9[intersect(names(w9),ALLvars)]

# in order to support the process of combining the data, we assigned a sufficient memory limit size
# memory.limit(size=999999) 

#creating the combined dataset for the planned analysis, called "E"
E <-  bind_rows(w1,w2,w3,w4,w5,w6,w7,w8,w9)
setwd("..")     #setting the working directory back to be the parent directory, where the following analysis is conducted
setwd(".")
save(E, file = "E_raw.Rdata")
#write.csv(E,'E_raw.csv')   # use this to create csv file
