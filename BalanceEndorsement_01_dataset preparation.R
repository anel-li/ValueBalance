                          #########################################
                            # Part II: DATA PREPARATION  (Steps 3-8) 
                          #########################################
                          
# download relevant packages
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("survey")
# install.packages("summarytools")
# install.packages ("foreign")
# install.packages ("DataExplorer")                          
# loading relevant packages
library(tidyverse) 
library(psych)
library(survey)
library(summarytools)
library(DataExplorer)
                          
rm(list = ls())             # used to delete objects from the memory/global environment. Will reload needed dataset on line 18
#setwd("..")                # leads to the parent directory, setting the working directory, the directory, where the R project is saved and dataset "E_raw.Rdata" was saved to
setwd(".")                  # setting the working directory, the directory, where the R project is saved and dataset "E_raw.Rdata" was saved to
                          
# loading dataset
load("E_raw.Rdata")                          
                        
##########################################
# (Step 3)  Select Countries for Analysis
##########################################

# selecting relevant countries, we decided to include all European countries, that participated at least in 5 rounds of the ESS
CNTRYlistv2 <- c("AT","BE","BG","CH","CY","CZ","DE","DK","EE","ES","FI","FR","GB","HU","IE","LT","NL","NO", "PL","PT","SE","SI","SK","UA")

### We keep countries having at least 5 rounds
E <- E[E$cntry %in% CNTRYlistv2,]

# Country variables refer to the following countries: 
# AT - Austria, BE - Belgium, BG - Bulgaria, CH - Switzerland, CY - Cyprus, CZ - Czechia,  
# DE - Germany, DK - Denmark, EE - Estonia, ES - Spain, FI - Finland, FR - France, GB - United Kingdom, 
# HU - Hungary, IE - Ireland, IT - Italy, LT - Lithuania, NL - Netherlands, NO - Norway, 
# PL - Poland, PT - Portugal, SE - Sweden, SI - Slovenia, SK - Slovakia, UA -Ukraine

table(E$cntry,E$essround) # Number of cases before cleaning 

#PVQ 21 item variables, saved as a vector:
i21 <- c("ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff", 
         "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl", 
         "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
         "impenv", "imptrad", "impfun")

# table of sample sizes per country per round (raw)

##########################################
# (Step 4) Clean Raw Data File E.Rdata
##########################################
# (4.1) Replacing values

# setting no answer, refusal and don't know to NA
iN <- c("vote", "health", "gndr")
E <- E %>% mutate_at(all_of(iN), ~replace(.,.==7|.==8|.==9,NA))

iNN <- c("ppltrst", "pplfair", "pplhlp", "trstprl", "stflife", "stfeco", 
         "stfgov", "stfdem", "happy", "rlgdgr", "hhmmb", "eisced", "eduyrs")
E <- E %>% mutate_at(all_of(iNN), ~replace(.,.==77|.==88|.==99,NA))

iNNN <- c("agea")
E <- E %>% mutate_at(all_of(iNNN), ~replace(.,.==777|.==888|.==999,NA))

iNNNN <- c("yrbrn")
E <- E %>% mutate_at(all_of(iNNNN), ~replace(.,.==7777|.==8888|.==9999,NA))

# (4.2) Schwartz PVQ-21
# (4.2.1) Checking NAs in PVQ-21

# computing the number of NA-s for every respondent in the 21 items
E$nmi <- rowSums(is.na(E[i21]))

# PVQ21 items: recoding values 7 8 9 to NA
E <- E %>% mutate_at(all_of(i21), ~replace(.,.==7|.==8|.==9,NA))

# computing the number of NA-s + invalid values (7,8,9) for every respondent in the 21 items
E$nmi789 <- rowSums(is.na(E[i21]))
table(E$nmi789,useNA = "always")     #prints out the list across the 21 PVQ items

# (4.2.2) Checking Straightliners in PVQ-21

countcases <- function(x, n) { rowSums(x == n) }
E$i21_1 <- countcases(E[i21],1)
E$i21_2 <- countcases(E[i21],2)
E$i21_3 <- countcases(E[i21],3)
E$i21_4 <- countcases(E[i21],4)
E$i21_5 <- countcases(E[i21],5)
E$i21_6 <- countcases(E[i21],6)

# counting the maximum number of times using the same value
E$i21_16max <- pmax(E$i21_1,E$i21_2,E$i21_3,E$i21_4,E$i21_5,E$i21_6,na.rm=T)
table(E$i21_16max)                   #prints out the list across the 21 PVQ items

# flagging cases, that will be dropped due to straightlining definition (more than 16)
E$dropS <- 0
E$dropS[E$i21_16max >16 ] <- 1
table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround)
round(table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround) / table(E$cntry,E$essround),3)   #prints out table of country by round

# (4.2.3) Checking for number of missings within the PVQ21 items, as described by Schwartz
# flagging cases, that will be dropped due due to missing data entries in more than 5 items (NA-s + invalid values (7,8,9))
E$dropM <- 0
E$dropM[E$nmi789>5] <- 1
table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround)
round(table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround) / table(E$cntry,E$essround),3)    #prints out table of country by round

# table with the number of cases per country per round
table(E[E$dropS==0&E$dropM==0,]$cntry,E[E$dropS==0&E$dropM==0,]$essround) 

# Rates per country per ESSround for straightliners and >5_missings 
round(table(E[E$dropS==1,]$cntry,E[E$dropS==1,]$essround) / table(E$cntry,E$essround),3) +
round(table(E[E$dropM==1,]$cntry,E[E$dropM==1,]$essround) / table(E$cntry,E$essround),3)

# setting straightliner + item nonresponse cases to NA
E[E$dropS==1|E$dropM==1,i21] <- NA

# (4.3) Initial Check of Product Moments of All Variables
# defining a vector of variables in order to compute the descriptive summaries for the selected vars
vars <- c("health", "happy", "stflife", "ppltrst","pplfair", "pplhlp", "trstprl", "gndr", "agea", "eduyrs", "rlgdgr", "vote", "essround")   
nE <- E[vars] 
ov <- descr(nE, style = 'rmarkdown')
print(ov, digits = 3)                                 #prints out a table of product moments
freq(nE, plain.ascii = FALSE, style = "rmarkdown")    #frequencies for variables in relevant subset nE

# (4.3.1) Correcting problems with variable
E$eduyrs[E$eduyrs>25] <- 25
table(E$eduyrs,useNA="always")       #check changes

##########################################
# (Step 5) Finalize Outcome Measures
##########################################
# (5.1) Reverse Score PVQ-21 items

E[i21] <- 7- E[i21]        # higher values now mean stronger endorsement of value

# (5.2) Self-Rated Health
table(E$health)
E$healthR <- 6 - E$health
table(E$healthR)          # can check that higher values now mean better health rating

# (5.3) Social Trust (soctrst) (average of ppltrst, pplfair, and pplhlp)

# cor(E[c("ppltrst","pplfair", "pplhlp")],method="pearson",use = "complete.obs")
iS <- c("ppltrst","pplfair", "pplhlp")
E$soctrst <- rowMeans(E[iS],na.rm=T)

# (5.4) Subjective Well-Being (SWB) (average of happy and stflife)

# cor(E[c("happy","stflife")],method="pearson",use = "complete.obs")
E$swb <- rowMeans(E[c("happy","stflife")],na.rm=T)

# (5.5) Vote (reverse code and remove non-eligible voters)
table(E$vote,useNA="ifany")
E$vote1 <- NULL
E$vote1 <- ifelse(E$vote==1,1,ifelse(E$vote==2,0,NA))
table(E$vote1,useNA = "always")   #check changes


# (5.6) Check Product Moments of All Variables - AFTER EDITING SEVERAL VARIABLES
# defining a vector of variables in order to compute the descriptive summaries for the selected vars
vars <- c("healthR", "swb", "soctrst", "trstprl", "gndr", "agea", "eduyrs", "rlgdgr","vote1", "essround")   
nE <- E[vars] 
ov <- descr(nE, style = 'rmarkdown')
print(ov, digits = 3)                                  #prints out a table of product moments
freq(nE, plain.ascii = FALSE, style = "rmarkdown")     #frequencies for variables in relevant subset nE

# a colour-coded correlation matrix
library(DataExplorer)
plot_correlation(
  nE,
  ggtheme = theme_minimal(),
  cor_args = list("use" = "pairwise.complete.obs")
)


##########################################
# (Step 6) Check Reliabilities
##########################################

library(psych)

# (6.1) Schwartz values
#i21univ <- c("ipeqopt","ipudrst", "impenv")       # universalism only
#i21ben <- c("iphlppl","iplylfr")                  # bnevolence only
iS <- c("ipeqopt","ipudrst", "impenv", "iphlppl","iplylfr") # all self-transcendence items
caSTR <- round(alpha(E[,iS])$total$raw_alpha,2)
caSTR             #Coefficient alpha for total sample for ST values

#i21ach <- c("ipshabt","ipsuces")                  # achievment only
#i21pow <- c("imprich","iprspot")                  # power only
iS <- c("ipshabt","ipsuces","imprich","iprspot")   # all self-enhancement items
caSEN <- round(alpha(E[,iS])$total$raw_alpha,2)
caSEN                                              #Coefficient alpha for total sample for SE values

# (6.2) Social trust
iS <- c("ppltrst", "pplfair", "pplhlp")
caSOC <- round(alpha(E[,iS])$total$raw_alpha,2)
caSOC                                              #Coefficient alpha for total sample for Social Trust

##########################################
# (Step 7) Create PVQ-21-Related Measures
##########################################
# (7.1) Schwartz 10 universal values

# in order to continue our analysis we calculated the Schwartz value clusters, focusing on 
# Self-enhancement (SEN) and Self-transcendence (STR), which are the two focus areas of our research project. 

# Overview of 21 value variables
# ipcrtiv 'Important to think new ideas and being creative'
# imprich 'Important to be rich, have money and expensive things'
# ipeqopt 'Important that people are treated equally and have equal opportunities'
# ipshabt 'Important to show abilities and be admired'
# impsafe 'Important to live in secure and safe surroundings'
# impdiff 'Important to try new and different things in life'
# ipfrule 'Important to do what is told and follow rules'
# ipudrst 'Important to understand different people'
# ipmodst 'Important to be humble and modest, not draw attention'
# ipgdtim 'Important to have a good time'
# impfree 'Important to make own decisions and be free'
# iphlppl 'Important to help people and care for others well-being'
# ipsuces 'Important to be successful and that people recognize achievements'
# ipstrgv 'Important that government is strong and ensures safety'
# ipadvnt 'Important to seek adventures and have an exiting life'
# ipbhprp 'Important to behave properly'
# iprspot 'Important to get respect from others'
# iplylfr 'Important to be loyal to friends and devote to people close'
# impenv  'Important to care for nature and environment'
# imptrad 'Important to follow traditions and customs'
# impfun  'Important to seek fun and things that give'.


# Calculation of the 10 value (using raw scores):
i21univ <- c("ipeqopt","ipudrst", "impenv")
E$UNI <- rowMeans(E[i21univ],na.rm=T)
i21ben <- c("iphlppl","iplylfr")
E$BEN <- rowMeans(E[i21ben],na.rm=T)
i21ach <- c("ipshabt","ipsuces")
E$ACH <- rowMeans(E[i21ach],na.rm=T)
i21pow <- c("imprich","iprspot")
E$POW <- rowMeans(E[i21pow],na.rm=T)
i21hed <- c("impfun","ipgdtim")
E$HED <- rowMeans(E[i21hed],na.rm=T)
i21sti <- c("impdiff","ipadvnt")
E$STI <- rowMeans(E[i21sti],na.rm=T)
i21sel <- c("ipcrtiv","impfree")
E$SEL <- rowMeans(E[i21sel],na.rm=T)
i21tra <- c("ipmodst","imptrad")
E$TRA <- rowMeans(E[i21tra],na.rm=T)
i21con <- c("ipbhprp","ipfrule")
E$CON <- rowMeans(E[i21con],na.rm=T)
i21sec <- c("impsafe","ipstrgv")
E$SEC <- rowMeans(E[i21sec],na.rm=T)

i10 <- c("UNI", "BEN", "ACH", "POW", "HED", "STI", "SEL", "TRA", "CON", "SEC")

# (7.2) Calculation of the 4 first order value clusters (raw)
# SELF_ENHANCEMENT (SEN)
i10sen <- c("POW", "ACH")
E$SEN <- rowMeans(E[i10sen],na.rm=T)
summary(E$SEN)

# SELF_TRANSCENDENCE (STR)
i10str <- c("UNI", "BEN")
E$STR <- rowMeans(E[i10str],na.rm=T)
summary(E$STR)

# OPENESS TO CHANGE (OTC) (not relevant for the planned analysis, but in terms of completing the circumplex model)
i10otc <- c("SEL", "STI", "HED")
E$OTC <- rowMeans(E[i10otc],na.rm=T)
summary(E$OTC)

# CONSERVATION  (CON) (not relevant for the planned analysis, but in terms of completing the circumplex model)
i10con <- c("SEC", "CON", "TRA")
E$CON <- rowMeans(E[i10con],na.rm=T)
summary(E$CON)

# (7.3) # calculating the SCHWARTZs original dimensions score (called ST dim in the corresponding paper)
E$SDSo <- E$STR-E$SEN
summary(E$SDSo)   #SDSo is referred to as ST dim in paper


# (7.4) Create new distance-based balance measure: 
# calculating the balanced endorsement based on the length of line through 2 alternating sequences of ST-SE-items
# (Variable called DBS9 within the code, referred as ST-SE-VB in the corresponding paper)
# for higher robustness two different orders of alternating items are taken into account (order A, order B):

# Order A
# u1 p1 u2 p2 u3 a1 b1 a2 b2     #shorter names that correspond with Table in ms
u1 <- "ipeqopt"
u2 <- "ipudrst"
u3 <- "impenv"
p1 <- "imprich"
p2 <- "iprspot"
a1 <- "ipshabt"
a2 <- "ipsuces"
b1 <- "iphlppl"
b2 <- "iplylfr"

E$BL2a <- 0
E$BL2a <- sqrt(1^2+(E[u1]-E[p1])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[p1]-E[u2])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[u2]-E[p2])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[p2]-E[u3])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[u3]-E[a1])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[a1]-E[b1])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[b1]-E[a2])^2)
E$BL2a <- E$BL2a + sqrt(1^2+(E[a2]-E[b2])^2)
summary(E$BL2a)

# transforming to 0-1 scale 
E$BL2a <- 1 - (E$BL2a - 8) / (8*sqrt(26)-8)

# Order B
# u3 a1 u2 a2 u1 p1 b1 p2 b2  #shorter names that correspond with Table in ms
E$BL2b <- 0
E$BL2b <- sqrt(1^2+(E[u3]-E[a1])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[a1]-E[u2])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[u2]-E[a2])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[a2]-E[u1])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[u1]-E[p1])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[p1]-E[b1])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[b1]-E[p2])^2)
E$BL2b <- E$BL2b + sqrt(1^2+(E[p2]-E[b2])^2)
summary(E$BL2b)

# transforming to 0-1 scale 
E$BL2b <- 1 - (E$BL2b - 8) / (8*sqrt(26)-8)

# taking a mean of Order A and Order B length-values
# DBS9: length-related measure based on 5 ST items and 4 SE items
E$DBS9 <- rowMeans(E[c("BL2a","BL2b")],na.rm=TRUE)  #adds new balance variable to dataset

# (7.5) Balance Category Scores
# We created 9 categories (3 balanced, 6 imbalanced) (see Table in paper).
# Balance levels - Imbalance levels (BIM)

# apriori score recoding for ST scores
E$STRb[E$STR >= 4.5 & E$STR <= 6]  <- 3 #"high scale entries"    
E$STRb[E$STR >= 2.5 & E$STR < 4.5] <- 2 #"middle scale entries"   
E$STRb[E$STR >= 1   & E$STR < 2.5] <- 1 #"lower scale entries"

# apriori score recoding for SE scores
E$SENb[E$SEN >= 4.5 & E$SEN <= 6]  <- 3 #"high scale entries"
E$SENb[E$SEN >= 2.5 & E$SEN < 4.5] <- 2 #"middle scale entries"
E$SENb[E$SEN >= 1   & E$SEN < 2.5] <- 1 #"low scale entries"

# All possible combinations to created 9 new categories: B = Balance, IB = Imbalance
E$B <- NA
E$B<-ifelse(E$STRb == 3 & (E$SENb == 3), 3,                            #high balance - on both scales end of scale
            ifelse(E$STRb == 2 & (E$SENb == 2), 2,                     #mid balance - on both scales mid of scale
                   ifelse(E$STRb == 1 & (E$SENb == 1), 1, NA)))        #low balance - on both scales start of scale

E$IB <- NA
E$IB<-ifelse(E$STRb == 3 & (E$SENb == 1), 6,                                 #high imbalance STR
             ifelse(E$STRb == 1 & (E$SENb == 3), 1,                              #high imbalance SEN
                    ifelse(E$STRb == 3 & (E$SENb == 2), 5,                         #mid imbalance STR mid
                           ifelse(E$STRb == 2 & (E$SENb == 3), 2,                    #mid imbalance SEN mid
                                  ifelse(E$STRb == 2 & (E$SENb == 1), 4,               #low imbalance  STR low
                                         ifelse(E$STRb == 1 & (E$SENb == 2), 3, NA))))))   #low imbalance SEN low


# storing both into one categorical variable (BIM)
E$BIM <- NA
E$BIM <-
  ifelse(E$STRb == 3 &
           (E$SENb == 3), 9,                        	#high level balance - on both scales end of scale  9
         ifelse(E$STRb == 2 &
                  (E$SENb == 2), 8,                 	#mid level balance - on both scales mid of scale   8
                ifelse(
                  E$STRb == 1 &
                    (E$SENb == 1), 7,               	#low level balance - on both scales start of scale 7
                  ifelse(E$STRb == 2 &
                           (E$SENb == 1), 5,          #low level imbalance  STR 5
                         ifelse(
                           E$STRb == 1 &
                             (E$SENb == 2), 6,        #low level imbalance  SEN 6
                           ifelse(E$STRb == 3 &
                                    (E$SENb == 2), 3,            	        #mid imbalance STR 3
                                  ifelse(
                                    E$STRb == 2 &
                                      (E$SENb == 3), 4,          	        #mid imbalance SEN 4
                                    ifelse(E$STRb == 3 &
                                             (E$SENb == 1), 1,          	#high difference imbalance STR 1
                                           ifelse(E$STRb == 1 &
                                                    (E$SENb == 3), 2, NA))  #high difference imbalance SEN 2
                                  ))
                         ))
                )))    

table(E$BIM,useNA = "always")   #creates list of categories and N
#hist(E$BIM)

# based on the BIM variable we calculated a 0 to 1 dummy variable referring to balanced or imbalance within one
E$B2[E$BIM==7|E$BIM==8|E$BIM==9] <- 1 #balanced
E$B2[E$BIM<7] <- 0 # not balanced

##########################################
# (Step 8) Create Datasets for Analyses
##########################################

# (8.1) Weighted Sample
E$fweight <- E$pspwght * E$pweight

# saving the cleaned data
save(E,file="E_prepared.Rdata")
#write.csv(E,'E_prepared.csv')

library(foreign)
write.foreign(E, "./E_prepared.txt", "./E_prepared.sps", package="SPSS")

# (8.2) Complete Case Sample

#For further analysis we prepared a complete case data set, taking the following code into account
load("E_prepared.Rdata")

### saving only complete cases
E <- E[is.na(E$DBS9)   ==F&is.na(E$SDSo)==F&
         is.na(E$healthR)==F&is.na(E$swb) ==F&is.na(E$soctrst)==F&is.na(E$trstprl)==F&is.na(E$vote1)==F
       ,]

# SDSo -> DBS9 residual
r <- lm(DBS9 ~ SDSo, data=E)
names(r)
summary(r$residuals)    #product moments for new adjusted ST-SE-VB score

save(E,file="E_completeCases.Rdata")
#write.csv(E,'E_completeCases.csv')

library(foreign)
write.foreign(E, "./E_completeCases.txt", "./E_completeCases.sps", package="SPSS")
