library(psych)

#RELIABILTIY CHECKS - Cronbachs Alpha
# C stands for country
# r stands for round


# OPENNESS TO CHANGE
caOTC <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caOTC) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10otc])$total$raw_alpha,2)
    caOTC <- rbind(caOTC,c(CNTRYlistv1[C],r,a))}  
}  
caOTC <- caOTC[is.na(caOTC$cntry)==F,]
caOTC <- reshape(data=caOTC,timevar =  "round",idvar=c("cntry"),direction="wide")

#setwd("C:/Panni/IPSDS/_BalanceThesis/Balance/data/plots")
setwd("C:/Users/elli/Documents/R/Rprojects/ValBalance/data/plots")
write.table(caOTC,"caOTC.txt",row.names = F)



# CONSERVATION
caCON <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caCON) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10con])$total$raw_alpha,2)
    caCON <- rbind(caCON,c(CNTRYlistv1[C],r,a))}  
}  
caCON <- caCON[is.na(caCON$cntry)==F,]
caCON <- reshape(data=caCON,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caCON,"caCON.txt",row.names = F)



# SELF-ENHANCEMENT
caSEN <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caSEN) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10sen])$total$raw_alpha,2)
    caSEN <- rbind(caSEN,c(CNTRYlistv1[C],r,a))}  
}  
caSEN <- caSEN[is.na(caSEN$cntry)==F,]
caSEN <- reshape(data=caSEN,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caSEN,"caSEN.txt",row.names = F)



# SELF-TRANSCENDENCE
caSTR <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caSTR) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10str])$total$raw_alpha,2)
    caSTR <- rbind(caSTR,c(CNTRYlistv1[C],r,a))}  
}  
caSTR <- caSTR[is.na(caSTR$cntry)==F,]
caSTR <- reshape(data=caSTR,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caSTR,"caSTR.txt",row.names = F)



# SOCIAL
caSOC <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caSOC) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10soc])$total$raw_alpha,2)
    caSOC <- rbind(caSOC,c(CNTRYlistv1[C],r,a))}  
}  
caSOC <- caSOC[is.na(caSOC$cntry)==F,]
caSOC <- reshape(data=caSOC,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caSOC,"caSOC.txt",row.names = F)



# PERSONAL
caPER <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caPER) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10per])$total$raw_alpha,2)
    caPER <- rbind(caPER,c(CNTRYlistv1[C],r,a))}  
}  
caPER <- caPER[is.na(caPER$cntry)==F,]
caPER <- reshape(data=caPER,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caPER,"caPER.txt",row.names = F)



# GROWTH
caGRO <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caGRO) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10gro])$total$raw_alpha,2)
    caGRO <- rbind(caGRO,c(CNTRYlistv1[C],r,a))}  
}  
caGRO <- caGRO[is.na(caGRO$cntry)==F,]
caGRO <- reshape(data=caGRO,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caGRO,"caGRO.txt",row.names = F)



# SELF-PROTECTION
caSPR <- as.data.frame(matrix(nrow=1,ncol=3))
colnames(caSPR) <- c("cntry","round","ca")

for (C in 1:15) {
  for (r in 1:9) {
    a <- round(alpha(E[E$cntry==CNTRYlistv1[C]&E$essround==r,i10spr])$total$raw_alpha,2)
    caSPR <- rbind(caSPR,c(CNTRYlistv1[C],r,a))}  
}  
caSPR <- caSPR[is.na(caSPR$cntry)==F,]
caSPR <- reshape(data=caSPR,timevar =  "round",idvar=c("cntry"),direction="wide")
write.table(caSPR,"caSPR.txt",row.names = F)



#___________
#tables for appendix
#Load the libraries
library(formattable)
library(dplyr)
library(tidyr)
#library(flextable)

formattable(caOTC, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caCON, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caSEN, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caSTR, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caSOC, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caPER, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caGRO, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))

formattable(caSPR, 
            align =c("l","c","c","c","c","c","c","c","c","c"), 
            list(`cntry` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")), 
              area(col = 2:10) ~ color_tile("#ff7f7f", "#FFFF00")))









