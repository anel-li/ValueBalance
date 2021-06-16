* Encoding: UTF-8.

******* PREPARATION OF ST-SE-VB AND ST-SE-VB-adj to accompany Wayment, Linek, & Tatrai (2021)  .

* Details of the study are registered at https://osf.io/x6zmk/ .

* This syntax produces ST-SE-VB and ST-SE-VB-adj only.

* Analyses can be reprduced using R files: 
*   BalanceEndorsement_00_dataset preparation.R
*   BalanceEndorsement_01_dataset setup.R
*   BalanceEndorsement_02_data analysis.R .
 

*PVQ 21 item variables .
fre  ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun .

* Checking Straightliners in PVQ-21 .
count i21_1 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (1) .
count i21_2 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (2) .
count i21_3 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (3) .
count i21_4 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (4) .
count i21_5 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (5) .
count i21_6 = ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (6) .


* counting the maximum number of times using the same value .
compute i21_16max = max(i21_1,i21_2,i21_3,i21_4,i21_5,i21_6) .

*  Checking for number of missings within the PVQ21 items, as described by Schwartz .
* cases to drop due  to missing data entries in more than 5 items (NA-s + invalid values (7,8,9)) .
compute nmi789 = nmiss(ipcrtiv to impfun).

*  dropping cases due to straightlining definition (more than 16) or mor than 5 NA-s + invalid values (7,8,9) . 
sel if (i21_16max < 16 and nmi789 <6 ) .


*  Reverse Score PVQ-21 items : higher values now mean stronger endorsement of value.
recode ipcrtiv imprich ipeqopt ipshabt impsafe impdiff ipfrule ipudrst ipmodst ipgdtim impfree iphlppl 
    ipsuces ipstrgv ipadvnt ipbhprp iprspot iplylfr impenv imptrad impfun (1=6) (2=5) (3=4) 
    (4=3) (5=2) (6=1). 

compute u1 = ipeqopt .
compute u2 = ipudrst .
compute u3 = impenv  .
compute p1 = imprich .
compute p2 = iprspot .
compute a1 = ipshabt .
compute a2 = ipsuces .
compute b1 = iphlppl .
compute b2 = iplylfr .

EXECUTE.


* Create new distance-based balance measure: 
* calculating the balanced endorsement based on the length of line through 2 alternating sequences of ST-SE-items
* (Variable called DBS9 within the code, referred as ST-SE-VB in the corresponding paper)
* for higher robustness two different orders of alternating items are taken into account (order A, order B):
* Order A .

compute bl2a = 0. 
compute bl2a = bl2a + (1**2+ (p1-u1)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (u2-p1)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (p2-u2)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (u3-p2)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (a1-u3)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (b1-a1)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (a2-b1)**2 )**0.5 .
compute bl2a = bl2a + (1**2+ (b2-a2)**2 )**0.5 .
fre bl2a /stat=all /format=notable.

* transforming it to 0-1 scale .
compute bl2a = 1- (bl2a - 8) / (8*26**0.5-8) .


* order B:  .
compute bl2b = 0. 
compute bl2b = bl2b + (1**2+ (a1-u3)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (u2-a1)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (a2-u2)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (u1-a2)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (p1-u1)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (b1-p1)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (p2-b1)**2 )**0.5 .
compute bl2b = bl2b + (1**2+ (b2-p2)**2 )**0.5 .
fre bl2b /stat=all /format=notable.

* transforming it to 0-1 scale .
compute bl2b = 1- (bl2b - 8) / (8*26**0.5-8) .

* taking a mean of Order A and Order B length-values
* STSEVB: length-related measure based on 5 ST items and 4 SE items .
compute STSEVB = mean(bl2a,bl2b) .

*******************************************************************
*  Adjustment of ST-SE-VB to account for SE>ST response tendencies
*  We create residual scores to serve as ADJUSTED ST-SE-VB score
*******************************************************************

****** STSEVB -> STSEVB-adj .
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT STSEVB
  /METHOD=ENTER STdim
  /SAVE RESID.

ren var (res_1 = STSEVBadj) .
******* STSEVBadj refers to ST-SE-VB-adj  in the paper. 

***************************************** 
* Create PVQ-21-Related Measures
*****************************************
* Schwartz 10 universal values 
* Our analyses focus on self-enhancement (SEN) and Self-transcendence (STR)

* universalism and benevolence .
compute UNI = mean(ipeqopt,ipudrst, impenv) .
compute BEN = mean(iphlppl,iplylfr) .

* self-transcendence .
compute STR = mean(UNI,BEN) .

* achievement and power .
compute ACH = mean(ipshabt,ipsuces) .
compute POW = mean(imprich,iprspot) .

* self-enhancement .
compute SEN = mean(ACH,POW) .

* calculating the SCHWARTZs original dimensions score (called ST dim in the corresponding paper).
compute STdim = STR-SEN .
fre STdim / format=notable /stat=all.




