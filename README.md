# ValueBalance

Project: Can A Balanced Endorsement of Self-Enhancement and Self-Transcendence Values Be Measured? An Investigation Using European Social Survey Data.
Authors: Heidi A. Wayment, Elisabeth Linek, Annamária Tátrai.

Content of OSF (https://osf.io/x6zmk/): 
1.	Data Analysis Plan (explanation of steps included in R code)
2.	Code Files
	a.	BalanceEndorsement_00_dataset preparation.R
	b.	BalanceEndorsement_01_dataset setup.R
	c.	BalanceEndorsement_02_data analysis.R
	d.	BalanceEndorsement_SPSS_syntax.sps
3.	Datasets
	a.	E_raw.Rdata
	b.	E_prepared.Rdata
	c.	E_completeCases.Rdata
	d.	E_raw.sav
	e.	E_prepared.sav
	f.	E_completeCases.sav
4.	Overview of Study Variables 
5.	ESS Documentation of PVQ-21
6.	Folder: Exploratory analyses and visualizations by country

##  Software Packages: -
R version 4.0.5 (2021-03-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Attached base packages: grid, stats, graphics, grDevices, utils, datasets, methods, base 

Other attached packages: plyr_1.8.6, plotly_4.9.3, kableExtra_1.3.4, summarytools_0.9.9, TMB_1.7.20, survey_4.0, survival_3.2-10, Matrix_1.3-3, jtools_2.1.3, psych_2.1.3, forcats_0.5.1, stringr_1.4.0, dplyr_1.0.6, purrr_0.3.4, readr_1.4.0, tidyr_1.1.3, tibble_3.1.1, ggplot2_3.3.3, tidyverse_1.3.1, DataExplorer_0.8.2 


## Instructions to reader:
In order to reproduce the conducted analyses, you have two options:
a)	download the raw ESS data files (NAME OF ZIP) and start the analysis using R file BalanceEndorsement_00_dataset setup.R 
b)	download the prepared Rdata file [completeCases R.data] and start the analysis using R file: BalanceEndorsement_01_dataset preparation.R followed by R file: BalanceEndorsement_02_data analysis.R
 
# Data Analysis Plan
Explanation of Steps Included in R Code Files

R file: BalanceEndorsement_00_dataset setup.R	

## Step 1. Assemble Initial Raw Data Files
Integrated data files for all nine ESS rounds were downloaded from the ESS Website (https://www.europeansocialsurvey.org/data/round-index.html).
(doi:10.21338/NSD-ESS1-2002 through doi:10.21339/NSD=ESS9-2018).

## Step 2. Create Single Raw Data File E.Rdata. 
Data from ESS rounds 1-9 were combined to form a single data set called E.Rdata. R package expss used to read in the data and the R package tidyverse, specifically dplyr, was used to bind the single sets to one overall dataset.R. The package expss used to read in the data and the R package tidyverse, specifically dplyr, was useed to bind the single sets to one overall dataset called E.Rdata. The variables selected for this dataset are included in attached OSF file (Overview of Study Variables.pdf). 

R and SPSS data sets are available in the OSF repository (E_raw.Rdata, E_raw.sav)

R file: BalanceEndorsement_01_dataset preparation.R

## Step 3. Select Countries. Retain data from countries who have ESS data for five or more rounds.

## Step 4. Clean Raw Data File

4.1 Replacing values: For all variables in the dataset, items marked as not applicable (6/66/666), refusal (7/77/777), don’t know (8/88/888) no answer (9/99/999) were set as missing or system missing (NA).

4.2 PVQ-21 specific:
Following Schwartz guidelines (http://essedunet.nsd.uib.no/cms/topics/1/) we removed missing (NA) data, cases who met criteria for straightlining PVQ21 item variables (16+ out of 21 variables with same scale ratings). We also removed cases with 5 or more missing values within the PVQ21 item variables. 

4.3 Initial check of product moments for all study variables. We make adjustments to total years of education variable (eduyrs) due to apparent outliers. 

## Step 5. Finalize Outcome Measures

5.1 Reverse score PVQ-21 items (higher score = stronger endorsement
5.2 Reverse self-rated health item (higher score = better health)
5.3 Create social trust score by averaging three items (ppltrst, pplfair, and pplhlp)
5.4 Create SWB scale by averaging two items (happy and stflife)
5.5 Re-code VOTE variable by setting ineligible to NA, vote = 1, non vote = 0
5.6 Re-check product moments of all variables after this editing process

## Step 6. Check Reliabilities
Computed coefficient alphas for multi-item scales

6.1 Schwartz self-transcendence (ST) and self-enhancement (SE) scores
6.2	Social Trust

## Step 7. Create PVQ-21-Related Measures

7.1	Ten universal values using raw scores
7.2 	Four higher-order value clusters (ST, SE, OC, CO) (these are the anchor values of two primary circumplex model dimensions)
7.3	Original Schwartz ST dim score (subtracting SE from ST)
7.4	Distance-Based Value Balance Measure ST-SE-VB: Original and Adjusted
	We create our original score and also the method to partial out ST > SE response pattern by regressing ST-SE-VB onto ST dim and saving residual score as our adjusted ST-SE-VB score.
7.5	Balance Category Scores

## Step 8. Create Data Sets for Analyses
8.1 Weighted sample (using ESS supplied weights)
R and SPSS data sets are available in the OSF repository (E_prepared.Rdata, E_prepared.sav)

8.2 Complete case sample (reduction in N due to setting ineligible voters to NA). 
R and SPSS data sets are available in the OSF repository (E_completeCase.Rdata, E_completeCase.sav)


R file: BalanceEndorsement_02_data analysis.R

## Step 9. Setting up Data for Final Analysis

Code listed to use the complete case data which includes our adjusted ST-SE-VB variable.
Available code to use E_prepared.Rdata which does not include adjusted ST-SE-VB variable and includes voting = NA)

## Step 10. Descriptive Analyses

10.1 	Check product moments (again) of all variables
10.2	Histograms of study variables
	10.2.1	Creation of adjusted ST-SE-VB score
10.3	Relationships among study variables
	10.3.1 	Unweighted correlations among continuous variables
	10.3.2	Create weighted data set; Weighted correlations among continuous variables
	10.3.3	Chi-square analyses for categorical variables

## Step 11. Hypothesis Testing
Using weighted data, we tested a series of regression models controlling for sex, age, education, religiosity, ESS round, & country. These regressions tested our main hypotheses regarding whether value balance is positively associated with SWB, self-rated health, social trust, political trust, and voting behavior. To examine our hypotheses, we used our adjusted ST-SE-VB score. For comparison, we also conducted separate sets of regressions using the scores that showed asymmetric relations with SE and ST (original ST-SE-VB and ST dim). 

11.1	Linear Regression Predicting Self-Rated Health
11.2	Linear Regression Predicting SWB
11.3	Linear Regression Predicting Social Trust
11.4	Linear Regression Predicting Political Trust
11.5	Logistic Regression Predicting Voting Behavior

## Step 12: Extra Analyses: ESS Round 9 Data

12.1	Regressions using weighted data
12.2	Regressions using complex survey design data

## Step 13: Extra Analyses: Country Level
In these analyses we examine some descriptive information regarding average levels of ST and SE values, scores on original ST dim (called SDSo in code) and our original ST-SE-VB scale (called DBS9 in code)
