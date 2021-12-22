getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

library(dplyr)
library(tidyr)
library(labelled)
library(mice)
library(miceadds)
library(micemd)
library(ggplot2)
library(lme4)
#install.packages("DataExplorer")
load('gdmt_11_final.RData')

####describe the data#### 
#quick look at the data
str(df.final, list.len=ncol(df.final))

#how many missing in each variable? 
lapply(df.final, function(x) 
  sum(is.na(x)))

####create the imp method vector for the subset model ####

 #level 2 data: 
   # for >2 level factor data, method: 2lonly.function, then designate the polyreg method 
   # for <=2 level factor data, method: 2lonly.function, then designate 2l.bin method
   # for continuous data, method: 2lonly.function, then designate 2l.norm method
 #level 1 data: 
   # for >2 level factor data, method: ??? i can't find an imputation method??
     # for lack of a better method, I will designate missing as unique factor level for this round of analysis
   # for <=2 level factor data, method: 2l.2stage.bin 
   # for continuous data, method: 2l.2stage.pmm 

#note:
  #imputation for MedExp, zip designation, population total, distress score, 
  # and covar_OutsideZIPregion are dependent on ZIP 

impmethod <- character(ncol(df.final))
names(impmethod) <- colnames(df.final)
print(impmethod)
class(impmethod)
length(impmethod)

#impM2 <- mice::make.method(data=df.final)  
  ##this command autopopulates the imputation method vector blindly based on data type 
  ##it doesn't know there are multilevel data 

#set imp method for outcome variables first - these are all binary
impmethod[colnames(subset(df.final, select = c(GDMT_HFBB:GDMT_MRAorContra)))] <- "2l.2stage.bin"
impmethod[colnames(subset(df.final, select = c(GDMT_RAASI:GDMT_ALLorContra)))] <- "2l.2stage.bin"
print(impmethod)

#set cluster_var/level defining variable 
#SITE_ID is the level 2 defining var; it has no missing data 
cluster_var=SITE_ID

#cluster variable traits 
#site level haracteristics (4): RESIDENTS (!), GH_HEART_TRANSPLANTS (!), MCNTRLi, BEDSIZE
impmethod['BEDSIZE'] <- "2lonly.function"  
impmethod['MCNTRLi'] <- "2lonly.function"  
impmethod['RESIDENTS'] <-"2lonly.function"  
impmethod['GH_HEART_TRANSPLANTS'] <- "2lonly.function"  

#define the imputation function for 2lonly.function 
imputationFunction <- list('BEDSIZE'='pmm', 
                           'MCNTRLi'='polyreg', 
                           'RESIDENTS'='logreg', 
                           'GH_HEART_TRANSPLANTS'= 'logreg')

#primary SES variables (10):
  # GENDERi, race2i, insurance, AGEi,  covar_dispo (!), 
  # distress_score (!), population_total (!), zip_designation (!), 
  # MedExp (!), covar_OutsideZIPregion (!) 
impmethod['GENDERi'] <- "2l.2stage.bin"
impmethod['race2i'] <- "" #????
#insurance has no missing data 
impmethod['AGEi'] <- "2l.2stage.pmm"
impmethod['covar_dispo'] <- "" #????
impmethod['population_total'] <- "2l.2stage.pmm"
impmethod['distress_score'] <- "2l.2stage.pmm"
impmethod['zip_designation'] <- "" #???? 
impmethod['MedExp'] <- "2l.2stage.bin"
impmethod['covar_OutsideZIP'] <- "2l.2stage.bin"

#clinical covariate data (5): 
  # OH_EF, covar_ino, OH_DISCHR (!), OH_DISCBPSYST (!), OH_TRANSPLANT
#OH_EF has no missing data 
impmethod['covar_ino'] <- "2l.2stage.bin"   
impmethod['OH_DISCHR'] <- "2l.2stage.pmm" 
impmethod['OH_DISCBPSYST'] <- "2l.2stage.pmm" 
#OH_TRANSPLANT has no missing data 

#inhosp Rx (3): 
  # inhospRx_BB (!), inhospRx_RAASI (!), inhospRx_MRA (!)
impmethod['inhospRx_BB'] <- "2l.2stage.bin" 
impmethod['inhospRx_RAASI'] <- "2l.2stage.bin" 
impmethod['inhospRx_MRA'] <- "2l.2stage.bin" 

#other medical comorbidities (17): 
  # PMHx_none, PMHx_afib, PMHx_aflutter, PMHx_COPD, PMHx_HTN, PMHx_priorMI, 
  # PMHx_priorCVATIA, PMHx_CHF, PMHx_ESRD, PMHx_Cr2, PMHx_MDD, PMHx_valvedz, 
  # PMHx_priorPCIorCABG, PMHx_LVAD, PMHx_CardioMEMS, PMHx_OSAOHS, PMHx_dm2 
impmethod['PMHx_none'] <- "2l.2stage.bin" 
impmethod['PMHx_afib'] <- "2l.2stage.bin" 
impmethod['PMHx_aflutter'] <- "2l.2stage.bin" 
impmethod['PMHx_COPD'] <- "2l.2stage.bin" 
impmethod['PMHx_HTN'] <- "2l.2stage.bin" 
impmethod['PMHx_priorMI'] <- "2l.2stage.bin" 
impmethod['PMHx_priorCVATIA'] <- "2l.2stage.bin" 
impmethod['PMHx_CHF'] <- "2l.2stage.bin" 
impmethod['PMHx_ESRD'] <- "2l.2stage.bin" 
impmethod['PMHx_Cr2'] <- "2l.2stage.bin" 
impmethod['PMHx_MDD'] <- "2l.2stage.bin" 
impmethod['PMHx_valvedz'] <- "2l.2stage.bin" 
impmethod['PMHx_priorPCIorCABG'] <- "2l.2stage.bin" 
impmethod['PMHx_LVAD'] <- "2l.2stage.bin" 
impmethod['PMHx_CardioMEMS'] <- "2l.2stage.bin" 
impmethod['PMHx_OSAOHS'] <- "2l.2stage.bin" 
impmethod['PMHx_dm2'] <- "2l.2stage.bin" 

#would really like to include HFS_oralmeds and dc_labs but missing data is up to 70% in these cases 
print(impmethod)


impmethod <- character(ncol(df.final))
names(impmethod) <- colnames(df.final)
print(impmethod)
class(impmethod)
length(impmethod)


str(df_fast, list.len=ncol(df_fast))
sum(complete.cases(df_fast))


addmargins(table(is.na(df_fast$zip1), df_fast$covar_dispo, exclude=NULL))


table(df_fast$insurance, exclude=NULL)

table(df_fast$GDMT_HFBB, exclude=NULL)
