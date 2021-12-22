getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)
library(tidyr)
library(mice)
library(pan)
library(ggplot2)
library(lattice)


load('gdmt_11.1_final.RData')

#create a new dataset based on catnomiss 
df.test <- df.final


#### clean data ####
#remove extraneous levels from dataset
df.test <- droplevels(df.test)

#asssume that all heart transplant and academic hospitals are known
#impute 0 for all missing of GH_HEART_TRANSPLANTS and RESIDENTS
df.test$GH_HEART_TRANSPLANTS[is.na(df.test$GH_HEART_TRANSPLANTS)] <- 0
df.test$RESIDENTS[is.na(df.test$RESIDENTS)] <- 0

#for insurance, combine NA and UTD 
df.test$insurance <- as.character(df.test$insurance)
df.test$insurance[is.na(df.test$insurance)] <- 'No Insurance/Not Documented/UTD'
df.test$insurance <- factor(df.test$insurance)
table(df.test$insurance)

#remove patients with missing data in key cat var with levels > 2
df.test <- subset(df.test, !(is.na(df.test$race2i)))
df.test <- subset(df.test, !(is.na(df.test$MCNTRLi)))

#for all other cat var with levels > 2, make missing a category but DO NOT include in final model 
# 'race2i', 'ADMITSOURCEi', 'zip1', 'covar_dispo', 'OH_HFHOSPADM',
# 'MedExp', 'zip_designation', 
# 'SITE_DCI_setting', 'SITE_DCI_quintile'
list = c('ADMITSOURCEi', 'zip1', 'covar_dispo', 'OH_HFHOSPADM',
         'zip_designation', 'SITE_DCI_setting', 'SITE_DCI_quintile')

#execute addNA() to add NA as an explicit level
df.test[list] <- lapply(df.test[list], function(x) addNA(x))
rm(list)

#create a new var combining afib and aflutter 
df.test <- 
  df.test %>% 
  mutate(PMHx_fibfl = ifelse(PMHx_afib %in% 1 | PMHx_aflutter %in% 1, 1, 
                             ifelse(is.na(PMHx_afib) & is.na(PMHx_aflutter), NA, 0)))

df.test$PMHx_fibfl <- factor(df.test$PMHx_fibfl)

#some of the Cr values are out of range (>50); replace these values 
df.test$covar_Crdisc <- ifelse(df.test$covar_Crdisc >= 50, df.test$covar_Crdisc/10, df.test$covar_Crdisc)

#in first iteration of test with df.final, a vector of 17.9Gb was created??? 
#so will reduce the size of the df in testing
#df.test <- subset(df.catnomiss, (SITE_ID <= 50))
df.test <- subset(df.test, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                       MedExp_adoption_date, MedExp_state, PMHx_priorPCIorCABG, zip_num))

#reduce number of ancillary variables to reduce resource use 
df.test <- subset(df.test, select = -c(PATIENT_ID, RCRDNUM, SITE_POSTAL_CODE, LOS, 
                                       JC_HXSMOKING, GH_ACADEMIC, GH_INTERVENTIONAL, SITESURG, 
                                       DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, 
                                       AHA_PROCEDURES_1:AHA_PROCEDURES_125,
                                       HF_ISOSORBIDE, HFS_MORTALITYRISK, OH_SYMPTOMS, 
                                       PREDMORT, SODIUMi_disc, WEIGHTDISCi, covar_dWeight, 
                                       covar_BMI, OH_HFHOSPADM, covar_ischemic, 
                                       AHA_BBPRIOR:OH_ARB, inhospRx_ACEI, inhospRx_ARB, inhospRx_hydralnitrate, 
                                       inhospRx_ARNI, PMHx_famHLD, PMHx_NEWinfdz, PMHx_CRTP, PMHx_CRTD, PMHx_PPM, 
                                       PMHx_priorPCI, PMHx_priorCABG, PMHx_HLD, PMHx_anemia, 
                                       OH_DISCBPDIAS, distress_quntile, SITE_DCI_quintile))

#remove the components of GDMT_ var to avoid collinearity
df.test <- subset(df.test, select = -c(GDMT_HFBB:GDMT_MRAorContra, GDMT_RAASI, GDMT_RAASIorContra))

#teh SITE_DCI var do not have great outflux 
#df.test <- subset(df.test, select = -c(SITE_DCI_setting:SITE_DCI_quintile))

#remove irrelevant PMHx var
df.test <- subset(df.test, select = -c(PMHx_afib, PMHx_aflutter, PMHx_PVD, PMHx_CAD, PMHx_priorMI, 
                                       PMHx_COPD, PMHx_OSAOHS, PMHx_CardioMEMS))

#keep data only if it the patient's zip code 
df.test <- subset(df.test, !is.na(df.test$distress_score))








####describe the new dataset #### 
df.test <- droplevels(df.test)

str(df.test, list.len=ncol(df.test))

#how many missing in each variable? 
lapply(df.test, function(x) 
  sum(is.na(x)))

#how many complete cases? 
sum(complete.cases(df.test))

#observe outflux for vars in df.test 
flux(df.test)

#understand how many missing elemeents per row 
df.test <- 
  df.test %>% 
  mutate(miss = rowSums(is.na(df.test)))

#histogram(~miss, data=df.test) 

summary(df.test$miss)
with(subset(df.test, df.test$miss >= 15), summary(miss))
sum(df.test$miss >= 15)

df.test <- subset(df.test, select = -c(miss))






####set up imputation model ####
fml <- GENDERi + race2i + insurance + AGEi + covar_OutsideZIPregion + ADMITSOURCEi + PMHx_none + PMHx_priorCVATIA + 
       PMHx_ICD + PMHx_CHF + PMHx_ESRD + PMHx_Cr2 + PMHx_MDD + PMHx_valvedz + PMHx_LVAD + PMHx_dm2 + covar_dispo + 
       BEDSIZE + GH_HEART_TRANSPLANTS + MCNTRLi + RESIDENTS + OH_EF + OH_TRANSPLANT + covar_ino + inhospRx_BB + 
       inhospRx_MRA + inhospRx_RAASI + inhospRx_none  ~ 1 + (1|SITE_ID)
