getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#must use mice version > 3.13.6 as this eliminates the problem with 2lmer failing when a cluster has n=1
#install.packages("devtools")
#devtools::install_github(repo="amices/mice")
 library(dplyr)
 library(tidyr)
 library(mice)
# library(ggplot2)
# library(lattice)s
 library(lme4)
 library(optimx)
 library(broom.mixed)
 library(performance)
# library(parlmice) 
 library(parallel)



load('gdmt_11.8_final.RData')

####clean data####
df.run <- subset(df.run, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                     MedExp_adoption_date, MedExp_state, 
                                     zip_num, RCRDNUM, zip1, SITE_POSTAL_CODE, PATIENT_ID,
                                     DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, AHA_PROCEDURES_1:AHA_PROCEDURES_125,
                                     inhospRx_ACEI, inhospRx_ARB, inhospRx_ARNI, inhospRx_hydralnitrate, 
                                     WEIGHTDISCi, covar_dWeight, covar_BMI, 
                                     covar_ischemic, HFS_MORTALITYRISK, OH_SYMPTOMS, PREDMORT, 
                                     PMHx_priorPCI, PMHx_priorCABG, PMHx_famHLD, PMHx_NEWinfdz, 
                                     PMHx_anemia, PMHx_priorMI, PMHx_CAD, 
                                     PMHx_afib, PMHx_aflutter, PMHx_priorPCIorCABG, PMHx_HLD, PMHx_PVD, 
                                     GH_ACADEMIC, GH_INTERVENTIONAL, SITESURG, 
                                     AHA_BBPRIOR:HF_ISOSORBIDE, OH_DISCBPDIAS, 
                                     distress_quntile, SITE_DCI_quintile, 
                                     GDMT_ACEI:GDMT_ARBorContra, GDMT_ARNIorContra:GDMT_ARNIorContra))

#remove all patients with an LVAD 
df.run <- subset(df.run, PMHx_LVAD == 0 | is.na(df.run$PMHx_LVAD))

#the run time was too long, need to simplify model 
df.run <- subset(df.run, select = -c(SITE_DCI_poptotal, SITE_DCI_setting, BEDSIZE, MedExp_status,
                                     SODIUMi_disc, covar_OutsideZIP, OH_HFHOSPADM, SITE_DCI_score,
                                     PMHx_LVAD, PMHx_OSAOHS,  
                                     LOS, 
                                     GDMT_HFBBorContra, GDMT_RAASIorContra, GDMT_MRAorContra, GDMT_ALL, GDMT_ALLorContra))

#remove patients with missing data in key cat var with levels > 2 
df.run <- subset(df.run, !(is.na(df.run$race2i)))
#df.run <- subset(df.run, !(is.na(df.run$MCNTRLi)))

#for all other cat var with levels > 2, make missing a category but DO NOT include in final model 
#execute addNA() to add NA as an explicit level
# list = c('insurance', 'ADMITSOURCEi', 'covar_dispo', 'OH_HFHOSPADM', 'zip_designation')
# df.run[list] <- lapply(df.run[list], function(x) addNA(x))
# rm(list)

df.run$insurance <- as.character(df.run$insurance)
df.run$insurance[is.na(df.run$insurance)] <- "UTD"
df.run$insurance <- as.factor(df.run$insurance)

df.run$ADMITSOURCEi <- as.character(df.run$ADMITSOURCEi)
df.run$ADMITSOURCEi[is.na(df.run$ADMITSOURCEi)] <- "UTD"
df.run$ADMITSOURCEi <- as.factor(df.run$ADMITSOURCEi)

df.run$covar_dispo <- as.character(df.run$covar_dispo)
df.run$covar_dispo[is.na(df.run$covar_dispo)] <- "UTD"
df.run$covar_dispo <- as.factor(df.run$covar_dispo)

df.run$zip_designation <- as.character(df.run$zip_designation)
df.run$zip_designation[is.na(df.run$zip_designation)] <- "UTD"
df.run$zip_designation <- as.factor(df.run$zip_designation)

df.run$MCNTRLi <- as.character(df.run$MCNTRLi)
df.run$MCNTRLi[is.na(df.run$MCNTRLi)] <- "UTD"
df.run$MCNTRLi <- as.factor(df.run$MCNTRLi)


#reduce the number of levels of covar_dispo
# df.run <- 
#   df.run %>% 
#   mutate(dispo = ifelse(covar_dispo == 'AcuteCareFacility', 'HospTransfer', 
#                  ifelse(covar_dispo == 'DoC', 'DoC', 
#                  ifelse(covar_dispo == 'Home', 'Home', 
#                  ifelse(covar_dispo == 'HomeHealthCare', 'HomeHealthCare', 
#                  ifelse(covar_dispo == 'Homless', 'Homeless', 
#                  ifelse(covar_dispo == 'ICF', 'OtherFacility',
#                  ifelse(covar_dispo == 'International', 'Home', 
#                  ifelse(covar_dispo == 'IRF', 'OtherFacility', 
#                  ifelse(covar_dispo == 'LTCH', 'OtherFacility', 
#                  ifelse(covar_dispo == 'SNF', 'OtherFacility', 
#                  ifelse(covar_dispo == 'OtherFacility', 'OtherFacility', 
#                  ifelse(covar_dispo == 'UTD', 'UTD', 'error')))))))))))))
# df.run$dispo <- as.factor(df.run$dispo)

#create a new binary variable- transfer to other health care facility? 
df.run <- 
  df.run %>% 
  mutate(DCtoContCare = ifelse(covar_dispo == 'AcuteCareFacility', 1, 
                            ifelse(covar_dispo == 'DoC', 0, 
                            ifelse(covar_dispo == 'Home', 0, 
                            ifelse(covar_dispo == 'HomeHealthCare', 1, 
                            ifelse(covar_dispo == 'Homless', 0, 
                            ifelse(covar_dispo == 'ICF', 1,
                            ifelse(covar_dispo == 'International', 0, 
                            ifelse(covar_dispo == 'IRF', 1, 
                            ifelse(covar_dispo == 'LTCH', 1, 
                            ifelse(covar_dispo == 'SNF', 1, 
                            ifelse(covar_dispo == 'OtherFacility', 1, 
                            ifelse(covar_dispo == 'UTD', NA, 999)))))))))))))
df.run$DCtoContCare <- as.factor(df.run$DCtoContCare)

#df.run <- subset(df.run, select = -c(covar_dispo))

#create a new var to say if someone has an ICD/PPM/CRTD
df.run <- 
  df.run %>% 
  mutate(PPMICDCRT = ifelse(PMHx_ICD %in% 1 | 
                            PMHx_PPM %in% 1 |
                            PMHx_CRTD %in% 1 |
                            PMHx_CRTP %in% 1 , 1, 
                     ifelse(is.na(PMHx_ICD) & 
                            is.na(PMHx_PPM) & 
                            is.na(PMHx_CRTD) &
                            is.na(PMHx_CRTP), NA, 
                     ifelse(PMHx_ICD == 0 & 
                            PMHx_PPM == 0 & 
                            PMHx_CRTD == 0 & 
                            PMHx_CRTP == 0 , 0, 99))))
df.run$PPMICDCRT <- as.factor(df.run$PPMICDCRT)

df.run <- subset(df.run, select = -c(PMHx_ICD, PMHx_PPM, PMHx_CRTD, PMHx_CRTP))

#create a new variable- does the patient have insurance? 
df.run <- 
  df.run %>% 
  mutate(insured = ifelse(insurance %in% 'Medicaid' | 
                          insurance %in% 'Medicare' | 
                          insurance %in% 'Other', 1,  
                   ifelse(insurance %in% 'noneORutd', 0, 
                   ifelse(insurance %in% 'UTD', NA, 999))))
df.run$insured <- as.factor(df.run$insured)


df.run$GDMT_BBcontra <- as.factor(df.run$GDMT_BBcontra)
df.run$GDMT_RAASIcontra <- as.factor(df.run$GDMT_RAASIcontra)
df.run$GDMT_MRAcontra <- as.factor(df.run$GDMT_MRAcontra)
df.run$GDMT_ARNIcontra <- as.factor(df.run$GDMT_ARNIcontra)


#remove patients with dc Na < 100 (unlikely compatible with life)
#df.run$SODIUMi_disc[df.run$SODIUMi_disc<=100] <- NA

#change Cr to the natural log of Cr 
#df.run$covar_Crdisc <- log(df.run$covar_Crdisc)


#create a new outcome using passive imputation that is the sum of the GDMT components 
#eg a patient gets 1 point for each component of GDMT prescribed, so it should range from [0,3]
# df.run <-
#   df.run %>%
#   mutate(GDMT_Rx_score = as.numeric(GDMT_HFBB) +
#                          as.numeric(GDMT_RAASI) +
#                          as.numeric(GDMT_MRA) - 3)
# 
# df.run <-
#   df.run %>%
#   mutate(GDMT_RxContra_score = as.numeric(GDMT_HFBBorContra) +
#            as.numeric(GDMT_RAASIorContra) +
#            as.numeric(GDMT_MRAorContra) -3)

####describe the df.run #### 
names(df.run)

str(df.run, list.len=ncol(df.run))

#how many missing in each variable? 
lapply(df.run, function(x) 
       sum(is.na(x))
       )

#how many complete cases? 
sum(complete.cases(df.run))

#observe outflux for vars in df.test 
#flux(df.run)

#understand how many missing elemeents per row 
# df.run <- 
#   df.run %>% 
#   mutate(miss = rowSums(is.na(df.run)))
# 
# histogram(~miss, data=df.run) 
# dev.off()
# 
# summary(df.run$miss)
# with(subset(df.run, df.run$miss >= 15), summary(miss))
# sum(df.run$miss >= 15)
# 
# df.run <- subset(df.run, select = -c(miss))



####set cluster_var/level defining variable ####
#SITE_ID is the level 2 defining var; it has no missing data 
df.run$SITE_ID <- as.numeric(df.run$SITE_ID)

###decrease sample size to during testing####
df.test <- subset(df.run, SITE_ID <=100)
df.test <- subset(df.test, !(is.na(df.test$GDMT_ARNI)))
df.test <- subset(df.test, select = -c(ADMITSOURCEi, covar_dispo, covar_ino, MCNTRLi, insurance, 
                                     PMHx_HTN, PMHx_MDD, PMHx_valvedz, PMHx_CardioMEMS, JC_HXSMOKING, 
                                     covar_OutsideZIPregion))




cluster_var=df.test$SITE_ID
print("cluster variable set")

####create the visitation sequence####
# vis <- c('GENDERi', 'GDMT_RAASI', 'GDMT_HFBB', 'followup', 'GDMT_MRA', 'GDMT_RAASIcontra', 'JC_HXSMOKING', 
#          'insured', 
#          'PMHx_none', 'PMHx_COPD', 'PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_CHF', 'PMHx_ESRD', 'PMHx_fibfl', 
#          'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 'PMHx_CardioMEMS', 'PMHx_dm2', 'PPMICDCRT', 
#          'GDMT_MRAcontra', 'GDMT_BBcontra', 
#          'inhospRx_none', 'inhospRx_RAASI', 'inhospRx_MRA', 'inhospRx_BB', 'OH_DISCBPSYST', 'OH_DISCHR', 
#          'covar_OutsideZIPregion', 'MedExp', 
#          'distress_score', 'population_total', 'DCtoContCare', 
#          'covar_Crdisc', 'covar_Kdisc')

####create the imputation method vector ####
impmethod <- character(ncol(df.test))
names(impmethod) <- colnames(df.test)
print("imputation vector created")


#set imputation methods by var (fully conditional specification)
impmethod['GDMT_HFBB'] <- "2l.bin" #ICC 0.070
impmethod['GDMT_BBcontra'] <- "2l.bin" #ICC 0.470
impmethod['GDMT_RAASI'] <- "logreg" #ICC 0.039
impmethod['GDMT_RAASIcontra'] <- "2l.bin" #ICC 0.410
impmethod['GDMT_MRA'] <- "2l.bin" #ICC 0.111
impmethod['GDMT_MRAcontra'] <- "2l.bin" #ICC 0.410 
impmethod['GDMT_ARNIcontra'] <- "2l.bin"

impmethod['GENDERi'] <- "logreg" #ICC 0.010

#impmethod['covar_OutsideZIPregion'] <- "2l.bin" #ICC 0.305

#impmethod['LOS'] <- "2l.pmm"

impmethod['PMHx_none'] <- "2l.bin" #ICC 0.124
impmethod['PMHx_COPD'] <- "2l.bin" #ICC 0.052
#impmethod['PMHx_HTN'] <- "2l.bin" #ICC 0.097
impmethod['PMHx_priorCVATIA'] <- "logreg" #ICC 0.038
impmethod['PPMICDCRT'] <- "2l.bin" #ICC 0.089
impmethod['PMHx_CHF'] <- "2l.bin" #ICC 0.117
#impmethod['PMHx_PPM'] <- "2l.bin"
impmethod['PMHx_ESRD'] <- "2l.bin" #ICC 0.129
impmethod['PMHx_Cr2'] <- "2l.bin" #ICC 0.177
#impmethod['PMHx_MDD'] <- "2l.bin" #ICC 0.173
#impmethod['PMHx_valvedz'] <- "2l.bin" #ICC 0.240
#impmethod['PMHx_CRTD'] <- "2l.bin"
#impmethod['PMHx_LVAD'] <- "2l.bin"
#impmethod['PMHx_CardioMEMS'] <- "2l.bin" #ICC 0.512
#impmethod['PMHx_OSAOHS'] <- "2l.bin"
impmethod['PMHx_dm2'] <- "logreg" #ICC 0.031
impmethod['PMHx_fibfl'] <- "logreg" #ICC 0.045

#impmethod['JC_HXSMOKING'] <- "2l.bin" #ICC 0.77

#impmethod['BEDSIZE'] <- "2lonly.pmm"
#impmethod['MCNTRLi'] <- "2lonly.function"

impmethod['inhospRx_none'] <- "2l.bin" #ICC 0.110
impmethod['inhospRx_BB'] <- "2l.bin" #ICC 0.087
impmethod['inhospRx_MRA'] <- "2l.bin" #ICC 0.110
impmethod['inhospRx_RAASI'] <- "logreg" #ICC 0.043 

impmethod['covar_Kdisc'] <- "pmm" #"2l.lmer" #ICC 0.044
impmethod['covar_Crdisc'] <- "pmm" #"2l.lmer" #ICC 0.011 

impmethod['OH_DISCBPSYST'] <- "pmm" #ICC 0.027
impmethod['OH_DISCHR'] <- "pmm" #ICC 0.022

impmethod['MedExp'] <- "2l.bin" #ICC 0.945
impmethod['population_total'] <- "2l.lmer" #ICC 0.437 
impmethod['distress_score'] <- "2l.lmer" #ICC 0.299

impmethod['followup'] <- "2l.bin" #ICC 0.593
impmethod['DCtoContCare'] <- "2l.bin" #ICC 0.750
impmethod['insured'] <- "2l.bin" #ICC 0.320
# 
# impmethod['GDMT_ALL'] <- "2l.bin"
# impmethod['GDMT_ALLorContra'] <- "2l.bin"
# 
# impmethod['GDMT_Rx_score'] <- "~I(as.numeric(GDMT_HFBB) + as.numeric(GDMT_RAASI) + as.numeric(GDMT_MRA) - 3)"
# impmethod['GDMT_RxContra_score'] "~I(as.numeric(GDMT_HFBBorContra) + 
#                                      as.numeric(GDMT_RAASIorContra) + as.numeric(GDMT_MRAorContra -3)"

#impmethod['SITE_DCI_poptotal'] <- "2lonly.pmm"
#impmethod['SITE_DCI_setting'] <- "2lonly.function"

#define the imputation function for 2lonly.function 
#imputationFunction <- list('MCNTRLi'='polyreg')

print(impmethod)
print("imputation methods assigned")


#####make the predicator matrix ####
pm <- make.predictorMatrix(df.test)

#set the SITE_ID as the cluster var 
pm[,'SITE_ID'] <- -2
pm['SITE_ID',] <- 0



#ensure that var derrived from each other are not used to predict each other 
# pm['GDMT_ALL', c('GDMT_HFBB', 'GDMT_HFBBorContra', 'GDMT_RAASI', 'GDMT_RAASIorContra', 
#                  'GDMT_MRA', 'GDMT_MRAorContra', 'GDMT_ALLorContra')] <- 0
# pm['GDMT_ALLorContra', c('GDMT_HFBB', 'GDMT_HFBBorContra', 'GDMT_RAASI', 'GDMT_RAASIorContra', 
#                          'GDMT_MRA', 'GDMT_MRAorContra', 'GDMT_ALL')] <- 0

# pm['GDMT_HFBB', c('GDMT_ALL', 'GDMT_HFBBorContra', 'GDMT_ALLorContra')] <- 0
#  pm['GDMT_HFBBorContra', c('GDMT_ALL', 'GDMT_HFBB', 'GDMT_ALLorContra')] <- 0
#  pm['GDMT_RAASI', c('GDMT_ALL', 'GDMT_RAASIorContra', 'GDMT_ALLorContra')] <- 0
#  pm['GDMT_RAASIorContra', c('GDMT_RAASI', 'GDMT_ALL','GDMT_ALLorContra')] <- 0
#  pm['GDMT_MRA', c('GDMT_ALL', 'GDMT_MRAorContra', 'GDMT_ALLorContra')] <- 0
#  pm['GDMT_MRAorContra', c('GDMT_ALL', 'GDMT_MRA', 'GDMT_ALLorContra')] <- 0

#pm['covar_OutsideZIPregion', c('zip_designation', 'population_total', 'distress_score', 
#                               'MedExp')] <- 0

pm['population_total', c('zip_designation', 'distress_score', 
                         'MedExp', 'covar_OutsideZIPregion')] <- 0
pm['distress_score', c('zip_designation', 'population_total', 
                       'MedExp', 'covar_OutsideZIPregion')] <- 0
pm['MedExp', c('zip_designation', 'population_total', 
               'distress_score', 'covar_OutsideZIPregion')] <- 0

pm['PMHx_none', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_COPD', c('PMHx_none', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_priorCVATIA', c('PMHx_COPD', 'PMHx_none', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PPMICDCRT', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PMHx_none', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_CHF', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_none', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_ESRD', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_none', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_Cr2', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_none', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
# pm['PMHx_MDD', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
#                   'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_none', 'PMHx_valvedz', 
#                   'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
# pm['PMHx_valvedz', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
#                   'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_none', 
#                   'PMHx_dm2', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_dm2', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_none', 'PMHx_fibfl', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
pm['PMHx_fibfl', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_dm2', 'PMHx_none', 'PMHx_HTN', 'PMHx_CardioMEMS')] <- 0
# pm['PMHx_HTN', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
#                    'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
#                    'PMHx_dm2', 'PMHx_none', 'PMHx_fibfl', 'PMHx_CardioMEMS')] <- 0
# pm['PMHx_CardioMEMS', c('PMHx_COPD', 'PMHx_priorCVATIA', 'PPMICDCRT', 'PMHx_CHF', 
#                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
#                  'PMHx_dm2', 'PMHx_none', 'PMHx_fibfl', 'PMHx_HTN')] <- 0

pm['OH_DISCBPSYST', 'OH_DISCHR'] <- 0
pm['OH_DISCHR', 'OH_DISCBPSYST'] <- 0

#pm[,c('GDMT_MRA', 'GDMT_HFBB', 'GDMT_RAASI', 'GDMT_ALL')] <- 0

pm['DCtoContCare', 'covar_dispo'] <- 0
pm[, 'DCtoContCare'] <- 0

pm['insured', 'insurance'] <- 0
pm[, 'insured'] <- 0

#pm[, 'GDMT_ARNI'] <- 0
pm[, 'GDMT_ARNIcontra'] <- 0

pm['GDMT_ARNI', 'GDMT_RAASI'] <- 0
pm['GDMT_ARNIcontra', 'GDMT_RAASIcontra'] <- 0
#do not use a variable as a predictor to impute itself 

# pm['GDMT_HFBB', ] <- 0
# pm['GDMT_HFBB', 'SITE_ID'] <- -2
# pm['GDMT_HFBB', 'GENDERi'] <- 1
# pm['GDMT_HFBB', 'race2i'] <- 1
# pm['GDMT_HFBB', 'insurance'] <- 1
# pm['GDMT_HFBB', 'AGEi'] <- 1
# pm['GDMT_HFBB', 'ADMITSOURCEi'] <- 1
# pm['GDMT_HFBB', 'OH_DISCHR'] <- 1
# pm['GDMT_HFBB', 'OH_DISCBPSYST'] <- 1
# pm['GDMT_HFBB', 'distress_score'] <- 1
# pm['GDMT_HFBB', 'GDMT_BBcontra'] <- 1
# pm['GDMT_HFBB', 'inhospRx_BB'] <- 1
# pm['GDMT_HFBB', 'PPMICDCRT'] <- 1

print("predictor matrix complete")
print(pm)


####create a MICE() object
print("creating a mice() object")

imprun <- mice(df.test, m=1, predictorMatrix=pm, method=impmethod,
                maxit=3, seed=88, cluster_var=cluster_var, 
                nAGQ=0, verbose=2,
                control1=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE),
                control2=lmerControl(optimizer='nloptwrap', optCtrl=list(maxfun=2e5)))







#must set .random.seed using set.seed as apparently the AppStream platform 
#does not have access to .random.seed in its environment 
set.seed(88) 
gc()

start_time <- Sys.time() 
print(start_time)
imprun <- parlmice(df.run, cluster.seed=88, n.core=3, cl.type="FORK",
                     predictorMatrix=pm, method=impmethod,
                     maxit=2, cluster_var=cluster_var, 
                     nAGQ=0, n.imp.core = 2,
                     control1=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE),
                     control2=lmerControl(optimizer='nloptwrap', optCtrl=list(maxfun=2e5)))
end_time <- Sys.time() 
end_time - start_time 
print("mids object created")

print("saving mids object")
write.mice.imputation(mi.res=imprun, name="mice_obj", mids2spss=F)
save(imprun, file = "impfinal.rda")
saveRDS(imprun, "/mnt/workspace/rstudio_projects/mids1.rds")

##### need to passively impute primary outcomes 
# long <- mice::complete(imprun, "long", include=TRUE)
# long$GDMT_Rx_score <- with(long, as.numeric(GDMT_HFBB) + as.numeric(GDMT_RAASI) + as.numeric(GDMT_MRA) - 3)
# 
# long <- 
#   long %>% 
#   mutate(newvar=AGEi*10)
# 
# long$var2 <- with(long, ifelse(GDMT_HFBB %in% 1 & GDMT_RAASI %in% 1 & GDMT_MRA %in% 1, 1, 0))
# 
# 
#   #visitSequence=vis, data.init = NULL, ignore = NULL, where = NULL, post = NULL, blots = NULL,
#   #imputationFunction=imputationFunction,
#   #try different optimizers (bobyqa, nloptr)
#   #control1=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE),
#   #verbose=2,
# 
# #  imputationFunction=imputationFunction
# 
# #view imputed datasets
# imptest$imp$covar_Kdisc
# 
# #assess convergence
# plot(imptest, distress_score ~ .it | .ms)
# dev.off() 

#### run the analysis phase with with.mids() 
#library(oridinal)
#with(imprun, clmm(formula, link="cloglog"))s



