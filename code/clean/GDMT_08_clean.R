getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)
library(labelled)


load('gdmt_7_complete.RData')
names(df_gdmt_complete)


#missing data? 
lapply(df_fast, function(x) 
  sum(is.na(x)))

#imputing NA to 0 in specific cases 
addmargins(table(df_gdmt_complete$GDMT_HFBB, exclude=NULL))

#impute NA to 0 in cases where patient has contra to BB but listed as NA for BB on dc 
addmargins(table(df_gdmt_complete$GDMT_HFBB, df_gdmt_complete$GDMT_HFBBorContra, exclude=NULL))
df_gdmt_complete <- 
  df_gdmt_complete %>% 
  mutate(GDMT_HFBB = ifelse(is.na(GDMT_HFBB) & GDMT_HFBBorContra==1, 0, GDMT_HFBB))
addmargins(table(df_gdmt_complete$GDMT_HFBB, df_gdmt_complete$GDMT_HFBBorContra, exclude=NULL))

#create a new variable that gives ischemic vs nonischemic; the remaining can be NA 
addmargins(table(df_gdmt_complete$OH_ISCHEMIC, exclude=NULL))
addmargins(table(df_gdmt_complete$OH_NONISCHEMIC, exclude=NULL))
addmargins(table(df_gdmt_complete$OH_ISCHEMIC, df_gdmt_complete$OH_NONISCHEMIC,  exclude=NULL))

df_gdmt_complete <- 
  df_gdmt_complete %>% 
  mutate(covar_ischemic = ifelse(OH_ISCHEMIC==1 & is.na(OH_NONISCHEMIC), 1,
                          ifelse(OH_NONISCHEMIC==1 & is.na(OH_ISCHEMIC), 2, 99))) 
df_gdmt_complete$covar_ischemic <- factor(df_gdmt_complete$covar_ischemic, 
                                          levels = c(1,2), 
                                          labels = c('ischemic', 'nonischemic'))

addmargins(table(df_gdmt_complete$covar_ischemic, exclude=NULL))
addmargins(table(df_gdmt_complete$covar_ischemic, df_gdmt_complete$OH_ISCHEMIC, exclude=NULL))
addmargins(table(df_gdmt_complete$covar_ischemic, df_gdmt_complete$OH_NONISCHEMIC, exclude=NULL))

#assume that all transplants are documented 
df_gdmt_complete$OH_TRANSPLANT[is.na(df_gdmt_complete$OH_TRANSPLANT)] <- 0
addmargins(table(df_gdmt_complete$OH_TRANSPLANT, exclude=NULL))


#dependent vars: GDMT_HFBB, GDMT_HFBBorContra, GDMT_ACEI, GDMT_ACEIorContra, GDMT_ARB, GDMT_ARBorContra, GDMT_ARNI, GDMT_ARNIorContra, GDMT_MRA, GDMT_MRAorContra
#primary vars: GENDERi, race2i, insurance, ZIP, AGEi, covar_OutsideZIP
#cluster: SITE_ID, PATIENT_ID
#covar-GDMTcontras: GDMT_BBcontra, GDMT_ACEIcontra_exp, GDMT_ARBcontra_exp, GDMT_ARNIcontra, GDMT_MRAcontra
#covar-hosp:  ADMITSOURCEi, LOS
#covar-prior Rx: {}
#covar-other dc Rx: {}
#covar-other dx: DYN_MEDHISTNONE:DYN_MEDHIST_36, JC_HXSMOKING
#covar-inhosp proc: {}
#covar-dispo: covar_dispo
#covar-site: BEDSIZE, GH_ACADEMIC, GH_HEART_TRANSPLANTS, GH_INTERVENTIONAL, MCNTRLi, RESIDENTS, SITESURG,
#covar-labs: {}
#covar-VS: {}
#covar-clinical: OH_EF, covar_ischemic, OH_TRANSPLANT,
#covar-inhosp Rx: {}

##notes: a lot of missing, separate analysis? ##
#covar-prior Rx: AHA_BBPRIOR, AHA_NOPRIORMEDS, OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, OH_ARB
#covar-other dc Rx: AHA_OTHDIURETIC, AHA_OTHLOOP, AHA_OTHNITRATE, AHA_OTHTHIAZIDE, HF_ISOSORBIDE, HF_HYDRALAZINEforLVSD
#covar-other dx: DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8
#covar-inhosp proc: AHA_PROCEDURES_1:AHA_PROCEDURES_4, AHA_PROCEDURES_10, AHA_PROCEDURES_11,AHA_PROCEDURES_108, AHA_PROCEDURES_112, AHA_PROCEDURES_116:AHA_PROCEDURES_120,AHA_PROCEDURES_123:AHA_PROCEDURES_125   
#covar-inhosp Rx: covar_ino, HFS_ORALMEDS_1:HFS_ORALMEDS_7
#covar-labs: covar_Kdisc, covar_Crdisc, SODIUMi_disc, BUNi_disc
#covar-VS: OH_DISCBPDIAS, OH_DISCBPSYST, OH_DISCHR, WEIGHTDISCi, covar_dWeight, covar_BMI
#covar-clinical: HFS_MORTALITYRISK, OH_HFHOSPADM, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, OH_symptoms, PREDMORT

#slim the datset to expedite manipulations
df_fast <- subset(df_gdmt_complete, select = c(
  RCRDNUM, 
  #dependent var: 
  GDMT_HFBB, GDMT_HFBBorContra, GDMT_ACEI, GDMT_ACEIorContra, 
  GDMT_ARB, GDMT_ARBorContra, GDMT_ARNI, GDMT_ARNIorContra, GDMT_MRA, GDMT_MRAorContra,
  #primary var: 
  GENDERi, race2i, insurance, ZIP, AGEi, covar_OutsideZIP, SITE_POSTAL_CODE, 
  #cluster: 
  SITE_ID, PATIENT_ID, 
  #covar-hosp:  
  ADMITSOURCEi, LOS, JC_DISCDATETIME, 
  #covar-other dx: 
  DYN_MEDHISTNONE:DYN_MEDHIST_36, JC_HXSMOKING, 
  #covar-dispo: 
  covar_dispo, 
  #covar-site: 
  BEDSIZE, GH_ACADEMIC, GH_HEART_TRANSPLANTS, GH_INTERVENTIONAL, MCNTRLi, RESIDENTS, SITESURG,
  #covar-clinical: 
  OH_EF, OH_TRANSPLANT, covar_ischemic, 
  #expanded: 
  #covar-prior Rx: 
  AHA_BBPRIOR, AHA_NOPRIORMEDS, OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, OH_ARB, 
  #covar-other dc Rx: dropped out most for overwhelming missingness
    #HF_HYDRALAZINEforLVSD, AHA_OTHNITRATE, AHA_OTHTHIAZIDE, 
    #AHA_OTHDIURETIC, AHA_OTHLOOP,  
  HF_ISOSORBIDE, 
  #covar-other dx: 
  DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, 
  #covar-inhosp proc: 
  AHA_PROCEDURES_1:AHA_PROCEDURES_4, AHA_PROCEDURES_10, AHA_PROCEDURES_11, 
  AHA_PROCEDURES_108, AHA_PROCEDURES_112, AHA_PROCEDURES_116:AHA_PROCEDURES_120,
  AHA_PROCEDURES_123:AHA_PROCEDURES_125,   
  #covar-inhosp Rx: 
  covar_ino, HFS_ORALMEDS_1:HFS_ORALMEDS_7, 
  #covar-labs: 
  covar_Kdisc, covar_Crdisc, SODIUMi_disc, 
  #covar-VS: 
  OH_DISCBPDIAS, OH_DISCBPSYST, OH_DISCHR, WEIGHTDISCi, covar_dWeight, covar_BMI, 
  #covar-clinical:
  HFS_MORTALITYRISK, OH_HFHOSPADM, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
  OH_SYMPTOMS, PREDMORT
))

view(df_fast)
#are there any complete cases? 
df_complete <- df_fast[complete.cases(df_fast), ]
  #no- lets use MI to add the data back in 


length((unique(df_fast$SITE_ID)))
length((unique(df_fast$PATIENT_ID)))

#save the new data frame 
save(df_fast, file = '/mnt/workspace/rstudio_projects/gdmt_8_fast.RData')


