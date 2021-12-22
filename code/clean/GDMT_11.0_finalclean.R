getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

library(dplyr)
library(tidyr)
library(labelled)
library(data.table)

load('gdmt_10_dci.RData')

####fix the insurance variable to replace the "missing" level with NA####
class(df_fast$insurance)
#df_fast$insurance <- addNA(df_fast$insurance)
df_fast$insurance[df_fast$insurance=='Missing'] <- NA
df_fast$insurance <- droplevels(df_fast$insurance)
table(df_fast$insurance, exclude=NULL)

#####generate a variable for any ACEI/ARB/ARNI or RAASEI+contraindication ####
df_fast <- 
  df_fast %>% 
  mutate(GDMT_RAASI = ifelse(GDMT_ACEI %in% 1 | 
                             GDMT_ARB %in% 1 | 
                             GDMT_ARNI %in% 1, 1, 
                      ifelse(is.na(GDMT_ACEI) & 
                             is.na(GDMT_ARB) &
                             is.na(GDMT_ARNI), NA, 0)))

addmargins(table(df_fast$GDMT_RAASI, exclude=NULL))
addmargins(table(df_fast$GDMT_RAASI, df_fast$GDMT_ACEI, exclude=NULL))
addmargins(table(df_fast$GDMT_RAASI, df_fast$GDMT_ARB, exclude=NULL))
addmargins(table(df_fast$GDMT_RAASI, df_fast$GDMT_ARNI, exclude=NULL))

df_fast <- 
  df_fast %>% 
  mutate(GDMT_RAASIorContra = ifelse(GDMT_ACEIorContra %in% 1 | 
                                     GDMT_ARBorContra %in% 1 | 
                                     GDMT_ARNIorContra %in% 1, 1, 
                              ifelse(is.na(GDMT_ACEIorContra) & 
                                     is.na(GDMT_ACEIorContra) &
                                     is.na(GDMT_ACEIorContra), NA, 0)))
                              
addmargins(table(df_fast$GDMT_RAASIorContra, exclude=NULL))

####generate a variable for ALL GDMT + GDMT/Contra ####
df_fast <- 
  df_fast %>% 
  mutate(GDMT_ALL = ifelse(GDMT_HFBB==1 & 
                           GDMT_RAASI==1 & 
                           GDMT_MRA==1, 1, 
                    ifelse(is.na(GDMT_HFBB) |
                           is.na(GDMT_RAASI) |
                           is.na(GDMT_MRA), NA, 0)))
addmargins(table(df_fast$GDMT_ALL, exclude=NULL))

df_fast <- 
  df_fast %>% 
  mutate(GDMT_ALLorContra = ifelse(GDMT_HFBBorContra==1 & 
                                   GDMT_RAASIorContra==1 & 
                                   GDMT_MRAorContra==1, 1, 
                            ifelse(is.na(GDMT_HFBB) |
                                   is.na(GDMT_RAASI) |
                                   is.na(GDMT_MRA), NA, 0)))
addmargins(table(df_fast$GDMT_ALLorContra, exclude=NULL))

#### assess internal validity of all outcome variables ##### 
DT <- data.table(df_fast)
dt <- DT[, .SD, .SDcols=c('RCRDNUM', 
                          'GDMT_HFBB', 'GDMT_ACEI', 'GDMT_ARB', 'GDMT_ARNI', 'GDMT_RAASI', 'GDMT_MRA', 'GDMT_ALL',
                          'GDMT_HFBBorContra', 'GDMT_ACEIorContra', 'GDMT_ARBorContra', 
                          'GDMT_ARNIorContra', 'GDMT_RAASIorContra', 'GDMT_MRAorContra', 'GDMT_ALLorContra')]
dt2 <- dt[, .SD[is.na(GDMT_ALL) | is.na(GDMT_ALLorContra)]]
rm(DT, dt, dt2)

####variable that matches patient's zip codes to the site zip code#### 
addmargins(table(df_fast$covar_OutsideZIP, exclude=NULL))

df_fast <-
  df_fast %>% 
  mutate(covar_OutsideZIP = ifelse(is.na(SITE_POSTAL_CODE) | is.na(zip1), NA, 
                            ifelse(as.character(SITE_POSTAL_CODE) != as.character(zip1), 1, 0)))

addmargins(table(df_fast$covar_OutsideZIP, exclude=NULL))
class(df_fast$covar_OutsideZIP)
#dt_zip <- data.table(df_fast)
#dt_zip <- dt_zip[, .SD, .SDcols=c('zip_num', 'zip1', 'SITE_POSTAL_CODE', 'covar_OutsideZIP', 'RCRDNUM')]

sum(is.na(df_fast$SITE_POSTAL_CODE))
var_label(df_fast$covar_OutsideZIP) <- "Pt vs Hosp ZIP different? 1 yes, 0 no"

####create a variable for matching pt/site regions (first 3 digits of zip)#### 
#the patient zip region
df_fast <-
  df_fast %>% 
  mutate(covar_zipRegion_pt = substr(df_fast$zip1, 1, 3))

#the site zip region 
df_fast <-
  df_fast %>% 
  mutate(covar_zipRegion_site = substr(df_fast$SITE_POSTAL_CODE, 1, 3))

#do the zip REGIONS match? 
df_fast <-
  df_fast %>% 
  mutate(covar_OutsideZIPregion = ifelse(covar_zipRegion_pt != covar_zipRegion_site, 1, 0))

#remove unneeded var
df_fast <- subset(df_fast, select = -c(covar_zipRegion_site, covar_zipRegion_pt))

names(df_fast)

####variable that says if the patient's state had expanded medicaid by the time of their dc####
addmargins(table(df_fast$MedExp_status, exclude=NULL))
class(df_fast$MedExp_status)

df_fast <-
  df_fast %>% 
  mutate(MedExp = ifelse(df_fast$MedExp_status == 'Adopted' & 
                         JC_DISCDATETIME >= MedExp_adoption_date, 1, 0))
addmargins(table(df_fast$MedExp, exclude=NULL))
addmargins(table(df_fast$MedExp_status, df_fast$MedExp, exclude=NULL))


####set factors explicitly ####
lapply(df_fast, function(x)
  class(x))

list <- colnames(subset(df_fast, select = c(GDMT_HFBB:GDMT_MRAorContra, 
                                            GDMT_RAASI:GDMT_ALLorContra,
                                            covar_OutsideZIP:PATIENT_ID, 
                                            zip_num, zip1, covar_OutsideZIPregion, 
                                            DYN_MEDHISTNONE:DYN_MEDHIST_36, JC_HXSMOKING, 
                                            covar_dispo, GH_ACADEMIC:GH_INTERVENTIONAL, RESIDENTS, 
                                            SITESURG, OH_TRANSPLANT, AHA_BBPRIOR:HF_ISOSORBIDE, 
                                            DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, 
                                            AHA_PROCEDURES_1:AHA_PROCEDURES_125, covar_ino, 
                                            HFS_ORALMEDS_1:HFS_ORALMEDS_7, 
                                            OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                            MedExp_state, MedExp)))
df_fast[,list] <- lapply(df_fast[,list], factor)
rm(list)

####create a new inhosp var to indicate ACEI/ARB/ARNI ####
df_fast <-
  df_fast %>% 
  mutate(inhospRx_RAASI = ifelse(df_fast$HFS_ORALMEDS_1 %in% 1 | df_fast$HFS_ORALMEDS_2 %in% 1 |
                                 df_fast$HFS_ORALMEDS_7 %in% 1, 1, 
                          ifelse(is.na(df_fast$HFS_ORALMEDS_1) & is.na(df_fast$HFS_ORALMEDS_2) &
                                 is.na(df_fast$HFS_ORALMEDS_7), NA, 0)))

addmargins(table(df_fast$HFS_ORALMEDS_1, df_fast$inhospRx_RAASI, exclude=NULL))                              
addmargins(table(df_fast$HFS_ORALMEDS_2, df_fast$inhospRx_RAASI, exclude=NULL))
addmargins(table(df_fast$HFS_ORALMEDS_7, df_fast$inhospRx_RAASI, exclude=NULL))

df_fast$inhospRx_RAASI <- factor(df_fast$inhospRx_RAASI)
df_fast$covar_OutsideZIPregion <- factor(df_fast$covar_OutsideZIPregion)

####keep only the first observation for each patient####
DT <- data.table(df_fast, key="PATIENT_ID,JC_DISCDATETIME")
  #dt <- DT[, .SD, .SDcols=c('RCRDNUM', 'SITE_ID', 'PATIENT_ID', 'JC_DISCDATETIME')] ##test dataset
dt <- DT[order(JC_DISCDATETIME), .SD[1L], by = PATIENT_ID]
#dt <- dt[, .SD[OH_TRANSPLANT != 1]]
df_fast <- setDF(dt)
rm(dt, DT)




lapply(df_fast, function(x) 
  sum(is.na(x))/nrow(df_fast))  

####rename DYN_MEDHIST_ to something more meaningful ####
df_fast <- rename(df_fast, PMHx_none = DYN_MEDHISTNONE) 
df_fast <- rename(df_fast, PMHx_afib = DYN_MEDHIST_1) 
df_fast <- rename(df_fast, PMHx_aflutter = DYN_MEDHIST_2) 
df_fast <- rename(df_fast, PMHx_COPD = DYN_MEDHIST_3) 
df_fast <- rename(df_fast, PMHx_HLD = DYN_MEDHIST_6) 
df_fast <- rename(df_fast, PMHx_HTN = DYN_MEDHIST_7)
df_fast <- rename(df_fast, PMHx_PVD = DYN_MEDHIST_8)
df_fast <- rename(df_fast, PMHx_CAD = DYN_MEDHIST_9)
df_fast <- rename(df_fast, PMHx_priorMI = DYN_MEDHIST_10) 
df_fast <- rename(df_fast, PMHx_priorCVATIA = DYN_MEDHIST_11) 
df_fast <- rename(df_fast, PMHx_ICD = DYN_MEDHIST_12) 
df_fast <- rename(df_fast, PMHx_CHF = DYN_MEDHIST_13) 
df_fast <- rename(df_fast, PMHx_anemia = DYN_MEDHIST_14) 
df_fast <- rename(df_fast, PMHx_PPM = DYN_MEDHIST_15) 
df_fast <- rename(df_fast, PMHx_ESRD = DYN_MEDHIST_16) 
df_fast <- rename(df_fast, PMHx_Cr2 = DYN_MEDHIST_17) 
df_fast <- rename(df_fast, PMHx_MDD = DYN_MEDHIST_19)
df_fast <- rename(df_fast, PMHx_priorPCI = DYN_MEDHIST_22) 
df_fast <- rename(df_fast, PMHx_priorCABG = DYN_MEDHIST_23) 
df_fast <- rename(df_fast, PMHx_valvedz = DYN_MEDHIST_24) 
df_fast <- rename(df_fast, PMHx_priorPCIorCABG = DYN_MEDHIST_25)
df_fast <- rename(df_fast, PMHx_CRTP = DYN_MEDHIST_26) 
df_fast <- rename(df_fast, PMHx_CRTD = DYN_MEDHIST_27) 
df_fast <- rename(df_fast, PMHx_LVAD = DYN_MEDHIST_28)
df_fast <- rename(df_fast, PMHx_CardioMEMS = DYN_MEDHIST_29) 
df_fast <- rename(df_fast, PMHx_OSAOHS = DYN_MEDHIST_30) 
df_fast <- rename(df_fast, PMHx_dm2 = DYN_MEDHIST_31) 
df_fast <- rename(df_fast, PMHx_famHLD = DYN_MEDHIST_32) 
df_fast <- rename(df_fast, PMHx_NEWinfdz = DYN_MEDHIST_36) 

####rename the HFS_ORALMEDS vars to something more meaningful ####
df_fast <- rename(df_fast, inhospRx_ACEI = HFS_ORALMEDS_1) 
df_fast <- rename(df_fast, inhospRx_ARB = HFS_ORALMEDS_2) 
df_fast <- rename(df_fast, inhospRx_BB = HFS_ORALMEDS_3) 
df_fast <- rename(df_fast, inhospRx_MRA = HFS_ORALMEDS_4) 
df_fast <- rename(df_fast, inhospRx_hydralnitrate = HFS_ORALMEDS_5) 
df_fast <- rename(df_fast, inhospRx_none = HFS_ORALMEDS_6) 
df_fast <- rename(df_fast, inhospRx_ARNI = HFS_ORALMEDS_7) 


names(df_fast)

df.final <- df_fast
#missingness bins 
#<5%
#PATIENT_ID, RCRDNUM, GDMT_HFBB, GDMT_HFBBorContra, GDMT_ACEI, GDMT_ACEIorContra, GDMT_ARBorContra
#GENDERi, race2i, insurance, AGEi, SITE_POSTAL_CODE, SITE_ID, JC_DISCDATETIME, MCNTRLi, OH_EF
#OH_TRANSPLANT, covar_ino, GDMT_RAASI, GDMT_RAASIorContra, 

#5-15%, BEDSIZE
#GDMT_ARB, DYN_MEDHISTNONE, DYN_MEDHIST_, JC_HXSMOKING, GDMT_ALL, GDMT_ALLorContra

#>15%
#zip_num, zip1, GDMT_ARNI, GDMT_ARNIorContra, GDMT_MRA, GDMT_MRAorContra, covar_OutsideZIP, 
#ADMITSOURCEi, LOS, covar_dispo, GH_ACADEMIC, GH_HEART_TRANSPLANTS, GH_INTERVENTIONAL, RESIDENTS, 
#SITESURG, covar_ischemic, AHA_BBPRIOR, AHA_NOPRIORMEDS, OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, 
#OH_ARB, HF_ISOSORBIDE, DYN_OTHERCONDITION_, AHA_PROCEDURES_, HFS_ORALMEDS_, 
#covar_Kdisc, covar_Crdisc, SODIUMi_disc, 
#OH_DISCBPDIAS, OH_DISCBPSYST, OH_DISCHR, 
#WEIGHTDISCi, covar_dWeight, covar_BMI, HFS_MORTALITYRISK, OH_HFHOSPADM, 
#OH_NONISCHEMIC_ETIOLOGY_, OH_SYMPTOMS, PREDMORT, MedExp_state, MedExp_status, MedExp_adoption_date, 
#zip_designation, population_total, $distress_score, distress_quntile, MedExp, covar_OutsideZIPregion, 
#


#top var of interest 
#outcome (2): GDMT_ALL, GDMT_ALLorContra
#cluster/level (1): SITE_ID
#site level haracteristics (3): RESIDENTS (!), GH_HEART_TRANSPLANTS (!), MCNTRLi,
#primary (10): GENDERi, race2i, insurance, AGEi,  covar_dispo (!), 
#         distress_score (!), population_total (!), zip_designation (!), MedExp (!), covar_OutsideZIPregion (!) 
#clinical (4): OH_EF, covar_ino, OH_DISCHR (!), OH_DISCBPSYST (!), OH_TRANSPLANT
#inhosp Rx (7): inhospRx_BB (!), inhospRx_RAASI (!), inhospRx_MRA (!)
#PMHx (17): PMHx_none, PMHx_afib, PMHx_aflutter, PMHx_COPD, PMHx_HTN, PMHx_priorMI, PMHx_priorCVATIA, PMHx_CHF, 
#           PMHx_ESRD, PMHx_Cr2, PMHx_MDD, PMHx_valvedz, PMHx_priorPCIorCABG, PMHx_LVAD, PMHx_CardioMEMS, PMHx_OSAOHS, PMHx_dm2, 

#would really like to include HFS_oralmeds and dc_labs 



#summarize the data 
str(df.final, list.len=ncol(df.final))

#How many unique patients are in df? 179425 unique patients
length((unique(with(df.final,PATIENT_ID))))

#How many unique sites are within df? 662 unique sites
length((unique(with(df.final,SITE_ID))))

#save the new data frame 
save(df.final, file = '/mnt/workspace/rstudio_projects/gdmt_11_final.RData')





