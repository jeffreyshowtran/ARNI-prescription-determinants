getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#library(dplyr)
#library(labelled)

load('gdmt_6_Rx_pvar.RData')
names(df_gdmt_Rx_pvar)

#covars: 
  #HOSP: ADMITSOURCEi, JC_ADMITDATE, JC_ADMITSOURCE, JC_DISCDATETIME, JC_TRANSOTHED, LOS, 
  #      TransferIn, aha_admitsource
  #demo: AGEi, (compare: SITE_POSTAL_CODE AND ZIP)
  #prior Rx: AHA_BBPRIOR, AHA_NOPRIORMEDS, OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, OH_ARB, 
  #          OH_ARNI, OH_hydralazine, OH_LOOP, OH_NITRATE, OH_THIAZIDE
  #dx: AHA_DIAGNOSIS, DYN_ATRIALFIB, DYN_MEDHISTNONE:DYN_MEDHIST_36, 
  #    DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, HXVAP, JC_HXSMOKING
  #dc Rx: AHA_OTHDIURETIC, AHA_OTHLOOP, AHA_OTHNITRATE, OH_OTHTHIAZIDE, HF_ISOSORBIDE, 
  #       HF_HYDRALAZINEforLVSD      
  #inhosp proc: AHA_PROCEDURES_1:AHA_PROCEDURES_4, AHA_PROCEDURES_10, AHA_PROCEDURES_11,
  #             AHA_PROCEDURES_108, AHA_PROCEDURES_112, AHA_PROCEDURES_116:AHA_PROCEDURES_120, 
  #             AHA_PROCEDURES_123:AHA_PROCEDURES_125 
  #dispo: AHA_SPECIAL, DISPOSITIONi, OTHFACILITYi
  #site: BEDSIZE,GH_ACADEMIC, GH_HEART_TRANSPLANTS, GH_INTERVENTIONAL, MCNTRL, MCNTRLi, 
  #      RESIDENTS, SITESURG,
  #labs: BUNi_disc, HFS_BUN_DC, SODIUMi_disc
  #      OH_POTASSIUM_DISC, OH_POTASSIUM_U_DISC, POTASSIUMi_disc
  #      CRCL_disc, OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc
  #VS: HEIGHTi, OH_DISCBPDIAS, OH_DISCBPSYST, OH_DISCHR, WEIGHTADMi, WEIGHTDISCi
  #clinical: HFS_MORTALITYRISK, OH_EF, OH_HFHOPSADM, OH_ISCHEMIC, OH_NONISCHEMIC, 
  #          OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, OH_symptoms, OH_TRANSPLANT, 
  #          PREDMORT, aha_arniclass1, aha_arniclass4, aha_arnionset
  #inhospRx: HFS_ORALMEDS_1:HFS_ORALMEDS_7, HFS_PTDOPAMINE, HFS_PTMILRINONE, HFS_PTNESIRITIDE, 
  #          HFS_PTNITROGLYCERINE, HFS_PTVASOANT, HFS_PTDOBUTAMINE, HFS_PTLOOP, HFS_OTHERIV, 
  #          ORALMEDS_Missing
  #cluster: REGION, SITE_ID, PATIENT_ID

#new variables: 
# covar_OutsideZIP, covar_dispo, covar_Kdisc, covar_Crdisc, covar_dWeight, covar_BMI, covar_ino

#discard variables 
# JC_ADMITSOURCE, transferIn, aha_admitsource, AHA_SPECIAL, DISPOSITIONi, OTHFACILITYi, 
# MCNTRL, HFS_BUN_DC, OH_POTASSIUM_DISC, OH_POTASSIUM_U_DISC, POTASSIUMi_disc, 
# OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc

#bring in BUNi_disc (wasn't imported into the initial dataset)
df_BUNi <- read_sas(data_file = "/mnt/workspace/GWTG/HF/hf_data_challenge_2021/v1_2021-03/data/hf_data_challenge.sas7bdat", 
                              col_select =c('RCRDNUM', 'BUNi_disc'))

addmargins(table(df_BUNi$BUNi_disc, exclude = NULL))

df_gdmt_Rx_pvar <- merge(df_gdmt_Rx_pvar, df_BUNi, by = 'RCRDNUM', all=FALSE)
#view(UNi)                    
rm(df_BUNi)
#labels
df_gdmt_Rx_pvar$ADMITSOURCEi <- factor(df_gdmt_Rx_pvar$ADMITSOURCEi, 
                                       levels = c(2:14), 
                                       labels = c('Non-Health Care Facility Point of Origin', 
                                                  'Clinic', 
                                                  'HMO referral', 
                                                  'Transfer From a Different Hospital', 
                                                  'Transfer From SNF of ICF', 
                                                  'Transfer from another Health Care Facility', 
                                                  'Emergency room', 'Court/law enforcement',
                                                  'Information not available',
                                                  'Transfer from a critical access hospital',
                                                  'IntraHospital Transfer w/ separate claim',
                                                  'Transfer from Ambulatory Surgery Center ', 
                                                  'Transfer from Hospice (still hospice GoC)'))
addmargins(table(df_gdmt_Rx_pvar$ADMITSOURCEi, exclude=NULL))

#need to exclude all those in hospice care! 
df_gdmt_Rx_pvar <- subset(df_gdmt_Rx_pvar, 
                           df_gdmt_Rx_pvar$ADMITSOURCEi != 'Transfer from Hospice (still hospice GoC)' |
                           is.na(df_gdmt_Rx_pvar$ADMITSOURCEi))

df_gdmt_Rx_pvar$AHA_DIAGNOSIS <- factor(df_gdmt_Rx_pvar$AHA_DIAGNOSIS, 
                                       levels = c(3:6,8,9,100,127:130), 
                                       labels = c('HF, 2ndary dx', 'HF, no CAD', 'CAD', 'UA', 
                                                  'cerebral vascular dz', 'PVD', 'Other', 
                                                  'AMI-NOS', 'AMI-STEMI', 'AMI-NSTEMI', 'HF+CAD'))

df_gdmt_Rx_pvar$HXVAP <- factor(df_gdmt_Rx_pvar$HXVAP, 
                                levels = c(1,2),
                                labels = c('Yes', 'No/ND'))
                                
df_gdmt_Rx_pvar$MCNTRL <- factor(df_gdmt_Rx_pvar$MCNTRL, 
                                 levels = c(12:16, 21:23, 30:33, 41:47), 
                                 labels = c('state', 'county', 'city', 'city-county', 'Hospital district or authority',
                                            'Church operated', 'NG-NP Catholic controlled', 'other church', 
                                            'for profit, investor-owned', 'individual', 'partnership', 'corporation', 
                                            'airforce', 'army', 'navy', 'Public Health Service', 'Veterans Affairs', 
                                            'other Federal', 'Public Health Service Indian Service'))

df_gdmt_Rx_pvar$MCNTRLi <- factor(df_gdmt_Rx_pvar$MCNTRLi, 
                                  levels = c(1:4), 
                                  labels = c('nonfed government', 'nongov, nonprofit', 
                                             'investor-owned, for profit', 'federal goverment'))

df_gdmt_Rx_pvar$OH_POTASSIUM_U_DISC <- factor(df_gdmt_Rx_pvar$OH_POTASSIUM_U_DISC, 
                                              levels = c(1,2,3), 
                                              labels = c('mEq/L', 'mmol/L', 'mg/dL'))

df_gdmt_Rx_pvar$OH_SCR_U_DISC <- factor(df_gdmt_Rx_pvar$OH_SCR_U_DISC, 
                                        levels = c(2,3), 
                                        labels = c('mg/dL', 'micromol/L'))

df_gdmt_Rx_pvar$OH_HFHOSPADM <- factor(df_gdmt_Rx_pvar$OH_HFHOSPADM, 
                                       levels = c(1:5), 
                                       labels = c('0', '1', '2', '>2', 'unknown'))

df_gdmt_Rx_pvar$REGION <- factor(df_gdmt_Rx_pvar$REGION, 
                                 levels = c(1:4), 
                                 labels = c('Northweast', 'Midwest', 'South', 'West'))

#hosp summaries-------------------------------------------------------
addmargins(table(df_gdmt_Rx_pvar$JC_ADMITSOURCE, exclude=NULL))
  #JC_ADMITSOURCE makese no sense and has too much missing data- should be discarded 

#mean length of stay 
class(df_gdmt_Rx_pvar$LOS)
mean(df_gdmt_Rx_pvar$LOS, na.rm = TRUE)
  #mean LoS is 5.2days 

addmargins(table(df_gdmt_Rx_pvar$ADMITSOURCEi, df_gdmt_Rx_pvar$TransferIn, exclude=NULL))
  #transferIn is a poor variable taht is not consistent 

addmargins(table(df_gdmt_Rx_pvar$JC_ADMITSOURCE, df_gdmt_Rx_pvar$aha_admitsource, exclude=NULL))
  #i don't think the data dictionary provides proper coding for JC_ADMITSOURCE and aha_admitsource


#demo-------------------------------------------------------
class(df_gdmt_Rx_pvar$AGEi)
mean(df_gdmt_Rx_pvar$AGEi, na.rm = TRUE)
  #mean age is 67.1years old 

#ZIP vs SITE_POSTAL_CODE - are they the same? if not this may be a predictor of poverty
df_gdmt_Rx_pvar <-
  df_gdmt_Rx_pvar %>% 
  mutate(covar_OutsideZIP = ifelse(SITE_POSTAL_CODE != ZIP, 1, 0))
table(df_gdmt_Rx_pvar$covar_OutsideZIP, exclude=NULL)

sum(is.na(df_gdmt_Rx_pvar$SITE_POSTAL_CODE))
var_label(df_gdmt_Rx_pvar$covar_OutsideZIP) <- "Pt vs Hosp ZIP different? 1 yes, 0 no"

#prior Rx-------------------------------------------------------
#prior Rx: AHA_BBPRIOR, AHA_NOPRIORMEDS, OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, OH_ARB, 
#          OH_ARNI, OH_hydralazine, OH_LOOP, OH_NITRATE, OH_THIAZIDE
lapply(subset(df_gdmt_Rx_pvar, select= c(AHA_BBPRIOR, AHA_NOPRIORMEDS, 
                                         OH_ACE_INHIBITOR, OH_ALDOSTERONE_ANT, 
                                         OH_ARB)), function(x) addmargins(table(x, exclude=NULL)))
  #42,197 were on a BB prior (unknown if HFBB)
  #18,296 were on an ACEI prior 
  #8773 were on an ARB prior 
  #11,915 were on an MRA prior 
  #4491 were on no meds at all 
  #info not captured in 144,722

#other Dx-------------------------------------------------------
#dx: AHA_DIAGNOSIS, DYN_ATRIALFIB, DYN_MEDHISTNONE:DYN_MEDHIST_36, 
#    DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, HXVAP, JC_HXSMOKING

var_label(df_gdmt_Rx_pvar$AHA_DIAGNOSIS) <- "Cardiac Diagnosis"
addmargins(table(df_gdmt_Rx_pvar$AHA_DIAGNOSIS, exclude=NULL))

var_label(df_gdmt_Rx_pvar$DYN_ATRIALFIB) <- "dx: atrial fib?"
var_label(df_gdmt_Rx_pvar$DYN_MEDHISTNONE) <- "no medical history?"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_1) <- "dx: atrial fibrillation?" 
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_2) <- "dx: atrial flutter?" 
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_3) <- "dx: COPD | asthma" 
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_6) <- "dx: HLD"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_7) <- "dx: HTN"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_8) <- "dx: PVD"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_9) <- "dx: CAD"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_10) <- "dx: prior MI"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_11) <- "dx: CVA/TIA"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_12) <- "dx: ICD only"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_13) <- "dx: heart failure"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_14) <- "dx: anemia"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_15) <- "dx: PPM"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_16) <- "dx: chronic dialysis"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_17) <- "dx: CKD (SCr >=2.0)"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_19) <- "dx: depression"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_22) <- "dx: prior PCI"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_23) <- "dx: prior CABG"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_24) <- "dx: valvular heart dz" 
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_25) <- "dx: h/o CABG vs PCI (unknown)"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_26) <- "dx: CRT-P only"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_27) <- "dx: CRT-D"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_28) <- "dx: LVAD"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_29) <- "dx: CardioMEMS"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_30) <- "dx: Sleep-disordered breathing"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_31) <- "dx: diabetes"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_32) <- "dx: familial HLD"
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_36) <- "dx: emerging infectious disease"


table(df_gdmt_Rx_pvar$DYN_MEDHIST_1, df_gdmt_Rx_pvar$DYN_ATRIALFIB, exclude=NULL)

lapply(subset(df_gdmt_Rx_pvar, select= c(DYN_MEDHISTNONE:DYN_MEDHIST_36)), 
       function(x) 
         {list(
          var_label(x),
          addmargins(table(x, exclude=NULL))
         )}
       )
var_label(df_gdmt_Rx_pvar$DYN_MEDHIST_10)

var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_1) <- "HF exacerbation: ACS/ischemia"
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_2) <- "HF exacerbation: uncontrolled HTN"
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_3) <- "HF exacerbation: pna/respiratory process"
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_4) <- "HF exacerbation: worsening renal failure"
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_5) <- "HF exacerbation: arrhyhthmia" 
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_6) <- "HF exacerbation: dietary noncompliance"         
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_7) <- "HF exacerbation: medication noncompliance"
var_label(df_gdmt_Rx_pvar$DYN_OTHERCONDITION_8) <- "HF exacerbation: other"

lapply(subset(df_gdmt_Rx_pvar, select= c(DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8)), 
       function(x) 
       {
         var_label(x)
         addmargins(table(x, exclude=NULL))
       }
)

addmargins(table(df_gdmt_Rx_pvar$HXVAP, exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$JC_HXSMOKING, exclude=NULL))

#dx Rx-------------------------------------------------------
#dc Rx: AHA_OTHDIURETIC, AHA_OTHLOOP, AHA_OTHNITRATE, OH_OTHTHIAZIDE, HF_ISOSORBIDE, 
#       HF_HYDRALAZINEforLVSD    
names(df_gdmt_Rx_pvar)
lapply(subset(df_gdmt_Rx_pvar, select=c(AHA_OTHDIURETIC, AHA_OTHLOOP, 
                                        AHA_OTHNITRATE, AHA_OTHTHIAZIDE, 
                                        HF_ISOSORBIDE, HF_HYDRALAZINEforLVSD)),
       function(x) addmargins(table(x, exclude=NULL)))


#inhosp proc-------------------------------------------------------
#inhosp proc: AHA_PROCEDURES_1:AHA_PROCEDURES_4, AHA_PROCEDURES_10, AHA_PROCEDURES_11,
#             AHA_PROCEDURES_108, AHA_PROCEDURES_112, AHA_PROCEDURES_116:AHA_PROCEDURES_120, 
#             AHA_PROCEDURES_123:AHA_PROCEDURES_125 


var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_1) <- "inhosp proc: none"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_2) <- "inhosp proc: coronary angiogram, no PCI"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_3) <- "inhosp proc: PCI, no stent"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_4) <- "inhosp proc: PCI + stent" 
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_10) <- "inhosp proc: CABG"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_11) <- "inhosp proc: valve surgery"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_108) <- "inhosp proc: RHC"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_112) <- "inhosp proc: PPM"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_116) <- "inhosp proc: HD | UF"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_117) <- "inhosp proc: IABP"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_118) <- "inhosp proc: LVAD/L-MCS"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_119) <- "inhosp proc: ventilator"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_120) <- "inhosp proc: OHT"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_123) <- "inhosp proc: HD"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_124) <- "inhosp proc: UF"
var_label(df_gdmt_Rx_pvar$AHA_PROCEDURES_125) <- "inhosp proc: CRT-P"

lapply(subset(df_gdmt_Rx_pvar, select=c(AHA_PROCEDURES_1:AHA_PROCEDURES_125)), 
       function(x) {
         var_label(x) 
         addmargins(table(x, exclude=NULL))
         }
       )


#dispo-------------------------------------------------------
#dispo: AHA_SPECIAL, DISPOSITIONi, OTHFACILITYi

addmargins(table(df_gdmt_Rx_pvar$AHA_SPECIAL,exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$DISPOSITIONi,exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$OTHFACILITYi,exclude=NULL))

addmargins(table(df_gdmt_Rx_pvar$AHA_SPECIAL, df_gdmt_Rx_pvar$DISPOSITIONi, exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$AHA_SPECIAL, df_gdmt_Rx_pvar$OTHFACILITYi, exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$DISPOSITIONi, df_gdmt_Rx_pvar$OTHFACILITYi, exclude=NULL))

  #AHA special only pertains to patients returning to self care
  #Disposition groups self care into home and then gives a bunch of other places 
  #other facility describes where patients go when sent to "other care facilities" 

df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_dispo = ifelse(DISPOSITIONi == 'Other Care Facility' 
                              & OTHFACILITYi == 'SNF', 'SNF', 
                       ifelse(DISPOSITIONi == 'Other Care Facility' 
                              & OTHFACILITYi == 'IRF', 'IRF', 
                       ifelse(DISPOSITIONi == 'Other Care Facility' 
                              & OTHFACILITYi == 'LTCH', 'LTCH', 
                       ifelse(DISPOSITIONi == 'Other Care Facility' 
                              & OTHFACILITYi == 'ICF', 'ICF', 
                       ifelse(DISPOSITIONi == 'Other Care Facility' 
                              & OTHFACILITYi == 'Other', 'Other Care Facility', 
                       ifelse(DISPOSITIONi == 'Home' 
                              & AHA_SPECIAL == 'Homeless', 'Homless', 
                       ifelse(DISPOSITIONi == 'Home' 
                              & AHA_SPECIAL == 'DoC', 'DoC',
                       ifelse(DISPOSITIONi == 'Home' 
                              & AHA_SPECIAL == 'International', 'International',
                       ifelse(DISPOSITIONi == 'Home' 
                              & AHA_SPECIAL == 'None/UTD', 'Home-UTD',
                       ifelse(DISPOSITIONi == 'Home' 
                              & AHA_SPECIAL == 'Home Health Care', 'Home Health care',
                       ifelse(DISPOSITIONi == 'Home' 
                              & is.na(AHA_SPECIAL), 'home', 
                       ifelse(DISPOSITIONi == 'Acute Care Facility', 'Acute Care Facility',
                       ifelse(DISPOSITIONi == 'None/UTD', 'None/UTD', 'error'))))))))))))))

addmargins(table(df_gdmt_Rx_pvar$covar_dispo, exclude = NULL))

#site-------------------------------------------------------
#site: BEDSIZE,GH_ACADEMIC, GH_HEART_TRANSPLANTS, GH_INTERVENTIONAL, MCNTRL, MCNTRLi, 
#      RESIDENTS, SITESURG

class(df_gdmt_Rx_pvar$BEDSIZE)
mean(df_gdmt_Rx_pvar$BEDSIZE, na.rm = TRUE)
sum(is.na(df_gdmt_Rx_pvar$BEDSIZE))

addmargins(table(df_gdmt_Rx_pvar$GH_ACADEMIC, exclude = NULL))
addmargins(table(df_gdmt_Rx_pvar$GH_HEART_TRANSPLANTS, exclude = NULL))
addmargins(table(df_gdmt_Rx_pvar$GH_INTERVENTIONAL, exclude = NULL))
addmargins(table(df_gdmt_Rx_pvar$GH_ACADEMIC, df_gdmt_Rx_pvar$GH_HEART_TRANSPLANTS, exclude = NULL))

addmargins(table(df_gdmt_Rx_pvar$MCNTRL, exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$MCNTRLi, exclude=NULL))
addmargins(table(df_gdmt_Rx_pvar$MCNTRL, df_gdmt_Rx_pvar$MCNTRLi, exclude=NULL))
  #MCNTRLi is probably a better variable - has less categories and encompasses all 

addmargins(table(df_gdmt_Rx_pvar$RESIDENTS, exclude=NULL))
  #10,6127 sites have residents 
addmargins(table(df_gdmt_Rx_pvar$GH_ACADEMIC, df_gdmt_Rx_pvar$RESIDENTS, exclude = NULL))

addmargins(table(df_gdmt_Rx_pvar$SITESURG, exclude=NULL))
  #121064 sites have CT surgery on site 


#labs-------------------------------------------------------
# BUNi_disc, HFS_BUN_DC, SODIUMi_disc
#      OH_POTASSIUM_DISC, OH_POTASSIUM_U_DISC, POTASSIUMi_disc
#      CRCL_disc, OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc

mean(df_gdmt_Rx_pvar$BUNi_disc, na.rm=TRUE)
sum(is.na(df_gdmt_Rx_pvar$BUNi_disc))
  #mean BUN is 32.0

mean(df_gdmt_Rx_pvar$HFS_BUN_DC, na.rm=TRUE)
sum(is.na(df_gdmt_Rx_pvar$HFS_BUN_DC))
  #mean BUN is 32.6

sum(is.na(df_gdmt_Rx_pvar$BUNi_disc) & is.na(df_gdmt_Rx_pvar$HFS_BUN_DC))
sum(!is.na(df_gdmt_Rx_pvar$BUNi_disc) & is.na(df_gdmt_Rx_pvar$HFS_BUN_DC))
sum(is.na(df_gdmt_Rx_pvar$BUNi_disc) & !is.na(df_gdmt_Rx_pvar$HFS_BUN_DC))


mean(df_gdmt_Rx_pvar$SODIUMi_disc, na.rm=TRUE)
  #mean Na 137.6
sum(is.na(df_gdmt_Rx_pvar$SODIUMi_disc))
  #there are 150,844 missing 

mean(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC, na.rm=TRUE)
  #mean K is 4.1/but can't take blindly as different units (see below)
sum(is.na(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC))
  #there are 151801 missing values

addmargins(table(df_gdmt_Rx_pvar$OH_POTASSIUM_U_DISC, exclude=NULL))
  #lots of missing; different units of measure so can't take the mean blindly 

mean(df_gdmt_Rx_pvar$POTASSIUMi_disc, na.rm=TRUE)
  #mean is 4.1 
sum(is.na(df_gdmt_Rx_pvar$POTASSIUMi_disc))
  #missing 151808 

sum(is.na(df_gdmt_Rx_pvar$POTASSIUMi_disc) & is.na(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC))
sum(!is.na(df_gdmt_Rx_pvar$POTASSIUMi_disc) & is.na(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC))
sum(is.na(df_gdmt_Rx_pvar$POTASSIUMi_disc) & !is.na(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC))
  #there are 151801 pts with missing K
  #there are no values where OH_POTASSIUM is missing and POTASSIUM_disc has a value (nonNA)
  #there are 7 values where OH_POTASSIUM has a value and POTASSIUM_disc is missing 

with(subset(df_gdmt_Rx_pvar, is.na(POTASSIUMi_disc) & !is.na(OH_POTASSIUM_DISC)), 
     addmargins(table(OH_POTASSIUM_U_DISC, OH_POTASSIUM_DISC, exclude=NULL))
     )
  #there are no units on the missing values anyways... but honestly they look like mEq/L


#create a new K variable merging i_disc and OH_ 
df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_Kdisc = ifelse(is.na(POTASSIUMi_disc) & !is.na(OH_POTASSIUM_DISC), 
                              OH_POTASSIUM_DISC, POTASSIUMi_disc))

sum(is.na(df_gdmt_Rx_pvar$covar_Kdisc))
  #151,801 missing 
mean(df_gdmt_Rx_pvar$covar_Kdisc, na.rm=TRUE)
  #mean is 4.0645
sum(is.na(df_gdmt_Rx_pvar$covar_Kdisc) & is.na(df_gdmt_Rx_pvar$OH_POTASSIUM_DISC))
sum(is.na(df_gdmt_Rx_pvar$covar_Kdisc) & is.na(df_gdmt_Rx_pvar$POTASSIUMi_disc))

#      CRCL_disc, OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc
mean(df_gdmt_Rx_pvar$CRCL_disc, na.rm=TRUE)
  #mean CrCl on dc was 58.76 
sum(is.na(df_gdmt_Rx_pvar$CRCL_disc))
  #there are 156,651 missing values 


mean(df_gdmt_Rx_pvar$OH_SCR_DISC, na.rm=TRUE)
  #mean Cr on dc was 1.65 (1.652115) but i don't know the units 
addmargins(table(df_gdmt_Rx_pvar$OH_SCR_U_DISC, exclude=NULL))
  #86 use micromol/L, the rest use mg/dL 
sum(is.na(df_gdmt_Rx_pvar$OH_SCR_U_DISC))
  #there are 149,673 missing values 

mean(df_gdmt_Rx_pvar$SCRi_disc, na.rm=TRUE) #units in mg/dL
  #mean is 1.65 (1.648755) 

sum(is.na(df_gdmt_Rx_pvar$SCRi_disc) & is.na(df_gdmt_Rx_pvar$OH_SCR_DISC) 
    & is.na(df_gdmt_Rx_pvar$CRCL_disc))
  #there are 149,691 patients missing a Cr value 
sum(is.na(df_gdmt_Rx_pvar$SCRi_disc) & !is.na(df_gdmt_Rx_pvar$OH_SCR_DISC))
  #there are 9 pts with a missing SCRi value but an existing OH_SCR value 
sum(!is.na(df_gdmt_Rx_pvar$SCRi_disc) & is.na(df_gdmt_Rx_pvar$OH_SCR_DISC))
  #there are 0 pts with an existing SCRi value but a missing OH_SCR value 

with(subset(df_gdmt_Rx_pvar, is.na(SCRi_disc) & !is.na(OH_SCR_DISC)), 
     addmargins(table(OH_SCR_U_DISC, OH_SCR_DISC, exclude=NULL))
     )
  #as usual, there are no units associated wit the above Cr levels but they appear to be mg/dL

#create a new SCr variable based on i_disc and OH_ 
df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_Crdisc = ifelse(is.na(SCRi_disc) & !is.na(OH_SCR_DISC), 
                               OH_SCR_DISC, SCRi_disc))

sum(is.na(df_gdmt_Rx_pvar$covar_Crdisc))
#149,491 missing 
mean(df_gdmt_Rx_pvar$covar_Crdisc, na.rm=TRUE)
#mean is 1.648753
sum(is.na(df_gdmt_Rx_pvar$covar_Crdisc) & is.na(df_gdmt_Rx_pvar$OH_SCR_DISC))
sum(is.na(df_gdmt_Rx_pvar$covar_Crdisc) & is.na(df_gdmt_Rx_pvar$SCRi_disc))

#vital signs-------------------------------------------------------
#VS: HEIGHTi, OH_DISCBPDIAS, OH_DISCBPSYST, OH_DISCHR, WEIGHTADMi, WEIGHTDISCi

sum(is.na(df_gdmt_Rx_pvar$HEIGHTi))
  #129967 missing 
mean(df_gdmt_Rx_pvar$HEIGHTi, na.rm=TRUE)
  #mean ht 171.5cm 

sum(is.na(df_gdmt_Rx_pvar$OH_DISCBPSYST))
  #94589 missing 
mean(df_gdmt_Rx_pvar$OH_DISCBPSYST, na.rm=TRUE)
  #mean sBP 118.4mmHg

sum(is.na(df_gdmt_Rx_pvar$OH_DISCBPDIAS))
  #94714 missing 
mean(df_gdmt_Rx_pvar$OH_DISCBPDIAS, na.rm=TRUE)
  #mean sBP 70.0mmHg  

sum(is.na(df_gdmt_Rx_pvar$OH_DISCHR))
  #95.671 missing 
mean(df_gdmt_Rx_pvar$OH_DISCHR, na.rm=TRUE)
  #mean HR 79.1bpm

sum(is.na(df_gdmt_Rx_pvar$WEIGHTADMi))
  #126310 missing 
mean(df_gdmt_Rx_pvar$WEIGHTADMi, na.rm=TRUE)
  #mean admission weight 89.0kg 

sum(is.na(df_gdmt_Rx_pvar$WEIGHTDISCi))
#143556 missing 
mean(df_gdmt_Rx_pvar$WEIGHTDISCi, na.rm=TRUE)
#mean discharge weight 86.1kg 

#create change in weight variable (delta weight); (-) means you lost weight during admission 
df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_dWeight = WEIGHTDISCi - WEIGHTADMi)
sum(is.na(df_gdmt_Rx_pvar$covar_dWeight)) #144,180 missing 
mean(df_gdmt_Rx_pvar$covar_dWeight, na.rm=TRUE) #mean is -3.2kg lost during admission 

#create discharge BMI variable 
df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_BMI = WEIGHTDISCi/(HEIGHTi/100)^2)
sum(is.na(df_gdmt_Rx_pvar$covar_BMI)) #147,446 missing 
mean(df_gdmt_Rx_pvar$covar_BMI, na.rm=TRUE) #mean BMI is 29.2 on dc 


#clinical-------------------------------------------------------
#clinical: HFS_MORTALITYRISK, OH_EF, OH_HFHOSPADM, OH_ISCHEMIC, OH_NONISCHEMIC, 
#          OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, OH_symptoms, OH_TRANSPLANT, 
#          PREDMORT, aha_arniclass1, aha_arniclass4, aha_arnionset

sum(is.na(df_gdmt_Rx_pvar$HFS_MORTALITYRISK)) #GWTG HF Mortality Risk Score
  #147,605 missing values 
mean(df_gdmt_Rx_pvar$HFS_MORTALITYRISK, na.rm=TRUE)
  #mean mortality risk is 2.82752    
table(df_gdmt_Rx_pvar$HFS_MORTALITYRISK, exclude=NULL)

sum(is.na(df_gdmt_Rx_pvar$OH_EF))
table(df_gdmt_Rx_pvar$OH_EF, exclude=NULL) 


sum(is.na(df_gdmt_Rx_pvar$OH_HFHOSPADM)) #No. of hospitalizations in last 6 months 
  #153,009 missing values 
table(df_gdmt_Rx_pvar$OH_HFHOSPADM, exclude=NULL) 


table(df_gdmt_Rx_pvar$OH_ISCHEMIC, exclude=NULL) 
  #34,987 ischemic HF patients 


table(df_gdmt_Rx_pvar$OH_NONISCHEMIC, exclude=NULL) 
  #34,629 nonischemic HF patients 


var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_1) <- 'hypertensive'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_3) <- 'EtOH/other drug'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_4) <- 'chemotherapy'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_5) <- 'viral'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_6) <- 'postpartum'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_7) <- 'familial'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_8) <- 'other etiology'
var_label(df_gdmt_Rx_pvar$OH_NONISCHEMIC_ETIOLOGY_9) <- 'unknown etiology'

lapply(subset(df_gdmt_Rx_pvar, select= c(OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9)), 
       function(x) 
       {
         var_label(x)
         addmargins(table(x, exclude=NULL))
       }
)

df_gdmt_Rx_pvar$OH_TRANSPLANT[is.na(df_gdmt_Rx_pvar$OH_TRANSPLANT)] <- 0
table(df_gdmt_Rx_pvar$OH_TRANSPLANT, exclude=NULL) 
  #242 TRANSPLANT patients 


#discharge symptoms 
df_gdmt_Rx_pvar$OH_SYMPTOMS <- factor(df_gdmt_Rx_pvar$OH_SYMPTOMS, 
                                      levels = c(2:6),
                                      labels = c('worse', 'unchanged', 'better- symptomatic', 
                                                 'better- asymptomatic', 'UTD'))
table(df_gdmt_Rx_pvar$OH_SYMPTOMS, exclude=NULL) 
  #157303 missing values 

sum(is.na(df_gdmt_Rx_pvar$PREDMORT)) 
  #146266 mising
mean(df_gdmt_Rx_pvar$PREDMORT, na.rm=TRUE)
  #average predicted mortality rate is 2.77% 


table(df_gdmt_Rx_pvar$aha_arniclass1, exclude=NULL) 
table(df_gdmt_Rx_pvar$aha_arniclass4, exclude=NULL) 
table(df_gdmt_Rx_pvar$aha_arniclass1, df_gdmt_Rx_pvar$aha_arniclass4, exclude=NULL) 
#not super useful wayyy too much missing data 

table(df_gdmt_Rx_pvar$aha_arnionset, exclude=NULL) 
  #6699 new onset HF patients 

#inhospRx-------------------------------------------------------
#inhospRx: HFS_ORALMEDS_1:HFS_ORALMEDS_7, HFS_PTDOPAMINE, HFS_PTMILRINONE, HFS_PTNESIRITIDE, 
#          HFS_PTNITROGLYCERINE, HFS_PTVASOANT, HFS_PTDOBUTAMINE, HFS_PTLOOP, HFS_OTHERIV, 
#          ORALMEDS_Missing

var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_1) <- 'ACEI'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_2) <- 'ARB'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_3) <- 'BB'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_4) <- 'MRA'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_5) <- 'hydral/nitrate'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_6) <- 'none'
var_label(df_gdmt_Rx_pvar$HFS_ORALMEDS_7) <- 'ARNI'

lapply(subset(df_gdmt_Rx_pvar, select= c(HFS_ORALMEDS_1:HFS_ORALMEDS_7)), 
       function(x) 
       {
         var_label(x)
         addmargins(table(x, exclude=NULL))
       }
      )

names(df_gdmt_Rx_pvar)

lapply(subset(df_gdmt_Rx_pvar, select= c(HFS_PTDOPAMINE:HFS_OTHERIV)), 
       function(x) 
       {
         addmargins(table(x, exclude=NULL))
       }
      )

df_gdmt_Rx_pvar <- 
  df_gdmt_Rx_pvar %>% 
  mutate(covar_ino = ifelse(HFS_PTDOBUTAMINE==1 | HFS_PTMILRINONE==1 | 
                            HFS_PTNESIRITIDE==1 | HFS_PTDOPAMINE==1, 1, 0))
df_gdmt_Rx_pvar$covar_ino[is.na(df_gdmt_Rx_pvar$covar_ino)] <- 0
addmargins(table(df_gdmt_Rx_pvar$covar_ino, exclude=NULL))
  #198433 didnt' get INO, 7689 did 

#cluster-------------------------------------------------------
#cluster: REGION, SITE_ID, PATIENT_ID

addmargins(table(df_gdmt_Rx_pvar$REGION, exclude=NULL))

#How many unique sites are within df? 662 unique sites
length((unique(df_gdmt_Rx_pvar$SITE_ID)))

#How many unique patients  are within df? 179425 unique sites
length((unique(df_gdmt_Rx_pvar$PATIENT_ID)))

#clean up data set -----------------------------------------
# JC_ADMITSOURCE, transferIn, aha_admitsource, AHA_SPECIAL, DISPOSITIONi, OTHFACILITYi, 
# MCNTRL, HFS_BUN_DC, OH_POTASSIUM_DISC, OH_POTASSIUM_U_DISC, POTASSIUMi_disc, 
# OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc


df_gdmt_complete <- subset(df_gdmt_Rx_pvar, select = -c(JC_ADMITSOURCE, TransferIn, 
                                                        aha_admitsource, AHA_SPECIAL, 
                                                        DISPOSITIONi, OTHFACILITYi, 
                                                        MCNTRL, HFS_BUN_DC, OH_POTASSIUM_DISC, 
                                                        OH_POTASSIUM_U_DISC, POTASSIUMi_disc,
                                                        OH_SCR_DISC, OH_SCR_U_DISC, SCRi_disc))

#save the new data frame 
save(df_gdmt_complete, file = '/mnt/workspace/rstudio_projects/gdmt_7_complete.RData')



