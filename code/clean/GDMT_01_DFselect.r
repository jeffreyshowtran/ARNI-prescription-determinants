
getwd() 
setwd("/mnt/workspace/GWTG/HF/hf_data_challenge_2021/v1_2021-03/data/")

#install.packages("tidyverse")

library(tidyverse)
library(haven)

df <- read_sas(data_file = "hf_data_challenge.sas7bdat", 
               col_select = c('ACEDISCCONTRASPEC_Missing', 'ACEIdisc', 'ADMITSOURCEi', 'AGEi', 'AHA_ACEDISCCONTRASPEC_1', 
                              'AHA_ACEDISCCONTRASPEC_2', 'AHA_ACEDISCCONTRASPEC_3', 'AHA_ACEDISCCONTRASPEC_4', 'AHA_ACEDISCCONTRASPEC_5', 
                              'AHA_ARB', 'AHA_ARBDISCCONTRASPEC_1', 'AHA_ARBDISCCONTRASPEC_2', 'AHA_ARBDISCCONTRASPEC_3', 
                              'AHA_ARBDISCCONTRASPEC_4', 'AHA_ARBDISCCONTRASPEC_5', 'AHA_BBPRIOR', 'AHA_BETADISCONTRATYPE_1', 
                              'AHA_BETADISCONTRATYPE_2', 'AHA_BETADISCONTRATYPE_3', 'AHA_BETADISCONTRATYPE_4', 'AHA_BETADISCONTRATYPE_5',
                              'AHA_BETADISCONTRATYPE_6', 'AHA_BETADISCONTRATYPE_7', 'AHA_DIAGDM', 'AHA_DIAGNOSIS', 
                              'AHA_NOPRIORMEDS', 'AHA_OTHDIURETIC', 'AHA_OTHLOOP', 'AHA_OTHNITRATE', 'AHA_OTHTHIAZIDE', 
                              'AHA_PROCEDURES_1', 'AHA_PROCEDURES_2', 'AHA_PROCEDURES_3', 'AHA_PROCEDURES_4', 'AHA_PROCEDURES_10', 
                              'AHA_PROCEDURES_11', 'AHA_PROCEDURES_108', 'AHA_PROCEDURES_116', 'AHA_PROCEDURES_117', 
                              'AHA_PROCEDURES_118', 'AHA_PROCEDURES_119', 'AHA_PROCEDURES_120', 'AHA_PROCEDURES_123', 
                              'AHA_PROCEDURES_124', 'AHA_PROCEDURES_112', 'AHA_PROCEDURES_125', 'AHA_SPECIAL', 'ALDOdisc', 
                              'ARBDISCCONTRASPEC_Missing', 'ARBdisc', 'BBdisc', 'BEDSIZE', 'BETADISCONTRATYPE_Missing', 
                              'BNPi_disc', 'CONTRA_ALDOSTERONE_Missing', 'CRCL_disc', 'DISPOSITIONi', 'DYN_ATRIALFIB', 
                              'DYN_MEDHISTNONE', 'DYN_MEDHIST_1', 'DYN_MEDHIST_2', 'DYN_MEDHIST_3', 'DYN_MEDHIST_6', 
                              'DYN_MEDHIST_7', 'DYN_MEDHIST_8', 'DYN_MEDHIST_9', 'DYN_MEDHIST_10', 'DYN_MEDHIST_11', 
                              'DYN_MEDHIST_12', 'DYN_MEDHIST_13', 'DYN_MEDHIST_14', 'DYN_MEDHIST_15', 'DYN_MEDHIST_16', 
                              'DYN_MEDHIST_17', 'DYN_MEDHIST_19', 'DYN_MEDHIST_22', 'DYN_MEDHIST_23', 'DYN_MEDHIST_24', 
                              'DYN_MEDHIST_25', 'DYN_MEDHIST_26', 'DYN_MEDHIST_27', 'DYN_MEDHIST_28', 'DYN_MEDHIST_29', 
                              'DYN_MEDHIST_30', 'DYN_MEDHIST_31', 'DYN_MEDHIST_32', 'DYN_MEDHIST_36', 'DYN_OTHERCONDITION_1', 
                              'DYN_OTHERCONDITION_1':'DYN_OTHERCONDITION_8', 'Ejfrac40', 'GENDERi', 'GH_ACADEMIC', 'GH_HEART_TRANSPLANTS', 
                              'GH_INTERVENTIONAL', 'HEIGHTi', 'HFS_BUN_DC', 'HFS_MORTALITYRISK', 'HFS_ORALMEDS_1':'HFS_ORALMEDS_7', 
                              'HFS_OTHERIV', 'HFS_PTDOBUTAMINE', 'HFS_PTDOPAMINE', 'HFS_PTLOOP', 'HFS_PTMILRINONE', 
                              'HFS_PTNESIRITIDE', 'HFS_PTNITROGLYCERINE', 'HFS_PTVASOANT', 'HF_ACEIARBforLVSD', 'HF_ALDOANTAGONIST', 
                              'HF_BBforLVSD', 'HF_CLASSBB', 'HF_CONTRAISOSORBIDE', 'HF_CONTRA_ALDOSTERONE_DISC', 
                              'HF_CONTRA_ALDOSTER_DISC_LIST_1':'HF_CONTRA_ALDOSTER_DISC_LIST_7', 
                              'HF_EVIDENCEBBforLVSD', 'HF_HYDRALAZINEforLVSD', 'HF_ISOSORBIDE', 'HF_OTHCONTRAISOSORBIDE_1':'HF_OTHCONTRAISOSORBIDE_3',
                              'HF_OTHER_ALDOSTERONE_1':'HF_OTHER_ALDOSTERONE_5', 'HF_RACE_2':'HF_RACE_6', 'HF_RACE_8', 
                              'HF_RACE_Missing', 'HFrbpEF', 'HISP_Missing', 'HXVAP', 'HYDRAdisc', 'INSURANCEi', 'Ischemic', 
                              'JC_ACEDISC', 'JC_ACEDISCCONTRA', 'JC_ACESPE', 'JC_ADMITDATE', 'JC_ADMITSOURCE', 'JC_ARBDISC', 
                              'JC_ARBDISCCONTRA', 'JC_ASIAN_1':'JC_ASIAN_7', 'JC_BETADISC', 'JC_BETADISCCONTRA', 'JC_COMFORTONLY', 
                              'JC_COMFORTONLY2', 'JC_DISCDATETIME', 'JC_HAWPAC_1':'JC_HAWPAC_4', 'JC_HOMELESS', 'JC_HXSMOKING', 
                              'JC_LVSD', 'JC_TRANSOTHED', 'LOS', 'LVEF', 'MCNTRL', 'MCNTRLi', 'OH_ACE_INHIBITOR', 'OH_ALDOSTERONE_ANT', 
                              'OH_ALDOSTERONE_DISC', 'OH_ARB', 'OH_ARNI', 'OH_BETABLOCKER_DISC', 'OH_DISCBPDIAS', 
                              'OH_DISCBPSYST', 'OH_DISCHR', 'OH_DISCHRND', 'OH_EF', 'OH_EF_NA', 'OH_EXPIRED_CARDIO', 
                              'OH_HFHOSPADM', 'OH_HYDRALAZINE', 'OH_IF_EXP_DEATH', 'OH_LOOP', 'OH_NITRATE', 'OH_ISCHEMIC', 
                              'OH_NONISCHEMIC', 'OH_NONISCHEMIC_ETIOLOGY_1', 'OH_NONISCHEMIC_ETIOLOGY_3':'OH_NONISCHEMIC_ETIOLOGY_9', 
                              'OH_POTASSIUM_DISC', 'OH_POTASSIUM_DISC_NA', 'OH_POTASSIUM_U_DISC', 'OH_PRIORHF', 'OH_SCR_DISC',
                              'OH_SCR_DISC_NA', 'OH_SCR_U_DISC', 'OH_SYMPTOMS', 'OH_TRANSPLANT', 'OH_THIAZIDE', 'ORALMEDS_Missing', 
                              'OTHER_ALDOSTERONE_Missing', 'OTHFACILITYi', 'PATIENT_ID', 'PATIENT_OS_ID', 'PMTSRCE_Missing', 
                              'POTASSIUMi_disc', 'PREDMORT', 'RACEi', 'RCRDNUM', 'REGION', 'RESIDENTS', 'SCRi_disc', 
                              'SITESURG', 'SITE_ID', 'SITE_OS_ID', 'SITE_POSTAL_CODE', 'SODIUMi_disc', 'SP_ETHNIC', 'SP_RACE', 
                              'TransferIn', 'TransferOut', 'WEIGHTADMi', 'WEIGHTDISCi', 'ZIP', 'aha_admitsource', 'aha_arni', 
                              'aha_arniaceiarb', 'aha_arniclass1', 'aha_arniclass4', 'aha_arnidisccontraspec_1':'aha_arnidisccontraspec_8', 
                              'aha_arnionset', 'jc_arnidisc', 'jc_arnidisccontra', 'jc_hisp_1':'jc_hisp_4', 'race2i'))
                              
#write_sas(df, path ='df_GDMT.sas7bdat')



