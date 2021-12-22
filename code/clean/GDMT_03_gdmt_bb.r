getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)
library(gmodels)
library(expss)

load('gdmt_2_include.RData')
view(df_gdmt)


#LABELS: 
df_gdmt$BBdisc <- factor(df_gdmt$BBdisc, 
                          levels = c(1,2,3), 
                          labels = c('Yes- prescribed', 'No- not prescribed', 'No- contraindicated'))

df_gdmt$HF_CLASSBB <- factor(df_gdmt$HF_CLASSBB, 
                             levels = c(1,2,3), 
                             labels = c('HF BB', 'nonHF BB', 'unknown class'))

df_gdmt$OH_BETABLOCKER_DISC <- factor(df_gdmt$OH_BETABLOCKER_DISC, 
                                      levels = c(2,3,4,5,6,7,9,10,11,12,13,14,15),
                                      labels = c('atenolol', 'bisoprolol', 'carvedilol', 'MTPtar', 'MTPsucc', 
                                                 'other', 'timolol', 'cartelol', 'propranolol', 'betaxolol', 
                                                 'penbutolol', 'acebutolol', 'pindolol'))

#_______________________________________________#
#let's look at beta blockers 
df_bb <- subset(df_gdmt, select = c(PATIENT_ID, AHA_BETADISCONTRATYPE_1:AHA_BETADISCONTRATYPE_7, BBdisc, 
                                    BETADISCONTRATYPE_Missing, HF_BBforLVSD, HF_CLASSBB, 
                                    HF_EVIDENCEBBforLVSD, JC_BETADISC, JC_BETADISCCONTRA, 
                                    OH_BETABLOCKER_DISC))
view(df_bb)

df_list <- subset(df_bb, select = -c(PATIENT_ID))
lapply(df_list, function(x) addmargins(table(x, exclude = NULL)))

addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$HF_EVIDENCEBBforLVSD, exclude = NULL)) 
#HF_EVIDENCEBBforLVSD not useful- too many missing
df_bb <- subset(df_bb, select = -c(HF_EVIDENCEBBforLVSD))

addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$JC_BETADISC, exclude = NULL)) 
#note that JC_BETADISC is ANY BB, not just HF BBs 

addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$BBdisc, exclude = NULL)) 
#interesting info, note that some patients for whom a BB was not prescribed did in fact receive a BB 

addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$HF_CLASSBB, exclude = NULL)) 
#HF_CLASSBB is not useful - it's just wrong... 
df_bb <- subset(df_bb, select = -c(HF_CLASSBB))

addmargins(table(df_bb$JC_BETADISC, df_bb$BBdisc, exclude = NULL)) 
#BBdisc is more useful than JC_BETADISC as JC_BETADISC has more missing values; they are fully in agreement 
df_bb <- subset(df_bb, select = -c(JC_BETADISC))


addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$JC_BETADISCCONTRA, exclude = NULL)) 

df_bb <- 
  df_bb %>% 
  mutate(BB_contra = ifelse(AHA_BETADISCONTRATYPE_1 == 1 | AHA_BETADISCONTRATYPE_2 == 1 | AHA_BETADISCONTRATYPE_3 == 1 | 
                            AHA_BETADISCONTRATYPE_4 == 1 | AHA_BETADISCONTRATYPE_5 == 1 | AHA_BETADISCONTRATYPE_6 == 1 | 
                            AHA_BETADISCONTRATYPE_7 == 1, 1, 0), .after=AHA_BETADISCONTRATYPE_7)

addmargins(table(df_bb$BB_contra,  exclude = NULL)) 
addmargins(table(df_bb$JC_BETADISCCONTRA, exclude = NULL))
addmargins(table(df_bb$JC_BETADISCCONTRA, df_bb$BB_contra, exclude = NULL))
addmargins(table(df_bb$BBdisc, df_bb$BB_contra, exclude = NULL))

#need to create my own HF_BB binary var based on OH_BETABLOCKER_DISC

df_bb <- 
  df_bb %>% 
  mutate(HF_BB = ifelse(OH_BETABLOCKER_DISC == 'bisoprolol' | OH_BETABLOCKER_DISC == 'carvedilol' | 
                        OH_BETABLOCKER_DISC == 'MTPsucc', 1, 
                        ifelse(is.na(OH_BETABLOCKER_DISC), NA, 0)))
                          
addmargins(table(df_bb$OH_BETABLOCKER_DISC, df_bb$HF_BB, exclude = NULL))                        
addmargins(table(df_bb$BBdisc, df_bb$HF_BB, exclude = NULL))                        

addmargins(table(df_bb$BB_contra, df_bb$BETADISCONTRATYPE_Missing, exclude = NULL))                        

df_bb$BB_contra[is.na(df_bb$BB_contra)] <- 0

df_bb <- 
  df_bb %>% 
  mutate(HF_BBorContra = ifelse(HF_BB == 1 | BB_contra == 1, 1, 
                                ifelse(is.na(HF_BB) & is.na(BB_contra), NA, 0)))

addmargins(table(df_bb$HF_BB, exclude = NULL))                        
addmargins(table(df_bb$BB_contra, exclude = NULL))   

addmargins(table(df_bb$HF_BBorContra, exclude = NULL))                        
addmargins(table(df_bb$HF_BBorContra, df_bb$HF_BB, exclude = NULL)) 
addmargins(table(df_bb$HF_BBorContra, df_bb$BB_contra, exclude = NULL))                        

addmargins(table(df_bb$HF_BBorContra, df_bb$BBdisc, exclude = NULL))                        


addmargins(table(df_bb$BB_contra, df_bb$JC_BETADISCCONTRA, exclude = NULL))   


names(df_bb)

#_______________________________________________#
#add key bb variables to the main dataset

#create a variable indicating all patients with a contra-indication to BB; this is similar to JC_BETADISCCONTRA
df_gdmt <- 
  df_gdmt %>% 
  mutate(GDMT_BBcontra = ifelse(AHA_BETADISCONTRATYPE_1 == 1 | AHA_BETADISCONTRATYPE_2 == 1 | AHA_BETADISCONTRATYPE_3 == 1 | 
                                AHA_BETADISCONTRATYPE_4 == 1 | AHA_BETADISCONTRATYPE_5 == 1 | AHA_BETADISCONTRATYPE_6 == 1 | 
                                AHA_BETADISCONTRATYPE_7 == 1, 1, 0), .after=AHA_BETADISCONTRATYPE_7)
df_gdmt$GDMT_BBcontra[is.na(df_gdmt$GDMT_BBcontra)] <- 0

#create a variable indicating which patients are on GDMT BB; the native var HF_CLASSBB and HF_BBforLVSD are inaccurate 
df_gdmt <- 
  df_gdmt %>% 
  mutate(GDMT_HFBB = ifelse(OH_BETABLOCKER_DISC == 'bisoprolol' | OH_BETABLOCKER_DISC == 'carvedilol' | 
                            OH_BETABLOCKER_DISC == 'MTPsucc', 1, 
                            ifelse(is.na(OH_BETABLOCKER_DISC), NA, 0)))

#create a variable indicating if a patient is either dc'd with BB or has a contraindication. 
#The native var BBdisc isn't specific to HF-BB
df_gdmt <- 
  df_gdmt %>% 
  mutate(GDMT_HFBBorContra = ifelse(GDMT_HFBB == 1 | GDMT_BBcontra == 1, 1, 
                                    ifelse(is.na(GDMT_HFBB) & is.na(GDMT_BBcontra), NA, 0)))

#clean up excess dc Rx BB-related var 
names(df_gdmt)
df_gdmt_bb <- subset(df_gdmt, select = -c(BETADISCONTRATYPE_Missing, HF_BBforLVSD, JC_BETADISC, HF_CLASSBB, 
                                          HF_EVIDENCEBBforLVSD, BBdisc))

#clean up the environment 
rm(df_bb)

#save the new data frame 
#save(df_gdmt_bb, file = '/mnt/workspace/rstudio_projects/gdmt_bb.RData')
