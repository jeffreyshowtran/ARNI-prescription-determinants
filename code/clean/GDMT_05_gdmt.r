getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#remove.packages("labelled")
library(haven)
library(tidyverse)
library(data.table)

load('gdmt_4_bb_raasi.RData')
names(df_gdmt_bb_raasi)

#bring in HF_ALDOSTERONE (wasn't imported into the initial dataset)
df_HF_ALDOSTERONE <- read_sas(data_file = "/mnt/workspace/GWTG/HF/hf_data_challenge_2021/v1_2021-03/data/hf_data_challenge.sas7bdat", 
                              col_select =c('RCRDNUM', 'HF_ALDOSTERONE'))

addmargins(table(df_HF_ALDOSTERONE$HF_ALDOSTERONE, exclude = NULL))

df_gdmt_Rx <- merge(df_gdmt_bb_raasi, df_HF_ALDOSTERONE, by = 'RCRDNUM', all=FALSE)

#labels-----------------------------------------------------------
df_gdmt_Rx$ALDOdisc <- factor(df_gdmt_Rx$ALDOdisc, 
                          levels = c(1,2,3), 
                          labels = c('Yes- prescribed', 'No- not prescribed', 'No- contraindicated'))

df_gdmt_Rx$HF_ALDOSTERONE <- factor(df_gdmt_Rx$HF_ALDOSTERONE, 
                                levels = c(1,2), 
                                labels = c('spironolactone', 'eplerenone'))

#prep MRA data frame------------------------------------------

df_mra <- subset(df_gdmt_Rx, select = c(PATIENT_ID, 
                                        ALDOdisc, HF_ALDOANTAGONIST, OH_ALDOSTERONE_DISC, HF_ALDOSTERONE,
                                        CONTRA_ALDOSTERONE_Missing, OTHER_ALDOSTERONE_Missing, 
                                        HF_CONTRA_ALDOSTERONE_DISC, 
                                        HF_CONTRA_ALDOSTER_DISC_LIST_1:HF_CONTRA_ALDOSTER_DISC_LIST_7, 
                                        HF_OTHER_ALDOSTERONE_1:HF_OTHER_ALDOSTERONE_5))
names(df_mra)

#variables--------------------------------------------------------
df_mra <- 
  df_mra %>% 
  mutate(HF_ALDOcontra = ifelse(HF_CONTRA_ALDOSTER_DISC_LIST_1 == 1 | HF_CONTRA_ALDOSTER_DISC_LIST_2 == 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_3 == 1 | HF_CONTRA_ALDOSTER_DISC_LIST_4 == 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_5 == 1 | HF_CONTRA_ALDOSTER_DISC_LIST_6 == 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_7 == 1, 1, 0)) 
addmargins(table(df_mra$HF_ALDOcontra, exclude = NULL))

df_mra <- 
  df_mra %>% 
  mutate(HF_ALDOcontraOTH = ifelse(HF_OTHER_ALDOSTERONE_1 == 1 | HF_OTHER_ALDOSTERONE_2 == 1 | 
                                   HF_OTHER_ALDOSTERONE_3 == 1 | HF_OTHER_ALDOSTERONE_4 == 1 | 
                                   HF_OTHER_ALDOSTERONE_5 == 1, 1, 0)) 
addmargins(table(df_mra$HF_ALDOcontraOTH, exclude = NULL))
#the HF_OTHER_ALDOSTERONE_ series is garbage- all empty rows 

#tables-----------------------------------------------------------
#who got an MRA? 
addmargins(table(df_mra$ALDOdisc, df_mra$HF_ALDOSTERONE, exclude = NULL))
  #ALDOdisc is a better variable - less missing data 
addmargins(table(df_mra$ALDOdisc, df_mra$OH_ALDOSTERONE_DISC, exclude = NULL))
  #ALDOdisc is a better variable - less missing data that OH_ALDO... 

#contraindications against an MRA? 
addmargins(table(df_mra$OTHER_ALDOSTERONE_Missing, exclude=NULL))
  #another garbage variable with no data- it's all 1's 
addmargins(table(df_mra$HF_CONTRA_ALDOSTERONE_DISC, exclude=NULL))
addmargins(table(df_mra$HF_CONTRA_ALDOSTERONE_DISC, df_mra$HF_ALDOcontra, exclude=NULL))
  #3pts who have an identified contra that are listed as not having a contra in the overall classification 
  #1088 who have an overall contra who's contra is not specifically designated by the HF_CONTRA_ALDOSTER_DISC_LIST_ var series 
addmargins(table(df_mra$ALDOdisc, df_mra$HF_ALDOcontra, exclude=NULL))
  #398pts with an MRA contra got one anyways  
  #3 patients with a contra were not prescribed one but not designated as not having a contra 
addmargins(table(df_mra$ALDOdisc, df_mra$HF_CONTRA_ALDOSTERONE_DISC, exclude=NULL))

#create essential variables--------------------------------------
df_mra <-
  df_mra %>%
  mutate(GDMT_MRAcontra = ifelse(df_mra$ALDOdisc %in% 'No- contraindicated' | df_mra$HF_ALDOcontra==1 | 
                                 df_mra$HF_CONTRA_ALDOSTERONE_DISC==1, 1, 
                                 ifelse(df_mra$HF_CONTRA_ALDOSTERONE_DISC==0, 0, 99)))
df_mra$GDMT_MRAcontra[(df_mra$HF_CONTRA_ALDOSTERONE_DISC==0)] <- 0
addmargins(table(df_mra$ALDOdisc, df_mra$GDMT_MRAcontra, exclude=NULL))
addmargins(table(df_mra$HF_ALDOcontra, df_mra$GDMT_MRAcontra, exclude=NULL))
addmargins(table(df_mra$HF_CONTRA_ALDOSTERONE_DISC, df_mra$GDMT_MRAcontra, exclude=NULL))

df_mra <- 
  df_mra %>%
  mutate(GDMT_MRA = ifelse(df_mra$ALDOdisc %in% 'Yes- prescribed', 1, 0))
addmargins(table(df_mra$ALDOdisc, df_mra$GDMT_MRA, exclude=NULL))

df_mra <-
  df_mra %>% 
  mutate(GDMT_MRAorContra = ifelse(GDMT_MRA==1 | GDMT_MRAcontra==1, 1, 0))
addmargins(table(df_mra$ALDOdisc, df_mra$GDMT_MRAorContra, exclude=NULL))


#import key var into main df------------------------------------------
df_gdmt_Rx <- 
  df_gdmt_Rx %>% 
  mutate(HF_ALDOcontra = ifelse(HF_CONTRA_ALDOSTER_DISC_LIST_1 %in% 1 | HF_CONTRA_ALDOSTER_DISC_LIST_2 %in% 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_3 %in% 1 | HF_CONTRA_ALDOSTER_DISC_LIST_4 %in% 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_5 %in% 1 | HF_CONTRA_ALDOSTER_DISC_LIST_6 %in% 1 | 
                                HF_CONTRA_ALDOSTER_DISC_LIST_7 %in% 1, 1,
                         ifelse(is.na(HF_CONTRA_ALDOSTER_DISC_LIST_1) & is.na(HF_CONTRA_ALDOSTER_DISC_LIST_2) & 
                                is.na(HF_CONTRA_ALDOSTER_DISC_LIST_3) & is.na(HF_CONTRA_ALDOSTER_DISC_LIST_4) &
                                is.na(HF_CONTRA_ALDOSTER_DISC_LIST_5) & is.na(HF_CONTRA_ALDOSTER_DISC_LIST_6) & 
                                is.na(HF_CONTRA_ALDOSTER_DISC_LIST_7), NA, 
                         ifelse(HF_CONTRA_ALDOSTERONE_DISC %in% 1 & !is.na(HF_CONTRA_ALDOSTERONE_DISC), 1, 0)))) 
addmargins(table(df_gdmt_Rx$HF_ALDOcontra, exclude=NULL))

df_gdmt_Rx <-
  df_gdmt_Rx %>%
  mutate(GDMT_MRAcontra = ifelse(ALDOdisc %in% 'No- contraindicated' | 
                                 HF_ALDOcontra %in% 1 | 
                                 HF_CONTRA_ALDOSTERONE_DISC %in% 1, 1, 
                          ifelse(HF_CONTRA_ALDOSTERONE_DISC %in% 0, 0,  
                          ifelse(ALDOdisc %in% 'Yes- prescribed', 0, 
                          ifelse(is.na(ALDOdisc) & 
                                 is.na(HF_ALDOcontra) &
                                 is.na(HF_CONTRA_ALDOSTERONE_DISC), NA, 99)))))
#df_gdmt_Rx$GDMT_MRAcontra[(df_gdmt_Rx$HF_CONTRA_ALDOSTERONE_DISC==0)] <- 0
addmargins(table(df_gdmt_Rx$GDMT_MRAcontra, exclude=NULL))
addmargins(table(df_gdmt_Rx$GDMT_MRAcontra, df_gdmt_Rx$ALDOdisc, exclude=NULL))


df_gdmt_Rx <- 
  df_gdmt_Rx %>%
  mutate(GDMT_MRA = ifelse(ALDOdisc %in% 'Yes- prescribed', 1, 
                    ifelse(is.na(ALDOdisc), NA, 0)))
addmargins(table(df_gdmt_Rx$GDMT_MRA, exclude=NULL))


df_gdmt_Rx <-
  df_gdmt_Rx %>% 
  mutate(GDMT_MRAorContra = ifelse(GDMT_MRA==1 | GDMT_MRAcontra==1, 1, 0))
addmargins(table(df_gdmt_Rx$GDMT_MRAorContra, exclude=NULL))


df_gdmt_Rx <- subset(df_gdmt_Rx, select = -c(HF_ALDOcontra, HF_ALDOANTAGONIST, OH_ALDOSTERONE_DISC, 
                                             CONTRA_ALDOSTERONE_Missing, OTHER_ALDOSTERONE_Missing, 
                                             HF_CONTRA_ALDOSTERONE_DISC,  
                                             HF_OTHER_ALDOSTERONE_1:HF_OTHER_ALDOSTERONE_5))
rm(df_HF_ALDOSTERONE, df_mra)
#save the new data frame---------------------------------------------------- 
#save(df_gdmt_Rx, file = '/mnt/workspace/rstudio_projects/gdmt_5_Rx.RData')
