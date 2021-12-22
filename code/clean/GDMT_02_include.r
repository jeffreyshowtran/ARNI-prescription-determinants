getwd() 
setwd("/mnt/workspace/rstudio_projects/data")

library(dplyr)
library(gmodels)

#df <- read_sas(data_file = 'df_GDMT.sas7bdat')
#save(df, file = '/mnt/workspace/rstudio_projects/gdmt_1_all.RData')
load('gdmt_1_all.RData')

view(df)

#order the data frame by patient id and site ID 
df[with(df, order(PATIENT_ID, SITE_ID)),]

#_______________________________________________#
#Labels: 
#EF labels 
df$HFrbpEF <- factor(df$HFrbpEF, 
                     levels = c(1,2,3,4), 
                     labels = c('N/A', 'EF<40', 'EF40-49', 'EF>=50'))

df$LVEF <- ordered(df$LVEF, 
                   levels = c(1,2,3), 
                   labels = c('EF<=40', 'EF41-49', 'EF>=50'))

df$Ejfrac40 <- factor(df$Ejfrac40, 
                      levels = c(0,1,43,44,45,55,66,77,88,99), 
                      labels = c("no", "yes", "eligible", "not eligible-reason known", "not eligible-reason unknown", 
                                 "not done-reason documented", "unknown", "none-contraindicated", 
                                 "not documented", "not applicable"))

#dispo labels 
df$OTHFACILITYi <- factor(df$OTHFACILITYi, 
                          levels = c(1,2,3,4,5), 
                          labels = c('SNF', 'IRF', 'LTCH', 'ICF', 'Other'))

df$OH_EXPIRED_CARDIO <- factor(df$OH_EXPIRED_CARDIO, 
                               levels = c(2,3,4,5), 
                               labels = c('ACS', 'Sudden Death', 'CHF', 'Other CVD'))

df$OH_IF_EXP_DEATH <- factor(df$OH_IF_EXP_DEATH, 
                             levels = c(1,2,3), 
                             labels = c('CVD death', 'non-CVD death', 'UTD'))

df$JC_COMFORTONLY2 <- factor(df$JC_COMFORTONLY2, 
                             levels = c(1,2,3,4), 
                             labels = c('HD 0-1', 'HD 2+', 'Timing Unclear', 'Not documented/UTD'))

df$AHA_SPECIAL <- factor(df$AHA_SPECIAL, 
                         levels = c(1,2,3,4,5), 
                         labels = c('Homeless', 'DoC',  'International', 'None/UTD', 'Home Health Care'))

df$DISPOSITIONi <- factor(df$DISPOSITIONi, 
                          levels = c(1,2,3,4,5,6,7,8), 
                          labels = c('Home', 'Hospice: home', 'Hospice: facility', 'Acute Care Facility', 
                                     'Other Care Facility', 'Expired', 'AMA', 'None/UTD'))



#______________________________________________#
#exclusion: EF>=40 

#EF: Ejfrac40, HFrbpEF, JC_LVSD, LVEF, OH_EF, OH_EF_NA 
#of note, cannot use the following var:
# - LVEF as it classifies HFrEF is <=40
# - JC_LVSD as it is inconsistent

#subset data: all vars a/w EF 
df_EF <- df[c('PATIENT_ID', 'Ejfrac40', 'HFrbpEF', 'JC_LVSD', 'LVEF', 'OH_EF', 'OH_EF_NA')]
view(df_EF)

#missing values for EF
sum(is.na(df$Ejfrac40))
sum(is.na(df$HFrbpEF))
sum(is.na(df$JC_LVSD))
sum(is.na(df$LVEF))
sum(is.na(df$OH_EF))

#how many patients are missing all EF indicators? - 4968
sum(is.na(df$Ejfrac40) & is.na(df$HFrbpEF) & is.na(df$JC_LVSD) & is.na(df$OH_EF))

#verify that Ejfrac40 and LVSD give the same info - note: they don't 
CrossTable(df_EF$Ejfrac40, df_EF$JC_LVSD, missing.include = FALSE)

df_error <- subset(df_EF, (Ejfrac40=="no" & JC_LVSD==1) | (Ejfrac40=="yes" & JC_LVSD==0))
view(df_error) #LVSD also seems to not be reliable as there are some cases where LVSD is indicated in patients with EF>=40

#verify Ejfrac where N/A values from HFrbpEF are marked as "yes" using OH_EF 
## create a new variable based on OH_EF
#compare Ejfrac40 and HFrbpEF - Ejfrac40 shows "yes" for N/A values of HFrbpEF- need to verify 
CrossTable(df_EF$Ejfrac40, df_EF$HFrbpEF) 
table(df_EF$Ejfrac40, df_EF$HFrbpEF)
df_EF <- 
  df_EF %>% 
  mutate(EF_bins1 = case_when(OH_EF >= 40 ~ '0', 
                              OH_EF < 40 ~ '1',
                              is.na(OH_EF) ~'99'), .after = Ejfrac40)
view(df_EF)
table(df_EF$Ejfrac40, df_EF$EF_bins1)
table(df_EF$HFrbpEF) #it seems EF_bins1, HFrbpEF, and Ejfrac40 are in agreement that there are 232260 patients with EF < 40

table(df_EF$HFrbpEF, df_EF$EF_bins1, exclude = NULL) #can use either EF_bins1 or HFrbpEF as inclusion 
table(df_EF$HFrbpEF, df_EF$Ejfrac40, exclude = NULL)

#______________________________________________#
#exclusion: dc to hospice care/dead

#subset data: all vars a/w dispo 
df_dispo <- df[c('PATIENT_ID', 'AHA_SPECIAL', 'DISPOSITIONi', 'JC_COMFORTONLY', 'JC_COMFORTONLY2', 
                 'OH_IF_EXP_DEATH', 'OH_EXPIRED_CARDIO', 'OTHFACILITYi', 'TransferOut')]
view(df_dispo)

table(df_dispo$AHA_SPECIAL, exclude = NULL)
table(df_dispo$JC_COMFORTONLY, exclude = NULL) #essentailly a useless variable as all cases are missing 
table(df_dispo$JC_COMFORTONLY2, exclude = NULL) 
table(df_dispo$DISPOSITIONi, exclude = NULL)
table(df_dispo$OH_IF_EXP_DEATH, exclude = NULL)
table(df_dispo$OH_EXPIRED_CARDIO, exclude = NULL)
table(df_dispo$OTHFACILITYi, exclude = NULL)
table(df_dispo$TransferOut, exclude = NULL)
#for hospice: useful to exclude on JC_COMFORTONLY2, DISPOSITIONi
#for death: exclude on OH_EXPIRED_CARDIO and OH_IF_EXP_DEATH


#table(df_dispo$TransferOut, df_dispo$DISPOSITIONi) #all the patients transferred out go to acute care facilities (?duh?)
table(df_dispo$JC_COMFORTONLY, df_dispo$JC_COMFORTONLY2, exclude = NULL)

table(df$JC_COMFORTONLY2, df_dispo$DISPOSITIONi, exclude = NULL) 
#_______________________________________________#
#subset out the data that meet inclusion critieria: 

table(df$HFrbpEF, exclude = NULL)
table(df_dispo$JC_COMFORTONLY2, exclude = NULL) 
table(df_dispo$DISPOSITIONi, exclude = NULL)
table(df_dispo$OH_IF_EXP_DEATH, exclude = NULL)
table(df_dispo$OH_EXPIRED_CARDIO, exclude = NULL)
addmargins(table(df_dispo$OH_EXPIRED_CARDIO, df_dispo$OH_IF_EXP_DEATH, exclude = NULL))

levels(df$HFrbpEF)[levels(df$HFrbpEF)=='N/A'] <- NA

addmargins(table(df$HFrbpEF, df$DISPOSITIONi, exclude = NULL))
addmargins(table(df$HFrbpEF, df$OH_IF_EXP_DEATH, exclude = NULL))
addmargins(table(df$DISPOSITIONi, df$OH_IF_EXP_DEATH, exclude = NULL))


df <- 
  df %>%
  mutate(include = ifelse(df$HFrbpEF %in% 'EF<40' &
                            (df$DISPOSITIONi != 'Hospice: home' & 
                             df$DISPOSITIONi != 'Hospice: facility' & 
                             df$DISPOSITIONi != 'Expired' & 
                             df$DISPOSITIONi != 'AMA' | 
                             is.na(df$DISPOSITIONi)) & 
                            (df$JC_COMFORTONLY2 == 'Not documented/UTD' |
                             is.na(df$JC_COMFORTONLY2)), 1, 0))
addmargins(table(df$include, exclude=NULL))


df_gdmt <- subset(df, df$HFrbpEF == 'EF<40' &
                     (df$DISPOSITIONi != 'Hospice: home' & 
                      df$DISPOSITIONi != 'Hospice: facility' & 
                      df$DISPOSITIONi != 'Expired' & 
                      df$DISPOSITIONi != 'AMA' | 
                      is.na(df$DISPOSITIONi)))
             
addmargins(table(df_gdmt$HFrbpEF, df_gdmt$DISPOSITIONi, exclude = NULL))

addmargins(table(df_gdmt$JC_COMFORTONLY2, df_gdmt$HFrbpEF, exclude=NULL))
df_gdmt <- subset(df_gdmt, df_gdmt$JC_COMFORTONLY2  == 'Not documented/UTD' | is.na(df_gdmt$JC_COMFORTONLY2) )

df_gdmt <- droplevels(df_gdmt)     

#df_gdmt excludes patients with EF>=40 and all patients sent to hospice or who died
view(df_gdmt)                      

#drop var: 
# Ejfrac40, HFrbpEF, JC_LVSD, LVEF, OH_EF_NA; Keep(OH_EF)
#JC_COMFORTONLY, JC_COMFORTONLY2, OH_IF_EXP_DEATH, OH_EXPIRED_CARDIO


df_gdmt <- subset(df_gdmt, select = -c(Ejfrac40, HFrbpEF, JC_LVSD, LVEF, 
                                       OH_EF_NA, JC_COMFORTONLY, JC_COMFORTONLY2, 
                                       OH_IF_EXP_DEATH, OH_EXPIRED_CARDIO))

#_______________________________________________#
#How many unique patients are in df? 514864 unique patients
id <- c(with(df_gdmt,PATIENT_ID))
length((unique(id)))

#How many unique sites are within df? 671 unique sites
site <- c(with(df,SITE_ID))
length((unique(site)))

#_______________________________________________#
#save the gdmt data frame
view(df_gdmt)
#save(df_gdmt, file = '/mnt/workspace/rstudio_projects/gdmt_include.RData')


       