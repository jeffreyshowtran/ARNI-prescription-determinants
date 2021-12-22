getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)

load('gdmt_5_Rx.RData')



#labels--------------------------------------------
df_gdmt_Rx$RACEi <- factor(df_gdmt_Rx$RACEi, 
                        levels = c(1,2,3,4,5,6,7,8),
                        labels = c('Hispanic', 'Black or African American', 
                                   'American Indian or Alaska Native', 'Asian', 
                                   'White', 'Native Hawaiian or Pacific Islander', 
                                   'Other', 'UTD'))

df_gdmt_Rx$SP_RACE <- factor(df_gdmt_Rx$SP_RACE, 
                        levels = c(2,3,4,5,6,8),
                        labels = c('Black or African American', 
                                   'American Indian or Alaska Native', 'Asian', 
                                   'White', 'Native Hawaiian or Pacific Islander', 
                                   'UTD'))

df_gdmt_Rx$race2i <- factor(df_gdmt_Rx$race2i, 
                          levels = c(1,2,3,4,5),
                          labels = c('White', 'Black', 'Hispanic (any race)', 
                                     'Asian', 'Other (includes UTD)'))

df_gdmt_Rx$INSURANCEi <- factor(df_gdmt_Rx$INSURANCEi, 
                             levels = c(1,2,3,4), 
                             labels = c('Other', 'Medicaid', 'Medicare', 
                                        'No Insurance/Not Documented/UTD'))

df_gdmt_Rx$GENDERi <- factor(df_gdmt_Rx$GENDERi, 
                             levels = c(0,1), 
                             labels = c('male', 'female'))

addmargins(table(df_gdmt_Rx$GENDERi, exclude = NULL))

#prep race df---------------------------------------
df_race <- subset(df_gdmt_Rx, select = c(RCRDNUM, 
                                         GENDERi, 
                                         HF_RACE_2:HF_RACE_6, HF_RACE_8, HF_RACE_Missing, 
                                         RACEi, SP_RACE, race2i, 
                                         HISP_Missing, SP_ETHNIC, jc_hisp_1:jc_hisp_4,
                                         JC_ASIAN_1:JC_ASIAN_7, 
                                         JC_HAWPAC_1:JC_HAWPAC_4,
                                         INSURANCEi, PMTSRCE_Missing,
                                         JC_HOMELESS, 
                                         ZIP))

#Tables--------------------------------------------
addmargins(table(df_race$RACEi, df_race$SP_RACE, exclude = NULL))
  #SP_RACE has a ton of NA and doesn't add anything to RACEi

addmargins(table(df_race$RACEi, df_race$race2i, exclude = NULL))
  #race2i is a condensed version of RACEi and doens't add any new info, 
  #but groups all native american ethnicities into a single race

#who has no insurance info? 
addmargins(table(df_race$INSURANCEi, df_race$PMTSRCE_Missing, exclude = NULL))
  #PMTSRCE_Missing adds no info to INSURANCEi

#how many zip codes are there? 22177
length(unique(df_race$ZIP))
addmargins(table(df_race$SP_ETHNIC, exclude = NULL))

#how many people have more than one race listed?
df_race <-
  df_race %>% 
  mutate(multiRace = select(., HF_RACE_2:HF_RACE_6, HF_RACE_8) %>% rowSums(na.rm=TRUE))
addmargins(table(df_race$multiRace, exclude=NULL))
view(df_race)
addmargins(table(df_race$RACEi, df_race$multiRace, exclude=NULL))
  #7085 have no race listed
  #394 have 2 races listed 
  #11 have 3 races listed 

#new var--------------------------------------------
df_gdmt_Rx <-
  df_gdmt_Rx %>% 
  mutate(insurance = INSURANCEi)
levels(df_gdmt_Rx$insurance) = c('Other', 'Medicaid', 'Medicare', 
                                 'No Insurance/Not Documented/UTD', 'Missing')
df_gdmt_Rx$insurance[is.na(df_gdmt_Rx$INSURANCEi)] <- 'Missing'
addmargins(table(df_gdmt_Rx$INSURANCEi, df_gdmt_Rx$insurance, exclude = NULL))

df_gdmt_Rx_pvar <- subset(df_gdmt_Rx, select = -c( 
                                         HF_RACE_2:HF_RACE_6, HF_RACE_8, HF_RACE_Missing, 
                                         SP_RACE, 
                                         HISP_Missing, SP_ETHNIC, jc_hisp_1:jc_hisp_4,
                                         JC_ASIAN_1:JC_ASIAN_7, 
                                         JC_HAWPAC_1:JC_HAWPAC_4,
                                         INSURANCEi, PMTSRCE_Missing))

#save the new data frame---------------------------------------------------- 
save(df_gdmt_Rx_pvar, file = '/mnt/workspace/rstudio_projects/gdmt_6_Rx_pvar.RData')
#view(df_gdmt_Rx_pvar)
