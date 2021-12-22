getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(tidyverse)
library(readxl)

# #bring in the DCI.xlsx file 
# df_dci = read_excel("dci.xlsx", col_names=TRUE)
# head(df_dci)
# 
# #factor the variables 
# table(df_dci$zip_designation, exclude=NULL)
# df_dci$zip_designation <- factor(df_dci$zip_designation, 
#                                  labels = c('Rural', 'SmallTown', 'Suburban', 'Urban'))
# class(df_dci$zip_designation)
# 
# table(df_dci$distress_quntile, exclude=NULL)
# df_dci$distress_quntile <- factor(df_dci$distress_quntile, 
#                                   ordered=TRUE, 
#                                   levels = c(1:5), 
#                                   labels = c('prosperous', 'comfortable', 'midtier', 
#                                              'atrisk', 'distressed'))
# class(df_dci$distress_quntile)


load('gdmt_11.4_final.RData')

df.dcisite <- subset(df.run, select = c(SITE_ID, SITE_DCI_score, SITE_DCI_poptotal, SITE_DCI_setting, SITE_DCI_quintile))

df.dcisite <- df.dcisite[complete.cases(df.dcisite),]

df.dcisite <- unique(df.dcisite)

df.dcisite <- rename(df.dcisite, SITE_score = SITE_DCI_score)
df.dcisite <- rename(df.dcisite, SITE_poptotal = SITE_DCI_poptotal)
df.dcisite <- rename(df.dcisite, SITE_setting = SITE_DCI_setting)
df.dcisite <- rename(df.dcisite, SITE_quintile = SITE_DCI_quintile)

df.run = merge(df.run, df.dcisite, by.x="SITE_ID", by.y="SITE_ID", all.x=TRUE)


sum(is.na(df.run$SITE_score))
sum(is.na(df.run$SITE_DCI_score))
sum(is.na(df.run$SITE_poptotal))
sum(is.na(df.run$SITE_DCI_poptotal))
sum(is.na(df.run$SITE_setting))
sum(is.na(df.run$SITE_DCI_setting))
sum(is.na(df.run$SITE_quintile))
sum(is.na(df.run$SITE_DCI_quintile))

df.run = subset(df.run, select = -c(SITE_score, SITE_poptotal, SITE_setting, SITE_quintile))
rm(df.dcisite)

#save the data frame 
save(df.run, file = '/mnt/workspace/rstudio_projects/gdmt_11.5_final.RData')

