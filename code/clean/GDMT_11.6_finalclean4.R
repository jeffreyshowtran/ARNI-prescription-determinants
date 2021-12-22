setwd("/mnt/workspace/GWTG/HF/hf_data_challenge_2021/v1_2021-03/data/")

#install.packages("tidyverse")

library(tidyverse)
library(haven)



df <- read_sas(data_file = "hf_data_challenge.sas7bdat", 
               col_select = c('RCRDNUM', 'JC_FOLLOWUP', 'HF_FUAPPO', 'HFS_FUVISITDATE'))

table(df$JC_FOLLOWUP, exclude=NULL)
table(df$HF_FUAPPO, exclude=NULL)
table(is.na(df$HFS_FUVISITDATE), exclude=NULL)

addmargins(table(df$HF_FUAPPO, df$JC_FOLLOWUP, exclude=NULL))



df <- 
  df %>% 
  mutate(followup = ifelse(HF_FUAPPO %in% 1 & JC_FOLLOWUP %in% 1, 1, 
                    ifelse(HF_FUAPPO %in% 0 & JC_FOLLOWUP %in% 0, 0, 
                    ifelse(is.na(HF_FUAPPO) & !is.na(JC_FOLLOWUP), JC_FOLLOWUP, 
                    ifelse(!is.na(HF_FUAPPO) & is.na(JC_FOLLOWUP), HF_FUAPPO,
                    ifelse(is.na(HF_FUAPPO) & is.na(JC_FOLLOWUP), NA, 
                    ifelse(HF_FUAPPO %in% 1 & JC_FOLLOWUP %in% 0, 1, 
                    ifelse(HF_FUAPPO %in% 0 & JC_FOLLOWUP %in% 1, 1, 999))))))))

df$followup[is.na(df$followup) & !is.na(df$HFS_FUVISITDATE)] <- 1

df <- subset(df, select = c(RCRDNUM, followup))

setwd("/mnt/workspace/rstudio_projects/data/")
load('gdmt_11.5_final.RData')


df.run = merge(df.run, df, by.x="RCRDNUM", by.y="RCRDNUM", all.x=TRUE)

df.run$followup <- as.factor(df.run$followup)

#save the new data frame 
save(df.run, file = '/mnt/workspace/rstudio_projects/gdmt_11.6_final.RData')
