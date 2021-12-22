getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

#library(dplyr)
#library(tidyr)
#library(labelled)
#library(mice)
library(readxl)
#library(micemd)
#library(VIM)


load('gdmt_8_fast.RData')
names(df_fast)

#clean up the variable "ZIP" 
df_fast <-
  df_fast %>% 
  mutate(ZIP = na_if(ZIP, ""))
sum(is.na(df_fast$ZIP))

df_fast <- separate(df_fast, col=ZIP, sep="-", into=c('zip1', 'zip2'))
head(df_fast)
addmargins(table(df_fast$zip1, exclude=NULL))
addmargins(table(df_fast$zip2, exclude=NULL))
sum(is.na(df_fast$zip1))
df_fast = subset(df_fast, select = -c(zip2))

#bring in the MedExp_state file 
df_MedExp = read_excel("MedExp_State.xlsx", col_names=TRUE)

#reformat zip to ZIP with 5digits for merge
df_MedExp <- 
  df_MedExp %>% 
  mutate(ZIP = sprintf("%05d", zip))
class(df_MedExp$ZIP)
df_MedExp = subset(df_MedExp, select = -c(zip))

addmargins(table(df_MedExp$MedExp_status, exclude=NULL))

#merge on zip code 
df_fast = merge(df_fast, df_MedExp, by.x="zip1", by.y="ZIP", all.x=TRUE)
sum(is.na(df_fast$MedExp_status))
  #there are 100655 missing when zip from MedExp not formatted correctly 
  #drops to 93016 when zip is formatted correctly 

#convert status to factor 
df_fast$MedExp_status <- factor(df_fast$MedExp_status, 
                                labels = c('Adopted', 'Not Adopted'))

#the zip codes for teh US territories (eg Puerto Rico, etc are not coded)
sum(is.na(df_fast$MedExp_status))

df_fast <- 
  df_fast %>% 
  mutate(zip_num = as.numeric(zip1))

#recoding Puerto Rico
df_fast$MedExp_status[df_fast$zip_num >= 600 & 
                      df_fast$zip_num <=799 &
                      is.na(df_fast$MedExp_status)] <- 'Not Adopted'

df_fast$MedExp_status[df_fast$zip_num >= 900 & 
                      df_fast$zip_num <= 999 &
                      is.na(df_fast$MedExp_status)] <- 'Not Adopted'

#recoding the Virgin islands 
df_fast$MedExp_status[df_fast$zip_num >= 800 & 
                      df_fast$zip_num <=899 &
                      is.na(df_fast$MedExp_status)] <- 'Not Adopted'


#add a zip code for DC 
df_fast$MedExp_status[df_fast$zip_num == 88888 &
                      is.na(df_fast$MedExp_status)] <- 'Adopted'
df_fast$MedExp_adoption_date[df_fast$zip_num == 88888 &
                             df_fast$MedExp_status %in% 'Adopted'] <- as.Date('2014-01-01')

#error checking, etc 
table(df_fast$MedExp_status, exclude=NULL)
df_ts = subset(df_fast, !is.na(df_fast$zip1) & is.na(df_fast$MedExp_status))
df_ts = subset(df_ts, select = c(RCRDNUM, zip1, MedExp_status))
addmargins(table(df_ts$zip1, df_ts$MedExp_status, exclude=NULL))
rm(df_ts)


#save the new data frame 
save(df_fast, file = '/mnt/workspace/rstudio_projects/gdmt_9_fast.RData')

