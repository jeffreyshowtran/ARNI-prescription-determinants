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



#bring in the DCI.xlsx file 
df_dci = read_excel("dci.xlsx", col_names=TRUE)
head(df_dci)

#factor the variables 
table(df_dci$zip_designation, exclude=NULL)
df_dci$zip_designation <- factor(df_dci$zip_designation, 
                                 labels = c('Rural', 'Small Town', 'Suburban', 'Urban'))
class(df_dci$zip_designation)

table(df_dci$distress_quntile, exclude=NULL)
df_dci$distress_quntile <- factor(df_dci$distress_quntile, 
                                  ordered=TRUE, 
                                  levels = c(1:5), 
                                  labels = c('prosperous', 'comfortable', 'mid-tier', 
                                             'at-risk', 'distressed'))
class(df_dci$distress_quntile)

#load the main dataset 
load('gdmt_9_fast.RData')

#merge on zip codes 
df_fast = merge(df_fast, df_dci, by.x="zip_num", by.y="Zipcode", all.x=TRUE)
sum(is.na(df_fast$distress_score))
head(df_fast)

#save the new data frame 
save(df_fast, file = '/mnt/workspace/rstudio_projects/gdmt_10_dci.RData')
