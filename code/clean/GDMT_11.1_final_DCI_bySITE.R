#add DCI data by SITE 

setwd("/mnt/workspace/rstudio_projects/data/")

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


#rename the var with a site tag 
df_dci <- rename(df_dci, SITE_DCI_setting = zip_designation) 
df_dci <- rename(df_dci, SITE_DCI_poptotal = population_total) 
df_dci <- rename(df_dci, SITE_DCI_score = distress_score)
df_dci <- rename(df_dci, SITE_DCI_quintile = distress_quntile)

load('gdmt_11_final.RData')

#clean SITE_POSTAL_CODE
table(df.final$SITE_POSTAL_CODE, exclude=NULL)
sum(df.final$SITE_POSTAL_CODE=="")
df.final$SITE_POSTAL_CODE[df.final$SITE_POSTAL_CODE==""] <- NA

#convert SITE_POSTAL_CODE and zipcode to character for matching 
class(df_dci$Zipcode)
df_dci$Zipcode <- as.character(df_dci$Zipcode)

class(df.final$SITE_POSTAL_CODE)
df.final$SITE_POSTAL_CODE <- as.character(df.final$SITE_POSTAL_CODE)

table(df.final$SITE_POSTAL_CODE, exclude=NULL)

#merge on zip codes 
df.final = merge(df.final, df_dci, by.x="SITE_POSTAL_CODE", by.y="Zipcode", all.x=TRUE)
#str(df.final, list.len=ncol(df.final))

sum(is.na(df.final$SITE_DCI_score))

#save the merged data frame 
save(df.final, file = '/mnt/workspace/rstudio_projects/gdmt_11.1_final.RData')
