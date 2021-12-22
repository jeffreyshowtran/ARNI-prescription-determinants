#creata dataset where missing data in categorical variables is treated as an explicit factor level rather than missing. 
#essentially no cat var with >2 levels will have any missing data 

getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

load('gdmt_11.1_final.RData')

####describe the data#### 
#quick look at the data
str(df.final, list.len=ncol(df.final))

#how many missing in each variable? 
lapply(df.final, function(x) 
  sum(is.na(x)))

#copy df.final to a new frame called catnomiss
df.catnomiss <- df.final

#drop empty levels of admitsource var 
df.catnomiss$ADMITSOURCEi <- droplevels(df.catnomiss$ADMITSOURCEi)

#create a list of the categorical variables where we will be adding NA as a category 
list = c('race2i', 'ADMITSOURCEi', 'zip1', 'covar_dispo', 'OH_HFHOSPADM',
         'MedExp', 'zip_designation', 
         'SITE_DCI_setting', 'SITE_DCI_quintile')

#execute addNA() to add NA as an explicit level
df.catnomiss[list] <- lapply(df.catnomiss[list], function(x) addNA(x))

#discard excess variables 
rm(list)

#save the new data frame 
save(df.catnomiss, file = '/mnt/workspace/rstudio_projects/gdmt_11.2_catnomiss.RData')


# , 
#MedExp, , population_total, distress_score, 
# and covar_OutsideZIPregion are dependent on ZIP 
#MCNTRLi is level 2 

#OH_SYMPTOMS is ordinal
#remove PMHx_priorPCIorCABG as it has nothing but 1
