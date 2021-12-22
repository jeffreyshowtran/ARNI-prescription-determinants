setwd("/mnt/workspace/rstudio_projects/data/")


load('/mnt/workspace/rstudio_projects/data/gdmt_11.10_testclean.RData')

###remove unwanted var from df
df <- subset(df, select= -c(MedExp, covar_dispo, insured, ADMITSOURCEi, MCNTRLi,
                            PMHx_HTN, PMHx_priorCVATIA, PMHx_dm2, PMHx_COPD))

###remove data if GDMT_ARNI is missing 
df <- subset(df, !is.na(df$GDMT_ARNI))

#re-insert missing for zip_designation
df$zip_designation[df$zip_designation == "UTD"] <- NA 

#re-insert missing for insurance
df$insurance[df$insurance == "UTD"] <- NA 
df$insurance <- factor(df$insurance, levels = c(levels(df$insurance), "none"))
df$insurance[df$insurance == "noneORutd"] <- "none"

#re-insert missing for MCNTRLi
#df$MCNTRLi[df$MCNTRLi == "UTD"] <- NA 

#re-insert missing for ADMITSOURCEi
#df$ADMITSOURCEi[df$ADMITSOURCEi == "UTD"] <- NA 

#re-insert missing for covar_dispo
#df$covar_dispo[df$covar_dispo == "UTD"] <- NA 

#add pre-hosp ARNI use 
table(df$OH_ARNI, exclude=NULL)
df$OH_ARNI <- as.factor(df$OH_ARNI)

#remove all dropped levels 
df <- droplevels(df)

save(df, file='/mnt/workspace/rstudio_projects/data/gdmt_11.11_clean.RData')
