getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)
library(tidyr)

load('gdmt_11.1_final.RData')

#create a new dataset based on catnomiss 
df.run <- df.final[order(df.test$SITE_ID),]

#### clean data ####
#remove extraneous levels from dataset
df.run <- droplevels(df.run)

#remove all special characters from var levels 
str(df.run, list.len=ncol(df.run))

table(df.run$ADMITSOURCEi, exclude=NULL)
df.run$ADMITSOURCEi <- as.character(df.run$ADMITSOURCEi)
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Non-Health Care Facility Point of Origin"] <- "NonHealthCare"
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Transfer From a Different Hospital"] <- "HospTransfer"
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Transfer From SNF of ICF"] <- "SNFtransfer"
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Transfer from another Health Care Facility"] <- "OtherTransfer"
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Emergency room"] <- "ED"
df.run$ADMITSOURCEi[df.run$ADMITSOURCEi=="Information not available"] <- "UTD"
df.run$ADMITSOURCEi <- as.factor(df.run$ADMITSOURCEi)

table(df.run$covar_dispo)
df.run$covar_dispo <- as.character(df.run$covar_dispo)
df.run$covar_dispo[df.run$covar_dispo=="Acute Care Facility"] <- "AcuteCareFacility"
df.run$covar_dispo[df.run$covar_dispo=="Home-UTD"] <- "Home"
df.run$covar_dispo[df.run$covar_dispo=="Home Health care"] <- "HomeHealthCare"
df.run$covar_dispo[df.run$covar_dispo=="None/UTD"] <- "UTD"
df.run$covar_dispo[df.run$covar_dispo=="Other Care Facility"] <- "OtherFacility"
df.run$covar_dispo <- as.factor(df.run$covar_dispo)

table(df.run$MCNTRL)
df.run$MCNTRLi <- as.character(df.run$MCNTRLi)
df.run$MCNTRLi[df.run$MCNTRLi=="nonfed government"] <- "NonFedGov"
df.run$MCNTRLi[df.run$MCNTRLi=="nongov, nonprofit"] <- "NonProfit"
df.run$MCNTRLi[df.run$MCNTRLi=="investor-owned, for profit"] <- "ForProfit"
df.run$MCNTRLi[df.run$MCNTRLi=="federal goverment"] <- "FedGov"
df.run$MCNTRLi <- as.factor(df.run$MCNTRLi)

table(df.run$SITE_DCI_quintile)
df.run$SITE_DCI_quintile <- as.character(df.run$SITE_DCI_quintile)
df.run$SITE_DCI_quintile[df.run$SITE_DCI_quintile=="mid-tier"] <- "midtier"
df.run$SITE_DCI_quintile[df.run$SITE_DCI_quintile=="at-risk"] <- "atrisk"
df.run$SITE_DCI_quintile <- factor(df.run$SITE_DCI_quintile, 
                                   levels=c("prosperous", "comfortable", "midtier", "atrisk", "distressed"),
                                   ordered=TRUE)

table(df.run$SITE_DCI_setting)
df.run$SITE_DCI_setting <- as.character(df.run$SITE_DCI_setting)
df.run$SITE_DCI_setting[df.run$SITE_DCI_setting=="Small Town"] <- "SmallTown"
df.run$SITE_DCI_setting <- as.factor(df.run$SITE_DCI_setting)

table(df.run$distress_quntile)
df.run$distress_quntile <- as.character(df.run$distress_quntile)
df.run$distress_quntile[df.run$distress_quntile=="mid-tier"] <- "midtier"
df.run$distress_quntile[df.run$distress_quntile=="at-risk"] <- "atrisk"
df.run$distress_quntile <- factor(df.run$distress_quntile, 
                                   levels=c("prosperous", "comfortable", "midtier", "atrisk", "distressed"),
                                   ordered=TRUE)

table(df.run$zip_designation)
df.run$zip_designation <- as.character(df.run$zip_designation)
df.run$zip_designation[df.run$zip_designation=="Small Town"] <- "SmallTown"
df.run$zip_designation <- as.factor(df.run$zip_designation)

table(df.run$MedExp_status)
df.run$MedExp_status <- as.character(df.run$MedExp_status)
df.run$MedExp_status[df.run$MedExp_status=="Not Adopted"] <- "0"
df.run$MedExp_status[df.run$MedExp_status=="Adopted"] <- "1"
df.run$MedExp_status <- as.factor(df.run$MedExp_status)

table(df.run$OH_HFHOSPADM)
df.run$OH_HFHOSPADM <- as.character(df.run$OH_HFHOSPADM)
df.run$OH_HFHOSPADM[df.run$OH_HFHOSPADM=="unknown"] <- NA
df.run$OH_HFHOSPADM <- factor(df.run$OH_HFHOSPADM, 
                                  levels=c("0", "1", "2", ">2"),
                                  ordered=TRUE)

table(df.run$OH_SYMPTOMS)
df.run$OH_SYMPTOMS <- as.character(df.run$OH_SYMPTOMS)
df.run$OH_SYMPTOMS[df.run$OH_SYMPTOMS=="better- symptomatic"] <- "improved"
df.run$OH_SYMPTOMS[df.run$OH_SYMPTOMS=="better- asymptomatic"] <- "resolved"
df.run$OH_SYMPTOMS[df.run$OH_SYMPTOMS=="UTD"] <- NA
df.run$OH_SYMPTOMS <- factor(df.run$OH_SYMPTOMS, 
                              levels=c("worse", "unchanged", "improved", "resolved"),
                              ordered=TRUE)

table(df.run$insurance)
df.run$insurance <- as.character(df.run$insurance)
df.run$insurance[df.run$insurance=="No Insurance/Not Documented/UTD"] <- "noneORutd"
df.run$insurance <- as.factor(df.run$insurance)

table(df.run$race2i)
df.run$race2i <- as.character(df.run$race2i)
df.run$race2i[df.run$race2i=="Hispanic (any race)"] <- "Hispanic"
df.run$race2i[df.run$race2i=="Other (includes UTD)"] <- "OtherUTD"
df.run$race2i <- as.factor(df.run$race2i)


#asssume that all heart transplant and academic hospitals are known
#impute 0 for all missing of GH_HEART_TRANSPLANTS and RESIDENTS
df.run$GH_HEART_TRANSPLANTS[is.na(df.run$GH_HEART_TRANSPLANTS)] <- 0
df.run$RESIDENTS[is.na(df.run$RESIDENTS)] <- 0

#create a new var combining afib and aflutter 
df.run <- 
  df.run %>% 
  mutate(PMHx_fibfl = ifelse(PMHx_afib %in% 1 | PMHx_aflutter %in% 1, 1, 
                             ifelse(is.na(PMHx_afib) & is.na(PMHx_aflutter), NA, 0)))

df.run$PMHx_fibfl <- factor(df.run$PMHx_fibfl)

#some of the Cr values are out of range (>50); replace these values 
df.run$covar_Crdisc <- ifelse(df.run$covar_Crdisc >= 50, df.run$covar_Crdisc/10, df.run$covar_Crdisc)

####describe the new dataset #### 
df.run <- droplevels(df.run)

#save the data frame 
save(df.run, file = '/mnt/workspace/rstudio_projects/gdmt_11.4_final.RData')
