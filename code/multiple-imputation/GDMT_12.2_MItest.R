getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

library(mice)
library(miceadds)
library(micemd)
library(ggplot2)
library(lattice)

load('gdmt_11.2_catnomiss.RData')

#asssume that all heart transplant hospitals are known; impute 0 for all missing of GH_HEART_TRANSPLANTS
df.catnomiss$GH_HEART_TRANSPLANTS[is.na(df.catnomiss$GH_HEART_TRANSPLANTS)] <- 0
df.catnomiss$RESIDENTS[is.na(df.catnomiss$RESIDENTS)] <- 0


#convert SITE_ID to number 
# this is required for MI when desigated as cluster ar
df.catnomiss$SITE_ID <- as.numeric(df.catnomiss$SITE_ID)

#in first iteration of test with df.final, a vector of 17.9Gb was created??? 
#so will reduce the size of the df in testing
#df.test <- subset(df.catnomiss, (SITE_ID <= 50))
df.test <- df.catnomiss
df.test <- subset(df.test, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                       MedExp_adoption_date, MedExp_state, PMHx_priorPCIorCABG, zip_num))


#reduce number of ancillary variables to reduce resource use 
df.test <- subset(df.test, select = -c(PATIENT_ID, RCRDNUM, zip1, SITE_POSTAL_CODE, LOS, 
                                       JC_HXSMOKING, GH_ACADEMIC, GH_INTERVENTIONAL, SITESURG, 
                                       DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, 
                                       AHA_PROCEDURES_1:AHA_PROCEDURES_125,
                                       HF_ISOSORBIDE, HFS_MORTALITYRISK, OH_SYMPTOMS, 
                                       PREDMORT, SODIUMi_disc, WEIGHTDISCi, covar_dWeight, 
                                       covar_BMI, OH_HFHOSPADM, covar_ischemic, 
                                       AHA_BBPRIOR:OH_ARB, inhospRx_ACEI, inhospRx_ARB, inhospRx_hydralnitrate, 
                                       inhospRx_ARNI, PMHx_famHLD, PMHx_NEWinfdz, PMHx_CRTP, PMHx_CRTD, PMHx_PPM, 
                                       PMHx_priorPCI, PMHx_priorCABG, PMHx_HLD, PMHx_anemia, 
                                       OH_DISCBPDIAS))

#remove the components of GDMT_ var to avoid collinearity
df.test <- subset(df.test, select = -c(GDMT_HFBB:GDMT_MRAorContra, GDMT_RAASI, GDMT_RAASIorContra))

#teh SITE_DCI var do not have great outflux 
df.test <- subset(df.test, select = -c(SITE_DCI_setting:SITE_DCI_quintile))

#take out a lot of var to see if i can get it to run 
df.test <- subset(df.test, select = -c(PMHx_none:PMHx_dm2))

df.test <- droplevels(df.test)

str(df.test, list.len=ncol(df.test))

#how many missing in each variable? 
lapply(df.test, function(x) 
  sum(is.na(x)))

#observe outflux for vars in df.test 
flux(df.test)
#fluxplot(df.test)

#create the imputation method vector 
impmethod <- character(ncol(df.test))
names(impmethod) <- colnames(df.test)
print("imputation vector created")

#set imp method for outcome variables first - these are all binary
#impmethod[colnames(subset(df.test, select = c(GDMT_HFBB:GDMT_MRAorContra)))] <- "2l.bin"
#impmethod[colnames(subset(df.test, select = c(GDMT_RAASI:GDMT_ALLorContra)))] <- "2l.bin"
impmethod['GDMT_ALL'] <- "2l.bin"
impmethod['GDMT_ALLorContra'] <- "2l.bin"

#set cluster_var/level defining variable 
#SITE_ID is the level 2 defining var; it has no missing data 
cluster_var=df.test$SITE_ID

#cluster variable traits 
#site level haracteristics (4): RESIDENTS (!), GH_HEART_TRANSPLANTS (!), MCNTRLi, BEDSIZE
#impmethod['BEDSIZE'] <- "2lonly.mean"  
#mpmethod['MCNTRLi'] <- "2lonly.function"  
#impmethod['RESIDENTS'] <-"2lonly.function"  #imputation function method: logreg
#impmethod['SITE_DCI_poptotal'] <- "2lonly.mean"
#impmethod['SITE_DCI_score'] <- "2lonly.norm"
 
#define the imputation function for 2lonly.function 
#imputationFunction <- list('MCNTRLi'='polyreg')

#primary SES variables (10):
# GENDERi, race2i, insurance, AGEi,  covar_dispo (!), 
# distress_score (!), population_total (!), zip_designation (!), 
# MedExp (!), covar_OutsideZIPregion (!) 
impmethod['GENDERi'] <- "2l.bin"
#impmethod['race2i'] <- "" #????
#insurance has no missing data 
#impmethod['AGEi'] <- "2l.lmer"
#impmethod['covar_dispo'] <- "" #????
impmethod['population_total'] <- "2l.lmer"
impmethod['distress_score'] <- "2l.lmer"
#impmethod['zip_designation'] <- "" #???? 
impmethod['MedExp'] <- "2l.bin"
impmethod['covar_OutsideZIP'] <- "2l.bin"

#clinical covariate data (5): 
# OH_EF, covar_ino, OH_DISCHR (!), OH_DISCBPSYST (!), OH_TRANSPLANT
#OH_EF has no missing data 
impmethod['covar_ino'] <- "2l.bin"   
impmethod['OH_DISCHR'] <- "2l.lmer" 
impmethod['OH_DISCBPSYST'] <- "2l.lmer" 
#OH_TRANSPLANT has no missing data 

#inhosp Rx (3): 
# inhospRx_BB (!), inhospRx_RAASI (!), inhospRx_MRA (!)
#impmethod['inhospRx_BB'] <- "2l.bin" 
#impmethod['inhospRx_RAASI'] <- "2l.bin" 
#impmethod['inhospRx_MRA'] <- "2l.bin" 
#impmethod['inhospRx_none'] <- "2l.bin"

#other medical comorbidities (17): 
# PMHx_none, PMHx_afib, PMHx_aflutter, PMHx_COPD, PMHx_HTN, PMHx_priorMI, 
# PMHx_priorCVATIA, PMHx_CHF, PMHx_ESRD, PMHx_Cr2, PMHx_MDD, PMHx_valvedz, 
# PMHx_priorPCIorCABG, PMHx_LVAD, PMHx_CardioMEMS, PMHx_OSAOHS, PMHx_dm2 
# impmethod['PMHx_none'] <- "2l.bin" 
# impmethod['PMHx_afib'] <- "2l.bin" 
# impmethod['PMHx_aflutter'] <- "2l.bin" 
# impmethod['PMHx_COPD'] <- "2l.bin" 
# impmethod['PMHx_HTN'] <- "2l.bin" 
# impmethod['PMHx_priorMI'] <- "2l.bin" 
# impmethod['PMHx_priorCVATIA'] <- "2l.bin" 
# impmethod['PMHx_CHF'] <- "2l.bin" 
# impmethod['PMHx_ESRD'] <- "2l.bin" 
# impmethod['PMHx_Cr2'] <- "2l.bin" 
# impmethod['PMHx_MDD'] <- "2l.bin" 
# impmethod['PMHx_valvedz'] <- "2l.bin" 
# impmethod['PMHx_LVAD'] <- "2l.bin" 
# impmethod['PMHx_CardioMEMS'] <- "2l.bin" 
# impmethod['PMHx_OSAOHS'] <- "2l.bin" 
# impmethod['PMHx_dm2'] <- "2l.bin"

#dc labs meds
#impmethod['covar_Kdisc'] <- "2l.lmer"
#impmethod['covar_Crdisc'] <- "2l.lmer"


print("all imputation methods assigned")

#####make the predicator matrix ####
pm <- make.predictorMatrix(df.test)

#set the SITE_ID as the cluster var 
pm[,'SITE_ID'] <- -2
pm['SITE_ID','SITE_ID'] <- 0

#simplify the imputation model by removing predictors with low outflox 
pm[, c('inhospRx_RAASI', 'inhospRx_BB', 'inhospRx_MRA', 'inhospRx_none', 
       'covar_Kdisc', 'covar_Crdisc', 'distress_quntile', 'distress_score', 
       'population_total', 'MedExp_status', 'OH_DISCHR', 'OH_DISCBPSYST', 
       'covar_OutsideZIP')] <- 0

#remove more var? still too many var 
#pm[, c('SITE_DCI_setting', 'SITE_DCI_poptotal', 'SITE_DCI_score')] <- 0
#pm[, c('PMHx_CardioMEMS','PMHx_OSAOHS', 'PMHx_afib', 'PMHx_aflutter', 'PMHx_ICD')] <- 0

#remove some var from the prediction (eg PATIENT_ID); eg setting <- 0
#pm[, c('PATIENT_ID', 'RCRDNUM')] <- c(0,0)

#set the cluster var characteristics as random efx with random 
#intercept and random slope; eg setting <- 2
#RESIDENTS (!), GH_HEART_TRANSPLANTS (!), MCNTRLi, BEDSIZE
pm[, c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 
       'MCNTRLi', 'BEDSIZE')] <- c(0,0,0,0)

#do not use a variable as a predictor to impute itself 
#pm['RESIDENTS','RESIDENTS'] <- 0
pm['MCNTRLi','MCNTRLi'] <- 0
pm['BEDSIZE','BEDSIZE'] <- 0
#pm['SITE_DCI_score','SITE_DCI_score'] <- 0
#pm['SITE_DCI_poptotal','SITE_DCI_poptotal'] <- 0

#for level 2 var, do not set other level 2 var as random effects 
# pm['RESIDENTS', c('GH_HEART_TRANSPLANTS', 'MCNTRLi', 'BEDSIZE', 
#                   'SITE_DCI_score', 'SITE_DCI_poptotal')] <- c(1,1,1,1,1)
# pm['MCNTRLi', c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 'BEDSIZE', 'SITE_DCI_score', 
#                 'SITE_DCI_poptotal')] <- c(1,1,1,1,1)
# pm['BEDSIZE', c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 'MCNTRLi', 'SITE_DCI_score', 
#                 'SITE_DCI_poptotal')] <- c(1,1,1,1,1)
# pm['SITE_DCI_score', c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 'MCNTRLi', 'BEDSIZE', 
#                 'SITE_DCI_poptotal')] <- c(1,1,1,1,1)
# pm['SITE_DCI_poptotal', c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 'MCNTRLi', 'BEDSIZE', 
#                        'SITE_DCI_score')] <- c(1,1,1,1,1)

print("running MICE")

imptest1.1 <- mice(df.test, m=2, predictorMatrix=pm, method=impmethod, 
                   maxit=1, seed=1234)
#  imputationFunction=imputationFunction, cluster_var=cluster_var, remove.collinear=FALSE

summary(complete(imptest1.1))
imptest1.1$imp$GENDERi
rm(imptest1.1, imptest1)
