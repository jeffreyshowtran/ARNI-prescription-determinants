getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#must use mice version > 3.13.6 as this eliminates the problem with 2lmer failing when a cluster has n=1
#install.packages("devtools")
#devtools::install_github(repo="amices/mice")
#install.packages("ordinal")

library(dplyr)
library(tidyr)
library(ggplot2)
# # library(lattice)
library(lme4)
library(optimx)
library(broom.mixed)
library(performance)
# library(performance)
# # library(parlmice) 
# library(parallel)
# library(ordinal)


load('gdmt_11.8.2_final.RData')


####clean data####
df.cc <- subset(df.run, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                     MedExp_adoption_date, MedExp_state, 
                                     zip_num, RCRDNUM, zip1, SITE_POSTAL_CODE, PATIENT_ID,
                                     DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, AHA_PROCEDURES_1:AHA_PROCEDURES_125,
                                     inhospRx_hydralnitrate, 
                                     covar_dWeight, covar_BMI, 
                                     covar_ischemic, HFS_MORTALITYRISK, OH_SYMPTOMS, PREDMORT, 
                                     PMHx_priorPCI, PMHx_priorCABG, PMHx_famHLD, PMHx_NEWinfdz, 
                                     PMHx_anemia, PMHx_priorMI, 
                                     PMHx_afib, PMHx_aflutter, PMHx_priorPCIorCABG, PMHx_HLD, PMHx_PVD, 
                                     GH_ACADEMIC, GH_INTERVENTIONAL, SITESURG, 
                                     AHA_BBPRIOR:HF_ISOSORBIDE, OH_DISCBPDIAS, 
                                     GDMT_ACEI:GDMT_ARBorContra, GDMT_ARNIorContra, GDMT_ALL, GDMT_ALLorContra))
#GDMT_HFBBorContra, GDMT_RAASIorContra, GDMT_MRAorContra, 

#remove all patients with an LVAD 
df.cc <- subset(df.cc, PMHx_LVAD == 0 | is.na(df.cc$PMHx_LVAD))


#create the complete cases dataset 
df.cc <- subset(df.cc, select = -c(SITE_DCI_poptotal, SITE_DCI_setting, BEDSIZE, MedExp_status,
                                     SODIUMi_disc, covar_OutsideZIP, OH_HFHOSPADM, SITE_DCI_score,
                                     PMHx_LVAD, PMHx_OSAOHS,  
                                     LOS))
#, covar_Crdisc, covar_Kdisc


#remove patients with missing data in key cat var with levels > 2 
df.cc <- subset(df.cc, !(is.na(df.cc$race2i)))
#df.run <- subset(df.run, !(is.na(df.cc$MCNTRLi)))

#for all other cat var with levels > 2, make missing a category but DO NOT include in final model 
#execute addNA() to add NA as an explicit level
# list = c('insurance', 'ADMITSOURCEi', 'covar_dispo', 'OH_HFHOSPADM', 'zip_designation')
# df.cc[list] <- lapply(df.cc[list], function(x) addNA(x))
# rm(list)

# df.cc$insurance <- as.character(df.cc$insurance)
# df.cc$insurance[is.na(df.cc$insurance)] <- "UTD"
# df.cc$insurance <- as.factor(df.cc$insurance)
# 
# df.cc$ADMITSOURCEi <- as.character(df.cc$ADMITSOURCEi)
# df.cc$ADMITSOURCEi[is.na(df.cc$ADMITSOURCEi)] <- "UTD"
# df.cc$ADMITSOURCEi <- as.factor(df.cc$ADMITSOURCEi)
# 
# df.cc$covar_dispo <- as.character(df.cc$covar_dispo)
# df.cc$covar_dispo[is.na(df.cc$covar_dispo)] <- "UTD"
# df.cc$covar_dispo <- as.factor(df.cc$covar_dispo)
# 
# df.cc$zip_designation <- as.character(df.cc$zip_designation)
# df.cc$zip_designation[is.na(df.cc$zip_designation)] <- "UTD"
# df.cc$zip_designation <- as.factor(df.cc$zip_designation)
# 
# df.cc$MCNTRLi <- as.character(df.cc$MCNTRLi)
# df.cc$MCNTRLi[is.na(df.cc$MCNTRLi)] <- "UTD"
# df.cc$MCNTRLi <- as.factor(df.cc$MCNTRLi)


#reduce the number of levels of covar_dispo
# df.cc <- 
#   df.cc %>% 
#   mutate(dispo = ifelse(covar_dispo == 'AcuteCareFacility', 'HospTransfer', 
#                  ifelse(covar_dispo == 'DoC', 'DoC', 
#                  ifelse(covar_dispo == 'Home', 'Home', 
#                  ifelse(covar_dispo == 'HomeHealthCare', 'HomeHealthCare', 
#                  ifelse(covar_dispo == 'Homless', 'Homeless', 
#                  ifelse(covar_dispo == 'ICF', 'OtherFacility',
#                  ifelse(covar_dispo == 'International', 'Home', 
#                  ifelse(covar_dispo == 'IRF', 'OtherFacility', 
#                  ifelse(covar_dispo == 'LTCH', 'OtherFacility', 
#                  ifelse(covar_dispo == 'SNF', 'OtherFacility', 
#                  ifelse(covar_dispo == 'OtherFacility', 'OtherFacility', 
#                  ifelse(covar_dispo == 'UTD', 'UTD', 'error')))))))))))))
# df.cc$dispo <- as.factor(df.cc$dispo)

#create a new binary variable- transfer to other health care facility? 
df.cc <- 
  df.cc %>% 
  mutate(DCtoContCare = ifelse(covar_dispo == 'AcuteCareFacility', 1, 
                       ifelse(covar_dispo == 'DoC', 0, 
                        ifelse(covar_dispo == 'Home', 0, 
                       ifelse(covar_dispo == 'HomeHealthCare', 1, 
                        ifelse(covar_dispo == 'Homless', 0, 
                       ifelse(covar_dispo == 'ICF', 1,
                        ifelse(covar_dispo == 'International', 0, 
                       ifelse(covar_dispo == 'IRF', 1, 
                        ifelse(covar_dispo == 'LTCH', 1, 
                       ifelse(covar_dispo == 'SNF', 1, 
                        ifelse(covar_dispo == 'OtherFacility', 1, 
                               ifelse(covar_dispo == 'UTD', NA, 999)))))))))))))
df.cc$DCtoContCare <- as.factor(df.cc$DCtoContCare)

#df.cc <- subset(df.cc, select = -c(covar_dispo))

#create a new var to say if someone has an ICD/PPM/CRTD
df.cc <- 
  df.cc %>% 
  mutate(PPMICDCRT = ifelse(PMHx_ICD %in% 1 | 
                              PMHx_PPM %in% 1 |
                              PMHx_CRTD %in% 1 |
                              PMHx_CRTP %in% 1 , 1, 
                            ifelse(is.na(PMHx_ICD) & 
                                     is.na(PMHx_PPM) & 
                                     is.na(PMHx_CRTD) &
                                     is.na(PMHx_CRTP), NA, 
                                   ifelse(PMHx_ICD == 0 & 
                                            PMHx_PPM == 0 & 
                                            PMHx_CRTD == 0 & 
                                            PMHx_CRTP == 0 , 0, 99))))
df.cc$PPMICDCRT <- as.factor(df.cc$PPMICDCRT)

df.cc <- subset(df.cc, select = -c(PMHx_ICD, PMHx_PPM, PMHx_CRTD, PMHx_CRTP))

#create a new variable- does the patient have insurance? 
df.cc <- 
  df.cc %>% 
  mutate(insured = ifelse(insurance %in% 'Medicaid' | 
                            insurance %in% 'Medicare' | 
                            insurance %in% 'Other', 1,  
                          ifelse(insurance %in% 'noneORutd', 0, 
                                 ifelse(insurance %in% 'UTD', NA, 999))))
df.cc$insured <- as.factor(df.cc$insured)

#remove patients with dc Na < 100 (unlikely compatible with life)
#df.cc$SODIUMi_disc[df.cc$SODIUMi_disc<=100] <- NA

#change Cr to the natural log of Cr 
#df.cc$covar_Crdisc <- log(df.cc$covar_Crdisc)


#create a new outcome using passive imputation that is the sum of the GDMT components 
#eg a patient gets 1 point for each component of GDMT prescribed, so it should range from [0,3]
df.cc <-
  df.cc %>%
  mutate(GDMT_Rx_score = as.numeric(GDMT_HFBB) +
                         as.numeric(GDMT_RAASI) +
                         as.numeric(GDMT_MRA) - 3)

df.cc <-
  df.cc %>%
  mutate(GDMT_RxContra_score = as.numeric(GDMT_HFBBorContra) +
           as.numeric(GDMT_RAASIorContra) +
           as.numeric(GDMT_MRAorContra) -3)

####describe the df.cc #### 
names(df.cc)

str(df.cc, list.len=ncol(df.cc))

#how many missing in each variable? 
lapply(df.cc, function(x) 
  sum(is.na(x))
)

#how many complete cases? 
sum(complete.cases(df.cc))


#isolate regression dataset 
#create the complete cases dataset 
df.cc <- subset(df.cc, select = c(GDMT_ARNI, GENDERi, race2i, AGEi, insurance, DCtoContCare,  
                                   PMHx_ESRD, PMHx_Cr2, WEIGHTDISCi, 
                                   GH_HEART_TRANSPLANTS, RESIDENTS, SITE_DCI_quintile, 
                                   OH_EF, OH_TRANSPLANT, covar_Kdisc, covar_Crdisc, OH_DISCBPSYST,  
                                   inhospRx_RAASI, inhospRx_ACEI, inhospRx_ARB, inhospRx_ARNI, OH_ARNI,
                                   followup, 
                                   distress_score, distress_quntile, zip_designation, SITE_ID, 
                                   GDMT_ARNIcontra, 
                                  contra_allergy, contra_hyperK, contra_HoTN, contra_otherMED, 
                                  contra_patient, contra_renalfxn, contra_system, contra_ACEI36))


#reformat key categorical variables 
df.cc$race2i <- as.character(df.cc$race2i)
df.cc$race2i = ifelse(df.cc$race2i == 'OtherUTD', 'Other', df.cc$race2i)
df.cc$race2i <- factor(df.cc$race2i, levels = c('White', 'Asian', 'Black', 'Hispanic', 'Other'))

df.cc$insurance <- as.character(df.cc$insurance)
df.cc$insurance = ifelse(df.cc$insurance == 'noneORutd', 'None', df.cc$insurance)
df.cc$insurance <- factor(df.cc$insurance, levels = c('Other', 'Medicaid', 'Medicare', 'None'))

df.cc$zip_designation <- as.character(df.cc$zip_designation)
df.cc$zip_designation <- factor(df.cc$zip_designation, levels = c('Urban', 'Suburban', 'SmallTown', 'Rural'))

#calculate creatinine clearance 
df.cc$covar_Crdisc[df.cc$covar_Crdisc %in% 0] <- NA 

df.cc <- 
  df.cc %>% 
  mutate(CrCl = ifelse(GENDERi %in% 'male' & !is.na(covar_Crdisc) & !is.na(AGEi) & !is.na(WEIGHTDISCi), ((140 - AGEi)*WEIGHTDISCi)/(72*covar_Crdisc), 
                ifelse(GENDERi %in% 'female'& !is.na(covar_Crdisc) & !is.na(AGEi) & !is.na(WEIGHTDISCi), 0.85*((140 - AGEi)*WEIGHTDISCi)/(72*covar_Crdisc), 
                       NA)))



#develop binary classificaitons of BP, K, and Cr
df.cc$binary_hyperK = ifelse(df.cc$covar_Kdisc > 5, 1, 0)
df.cc$binary_hyperK <- as.factor(df.cc$binary_hyperK)

df.cc$CrCl30 = ifelse(df.cc$CrCl < 30, 1, 0)
df.cc$CrCl30 <- as.factor(df.cc$CrCl30)

df.cc$HoTN = ifelse(df.cc$OH_DISCBPSYST < 90, 1, 0)
df.cc$HoTN <- as.factor(df.cc$HoTN)

#grand mean center distress_score, age, and ejection fraction 
gm_DCI = mean(df.cc$distress_score, na.rm=TRUE)
df.cc$GMrescale_DCI = df.cc$distress_score - gm_DCI 

gm_EF = mean(df.cc$OH_EF, na.rm=TRUE)
df.cc$GMrescale_EF = df.cc$OH_EF - gm_EF

gm_age = mean(df.cc$AGEi, na.rm=TRUE)
df.cc$GMrescale_age = df.cc$AGEi - gm_age

rm(gm_DCI, gm_EF, gm_age)

df.cc$distress_quntile <- factor(df.cc$distress_quntile, ordered = FALSE)
df.cc$SITE_DCI_quintile <- factor(df.cc$SITE_DCI_quintile, ordered = FALSE)
df.cc$OH_ARNI <- factor(df.cc$OH_ARNI, ordered = FALSE)

#create a new composite outcome with 1 for ARNI contra based on system or patient reasons 
df.cc <-
  df.cc %>%
  mutate(subjcontra = ifelse(contra_patient %in% 1 | contra_system %in% 1, 1, 0))
df.cc$subjcontra <- as.factor(df.cc$subjcontra)

print('ready')

#### run the analysis phase 
#build an empty model 
#m0 <- glmer(GDMT_ARNI ~ (1 | SITE_ID), data=df.cc, family='binomial')
#icc(m0) # ICC = 0.254, random effects model is indicated 
#m4 swapped GDMT_ARNIcontra for its individual components 



start_time <- Sys.time() 
m6 <- glmer(GDMT_ARNI ~ GENDERi + race2i + GMrescale_age + insurance + DCtoContCare + 
              binary_hyperK + CrCl30 + HoTN + 
              PMHx_ESRD + PMHx_Cr2 + 
              GH_HEART_TRANSPLANTS + RESIDENTS + 
              GMrescale_EF + OH_TRANSPLANT +
              followup + 
              distress_quntile + zip_designation + 
              inhospRx_ACEI + inhospRx_ARB + inhospRx_ARNI +
              GDMT_ARNIcontra +
              (1 | SITE_ID), 
            data=df.cc, family='binomial', verbose=2, 
            control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
saveRDS(m6, file='m6.rds')

#calculate the standard errors 
summary(m6)
se6 <- sqrt(diag(vcov(m6)))
tab6 <- cbind(Est = fixef(m6), LL = fixef(m6) - 1.96*se6, UL = fixef(m6) + 1.96* se6)
exp(tab6)
Rtab6 <- format(round(exp(tab6),3), nsmall=3)
Rtab6
# hist(residuals(m6))
# tidy(m6) %>% print(n=40)

#with OH_ARNI and SITE_DCI_quint
m7 <- glmer(GDMT_ARNI ~ GENDERi + race2i + GMrescale_age + insurance + DCtoContCare + 
              binary_hyperK + CrCl30 + HoTN + 
              PMHx_ESRD + PMHx_Cr2 + 
              GH_HEART_TRANSPLANTS + RESIDENTS + SITE_DCI_quintile + 
              GMrescale_EF + OH_TRANSPLANT +
              followup + 
              distress_quntile + zip_designation + 
              inhospRx_ACEI + inhospRx_ARB + inhospRx_ARNI + OH_ARNI + 
              GDMT_ARNIcontra +
              (1 | SITE_ID), 
            data=df.cc, family='binomial', verbose=2, 
            control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
saveRDS(m7, file='m7.rds')

#calculate the standard errors 
summary(m7)
se7 <- sqrt(diag(vcov(m7)))
tab7 <- cbind(Est = fixef(m7), LL = fixef(m7) - 1.96*se7, UL = fixef(m7) + 1.96* se7)
exp(tab7)
Rtab7 <- format(round(exp(tab7),3), nsmall=3)
Rtab7
hist(residuals(m7))
tidy(m7) %>% print(n=40)


#post HOC analysis
ph1 <- glmer(inhospRx_ARNI ~ GENDERi + race2i + GMrescale_age + insurance +
               PMHx_ESRD + PMHx_Cr2 + 
               GH_HEART_TRANSPLANTS + RESIDENTS + SITE_DCI_quintile + 
               GMrescale_EF + OH_TRANSPLANT +
               distress_quntile + zip_designation + OH_ARNI + 
               GDMT_ARNIcontra +
               (1 | SITE_ID), 
             data=df.cc, family='binomial', verbose=2, 
             control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
saveRDS(ph1, file='ph1.rds')
se_ph1 <- sqrt(diag(vcov(ph1)))
tab_ph1 <- cbind(Est = fixef(ph1), LL = fixef(ph1) - 1.96*se_ph1, UL = fixef(ph1) + 1.96* se_ph1)
exp(tab_ph1)
Rtab_ph1 <- format(round(exp(tab_ph1),3), nsmall=3)
Rtab_ph1

end_time <- Sys.time()
end_time - start_time 

# ph2 <- glmer(GDMT_ARNIcontra ~ GENDERi + race2i + GMrescale_age + insurance + 
#                PMHx_ESRD + PMHx_Cr2 + 
#                GH_HEART_TRANSPLANTS + RESIDENTS + SITE_DCI_quintile + 
#                GMrescale_EF + OH_TRANSPLANT +
#                distress_quntile + zip_designation + 
#                (1 | SITE_ID), 
#              data=df.cc, family='binomial', verbose=2, 
#              control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
# se_ph2 <- sqrt(diag(vcov(ph2)))
# tab_ph2 <- cbind(Est = fixef(ph2), LL = fixef(ph2) - 1.96*se_ph2, UL = fixef(ph2) + 1.96* se_ph2)
# exp(tab_ph2)
# Rtab_ph2 <- format(round(exp(tab_ph2),3), nsmall=3)
# Rtab_ph2


#ARNI contra based on system or patient reasons 
ph4 <- glmer(subjcontra ~ GENDERi + race2i + GMrescale_age + insurance + 
               PMHx_ESRD + PMHx_Cr2 + 
               GH_HEART_TRANSPLANTS + RESIDENTS + 
               GMrescale_EF + OH_TRANSPLANT +
               distress_quntile + zip_designation + 
               inhospRx_ACEI + inhospRx_ARB + inhospRx_ARNI + 
               (1 | SITE_ID), 
             data=df.cc, family='binomial', 
             control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
saveRDS(ph4, file='ph4.rds')
summary(ph4)
se_ph4 <- sqrt(diag(vcov(ph4)))
tab_ph4 <- cbind(Est = fixef(ph4), LL = fixef(ph4) - 1.96*se_ph4, UL = fixef(ph4) + 1.96* se_ph4)
exp(tab_ph4)
Rtab_ph4 <- format(round(exp(tab_ph4),3), nsmall=3)
Rtab_ph4


#looking at SES and OH_ARNI  
ph5 <- glmer(OH_ARNI ~ GENDERi + race2i + GMrescale_age + insurance + 
               PMHx_ESRD + PMHx_Cr2 + 
               GMrescale_EF + OH_TRANSPLANT +
               distress_quntile + zip_designation + 
               GDMT_ARNIcontra + 
               (1 | SITE_ID), 
             data=df.cc, family='binomial', 
             control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE))
saveRDS(ph5, file='ph5.rds')
summary(ph5)
se_ph5 <- sqrt(diag(vcov(ph5)))
tab_ph5 <- cbind(Est = fixef(ph5), LL = fixef(ph5) - 1.96*se_ph5, UL = fixef(ph5) + 1.96* se_ph5)
exp(tab_ph5)
Rtab_ph5 <- format(round(exp(tab_ph5),3), nsmall=3)
Rtab_ph5


#m0 is an intercept only model 
#m1-3 are test models 
#m4 breaks ARNIcontra into its individual components and the model blows up (causes complete separation)
#m5 is a nice model but the interpretation of ordinal data is too sophisticated for MDs 
#m6 we run distress_quintile as an unordered factor 

#make a plot of predicted probabilities based on OH_EF, then we will segregate these predictions 
#on distress quintile 

#create a new complete dataset 
df <- subset(df.cc, select = c(GDMT_ARNI, GENDERi, race2i, GMrescale_age, insurance, DCtoContCare, 
                                 binary_hyperK, CrCl30, HoTN, OH_EF, OH_ARNI, 
                                 PMHx_ESRD, PMHx_Cr2, 
                                 GH_HEART_TRANSPLANTS, RESIDENTS, SITE_DCI_quintile, 
                                 GMrescale_EF, OH_TRANSPLANT, 
                                 followup,  
                                 distress_quntile, zip_designation,  
                                 inhospRx_ACEI, inhospRx_ARB, inhospRx_ARNI,  
                                 GDMT_ARNIcontra, SITE_ID, subjcontra))
df <- df.cc[complete.cases(df),]

#create a bar graph showing the number of GDMT prescriptions per site
dfhist <- subset(df, select=c(GDMT_ARNI, SITE_ID))
dfhist$SITE_ID   <- as.numeric(dfhist$SITE_ID)
dfhist$GDMT_ARNI   <- as.numeric(dfhist$GDMT_ARNI) -1
counts <- aggregate(dfhist$GDMT_ARNI, by=list(Category=dfhist$SITE_ID), FUN=sum)
counts <- cbind(1:196, counts)
colnames(counts) <- c('sites', 'SITEID', 'ARNIprescribed')

ggplot(counts, aes(x=sites, y=ARNIprescribed)) + 
  geom_bar(stat='identity') + 
  ylab("Count of ARNI prescriptions")
#ggsave('ARNI_bySITE.png')


#set variables as desired for prediction 
df$inhospRx_ARNI <- 1
df$inhospRx_ARNI <- as.factor(df$inhospRx_ARNI)

df$GDMT_ARNIcontra = ifelse(df$distress_quntile %in% 'prosperous', rbinom(nrow(df), 1, 0),
                     ifelse(df$distress_quntile %in% 'comfortable', rbinom(nrow(df), 1, 0.3),
                     ifelse(df$distress_quntile %in% 'midtier', rbinom(nrow(df), 1, 0.40),
                     ifelse(df$distress_quntile %in% 'atrisk', rbinom(nrow(df), 1, 0.45),
                     ifelse(df$distress_quntile %in% 'distressed', rbinom(nrow(df), 1, 1), 999)))))
df$GDMT_ARNIcontra[is.na(df$GDMT_ARNIcontra)] <- 1
table(df$GDMT_ARNIcontra, df$distress_quntile, exclude=NULL)
df$GDMT_ARNIcontra <- as.factor(df$GDMT_ARNIcontra)
                            
# df$GDMT_ARNIcontra <- 0
# df$GDMT_ARNIcontra[df$distress_quntile %in% 'distressed'] <- 1
# df$GDMT_ARNIcontra <- factor(df$GDMT_ARNIcontra, levels = c(0,1))

df$DCtoContCare <- 0
df$DCtoContCare <- factor(df$DCtoContCare, levels = c(0,1))

df$binary_hyperK <- 0
df$binary_hyperK <- factor(df$binary_hyperK, levels = c(0,1))

df$CrCl30 <- 0
df$CrCl30 <- factor(df$CrCl30, levels = c(0,1))

df$zip_designation <- 'Urban'
df$zip_designation <- as.factor(df$zip_designation)

df$inhospRx_ACEI <- 0
df$inhospRx_ACEI <- factor(df$inhospRx_ACEI, levels = c(0,1))

df$inhospRx_ARB <- 0
df$inhospRx_ARB <- factor(df$inhospRx_ARB, levels = c(0,1))

df$race2i <- 'White'
df$race2i <- as.factor(df$race2i)


jvalues <- with(df, seq(from = min(OH_EF), to = max(OH_EF), length.out = 100))

pp2 <- lapply(levels(df$distress_quntile), function(quint) {
  df$distress_quntile[] <- quint
  lapply(jvalues, function(j) {
    df$GMrescale_EF <- j
    predict(m6, newdata=df, type = "response")
  })
})

plotdat2 <- lapply(pp2, function(X) {
  temp <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(0.25, 0.75)))
  } ))
  temp <- as.data.frame(cbind(temp, jvalues))
  colnames(temp) <- c("PredictedProbability", "Lower", "Upper", "EjectionFraction")
  return(temp)
})

plotdat2 <- do.call(rbind, plotdat2)

plotdat2$distress_quntile <- factor(rep(levels(df$distress_quntile), each = length(jvalues)))


plotdat2$distress_quntile <- as.character(plotdat2$distress_quntile)
plotdat2$distress_quntile <- factor(plotdat2$distress_quntile, 
                              labels = c('prosperous', 'comfortable', 'midtier', 'atrisk', 'distressed'), 
                              ordered = TRUE)

ggplot(plotdat2, aes(x=EjectionFraction, y=PredictedProbability)) + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, fill = distress_quntile), alpha = 0.25) + 
  geom_line(aes(colour=distress_quntile), size=2) + 
  ylim(c(0,1)) + facet_wrap(~ distress_quntile) + 
  scale_colour_manual(values=c("purple", "blue", "green", "orange", "red")) + 
  scale_fill_manual(values=c("purple", "blue", "green", "orange", "red"))



pp <- lapply(jvalues, function(j) { 
  df$GMrescale_EF <- j 
  predict(m6, newdata=df, type = "response")
  })

plotdat <- t(sapply(pp, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

plotdat <- as.data.frame(cbind(plotdat, jvalues))
colnames(plotdat) <- c("PredictedProbability", "Lower", "Upper", "DifferenceFromMeanEF")

ggplot(plotdat, aes(x = DifferenceFromMeanEF, y = PredictedProbability)) + 
  geom_linerange(aes(ymin=Lower, ymax=Upper)) + 
  geom_line(size=2) + ylim(c(0,1))
