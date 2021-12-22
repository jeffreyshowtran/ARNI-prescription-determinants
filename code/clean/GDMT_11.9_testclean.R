getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

load('gdmt_2_include.RData')
df.arni <- subset(df_gdmt, select = c(RCRDNUM, OH_ARNI))
rm(df_gdmt)

load('gdmt_11.8_final.RData')
df.run = merge(df.run, df.arni, by.x="RCRDNUM", by.y="RCRDNUM", all.x=TRUE)
rm(df.arni)
####clean data####
df.run <- subset(df.run, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                     MedExp_adoption_date, MedExp_state, 
                                     zip_num, zip1, SITE_POSTAL_CODE, PATIENT_ID,
                                     DYN_OTHERCONDITION_1:DYN_OTHERCONDITION_8, AHA_PROCEDURES_1:AHA_PROCEDURES_125,
                                    inhospRx_hydralnitrate, 
                                     WEIGHTDISCi, covar_dWeight, covar_BMI, 
                                     covar_ischemic, HFS_MORTALITYRISK, OH_SYMPTOMS, PREDMORT, 
                                     PMHx_priorPCI, PMHx_priorCABG, PMHx_famHLD, PMHx_NEWinfdz, 
                                     PMHx_anemia, PMHx_priorMI, PMHx_CAD, 
                                     PMHx_afib, PMHx_aflutter, PMHx_priorPCIorCABG, PMHx_HLD, PMHx_PVD, 
                                     GH_ACADEMIC, GH_INTERVENTIONAL, SITESURG, 
                                     AHA_BBPRIOR:HF_ISOSORBIDE, OH_DISCBPDIAS, 
                                     distress_quntile, SITE_DCI_quintile))

#remove all patients with an LVAD 
df.run <- subset(df.run, PMHx_LVAD == 0 | is.na(df.run$PMHx_LVAD))

#the run time was too long, need to simplify model 
df.run <- subset(df.run, select = -c(SITE_DCI_poptotal, SITE_DCI_setting, BEDSIZE, MedExp_status,
                                     SODIUMi_disc, covar_OutsideZIP, OH_HFHOSPADM, SITE_DCI_score,
                                     PMHx_LVAD, PMHx_OSAOHS,  
                                     LOS, 
                                     GDMT_HFBBorContra, GDMT_RAASIorContra, GDMT_MRAorContra, GDMT_ALL, GDMT_ALLorContra))


df.run <- 
  df.run %>% 
  mutate(GDMT_ACEIARB = ifelse(GDMT_ACEI %in% '1' | GDMT_ARB %in% '1', 1, 
                        ifelse(GDMT_ACEI %in% '0' & GDMT_ARB %in% '0', 0, 
                        ifelse(GDMT_ACEI %in% '0' & is.na(GDMT_ARB), 0, 
                        ifelse(is.na(GDMT_ACEI) & GDMT_ARB %in% '0', 0, 
                        ifelse(is.na(GDMT_ACEI) & is.na(GDMT_ARB), NA, 999))))))
df.run$GDMT_ACEIARB <- as.factor(df.run$GDMT_ACEIARB)

df.run <- 
  df.run %>% 
  mutate(inhospRx_ACEIARB = ifelse(inhospRx_ACEI %in% '1' | inhospRx_ARB %in% '1', 1, 
                            ifelse(inhospRx_ACEI %in% '0' & inhospRx_ARB %in% '0', 0, 
                            ifelse(inhospRx_ACEI %in% '0' & is.na(inhospRx_ARB), 0, 
                            ifelse(is.na(inhospRx_ACEI) & inhospRx_ARB %in% '0', 0, 
                            ifelse(is.na(inhospRx_ACEI) & is.na(inhospRx_ARB), NA, 999))))))
df.run$inhospRx_ACEIARB <- as.factor(df.run$inhospRx_ACEIARB)


df.run <- subset(df.run, select = -c(GDMT_ACEI:GDMT_ARBorContra, GDMT_ARNIorContra, 
                                     inhospRx_ACEI, inhospRx_ARB, GDMT_RAASI, inhospRx_RAASI))

# match("inhospRx_ARNI", names(df.run))
# df.run <- df.run[,c(1, 2, 53, 3, 4, 48:51, 5:47, 52, 54)]
# df.run <- df.run[,c(1:41, 54, 42:53)]
#remove patients with missing data in key cat var with levels > 2 
df.run <- subset(df.run, !(is.na(df.run$race2i)))
#df.run <- subset(df.run, !(is.na(df.run$MCNTRLi)))

#for all other cat var with levels > 2, make missing a category but DO NOT include in final model 
#execute addNA() to add NA as an explicit level
# list = c('insurance', 'ADMITSOURCEi', 'covar_dispo', 'OH_HFHOSPADM', 'zip_designation')
# df.run[list] <- lapply(df.run[list], function(x) addNA(x))
# rm(list)

df.run$insurance <- as.character(df.run$insurance)
df.run$insurance[is.na(df.run$insurance)] <- "UTD"
df.run$insurance <- as.factor(df.run$insurance)

df.run$ADMITSOURCEi <- as.character(df.run$ADMITSOURCEi)
df.run$ADMITSOURCEi[is.na(df.run$ADMITSOURCEi)] <- "UTD"
df.run$ADMITSOURCEi <- as.factor(df.run$ADMITSOURCEi)

df.run$covar_dispo <- as.character(df.run$covar_dispo)
df.run$covar_dispo[is.na(df.run$covar_dispo)] <- "UTD"
df.run$covar_dispo <- as.factor(df.run$covar_dispo)

df.run$zip_designation <- as.character(df.run$zip_designation)
df.run$zip_designation[is.na(df.run$zip_designation)] <- "UTD"
df.run$zip_designation <- as.factor(df.run$zip_designation)

df.run$MCNTRLi <- as.character(df.run$MCNTRLi)
df.run$MCNTRLi[is.na(df.run$MCNTRLi)] <- "UTD"
df.run$MCNTRLi <- as.factor(df.run$MCNTRLi)


#reduce the number of levels of covar_dispo
# df.run <- 
#   df.run %>% 
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
# df.run$dispo <- as.factor(df.run$dispo)

#create a new binary variable- transfer to other health care facility? 
df.run <- 
  df.run %>% 
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
df.run$DCtoContCare <- as.factor(df.run$DCtoContCare)

#df.run <- subset(df.run, select = -c(covar_dispo))

#create a new var to say if someone has an ICD/PPM/CRTD
df.run <- 
  df.run %>% 
  mutate(PMHx_PPMICDCRT = ifelse(PMHx_ICD %in% 1 | 
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
df.run$PMHx_PPMICDCRT <- as.factor(df.run$PMHx_PPMICDCRT)

df.run <- subset(df.run, select = -c(PMHx_ICD, PMHx_PPM, PMHx_CRTD, PMHx_CRTP))

#create a new variable- does the patient have insurance? 
df.run <- 
  df.run %>% 
  mutate(insured = ifelse(insurance %in% 'Medicaid' | 
                            insurance %in% 'Medicare' | 
                            insurance %in% 'Other', 1,  
                          ifelse(insurance %in% 'noneORutd', 0, 
                                 ifelse(insurance %in% 'UTD', NA, 999))))
df.run$insured <- as.factor(df.run$insured)

df.run$GDMT_BBcontra <- as.factor(df.run$GDMT_BBcontra)
df.run$GDMT_RAASIcontra <- as.factor(df.run$GDMT_RAASIcontra)
df.run$GDMT_MRAcontra <- as.factor(df.run$GDMT_MRAcontra)
df.run$GDMT_ARNIcontra <- as.factor(df.run$GDMT_ARNIcontra)


df.run <- subset(df.run, select = -c(PMHx_MDD, PMHx_valvedz, PMHx_CardioMEMS, JC_HXSMOKING))
df <- droplevels(df.run)

# match("PMHx_PPMICDCRT", names(df.run))
# df.run <- df.run[,c(1:22, 48, 23:47, 49)]
# match("PMHx_fibfl", names(df.run))
# df <- df.run[,c(1:23, 45, 24:44, 46:49)]


#remove patients with dc Na < 100 (unlikely compatible with life)
#df.run$SODIUMi_disc[df.run$SODIUMi_disc<=100] <- NA

#change Cr to the natural log of Cr 
#df.run$covar_Crdisc <- log(df.run$covar_Crdisc)


#create a new outcome using passive imputation that is the sum of the GDMT components 
#eg a patient gets 1 point for each component of GDMT prescribed, so it should range from [0,3]
# df.run <-
#   df.run %>%
#   mutate(GDMT_Rx_score = as.numeric(GDMT_HFBB) +
#                          as.numeric(GDMT_RAASI) +
#                          as.numeric(GDMT_MRA) - 3)
# 
# df.run <-
#   df.run %>%
#   mutate(GDMT_RxContra_score = as.numeric(GDMT_HFBBorContra) +
#            as.numeric(GDMT_RAASIorContra) +
#            as.numeric(GDMT_MRAorContra) -3)



#save the new data frame 
save(df, file = '/mnt/workspace/rstudio_projects/gdmt_11.10.2_testclean.RData')
