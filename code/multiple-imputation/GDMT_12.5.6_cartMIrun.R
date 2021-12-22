getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#must use mice version > 3.13.6 as this eliminates the problem with 2lmer failing when a cluster has n=1
#install.packages("devtools")
#devtools::install_github(repo="amices/mice")

 library(dplyr)
 library(tidyr)
 library(mice)
 library(miceadds)
 library(micemd)
 library(lme4)
 library(optimx)
 library(broom.mixed)
 library(performance)
 library(parallel)
 library(MASS)
 library(miceadds)
 library(mvmeta)
 library(randomForest)


load('gdmt_11.9_testclean.RData')

####describe the df #### 
names(df)
str(df, list.len=ncol(df))

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

#remove all dropped levels 
df <- droplevels(df)

#how many missing in each variable? 
lapply(df, function(x) 
       sum(is.na(x))
       )

#how many complete cases? 
sum(complete.cases(df))

####set cluster_var/level defining variable ####
#SITE_ID is the level 2 defining var; it has no missing data 
#df$SITE_ID <- as.numeric(df$SITE_ID)

#df <- subset(df, SITE_ID <=100)

df$SITE_ID <- as.factor(df$SITE_ID)
cluster_var=df$SITE_ID
print("cluster variable set")

####create the visitation sequence####
# vis <- c('GENDERi', 'GDMT_RAASI', 'GDMT_HFBB', 'followup', 'GDMT_MRA', 'GDMT_RAASIcontra', 'JC_HXSMOKING', 
#          'insured', 
#          'PMHx_none', 'PMHx_COPD', 'PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_CHF', 'PMHx_ESRD', 'PMHx_fibfl', 
#          'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 'PMHx_CardioMEMS', 'PMHx_dm2', 'PPMICDCRT', 
#          'GDMT_MRAcontra', 'GDMT_BBcontra', 
#          'inhospRx_none', 'inhospRx_RAASI', 'inhospRx_MRA', 'inhospRx_BB', 'OH_DISCBPSYST', 'OH_DISCHR', 
#          'covar_OutsideZIPregion', 'MedExp', 
#          'distress_score', 'population_total', 'DCtoContCare', 
#          'covar_Crdisc', 'covar_Kdisc')

####create the imputation method vector ####
impmethod <- character(ncol(df))
names(impmethod) <- colnames(df)
print("imputation vector created")


#set imputation methods by var (fully conditional specification)
# impmethod['SITE_ID'] <- "pmm" 
# impmethod['GDMT_ARNI'] <- "logreg"
# impmethod['race2i'] <- "logreg"
# impmethod['AGEi'] <- "lmer"
# impmethod['GH_HEART_TRANSPLANTS'] <- ""
# impmethod['RESIDENTS'] <- ""
# impmethod['OH_EF'] <- ""
# impmethod['OH_TRANSPLANT'] <- ""
# impmethod['covar_ino'] <- ""
# impmethod['zip_designation'] <- ""

impmethod['SITE_ID'] <- "" 
impmethod['GDMT_HFBB'] <- "cart" 
impmethod['GDMT_ACEIARB'] <- "cart" 
impmethod['GDMT_ARNI'] <- "" 
impmethod['GDMT_MRA'] <- "cart"

impmethod['GDMT_BBcontra'] <- "cart"
impmethod['GDMT_RAASIcontra'] <- "cart"
impmethod['GDMT_MRAcontra'] <- "cart" 
impmethod['GDMT_ARNIcontra'] <- "cart" 

impmethod['GENDERi'] <- "cart" 
impmethod['race2i'] <- ""
impmethod['insurance'] <- "cart"
impmethod['AGEi'] <- ""
#impmethod['covar_dispo'] <- "rf"

impmethod['PMHx_none'] <- "cart" 
#impmethod['PMHx_COPD'] <- "2l.binary" 
#impmethod['PMHx_HTN'] <- "2l.binary" 
#impmethod['PMHx_priorCVATIA'] <- "logreg"
impmethod['PMHx_CHF'] <- "cart" 
impmethod['PMHx_ESRD'] <- "cart"
impmethod['PMHx_Cr2'] <- "cart"
#impmethod['PMHx_dm2'] <- "logreg"
impmethod['PMHx_PPMICDCRT'] <- "cart"
impmethod['PMHx_fibfl'] <- "cart"

impmethod['GH_HEART_TRANSPLANTS'] <- ""
#impmethod['MCNTRLi'] <- "rf"
impmethod['RESIDENTS'] <- ""
impmethod['OH_EF'] <- ""
impmethod['OH_TRANSPLANT'] <- ""
impmethod['covar_ino'] <- ""

impmethod['inhospRx_BB'] <- "cart"
impmethod['inhospRx_MRA'] <- "cart"
impmethod['inhospRx_none'] <- "cart"
impmethod['inhospRx_ARNI'] <- "cart"
impmethod['inhospRx_ACEIARB'] <- "cart"

impmethod['covar_Kdisc'] <- "pmm"
impmethod['covar_Crdisc'] <- "pmm"
impmethod['OH_DISCBPSYST'] <- "pmm"
impmethod['OH_DISCHR'] <- "pmm"

impmethod['zip_designation'] <- "cart"
impmethod['population_total'] <- "pmm"
impmethod['distress_score'] <- "pmm"
impmethod['covar_OutsideZIPregion'] <- "cart"
impmethod['followup'] <- "cart"
impmethod['DCtoContCare'] <- "cart"

print(impmethod)
print("imputation methods assigned")


#####make the predicator matrix ####
pm <- mice::make.predictorMatrix(df)

#set the SITE_ID as the cluster var 
pm[,'SITE_ID'] <- 1
pm['SITE_ID','SITE_ID'] <- 0

pm['SITE_ID',] <- 0
pm['GDMT_ARNI',] <- 0
pm['race2i',] <- 0
pm['AGEi',] <- 0
pm['GH_HEART_TRANSPLANTS',] <- 0
pm['RESIDENTS',] <- 0
pm['OH_EF',] <- 0
pm['OH_TRANSPLANT',] <- 0
pm['covar_ino',] <- 0

#ensure that var derrived from each other are not used to predict each other
#pm['insured', 'insurance'] <- 0
#pm['DCtoContCare', 'covar_dispo'] <- 0

#ensure that derived var are not used to predict anything else 
#pm[, 'insured'] <- 0
#pm[, 'DCtoContCare'] <- 0

#do not use a variable as a predictor to impute itself 

print("predictor matrix complete")
print(pm)


####create a MICE() object
print("creating a mice() object")

start_time <- Sys.time()
print(start_time)
test_cart <- mice(df, m=2, maxit=2,
                predictorMatrix=pm, method=impmethod,
                cluster.seed=88,
                verbose=2,
                control=lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
end_time <- Sys.time()
end_time - start_time



start_time <- Sys.time() 
print(start_time)
imp_cart <- parlmice(df, n.core=30, n.imp.core = 1, cl.type="FORK",
                maxit=8, cluster.seed=88,
                predictorMatrix=pm, method=impmethod,
                verbose=2,
                control=lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
end_time <- Sys.time() 
end_time - start_time 


print("saving mids object")
write.mice.imputation(mi.res=imp_cart, name="imp_cart", mids2spss = FALSE)


# 
# print("creating a mice() object")
# 
# start_time <- Sys.time() 
# print(start_time)
# imp_rf <- parlmice(df, n.core=5, n.imp.core = 1, cl.type="FORK",
#                    maxit=3, predictorMatrix=pm, method=impmethod,
#                    cluster_var=cluster_var, 
#                    nAGQ=0, cluster.seed=88, 
#                    control1=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE),
#                    control2=lmerControl(optimizer='nloptwrap', optCtrl=list(maxfun=2e5)))
# end_time <- Sys.time() 
# end_time - start_time 
print("mids object created")

#print("saving mids object")
#write.mice.imputation(mi.res=impobj, name="impdir_final", mids2spss = FALSE)
# save(imprun, file = "impfinal.rda")
# saveRDS(imprun, "/mnt/workspace/rstudio_projects/mids1.rds")

#check diagnostics of the MI procedure 
plot(imp_rf)

densityplot(imp_rf, ~ GDMT_HFBB) #RF; ICC 0.07; DP decent 
densityplot(imp_rf, ~ GDMT_ACEIARB) #RF; ICC 0.039; DP bad
densityplot(imp_rf, ~ GDMT_MRA) #RF; ICC 0.111; DP decent

densityplot(imp_rf, ~ GDMT_BBcontra) #RF; ICC 0.470; DP decent
densityplot(imp_rf, ~ GDMT_RAASIcontra) #2l.binary; ICC 0.410; DP random
densityplot(imp_rf, ~ GDMT_MRAcontra) #2l.binary; ICC 0.410; DP random 
densityplot(imp_rf, ~ GDMT_ARNIcontra) #2l.binary; ICC NULL; DP random 

densityplot(imp_rf, ~ GENDERi) #logreg; ICC 0.010; DP bad 
densityplot(imp_rf, ~ insurance) #2l.binary; ICC 0.320; random

densityplot(imp_rf, ~ PMHx_none) #2l.binary; ICC 0.124; DP random
densityplot(imp_rf, ~ PMHx_CHF) #2l.binary; ICC 0.117; DP random
densityplot(imp_rf, ~ PMHx_ESRD) #2l.binary; ICC 0.129; DP random
densityplot(imp_rf, ~ PMHx_Cr2) #2l.glm.bin; ICC 0.177; DP random
densityplot(imp_rf, ~ PMHx_PPMICDCRT) #2l.binary; ICC 0.089; DP random
densityplot(imp_rf, ~ PMHx_fibfl) #logreg; ICC 0.045; DP bad 

densityplot(imp_rf, ~ inhospRx_BB) #2l.binary; ICC 0.087; DP random
densityplot(imp_rf, ~ inhospRx_ACEIARB) #2l.binary; ICC NULL; DP bad 
densityplot(imp_rf, ~ inhospRx_MRA)

densityplot(imp_rf, ~ covar_Kdisc) #pmm; ICC 0.044; DP excellent
densityplot(imp_rf, ~ covar_Crdisc) #pmm; ICC 0.011; DP excellent
densityplot(imp_rf, ~ OH_DISCBPSYST) #pmm; ICC 0.027; DP excellent
densityplot(imp_rf, ~ OH_DISCHR) #pmm; ICC 0.022; DP excelent  

densityplot(imp_rf, ~ population_total) #2l.glm.norm; ICC 0.437; DP nonsensical 
densityplot(imp_rf, ~ distress_score) #pmm; ICC 0.299; DP good   
densityplot(imp_rf, ~ covar_OutsideZIPregion) #2l.binary; ICC NULL; random 
densityplot(imp_rf, ~ followup) #2l.binary; ICC 0.593; random
densityplot(imp_rf, ~ DCtoContCare) #2l.binary; ICC 0.750; random
densityplot(imp_rf, ~ zip_designation)


#the binary variable density plots suggest the results may be inaccurae. 
#consider the mice.impute.rf method (random forest) 

##### need to passively impute primary outcomes 
# long <- mice::complete(imprun, "long", include=TRUE)
# long$GDMT_Rx_score <- with(long, as.numeric(GDMT_HFBB) + as.numeric(GDMT_RAASI) + as.numeric(GDMT_MRA) - 3)
# 
# long <- 
#   long %>% 
#   mutate(newvar=AGEi*10)
# 
# long$var2 <- with(long, ifelse(GDMT_HFBB %in% 1 & GDMT_RAASI %in% 1 & GDMT_MRA %in% 1, 1, 0))
# 
# 
#   #visitSequence=vis, data.init = NULL, ignore = NULL, where = NULL, post = NULL, blots = NULL,
#   #imputationFunction=imputationFunction,
#   #try different optimizers (bobyqa, nloptr)
#   #control1=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE),
#   #verbose=2,
# 
# #  imputationFunction=imputationFunction
# 
# #view imputed datasets
# imptest$imp$covar_Kdisc
# 
# #assess convergence
# plot(imptest, distress_score ~ .it | .ms)
# dev.off() 

#### run the analysis phase with with.mids() 
#library(oridinal)
#with(imprun, clmm(formula, link="cloglog"))s



