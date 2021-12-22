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


load('gdmt_11.10_testclean.RData')

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

#add pre-hosp ARNI use 
table(df$OH_ARNI, exclude=NULL)
df$OH_ARNI <- as.factor(df$OH_ARNI)

#remove all dropped levels 
df <- droplevels(df)

#how many missing in each variable? 
lapply(df, function(x) 
       sum(is.na(x))
       )

#how many complete cases? 
sum(complete.cases(df))

#understand the pattern of missingness 
md.pattern(df, plot=FALSE)

#look at the ICC for each var to see if multilevel MI is necessary 
varlist <- colnames(df)
hasmiss <- list() 
miss_f <- list() 
miss_n <- list() 

for(i in 1:ncol(df)) { 
        if(with(df,sum(is.na(eval(as.name(varlist[i]))))) > 0) {
                hasmiss <- append(hasmiss, varlist[i])
        }
}

for(i in 1:length(hasmiss)) { 
        if (with(df,class(eval(as.name(hasmiss[[i]])))) == "factor") {
                miss_f <- append(miss_f, hasmiss[i])
        } else {
                miss_n <- append(miss_n, hasmiss[i])
        }
}


rm(varlist, hasmiss)

fun_f <- function(i) {
        
        m0 <- glmer(eval(as.name(i)) ~ (1|SITE_ID),
                    data=df,
                    family='binomial',
                    control=glmerControl(optimizer='bobyqa',
                                         optCtrl=list(maxfun=2e5),
                                         calc.derivs=TRUE))
        
        ICC <- m0@theta[1]^2/(m0@theta[1]^2 + (3.14159^2/3))
        
        print(i)
        print(ICC)
        
}

fun_n <- function(i) {
        
        ICC <- multilevel::ICC1(aov(eval(as.name(i)) ~ SITE_ID, data=df))
        
        print(i)
        print(ICC)
        
}

suppress <- lapply(miss_f, fun_f)
suppress <- lapply(miss_n, fun_n)

rm(miss_n, miss_f, fun_n, fun_f, suppress)


####set cluster_var/level defining variable ####
#SITE_ID is the level 2 defining var; it has no missing data 
#df$SITE_ID <- as.numeric(df$SITE_ID)

#df <- subset(df, SITE_ID <=100)

df$SITE_ID <- as.factor(df$SITE_ID)
#df$cluster <- as.numeric(df$SITE_ID)
#cluster_var=df$cluster
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
impmethod['GDMT_HFBB'] <- "rf" 
impmethod['GDMT_ACEIARB'] <- "rf" 
impmethod['GDMT_ARNI'] <- "" 
impmethod['GDMT_MRA'] <- "rf"

impmethod['GDMT_BBcontra'] <- "rf"
impmethod['GDMT_RAASIcontra'] <- "rf"
impmethod['GDMT_MRAcontra'] <- "rf" 
impmethod['GDMT_ARNIcontra'] <- "rf" 
impmethod['OH_ARNI'] <- "rf"

impmethod['GENDERi'] <- "rf" 
impmethod['race2i'] <- ""
impmethod['insurance'] <- "rf"
impmethod['AGEi'] <- ""
#impmethod['covar_dispo'] <- "rf"

impmethod['PMHx_none'] <- "rf" 
#impmethod['PMHx_COPD'] <- "2l.binary" 
#impmethod['PMHx_HTN'] <- "2l.binary" 
#impmethod['PMHx_priorCVATIA'] <- "logreg"
impmethod['PMHx_CHF'] <- "rf" 
impmethod['PMHx_ESRD'] <- "rf"
impmethod['PMHx_Cr2'] <- "rf"
#impmethod['PMHx_dm2'] <- "logreg"
impmethod['PMHx_PPMICDCRT'] <- "rf"
impmethod['PMHx_fibfl'] <- "rf"

impmethod['GH_HEART_TRANSPLANTS'] <- ""
#impmethod['MCNTRLi'] <- "rf"
impmethod['RESIDENTS'] <- ""
impmethod['OH_EF'] <- ""
impmethod['OH_TRANSPLANT'] <- ""
impmethod['covar_ino'] <- ""

impmethod['inhospRx_BB'] <- "rf"
impmethod['inhospRx_MRA'] <- "rf"
impmethod['inhospRx_none'] <- "rf"
impmethod['inhospRx_ARNI'] <- "rf"
impmethod['inhospRx_ACEIARB'] <- "rf"

impmethod['covar_Kdisc'] <- "pmm"
impmethod['covar_Crdisc'] <- "pmm"
impmethod['OH_DISCBPSYST'] <- "pmm"
impmethod['OH_DISCHR'] <- "pmm"

impmethod['zip_designation'] <- "rf"
impmethod['population_total'] <- "pmm"
impmethod['distress_score'] <- "pmm"
impmethod['covar_OutsideZIPregion'] <- "rf"
impmethod['followup'] <- "rf"
impmethod['DCtoContCare'] <- "rf"

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

# start_time <- Sys.time()
# print(start_time)
# test_rf <- mice(df, m=2, maxit=2,
#                 predictorMatrix=pm, method=impmethod,
#                 cluster.seed=88,
#                 verbose=2,
#                 ntree= 15,
#                 control=lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
# end_time <- Sys.time()
# end_time - start_time



start_time <- Sys.time() 
print(start_time)
imp_rf2 <- parlmice(df, n.core=30, n.imp.core = 1, cl.type="FORK",
                maxit=15, cluster.seed=88,
                predictorMatrix=pm, method=impmethod,
                ntree = 15, 
                verbose=2,
                control=lmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
end_time <- Sys.time() 
end_time - start_time 


print("saving mids object")
write.mice.imputation(mi.res=imp_rf2, name="imp_rf2", mids2spss = FALSE)


#check diagnostics of the MI procedure 
plot(imp_rf2)

densityplot(imp_rf2, ~ GDMT_HFBB) #RF; ICC 0.07; DP decent 
densityplot(imp_rf2, ~ GDMT_ACEIARB) #RF; ICC 0.039; DP bad
densityplot(imp_rf2, ~ GDMT_MRA) #RF; ICC 0.111; DP decent

densityplot(imp_rf2, ~ GDMT_BBcontra) #RF; ICC 0.470; DP decent
densityplot(imp_rf2, ~ GDMT_RAASIcontra) #2l.binary; ICC 0.410; DP random
densityplot(imp_rf2, ~ GDMT_MRAcontra) #2l.binary; ICC 0.410; DP random 
densityplot(imp_rf2, ~ GDMT_ARNIcontra) #2l.binary; ICC NULL; DP random 

densityplot(imp_rf2, ~ GENDERi) #logreg; ICC 0.010; DP bad 
densityplot(imp_rf2, ~ insurance) #2l.binary; ICC 0.320; random

densityplot(imp_rf2, ~ PMHx_none) #2l.binary; ICC 0.124; DP random
densityplot(imp_rf2, ~ PMHx_CHF) #2l.binary; ICC 0.117; DP random
densityplot(imp_rf2, ~ PMHx_ESRD) #2l.binary; ICC 0.129; DP random
densityplot(imp_rf2, ~ PMHx_Cr2) #2l.glm.bin; ICC 0.177; DP random
densityplot(imp_rf2, ~ PMHx_PPMICDCRT) #2l.binary; ICC 0.089; DP random
densityplot(imp_rf2, ~ PMHx_fibfl) #logreg; ICC 0.045; DP bad 

densityplot(imp_rf2, ~ inhospRx_BB) #2l.binary; ICC 0.087; DP random
densityplot(imp_rf2, ~ inhospRx_ACEIARB) #2l.binary; ICC NULL; DP bad 
densityplot(imp_rf2, ~ inhospRx_MRA)

densityplot(imp_rf2, ~ OH_ARNI)

densityplot(imp_rf2, ~ covar_Kdisc) #pmm; ICC 0.044; DP excellent
densityplot(imp_rf2, ~ covar_Crdisc) #pmm; ICC 0.011; DP excellent
densityplot(imp_rf2, ~ OH_DISCBPSYST) #pmm; ICC 0.027; DP excellent
densityplot(imp_rf2, ~ OH_DISCHR) #pmm; ICC 0.022; DP excelent  

densityplot(imp_rf2, ~ population_total) #2l.glm.norm; ICC 0.437; DP nonsensical 
densityplot(imp_rf2, ~ distress_score) #pmm; ICC 0.299; DP good   
densityplot(imp_rf2, ~ covar_OutsideZIPregion) #2l.binary; ICC NULL; random 
densityplot(imp_rf2, ~ followup) #2l.binary; ICC 0.593; random
densityplot(imp_rf2, ~ DCtoContCare) #2l.binary; ICC 0.750; random
densityplot(imp_rf2, ~ zip_designation)

####run the model on the imputed datasets####
# start_time <- Sys.time() 
# fit <- with(imp_rf2, glmer(GDMT_ARNI ~ GDMT_HFBB + GDMT_ACEIARB + GDMT_MRA + GDMT_RAASIcontra + 
#                                   GENDERi + relevel(race2i, ref="White") + relevel(insurance, ref="Other") + AGEi + 
#                                   PMHx_none + PMHx_CHF + PMHx_ESRD + PMHx_Cr2 + 
#                                   PMHx_PPMICDCRT + PMHx_fibfl + 
#                                   GH_HEART_TRANSPLANTS + RESIDENTS + 
#                                   OH_EF + OH_TRANSPLANT + covar_ino + 
#                                   inhospRx_BB + inhospRx_MRA + inhospRx_none + 
#                                   inhospRx_ARNI + inhospRx_ACEIARB + 
#                                   covar_Kdisc + covar_Crdisc + 
#                                   OH_DISCBPSYST + OH_DISCHR + 
#                                   relevel(zip_designation, ref="Urban") + population_total + 
#                                   distress_score + covar_OutsideZIPregion + 
#                                   followup + DCtoContCare + 
#                                   (1 | SITE_ID), 
#                           family='binomial', verbose=2, 
#                           control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e7), calc.derivs=TRUE)))
# end_time <- Sys.time() 
# end_time - start_time 


####take a look at some of hte fitted models 
# summary(fit$analyses[[1]])
# 
# ####pool the models####
# pool <- pool(fit)
# pool$pooled
# result <- summary(pool)

#display the coefficients and SEs as odds ratios 
table <- cbind(Var=result$term, 
               Est=exp(result$estimate), 
               SE=exp(result$std.error), 
               LL=exp(result$estimate - 1.96*result$std.error),
               UL=exp(result$estimate + 1.96*result$std.error),
               p=result$p.value)

tab2 <- format(round(table,3), nsmall=3)
result$term

#add quintile DCI to the pre-imputation dataset 
df <- df %>% 
        mutate(DCI_quint=ifelse(distress_score < 20, 1, 
                         ifelse(distress_score >= 20 & distress_score < 40, 2, 
                         ifelse(distress_score >= 40 & distress_score < 60, 3, 
                         ifelse(distress_score >= 60 & distress_score < 80, 4, 
                         ifelse(distress_score >= 80, 5, 999))))))
df$DCI_quint <- factor(df$DCI_quint, 
                         levels=c(1,2,3,4,5), 
                         labels=c('prosperous', 'comfortable', 'midtier', 
                                  'atrisk', 'distressed'))

df <- df %>% 
        mutate(sBP90=ifelse(OH_DISCBPSYST < 90, 1, 0))
df$sBP90 <- as.factor(df$sBP90)

df <- df %>% 
        mutate(HR60=ifelse(OH_DISCHR < 60, 1, 0))
df$HR60 <- as.factor(df$HR60)

df <- df %>% 
        mutate(K5=ifelse(covar_Kdisc >= 5, 1, 0))
df$K5 <- as.factor(df$K5)

gm_age = mean(df$AGEi, na.rm=TRUE)
df$age_GMrescale = df$AGEi - gm_age

gm_EF = mean(df$OH_EF, na.rm=TRUE)
df$EF_GMrescale = df$OH_EF - gm_EF

gm_Cr = mean(df$covar_Crdisc, na.rm=TRUE)
df$Crdisc_GMrescale = df$covar_Crdisc - gm_Cr

rm(gm_age, gm_EF, gm_Cr)

#before fitting the model, perform grand-mean centering and re-classify distress_score into quintiles
load("/mnt/workspace/rstudio_projects/data/imp_rf2/imp_rf2.Rdata")
imp_rf2 <- mi.res
rm(mi.res)

long <- mice::complete(imp_rf2, "long", include=TRUE)

gm_DCI = mean(long$distress_score, na.rm=TRUE)
long$DCI_GMrescale = long$distress_score - gm_DCI

gm_EF = mean(long$OH_EF, na.rm=TRUE)
long$EF_GMrescale = long$OH_EF - gm_EF

gm_age = mean(long$AGEi, na.rm=TRUE)
long$age_GMrescale = long$AGEi - gm_age

gm_K = mean(long$covar_Kdisc, na.rm=TRUE)
long$Kdisc_GMrescale = long$covar_Kdisc - gm_K

gm_Cr = mean(long$covar_Crdisc, na.rm=TRUE)
long$Crdisc_GMrescale = long$covar_Crdisc - gm_Cr

gm_sBP = mean(long$OH_DISCBPSYST, na.rm=TRUE)
long$sBP_GMrescale = long$OH_DISCBPSYST - gm_sBP

gm_HR = mean(long$OH_DISCHR, na.rm=TRUE)
long$HR_GMrescale = long$OH_DISCHR - gm_HR

gm_poptotal = mean(long$population_total, na.rm=TRUE)
long$poptotal_GMrescale = long$population_total - gm_poptotal

long <- long %>% 
        mutate(DCI_quint=ifelse(distress_score < 20, 1, 
                         ifelse(distress_score >= 20 & distress_score < 40, 2, 
                         ifelse(distress_score >= 40 & distress_score < 60, 3, 
                         ifelse(distress_score >= 60 & distress_score < 80, 4, 
                         ifelse(distress_score >= 80, 5, 999))))))
long$DCI_quint <- factor(long$DCI_quint, 
                         levels=c(1,2,3,4,5), 
                         labels=c('prosperous', 'comfortable', 'midtier', 
                                  'atrisk', 'distressed'))

long <- long %>% 
        mutate(sBP90=ifelse(OH_DISCBPSYST < 90, 1, 0))
long$sBP90 <- as.factor(long$sBP90)

long <- long %>% 
        mutate(HR60=ifelse(OH_DISCHR < 60, 1, 0))
long$HR60 <- as.factor(long$HR60)

long <- long %>% 
        mutate(K5=ifelse(covar_Kdisc >= 5, 1, 0))
long$K5 <- as.factor(long$K5)

rm(gm_DCI, gm_EF, gm_age, gm_K, gm_Cr, gm_sBP, gm_HR, gm_poptotal)

imprf2_GMC <- as.mids(long)

start_time <- Sys.time() 
fit_GMC <- with(imprf2_GMC, glmer(GDMT_ARNI ~ GDMT_HFBB + GDMT_ACEIARB + GDMT_MRA + 
                                  GDMT_RAASIcontra + inhospRx_ARNI + OH_ARNI +
                                  relevel(race2i, ref="White") + relevel(insurance, ref="Other") + 
                                  age_GMrescale + GENDERi + 
                                  PMHx_none + PMHx_CHF + PMHx_Cr2 + PMHx_ESRD +  
                                  GH_HEART_TRANSPLANTS + RESIDENTS + 
                                  EF_GMrescale + OH_TRANSPLANT + 
                                  covar_ino + 
                                  K5 + Crdisc_GMrescale + 
                                  sBP90 + HR60 + 
                                  relevel(zip_designation, ref="Urban") +  
                                  relevel(DCI_quint, ref="prosperous") + 
                                  followup + DCtoContCare + 
                                  (1 | SITE_ID), 
                          family='binomial', verbose=2, 
                          control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE)))
end_time <- Sys.time() 
end_time - start_time 

##### need to passively impute primary outcomes asd
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



