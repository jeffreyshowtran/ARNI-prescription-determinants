getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

install.packages("broom.mixed")
install.packages("optimx")
install.packages("devtools")
devtools::install_github(repo="amices/mice")
  #this is necessary to load mice version > 3.13.6 as this eliminates the problem with 2lmer failing when a cluster has n=1

library(dplyr)
library(tidyr)
library(miceadds)
library(micemd)
library(ggplot2)
library(lattice)
library(lme4)
library(optimx)
library(broom.mixed)
library(MASS)


load('gdmt_11.1_final.RData')

#create a new dataset based on catnomiss 
df.test <- df.final

df.test <- df.test[order(df.test$SITE_ID),]

#### clean data ####
#remove extraneous levels from dataset
df.test <- droplevels(df.test)

#asssume that all heart transplant and academic hospitals are known
#impute 0 for all missing of GH_HEART_TRANSPLANTS and RESIDENTS
df.test$GH_HEART_TRANSPLANTS[is.na(df.test$GH_HEART_TRANSPLANTS)] <- 0
df.test$RESIDENTS[is.na(df.test$RESIDENTS)] <- 0

#for insurance, combine NA and UTD 
df.test$insurance <- as.character(df.test$insurance)
df.test$insurance[is.na(df.test$insurance)] <- 'No Insurance/Not Documented/UTD'
df.test$insurance <- factor(df.test$insurance)
table(df.test$insurance)

#for admit source, combine NA and info not available 
df.test$ADMITSOURCEi <- as.character(df.test$ADMITSOURCEi)
df.test$ADMITSOURCEi[is.na(df.test$ADMITSOURCEi)] <- 'Information not available'
df.test$ADMITSOURCEi <- as.factor(df.test$ADMITSOURCEi)
table(df.test$ADMITSOURCEi)

#remove patients with missing data in key cat var with levels > 2
df.test <- subset(df.test, !(is.na(df.test$race2i)))
df.test <- subset(df.test, !(is.na(df.test$MCNTRLi)))

#for all other cat var with levels > 2, make missing a category but DO NOT include in final model 
# 'race2i', 'ADMITSOURCEi', 'zip1', 'covar_dispo', 'OH_HFHOSPADM',
# 'MedExp', 'zip_designation', 
# 'SITE_DCI_setting', 'SITE_DCI_quintile'
list = c('zip1', 'covar_dispo', 'OH_HFHOSPADM',
         'zip_designation', 'SITE_DCI_setting', 'SITE_DCI_quintile')

#execute addNA() to add NA as an explicit level
df.test[list] <- lapply(df.test[list], function(x) addNA(x))
rm(list)

#create a new var combining afib and aflutter 
df.test <- 
  df.test %>% 
  mutate(PMHx_fibfl = ifelse(PMHx_afib %in% 1 | PMHx_aflutter %in% 1, 1, 
                      ifelse(is.na(PMHx_afib) & is.na(PMHx_aflutter), NA, 0)))

df.test$PMHx_fibfl <- factor(df.test$PMHx_fibfl)

#some of the Cr values are out of range (>50); replace these values 
df.test$covar_Crdisc <- ifelse(df.test$covar_Crdisc >= 50, df.test$covar_Crdisc/10, df.test$covar_Crdisc)

#in first iteration of test with df.final, a vector of 17.9Gb was created??? 
#so will reduce the size of the df in testing
#df.test <- subset(df.catnomiss, (SITE_ID <= 50))
df.test <- subset(df.test, select = -c(JC_DISCDATETIME, OH_NONISCHEMIC_ETIOLOGY_1:OH_NONISCHEMIC_ETIOLOGY_9, 
                                       MedExp_adoption_date, MedExp_state, PMHx_priorPCIorCABG, zip_num))

#remove all patients with an LVAD 
df.test <- subset(df.test, PMHx_LVAD == 0 | is.na(df.test$PMHx_LVAD))

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
                                       OH_DISCBPDIAS, distress_quntile, SITE_DCI_quintile))


#remove the components of GDMT_ var to avoid collinearity
df.test <- subset(df.test, select = -c(GDMT_HFBB:GDMT_MRAorContra, GDMT_RAASI, GDMT_RAASIorContra))

#teh SITE_DCI var do not have great outflux 
#df.test <- subset(df.test, select = -c(SITE_DCI_setting:SITE_DCI_quintile))

#remove irrelevant PMHx var
df.test <- subset(df.test, select = -c(PMHx_afib, PMHx_aflutter, PMHx_PVD, PMHx_CAD, PMHx_priorMI, 
                                       PMHx_COPD, PMHx_OSAOHS, PMHx_CardioMEMS))

####describe the new dataset #### 
df.test <- droplevels(df.test)

str(df.test, list.len=ncol(df.test))

#how many missing in each variable? 
lapply(df.test, function(x) 
  sum(is.na(x)))

#how many complete cases? 
sum(complete.cases(df.test))

#observe outflux for vars in df.test 
flux(df.test)

#understand how many missing elemeents per row 
 df.test <- 
   df.test %>% 
   mutate(miss = rowSums(is.na(df.test)))

#histogram(~miss, data=df.test) 

summary(df.test$miss)
with(subset(df.test, df.test$miss >= 15), summary(miss))
sum(df.test$miss >= 15)

df.test <- subset(df.test, select = -c(miss))

####set cluster_var/level defining variable ####
#SITE_ID is the level 2 defining var; it has no missing data 
df.test$SITE_ID <- as.numeric(df.test$SITE_ID)
cluster_var=df.test$SITE_ID
print("cluster variable set")

####set the visit sequence ####
vis <- c('GENDERi', 'PMHx_fibfl', 'PMHx_none', 'PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF',  
         'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 'PMHx_dm2', 
         'GDMT_ALLorContra', 'GDMT_ALL', 
         'BEDSIZE', 'SITE_DCI_score', 'SITE_DCI_setting', 
         'inhospRx_BB', 'inhospRx_RAASI', 'inhospRx_MRA', 'inhospRx_none', 
         'MedExp_status', 'MedExp', 'population_total', 'distress_score', 
         'covar_OutsideZIP', 'covar_OutsideZIPregion', 
         'OH_DISCBPSYST', 'OH_DISCHR', 
         'covar_Crdisc', 'covar_Kdisc')

####create the imputation method vector ####
impmethod <- character(ncol(df.test))
names(impmethod) <- colnames(df.test)
print("imputation vector created")

#set imputation methods by var (32 imputations to be carried out)
impmethod['GENDERi'] <- "2l.bin"

#mpmethod['covar_OutsideZIP'] <- "2l.bin"
impmethod['covar_OutsideZIPregion'] <- "2l.bin"

impmethod['PMHx_none'] <- "2l.bin"
impmethod['PMHx_HTN'] <- "2l.bin"
impmethod['PMHx_priorCVATIA'] <- "2l.bin"
impmethod['PMHx_ICD'] <- "2l.bin"
impmethod['PMHx_CHF'] <- "2l.bin"
impmethod['PMHx_ESRD'] <- "2l.bin"
impmethod['PMHx_Cr2'] <- "2l.bin"
impmethod['PMHx_MDD'] <- "2l.bin"
impmethod['PMHx_valvedz'] <- "2l.bin"
impmethod['PMHx_LVAD'] <- "2l.bin"
impmethod['PMHx_dm2'] <- "2l.bin"
impmethod['PMHx_fibfl'] <- "2l.bin"

impmethod['BEDSIZE'] <- "" #2lonly.pmm

impmethod['inhospRx_none'] <- "2l.bin"
impmethod['inhospRx_BB'] <- "2l.bin"
impmethod['inhospRx_MRA'] <- "2l.bin"
impmethod['inhospRx_RAASI'] <- "2l.bin"

impmethod['covar_Kdisc'] <- "2l.lmer"
impmethod['covar_Crdisc'] <- "2l.lmer"

impmethod['OH_DISCBPSYST'] <- "2l.lmer"
impmethod['OH_DISCHR'] <- "2l.lmer"

impmethod['MedExp_status'] <- "2l.bin"
impmethod['MedExp'] <- "2l.bin"
impmethod['population_total'] <- "2l.lmer"
impmethod['distress_score'] <- "2l.lmer"

impmethod['GDMT_ALL'] <- "2l.bin"
impmethod['GDMT_ALLorContra'] <- "2l.bin"

#impmethod['SITE_DCI_poptotal'] <- "2lonly.pmm"
#impmethod['SITE_DCI_score'] <- "2lonly.pmm"

print(impmethod)
print("imputation methods assigned")


#####make the predicator matrix ####
pm <- make.predictorMatrix(df.test)

#set the SITE_ID as the cluster var 
pm[,'SITE_ID'] <- -2
pm['SITE_ID','SITE_ID'] <- 0

#ensure that var derrived from each other are not used to predict each other 
pm['GDMT_ALL','GDMT_ALLorContra'] <- 0
pm['GDMT_ALLorContra','GDMT_ALL'] <- 0

pm['covar_OutsideZIP', c('MedExp_status', 'zip_designation', 'population_total', 'distress_score', 
                         'covar_OutsideZIPregion', 'MedExp', 'SITE_DCI_setting', 'SITE_DCI_poptotal', 
                         'SITE_DCI_score')] <- c(0,0,0,0,0,0,0,0,0)
pm['covar_OutsideZIPregion', c('MedExp_status', 'zip_designation', 'population_total', 'distress_score', 
                               'covar_OutsideZIP', 'MedExp', 'SITE_DCI_setting', 'SITE_DCI_poptotal', 
                               'SITE_DCI_score')] <- c(0,0,0,0,0,0,0,0,0)

pm['MedExp_status', c('zip_designation', 'population_total', 'distress_score', 
                      'covar_OutsideZIP', 'MedExp', 'covar_OutsideZIPregion')] <- c(0,0,0,0,0,0)
pm['population_total', c('MedExp_status', 'zip_designation', 'distress_score', 
                         'covar_OutsideZIP', 'MedExp', 'covar_OutsideZIPregion')] <- c(0,0,0,0,0,0)
pm['distress_score', c('MedExp_status', 'zip_designation', 'population_total', 
                       'covar_OutsideZIP', 'MedExp', 'covar_OutsideZIPregion')] <- c(0,0,0,0,0,0)
pm['MedExp', c('MedExp_status', 'zip_designation', 'population_total', 
                       'covar_OutsideZIP', 'distress_score', 'covar_OutsideZIPregion')] <- c(0,0,0,0,0,0)

pm['SITE_DCI_setting', c('covar_OutsideZIP', 'covar_OutsideZIPregion', 
                         'SITE_DCI_poptotal', 'SITE_DCI_score')] <- c(0,0,0,0)
pm['SITE_DCI_poptotal', c('covar_OutsideZIP', 'covar_OutsideZIPregion', 
                          'SITE_DCI_setting', 'SITE_DCI_score')] <- c(0,0,0,0)
pm['SITE_DCI_score', c('covar_OutsideZIP', 'covar_OutsideZIPregion', 
                       'SITE_DCI_poptotal', 'SITE_DCI_setting')] <- c(0,0,0,0)

pm['PMHx_none', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_HTN', c('PMHx_none', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_priorCVATIA', c('PMHx_HTN', 'PMHx_none', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_ICD', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_none', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_CHF', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_none', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_ESRD', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_none', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_Cr2', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_none', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_MDD', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_none', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_valvedz', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_none', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_LVAD', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_none', 'PMHx_dm2', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_dm2', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_none', 'PMHx_fibfl')] <- c(0,0,0,0,0,0,0,0,0,0,0)
pm['PMHx_fibfl', c('PMHx_HTN', 'PMHx_priorCVATIA', 'PMHx_ICD', 'PMHx_CHF', 
                  'PMHx_ESRD', 'PMHx_Cr2', 'PMHx_MDD', 'PMHx_valvedz', 
                  'PMHx_LVAD', 'PMHx_dm2', 'PMHx_none')] <- c(0,0,0,0,0,0,0,0,0,0,0)

pm['OH_DISCBPSYST', 'OH_DISCHR'] <- 0
pm['OH_DISCHR', 'OH_DISCBPSYST'] <- 0


#set the cluster var characteristics as random efx with random 
#intercept and random slope; eg setting <- 2
 pm[, c('RESIDENTS', 'GH_HEART_TRANSPLANTS',
        'MCNTRLi', 'BEDSIZE',
        'SITE_DCI_setting', 'SITE_DCI_poptotal', 'SITE_DCI_score')] <- 2

#do not use a variable as a predictor to impute itself 
pm['BEDSIZE','BEDSIZE'] <- 0
pm['SITE_DCI_score','SITE_DCI_score'] <- 0
pm['SITE_DCI_poptotal','SITE_DCI_poptotal'] <- 0

print("predictor matrix complete")
print(pm)

####trouble shooting flat line behavior####
#first, must use mice package version 3.13.6 or higher as it solves a problem with clusters of n < 2 
table(df.test$SITE_ID)
df.test <- subset(df.test, SITE_ID != 144 & 
                           SITE_ID != 169 & 
                           SITE_ID != 394 & 
                           SITE_ID != 540 & 
                           SITE_ID != 583 & 
                           SITE_ID != 594 & 
                           SITE_ID != 595)


#remove variables from the model by setting to 0 
pm[, 'covar_OutsideZIP'] <- 0
pm[, c('covar_OutsideZIP', 'covar_Crdisc', 'covar_Kdisc', 'OH_DISCHR', "OH_DISCBPSYST", 
       'SITE_DCI_setting', 'SITE_DCI_poptotal', 'SITE_DCI_score', 'PMHx_fibfl', 
       'inhospRx_RAASI', 'MedExp', 'covar_OutsideZIPregion', 'GDMT_ALLorContra', 'GDMT_ALL', 
       'population_total', 'zip_designation', 'MedExp_status', 'distress_score')] <- 0
pm['GENDERi',] <- 0 
pm['GENDERi', c('race2i', 'insurance', 'AGEi', 'PMHx_none')] <- 1
pm['GENDERi', 'SITE_ID'] <- -2
impmethod['GENDERi'] <- "2l.bin"
df.test$SITE_ID <- as.integer(df.test$SITE_ID)

#####duplicate source code with single var test case ####
# warnings hint that the imputation step is failing at creating the initial stochastic model, 
# so there is no posterior distribution to draw from. Need to figure out why source code is 
# failing to generate the initial model 
#base variables
type1 <- pm['GENDERi',]
x1 <- df.test[, c(2:4, 6, 8)]
y1 <- df.test[, 'GENDERi']
ry1 = !is.na(df.test$GENDERi)
wy1 <- !ry1

#computations
x1 <- cbind(1, as.matrix(x1))
type1 <- c(2, type1)
names(type1)[1] <- colnames(x1)[1] <- "(Intercept)"

clust1 <- names(type1[type1 == -2])
rande1 <- names(type1[type1 == 2])
fixe1 <- names(type1[type1 > 0])

X1 <- x1[ , fixe1, drop=FALSE]
Z1 <- x1[ , rande1, drop=FALSE]
xobs1 <- x1[ry1, , drop=FALSE]
yobs1 <- y1[ry1]


fr1 <- ifelse(length(rande1) > 1, 
              paste("+ ( 1 +", paste(rande1[-1L], collapse="+")), 
                    "+ ( 1 ")
randmodel1 <- paste("yobs1 ~ ", paste(fixe1[-1L], collapse = "+"), fr1, "|", clust1, ")" )

fit1 <- lme4::glmer(formula(randmodel1), 
                    data=data.frame(yobs1, xobs1), 
                    family = binomial, 
                    control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE), 
                    nAGQ=0)  #this runs just fine.... so maybe not passing some of the base variables? still 
                    #i still don't underestand how it's generating type, x, y, and ry (see initialize.imp below)

rm(type1, x1, y1, ry1, clust1, rande1, fixe1, X1, Z1, xobs1, yobs1, fr1, randmodel1, fit1, wy1)

#to carry on from the above, the initialize.imp step is what bridges mice() and mice.impute.2l.bin() 
#it's where x,y,ry are defined. Also note we fall under "Case B" in the initial mice() arguments 
#note the critical "empty parameters that are essential for running this code: 
# where, ignore, blocks, formulas that the mice function definces below through a series of embedded fxns

#the mice function returns the following var: 
predictorMatrix <- pm
for (i in row.names(predictorMatrix)) { 
  predictorMatrix[i, grep(paste0("^", i, "$"), colnames(predictorMatrix))] <- 0
  }
blocks <- make.blocks(colnames(predictorMatrix), partition = "scatter")
formulas <- make.formulas(df.test, blocks, predictorMatrix = predictorMatrix)
ignore= rep(FALSE, nrow(df.test))

keyword <- "missing"
where <- switch(keyword, 
                missing = is.na(df.test), 
                all = matrix(TRUE, nrow=nrow(df.test), ncol=ncol(df.test)),
                none = matrix(FALSE, nrow=nrow(df.test), ncol=ncol(df.test)), 
                observed = !is.na(data))
dimnames(where) <- dimnames(df.test)

m=2
maxit = 3

nmis <- apply(is.na(df.test), 2, sum)

#under the initialize step: 
imp1 <- vector("list", ncol(df.test))
names(imp1) <- names(df.test)

r2 <- !is.na(df.test)
y2 <- df.test[, "GENDERi"] #this matches with what I assumed above, line 325 
ry2 <- r2[, "GENDERi"] & !ignore #ry1 is  identical to line 326 
wy2 <- where[, "GENDERi"] #wy2 is same as wy1 
imp1[["GENDERi"]] <- as.data.frame(matrix(NA, nrow=sum(wy2), ncol=m))
dimnames(imp1[["GENDERi"]]) <- list(row.names(df.test)[wy2], 1:m)
data.init = NULL
post = NULL
blots = NULL


for (i in seq_len(m)) { 
  if (nmis["GENDERi"] < nrow(df.test)) {
    if(is.null(data.init)) {
      imp1[["GENDERi"]][, i] <- mice.impute.sample(y2, ry2, wy=wy2)
    } else {
      imp1[["GENDERi"]][, i] <- data.init[wy, j]
    } 
  } else { 
    imp1[["GENDERi"]][, i] <- rnorm(nrow(df.test))
  }
}

imp1[["GENDERi"]] #this is a completely random sample of data... 

#we return to the main mice() function with the new "imp1" object and create a few more objects 
#before we proceed to iterate 
post <- vector("character", length = ncol(df.test))
names(post) <- colnames(df.test)

blots <- vector("list", length(blocks))
for (i in seq_along(blots)) blots[[i]] <- alist() 
names(blots) <- names(blocks)

from <- 1 
to <- from + maxit - 1 

#now go to sampler.... in hand are the ignore, where, imp1, blocks, post, blots, formulas, and from/to objects created above 
r3 <- !is.na(df.test) #same as r2 above

vars <- unique(unlist(blocks))
chain <- array(NA, dim = c(length(vars), maxit, m))
dimnames(chain) <- list(vars, seq_len(maxit), paste("Chain", seq_len(m)))

chainMean <- chainVar <- chain

# if (maxit < 1) iteration <- 0 
# if (maxit >= 1) { 
#   for (k in from:to) { 
#     iteration <- k 
#     for (i in seq_len(m)) { 
#       for (h)}}

theMethod <- impmethod["GENDERi"]
empt <- theMethod == ""

is.passive <- function(string) {
  "~" == substring(string, 1, 1)
}

handles.arg <- function(f, a = "data") {
  if (!is.function(f)) { 
    return(FALSE)
  }
  a %in% names(formals(f))
}

handles.format <- function(fn) {
  f <- get(fn)
  handles.arg(f, "format")
}

mult <- !empt && !is.passive(theMethod) &&
  handles.format(paste0("mice.impute.", theMethod))

univ <- !empt && !is.passive(theMethod) &&
  !handles.format(paste0("mice.impute.", theMethod))

b <- blocks[["GENDERi"]]
ct <- attr(blocks, "calltype")
calltype <- ifelse(length(ct)==1, ct[1], ct["GENDERi"])
type <- predictorMatrix["GENDERi", ]
ff <- NULL
visitSequence <- vis 

for (h in visitSequence) {
  for (j in blocks [[h]]) {
    print(j)
  }
}

user <- blots[["GENDERi"]]


####sampler function####
sampler.univ <- function(data, r, where, type, formula, method, yname, k,
                         calltype = "type", user, ignore, ...) {
  j <- yname[1L]
  
  if (calltype == "type") {
    vars <- colnames(data)[type != 0]
    pred <- setdiff(vars, j)
    if (length(pred) > 0L) {
      formula <- reformulate(pred, response = j)
      formula <- update(formula, ". ~ . ")
    } else {
      formula <- as.formula(paste0(j, " ~ 1"))
    }
  }
  
  if (calltype == "formula") {
    # move terms other than j from lhs to rhs
    ymove <- setdiff(lhs(formula), j)
    formula <- update(formula, paste(j, " ~ . "))
    if (length(ymove) > 0L) {
      formula <- update(formula, paste("~ . + ", paste(ymove, collapse = "+")))
    }
  }
  
  # get the model matrix
  x <- obtain.design(data, formula)
  
  # expand type vector to model matrix, remove intercept
  if (calltype == "type") {
    type <- type[labels(terms(formula))][attr(x, "assign")]
    x <- x[, -1L, drop = FALSE]
    names(type) <- colnames(x)
  }
  if (calltype == "formula") {
    x <- x[, -1L, drop = FALSE]
    type <- rep(1L, length = ncol(x))
    names(type) <- colnames(x)
  }
  
  # define y, ry and wy
  y <- data[, j]
  ry <- complete.cases(x, y) & r[, j] & !ignore
  wy <- complete.cases(x) & where[, j]
  
  # nothing to impute
  if (all(!wy)) {
    return(numeric(0))
  }
  
  cc <- wy[where[, j]]
  if (k == 1L) check.df(x, y, ry)
  
  # remove linear dependencies
  keep <- remove.lindep(x, y, ry, ...)
  x <- x[, keep, drop = FALSE]
  type <- type[keep]
  if (ncol(x) != length(type)) {
    stop("Internal error: length(type) != number of predictors")
  }
  
  # here we go
  f <- paste("mice.impute", method, sep = ".")
  imputes <- data[wy, j]
  imputes[!cc] <- NA
  
  args <- c(list(y = y, ry = ry, x = x, wy = wy, type = type), user, list(...))
  imputes[cc] <- do.call(f, args = args)
  imputes
}

obtain.design <- function(data, formula = ~.) {
  mf <- model.frame(formula, data = data, na.action = na.pass)
  model.matrix(formula, data = mf)
}

update.design <- function(design, data, varname = ".") {
  # Updates columns of the design matrix related to variable
  # varname in data
  
  varname <- as.character(varname[1])
  idx <- attr(design, "assign") %in% grep(varname, names(data))
  
  # variable j not found
  if (varname == "" || !any(idx)) {
    return(design)
  }
  
  # create model frame of variable j only
  fj <- as.formula(paste("~", varname))
  mfj <- model.frame(fj, data = data, na.action = na.pass)
  design[, idx] <- model.matrix(fj, data = mfj)[, -1, drop = FALSE]
  design
}

check.df <- function(x, y, ry) {
  # if needed, writes the df warning message to the log
  df <- sum(ry) - ncol(x) - 1
  mess <- paste("df set to 1. # observed cases:", sum(ry), " # predictors:", ncol(x) + 1)
  if (df < 1 && sum(ry) > 0) {
    updateLog(out = mess, frame = 4)
  }
}

remove.lindep <- function(x, y, ry, eps = 1e-04, maxcor = 0.99,
                          allow.na = TRUE, frame = 4, ...) {
  # returns a logical vector of length ncol(x)
  
  if (ncol(x) == 0) {
    return(NULL)
  }
  
  # setting eps = 0 bypasses remove.lindep()
  if (eps == 0) {
    return(rep.int(TRUE, ncol(x)))
  }
  if (eps < 0) {
    stop("\n Argument 'eps' must be positive.")
  }
  
  # Keep all predictors if we allow imputation of fully missing y
  if (allow.na && sum(ry) == 0) {
    return(rep.int(TRUE, ncol(x)))
  }
  
  xobs <- x[ry, , drop = FALSE]
  yobs <- as.numeric(y[ry])
  if (var(yobs) < eps) {
    return(rep(FALSE, ncol(xobs)))
  }
  
  keep <- unlist(apply(xobs, 2, var) > eps)
  keep[is.na(keep)] <- FALSE
  highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs) < maxcor))
  keep <- keep & highcor
  if (all(!keep)) {
    updateLog(
      out = "All predictors are constant or have too high correlation.",
      frame = frame
    )
  }
  
  # no need to calculate correlations, so return
  k <- sum(keep)
  if (k <= 1L) {
    return(keep)
  } # at most one TRUE
  
  # correlation between x's
  cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
  eig <- eigen(cx, symmetric = TRUE)
  ncx <- cx
  while (eig$values[k] / eig$values[1] < eps) {
    j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
    keep[keep][j] <- FALSE
    ncx <- cx[keep[keep], keep[keep], drop = FALSE]
    k <- k - 1
    eig <- eigen(ncx)
  }

  return(keep)
}

find.collinear <- function(x, threshold = 0.999, ...) {
  nvar <- ncol(x)
  x <- data.matrix(x)
  r <- !is.na(x)
  nr <- apply(r, 2, sum, na.rm = TRUE)
  ord <- order(nr, decreasing = TRUE)
  xo <- x[, ord, drop = FALSE] ## SvB 24mar2011
  varnames <- dimnames(xo)[[2]]
  z <- suppressWarnings(cor(xo, use = "pairwise.complete.obs"))
  hit <- outer(seq_len(nvar), seq_len(nvar), "<") & (abs(z) >= threshold)
  out <- apply(hit, 2, any, na.rm = TRUE)
  return(varnames[out])
}

####try running the sampler function ####

j = "GENDERi"
k=1

sampler.univ(data=df.test, r=r3, where = where, type = type, formula=ff, method=theMethod, yname=j, k=k, calltype=calltype, 
             user=user, ignore=ignore, control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE), nAGQ=0)





highcor <- suppressWarnings(unlist(apply(xobs, 2, cor, yobs) < maxcor))


cx <- cor(xobs[, keep, drop = FALSE], use = "all.obs")
eig <- eigen(cx, symmetric = TRUE)
ncx <- cx
while (eig$values[k] / eig$values[1] < eps) {
  j <- seq_len(k)[order(abs(eig$vectors[, k]), decreasing = TRUE)[1]]
  keep[keep][j] <- FALSE
  ncx <- cx[keep[keep], keep[keep], drop = FALSE]
  k <- k - 1
  eig <- eigen(ncx)
}

keep <- remove.lindep(x, y, ry)
#note that there are no linear dependencies, at least when building the imputation model for GENDERi

x3 <- x[ , keep, drop = FALSE]  #note that 'SITE_DCI_settingNA' is "FALSE" and so is removed so x3 has 1 less column than x
type3 <- type[keep]


f <- paste("mice.impute", theMethod, sep = ".")
y3 <- data[,j]
ry3 <- complete.cases(x,y3) & r3[,j] & !ignore
wy3 <- complete.cases(x3) & where[,j]
cc <- wy3[where[,j]]

imputes <- data[wy3, j]
imputes[!cc] <- NA 


args <- c(list(y = y3, ry = ry3, x = x3, wy = wy3, type = type3), user, 
          list(control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE), nAGQ=0))
imputes[cc] <- do.call(f, args = args)

#we see above that we have finally reduplicated the error, so something is happening in the 
#mice.impute.2l.bin command with the arguments generated above. Let's walk through 
#the 2l.bin function with the arguments to see what happens 

x4 <- cbind(1, as.matrix(x3))
type4 <- c(2, type3)
names(type4)[1] <- colnames(x4)[1] <- "(Intercept)"

clust4 <- names(type4[type4 == -2])
rande4 <- names(type4[type4 == 2])
fixe4 <- names(type4[type4 > 0])

X4 <- x4[ ,fixe4, drop = FALSE]
Z4 <- x4[ ,rande4, drop = FALSE]
xobs4 <- x4[ry3, , drop=FALSE]
yobs4 <- y3[ry3]

fr <- ifelse(length(rande4) > 1, 
             paste("+ ( 1 +", paste(rande4[-1L], collapse ="+")),
             "+ ( 1 ")
             
randmodel <- paste(
  "yobs4 ~ ", paste(fixe4[-1L], collapse = "+"), 
  fr, "|", clust4, ")"
)


#the command fails because the formula has multiple special characters involved that 
#come from the way the levels are defined

rm(blocks, imp1, predictorMatrix, blocks, formulas, y1, r1, ry1, ignore, 
   where, wy2, nmis, data.init, post, chain, is.passive, handles.arg, handles.format) 


####create a MICE() object
print("creating a mice() object")
#need maxit > 25
#need at least m= 50

imptest <- mice(df.test, m=2, predictorMatrix=pm, method=impmethod, 
                maxit=2, seed=888, cluster_var=cluster_var, visitSequence=vis, 
                data.init = NULL, ignore = NULL, where = NULL, post = NULL, blots = NULL,
                control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=FALSE), nAGQ=0)
#failed to converge using optimizer='bobyqa'
#failed to converge using optimizer='Nelder_Mead'
#failed to converge using optimizer='nloptwrap', optimx.nlminb, nlminb, L-BFGS-B
#tried turning off all random effects (setting cluster char to "2" in the predmatrix)
#for the bobyqa and Nelder_Mead optimizers, set optCtrl=list(maxfun=2e7), no effect 

#solutions? 
#generate the "where" argument on your own 
#gererate the "ignore" argument on your own 


print("mice object <impfinal> created")

save(impfinal, file = "impfinal.rda")
write.mice.imputation(mi.res=impfinal, name="mice_impf", mids2spss=F)
#  imputationFunction=imputationFunction
imptest$imp$distress_score

#assess convergence
plot(imptest, population_total ~ .it | .ms)
dev.off() 









#fitting phase  
fit <- with(impfinal, 
            glmer(GDMT_ALLorContra ~ 
                    GENDERi + race2i + insurance + AGEi + distress_score + population_total + 
                    covar_OutsideZIPregion + MedExp + 
                    PMHx_none + PMHx_CHF + PMHx_ESRD + PMHx_Cr2 + PMHx_LVAD + PMHx_dm2 + PMHx_valvedz + 
                    PMHx_priorCVATIA + PMHx_fibfl + PMHx_LVAD + 
                    OH_EF + OH_TRANSPLANT + covar_ino + 
                    covar_Crdisc + covar_Kdisc + OH_DISCBPSYST + OH_DISCHR + 
                    inhospRx_BB + inhospRx_RAASI + inhospRx_MRA + inhospRx_none + 
                    MCNTRLi+ 
                    (1|SITE_ID) + (1|RESIDENTS) + (1|GH_HEART_TRANSPLANTS), 
                  family = binomial(link='logit'), 
                  control = glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5))))

saveRDS(fit, "/mnt/workspace/rstudio_projects/mice_est.rds")
#pooling phase:  
est1 <- pool(fit)
#### run rhe model with different iterations: 
# 1) with the imputed  data
# 2) with all patients with a distress score and discharge labs
