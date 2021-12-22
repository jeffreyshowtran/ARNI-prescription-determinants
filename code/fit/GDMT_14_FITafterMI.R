
setwd("/mnt/workspace/rstudio_projects/data/")

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


load("/mnt/workspace/rstudio_projects/data/imp_rf2/imp_rf2.Rdata")

#before fitting the model, perform grand-mean centering and re-classify distress_score into quintiles
imp_rf2 <- mi.res
rm(mi.res)

long <- mice::complete(imp_rf2, "long", include=TRUE)
# 
# gm_DCI = mean(long$distress_score, na.rm=TRUE)
# long$DCI_GMrescale = long$distress_score - gm_DCI

gm_EF = mean(long$OH_EF, na.rm=TRUE)
long$EF_GMrescale = long$OH_EF - gm_EF

gm_age = mean(long$AGEi, na.rm=TRUE)
long$age_GMrescale = long$AGEi - gm_age
# 
# gm_K = mean(long$covar_Kdisc, na.rm=TRUE)
# long$Kdisc_GMrescale = long$covar_Kdisc - gm_K

gm_Cr = mean(long$covar_Crdisc, na.rm=TRUE)
long$Crdisc_GMrescale = long$covar_Crdisc - gm_Cr
# 
# gm_sBP = mean(long$OH_DISCBPSYST, na.rm=TRUE)
# long$sBP_GMrescale = long$OH_DISCBPSYST - gm_sBP
# 
# gm_HR = mean(long$OH_DISCHR, na.rm=TRUE)
# long$HR_GMrescale = long$OH_DISCHR - gm_HR
# 
# gm_poptotal = mean(long$population_total, na.rm=TRUE)
# long$poptotal_GMrescale = long$population_total - gm_poptotal

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

#rm(gm_DCI, gm_EF, gm_age, gm_K, gm_Cr, gm_sBP, gm_HR, gm_poptotal)
rm(gm_EF, gm_age, gm_Cr)

imprf2_GMC <- as.mids(long)
rm(long)

#set up parallel computing task 
mids = imprf2_GMC

f <- function(i) {
  
  data.i <- complete(mids,i)
  
  # fits <- glmer(GDMT_ARNI ~ inhospRx_ARNI + (1 | SITE_ID),
  #                 family='binomial', data = data.i,
  #                 control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
  
  fits <- glmer(GDMT_ARNI ~ GDMT_HFBB + GDMT_ACEIARB + GDMT_MRA +
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
        family='binomial', data = data.i,
        control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5), calc.derivs=TRUE))
  
  return(fits)
}

#initiate a cluster using 30 cores. Cluster type is "FORK"
n.core = 30
iter = imp_rf2$m

cl <- makeCluster(n.core, type="FORK")

parallel::clusterEvalQ(cl, library(lme4))
parallel::clusterExport(cl, varlist="mids", envir=environment())


fit <- parLapply(cl, 1:iter, f)

parallel::stopCluster(cl)

#assemble a mira object 
object <- list(call=match.call(), call1=imprf2_GMC$call, nmis=imprf2_GMC$nmis, analyses=fit)
oldClass(object) <- c("mira", "matrix")
object
#saveRDS(object, "mymodel.rds")


#pool the results 
pool <- pool(object)
pool$pooled
result <- summary(pool)

rm(n.core, iter, cl)

#create a odds ratio table 
table <- cbind(Var=result$term, 
               Est=exp(result$estimate), 
               SE=exp(result$std.error), 
               LL=exp(result$estimate - 1.96*result$std.error),
               UL=exp(result$estimate + 1.96*result$std.error),
               p=result$p.value)

table2 <- format(round(table,3), nsmall=3)
result$term


#calculate the ICC (notice it doesn't matter which imputed data set you use)
m0 <- glmer(GDMT_ARNI ~ (1|SITE_ID), 
            data=mice::complete(imprf2_GMC,2), 
            family='binomial', 
            control=glmerControl(optimizer='bobyqa', 
                                 optCtrl=list(maxfun=2e5), 
                                 calc.derivs=TRUE))
ICC <- m0@theta[1]^2/(m0@theta[1]^2 + (3.14159^2/3))

#sensitivity analysis: run the model using complete case analysis 
cc_analysis <-  glmer(GDMT_ARNI ~ GDMT_HFBB + GDMT_ACEIARB + GDMT_MRA +
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
                family='binomial', data = df, verbose=2,
                control=glmerControl(optimizer='bobyqa', 
                                     optCtrl=list(maxfun=2e5), 
                                     calc.derivs=TRUE))

summary(cc_analysis)
se <- sqrt(diag(vcov(cc_analysis)))
cc_analysis_table <- cbind(Est = fixef(cc_analysis), LL = fixef(cc_analysis) - 1.96*se, UL = fixef(cc_analysis) + 1.96* se)
exp(cc_analysis_table)
cc_analysis_table_rounded <- format(round(exp(cc_analysis_table),3), nsmall=3)
cc_analysis_table_rounded

