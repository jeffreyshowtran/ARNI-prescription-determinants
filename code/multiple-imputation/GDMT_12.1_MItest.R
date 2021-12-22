#this is a test of the polyreg method within MICE. We are giong to try to 
#fit a univariate multilevel MI model using polyreg by specifying a hierarchical 
#data structure using the predictor matrix. The test variable for polyreg MI is 
#race2i, but we will start with imputation of a method we know should work, 2l.2stage.pmm
# to impute OH_DISCHR 

getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

library(dplyr)
library(tidyr)
library(labelled)
library(mice)
library(miceadds)
library(micemd)
library(ggplot2)
library(lattice)

load('gdmt_11.2_catnomiss.RData')

#####convert SITE_ID to number ####
# this is required for MI when desigated as cluster ar
df.final$SITE_ID <- as.numeric(df.final$SITE_ID)

#in first iteration of test with df.final, a vector of 17.9Gb was created??? 
#so will reduce the size of the df in testing
df.test <- subset(df.final, (SITE_ID <= 50))

#understand zip codes by site
with(subset(df.test, SITE_ID==8), 
     table(droplevels(zip1), droplevels(SITE_POSTAL_CODE), exclude=NULL))

#understand missing race2i by cluster var 
histogram(~SITE_ID | is.na(zip1), data=df.test)

addmargins(table(df.test$SITE_ID, exclude=NULL))

####describe the data#### 
#quick look at the data
str(df.test, list.len=ncol(df.test))

#how many missing in each variable? 
lapply(df.test, function(x) 
  sum(is.na(x))/nrow(df.test))

####understand Agei by cluster var 
histogram(~SITE_ID | is.na(OH_DISCHR), data=df.test)

#understand missing race2i by cluster var 
histogram(~SITE_ID | is.na(race2i), data=df.test)

####create the imputation method vector ####
#indicate polyreg as method for imputing race
impmethod <- character(ncol(df.test))
names(impmethod) <- colnames(df.test)
#impmethod['race2i'] <- "polyreg" 
impmethod['OH_DISCHR'] <- "2l.lmer"
print(impmethod)

#####make the predicator matrix ####
pm <- make.predictorMatrix(df.test)

#set the SITE_ID as the cluster var 
#of note, cluster var must be integer 
class(df.test$SITE_ID)
pm['OH_DISCHR','SITE_ID'] <- -2 

#remove some var from the prediction (eg PATIENT_ID); eg setting <- 0
pm['OH_DISCHR', c('PATIENT_ID', 'zip_num', 'RCRDNUM', 
               'MedExp_adoption_date')] <- c(0,0,0,0)

#set the cluster var characteristics as random efx with random 
#intercept and random slope; eg setting <- 2
#RESIDENTS (!), GH_HEART_TRANSPLANTS (!), MCNTRLi, BEDSIZE
pm['OH_DISCHR', c('RESIDENTS', 'GH_HEART_TRANSPLANTS', 
               'MCNTRLi', 'BEDSIZE')] <- c(2,2,2,2)


print(pm['OH_DISCHR',])

#run the mice MI model 
imptest1 <- mice(df.test, m=5, predictorMatrix=pm, method=impmethod, 
                 maxit=10, printFlag=FALSE, seed=1234)
summary(complete(imptest1))
imptest1$imp$OH_DISCHR

sum(is.na(df.test$OH_DISCHR))
table(df.test$race2i, exclude=NULL)


table(df.test$covar_dispo, exclude = NULL)
names(imptest1)
imptest1$loggedEvents
