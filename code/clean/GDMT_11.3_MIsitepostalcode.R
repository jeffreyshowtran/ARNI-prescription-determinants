#fill in SITE_POSTAL_CODE where it is unknown in one patient but known in others 

getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

#install.packages("readxl")

library(mice)
library(miceadds)
library(micemd)
library(ggplot2)
library(lattice)

load('gdmt_11.1_final.RData')

#only need a few var for this imputation since it is more of a data repair 
df.imp <- subset(df.final, select = c(RCRDNUM, SITE_ID, SITE_POSTAL_CODE))

#are there any sites where the site postal code is NOT missing for ALL patients within that SITE?
df.imp2 <- subset(df.imp, is.na(SITE_POSTAL_CODE))
df.imp2 <- subset(df.imp2, select = c(SITE_ID))

df.imp3 = merge(df.imp, df.imp2, by.x='SITE_ID', by.y='SITE_ID', all.y=TRUE)
length(unique(df.imp3$SITE_ID))
df.imp4 = subset(df.imp3, !is.na(SITE_POSTAL_CODE))
#there are 53 unique sites where postal code is NOT given and NONE of the sites have patients where that site is listed 
#hence there is no point to the remainder of this code as 2lonly.mean() has no data to extrapolate from 

#how many missing in each variable? 
lapply(df.imp, function(x) 
  sum(is.na(x)))

#SITE_ID is the level 2 defining var; it has no missing data 
cluster_var=df.test$SITE_ID
df.imp$SITE_ID <- as.numeric(df.imp$SITE_ID)

#create the imputation method vector 
IM <- character(ncol(df.imp))
names(IM) <- colnames(df.imp)
print("imputation vector created")

#set imp method for SITE_POSTAL_CODE
IM['SITE_POSTAL_CODE'] <- "2lonly.mean"

print("all imputation methods assigned")

#####make the predicator matrix ####
PM <- make.predictorMatrix(df.imp)

#set the SITE_ID as the cluster var 
PM[,'SITE_ID'] <- -2
PM['SITE_ID','SITE_ID'] <- 0

PM[,'RCRDNUM'] <- 0


print("running MICE")
imptest1.2 <- mice(df.imp, m=5, predictorMatrix=PM, method=IM, 
                   maxit=5, seed=1234, cluster_var=cluster_var)

rm(IM, PM, cluster_var, df.imp2, df.imp3, df.imp4)
rm(imptest1.2)
