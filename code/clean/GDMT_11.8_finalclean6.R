getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

load('gdmt_2_include.RData')

df <- subset(df_gdmt, select = c(RCRDNUM, 
                                 aha_arnidisccontraspec_1:aha_arnidisccontraspec_8, jc_arnidisccontra,
                                 aha_arni, jc_arnidisc, OH_ARNI))


df <- 
  df %>% 
  mutate(GDMT_ARNIcontra = ifelse(aha_arnidisccontraspec_1 %in% 1 | 
                                    aha_arnidisccontraspec_2 %in% 1 | 
                                    aha_arnidisccontraspec_3 %in% 1 | 
                                    aha_arnidisccontraspec_4 %in% 1 | 
                                    aha_arnidisccontraspec_5 %in% 1 | 
                                    aha_arnidisccontraspec_6 %in% 1 | 
                                    aha_arnidisccontraspec_7 %in% 1 | 
                                    aha_arnidisccontraspec_8 %in% 1 | 
                                    jc_arnidisccontra %in% 1, 1, 
                            ifelse(aha_arnidisccontraspec_1 %in% 0 | 
                                     aha_arnidisccontraspec_2 %in% 0 | 
                                     aha_arnidisccontraspec_3 %in% 0 | 
                                     aha_arnidisccontraspec_4 %in% 0 | 
                                     aha_arnidisccontraspec_5 %in% 0 | 
                                     aha_arnidisccontraspec_6 %in% 0 | 
                                     aha_arnidisccontraspec_7 %in% 0 | 
                                     aha_arnidisccontraspec_8 %in% 0 | 
                                     jc_arnidisccontra %in% 0, 0,
                           ifelse(is.na(aha_arnidisccontraspec_1) & 
                                    is.na(aha_arnidisccontraspec_2) &
                                    is.na(aha_arnidisccontraspec_3) &
                                    is.na(aha_arnidisccontraspec_4) &
                                    is.na(aha_arnidisccontraspec_5) &
                                    is.na(aha_arnidisccontraspec_6) &
                                    is.na(aha_arnidisccontraspec_7) &
                                    is.na(aha_arnidisccontraspec_8) &
                                    is.na(jc_arnidisccontra), NA, 999))))
df$GDMT_ARNIcontra <- as.factor(df$GDMT_ARNIcontra)

df <- rename(df, contra_ACEI36 = aha_arnidisccontraspec_1)
df <- rename(df, contra_allergy = aha_arnidisccontraspec_2)
df <- rename(df, contra_hyperK = aha_arnidisccontraspec_3)
df <- rename(df, contra_HoTN = aha_arnidisccontraspec_4)
df <- rename(df, contra_otherMED = aha_arnidisccontraspec_5)
df <- rename(df, contra_patient = aha_arnidisccontraspec_6)
df <- rename(df, contra_renalfxn = aha_arnidisccontraspec_7)
df <- rename(df, contra_system = aha_arnidisccontraspec_8)

df <- 
  df %>% 
  mutate(contra_ACEI36 = ifelse(!is.na(jc_arnidisccontra) & 
                                  is.na(contra_ACEI36), 0, contra_ACEI36))
df$contra_ACEI36 <- as.factor(df$contra_ACEI36)

df <- 
  df %>% 
  mutate(contra_allergy = ifelse(!is.na(jc_arnidisccontra) & 
                                   is.na(contra_allergy), 0, contra_allergy))
df$contra_allergy <- as.factor(df$contra_allergy)

df <- 
  df %>% 
  mutate(contra_hyperK = ifelse(!is.na(jc_arnidisccontra) & 
                                  is.na(contra_hyperK), 0, contra_hyperK))
df$contra_hyperK <- as.factor(df$contra_hyperK)

df <- 
  df %>% 
  mutate(contra_HoTN = ifelse(!is.na(jc_arnidisccontra) & 
                                is.na(contra_HoTN), 0, contra_HoTN))
df$contra_HoTN <- as.factor(df$contra_HoTN)

df <- 
  df %>% 
  mutate(contra_otherMED = ifelse(!is.na(jc_arnidisccontra) &
                                    is.na(contra_otherMED), 0, contra_otherMED))
df$contra_otherMED <- as.factor(df$contra_otherMED)

df <- 
  df %>% 
  mutate(contra_patient = ifelse(!is.na(jc_arnidisccontra) & 
                                   is.na(contra_patient), 0, contra_patient))
df$contra_patient <- as.factor(df$contra_patient)

df <- 
  df %>% 
  mutate(contra_renalfxn = ifelse(!is.na(jc_arnidisccontra) & 
                                    is.na(contra_renalfxn), 0, contra_renalfxn))
df$contra_renalfxn <- as.factor(df$contra_renalfxn)

df <- 
  df %>% 
  mutate(contra_system = ifelse(!is.na(jc_arnidisccontra) & 
                                  is.na(contra_system), 0, contra_system))
df$contra_system <- as.factor(df$contra_system)

df <- subset(df, select = -c(jc_arnidisccontra, aha_arni, jc_arnidisc))


load('gdmt_11.7_final.RData')
df.run = merge(df.run, df, by.x="RCRDNUM", by.y="RCRDNUM", all.x=TRUE)

#save the new data frame 
save(df.run, file = '/mnt/workspace/rstudio_projects/gdmt_11.8.2_final.RData')
