getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

load('gdmt_2_include.RData')

df <- subset(df_gdmt, select = c(RCRDNUM, 
                                 AHA_BETADISCONTRATYPE_1:AHA_BETADISCONTRATYPE_7, JC_BETADISCCONTRA, BBdisc, 
                                 AHA_ACEDISCCONTRASPEC_1:AHA_ACEDISCCONTRASPEC_5, JC_ACEDISCCONTRA, ACEIdisc, 
                                 AHA_ARBDISCCONTRASPEC_1:AHA_ARBDISCCONTRASPEC_5, JC_ARBDISCCONTRA, ARBdisc, 
                                 aha_arnidisccontraspec_1:aha_arnidisccontraspec_8, jc_arnidisccontra,
                                 aha_arniaceiarb, 
                                 HF_CONTRA_ALDOSTER_DISC_LIST_1:HF_CONTRA_ALDOSTER_DISC_LIST_7, 
                                 ALDOdisc, HF_CONTRA_ALDOSTERONE_DISC))

df <- 
  df %>% 
  mutate(GDMT_BBcontra = ifelse(AHA_BETADISCONTRATYPE_1 %in% 1 | 
                                AHA_BETADISCONTRATYPE_2 %in% 1 | 
                                AHA_BETADISCONTRATYPE_3 %in% 1 | 
                                AHA_BETADISCONTRATYPE_4 %in% 1 | 
                                AHA_BETADISCONTRATYPE_5 %in% 1 | 
                                AHA_BETADISCONTRATYPE_6 %in% 1 | 
                                AHA_BETADISCONTRATYPE_7 %in% 1 | 
                                JC_BETADISCCONTRA %in% 1 | BBdisc %in% 3, 1, 
                         ifelse(JC_BETADISCCONTRA %in% 0 |
                                AHA_BETADISCONTRATYPE_1 %in% 0 |
                                AHA_BETADISCONTRATYPE_2 %in% 0 |
                                AHA_BETADISCONTRATYPE_3 %in% 0 |
                                AHA_BETADISCONTRATYPE_4 %in% 0 |
                                AHA_BETADISCONTRATYPE_5 %in% 0 |
                                AHA_BETADISCONTRATYPE_6 %in% 0 |
                                AHA_BETADISCONTRATYPE_7 %in% 0, 0, 
                         ifelse(is.na(AHA_BETADISCONTRATYPE_1) & 
                                is.na(AHA_BETADISCONTRATYPE_2) &
                                is.na(AHA_BETADISCONTRATYPE_3) &
                                is.na(AHA_BETADISCONTRATYPE_4) &
                                is.na(AHA_BETADISCONTRATYPE_5) &
                                is.na(AHA_BETADISCONTRATYPE_6) &
                                is.na(AHA_BETADISCONTRATYPE_7) &
                                is.na(JC_BETADISCCONTRA), NA, 999))))

df <- 
  df %>% 
  mutate(GDMT_ACEIcontra = ifelse(AHA_ACEDISCCONTRASPEC_1 %in% 1 | 
                                  AHA_ACEDISCCONTRASPEC_2 %in% 1 | 
                                  AHA_ACEDISCCONTRASPEC_3 %in% 1 | 
                                  AHA_ACEDISCCONTRASPEC_4 %in% 1 | 
                                  AHA_ACEDISCCONTRASPEC_5 %in% 1 | 
                                  JC_ACEDISCCONTRA %in% 1 | ACEIdisc %in% 3, 1, 
                           ifelse(JC_ACEDISCCONTRA %in% 0 |
                                  AHA_ACEDISCCONTRASPEC_1 %in% 0 |
                                  AHA_ACEDISCCONTRASPEC_2 %in% 0 |
                                  AHA_ACEDISCCONTRASPEC_3 %in% 0 |
                                  AHA_ACEDISCCONTRASPEC_4 %in% 0 |
                                  AHA_ACEDISCCONTRASPEC_5 %in% 0, 0, 
                           ifelse(is.na(AHA_ACEDISCCONTRASPEC_1) & 
                                  is.na(AHA_ACEDISCCONTRASPEC_2) &
                                  is.na(AHA_ACEDISCCONTRASPEC_3) &
                                  is.na(AHA_ACEDISCCONTRASPEC_4) &
                                  is.na(AHA_ACEDISCCONTRASPEC_5) &
                                  is.na(JC_ACEDISCCONTRA), NA, 999))))
 
df <- 
  df %>% 
  mutate(GDMT_ARBcontra = ifelse(AHA_ARBDISCCONTRASPEC_1 %in% 1 | 
                                  AHA_ARBDISCCONTRASPEC_1 %in% 1 | 
                                  AHA_ARBDISCCONTRASPEC_1 %in% 1 | 
                                  AHA_ARBDISCCONTRASPEC_1 %in% 1 | 
                                  AHA_ARBDISCCONTRASPEC_1 %in% 1 | 
                                  JC_ARBDISCCONTRA %in% 1 | ARBdisc %in% 3, 1, 
                           ifelse(JC_ARBDISCCONTRA %in% 0 |
                                  AHA_ARBDISCCONTRASPEC_1 %in% 0 |
                                  AHA_ARBDISCCONTRASPEC_1 %in% 0 |
                                  AHA_ARBDISCCONTRASPEC_1 %in% 0 |
                                  AHA_ARBDISCCONTRASPEC_1 %in% 0 |
                                  AHA_ARBDISCCONTRASPEC_1 %in% 0, 0, 
                           ifelse(is.na(AHA_ARBDISCCONTRASPEC_1) & 
                                  is.na(AHA_ARBDISCCONTRASPEC_1) &
                                  is.na(AHA_ARBDISCCONTRASPEC_1) &
                                  is.na(AHA_ARBDISCCONTRASPEC_1) &
                                  is.na(AHA_ARBDISCCONTRASPEC_1) &
                                  is.na(JC_ARBDISCCONTRA), NA, 999))))

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

df <- 
  df %>% 
  mutate(GDMT_RAASIcontra = ifelse(GDMT_ACEIcontra %in% 1 | 
                                   GDMT_ARBcontra  %in% 1 | 
                                   GDMT_ARNIcontra %in% 1, 1, 
                            ifelse(GDMT_ACEIcontra %in% 0 | 
                                   GDMT_ARBcontra  %in% 0 | 
                                   GDMT_ARNIcontra %in% 0, 0,
                            ifelse(is.na(GDMT_ACEIcontra) & 
                                   is.na(GDMT_ARBcontra) &
                                   is.na(GDMT_ARNIcontra), NA, 999))))
df$GDMT_RAASIcontra[df$aha_arniaceiarb==1] <- 1

df <- 
  df %>% 
  mutate(GDMT_MRAcontra = ifelse(HF_CONTRA_ALDOSTER_DISC_LIST_1 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_2 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_3 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_4 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_5 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_6 %in% 1 | 
                                  HF_CONTRA_ALDOSTER_DISC_LIST_7 %in% 1 | 
                                  HF_CONTRA_ALDOSTERONE_DISC %in% 1 | ALDOdisc %in% 3, 1, 
                           ifelse(HF_CONTRA_ALDOSTERONE_DISC %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_1 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_2 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_3 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_4 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_5 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_6 %in% 0 |
                                  HF_CONTRA_ALDOSTER_DISC_LIST_7 %in% 0, 0, 
                           ifelse(is.na(HF_CONTRA_ALDOSTER_DISC_LIST_1) & 
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_2) &
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_3) &
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_4) &
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_5) &
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_6) &
                                  is.na(HF_CONTRA_ALDOSTER_DISC_LIST_7) &
                                  is.na(HF_CONTRA_ALDOSTERONE_DISC), NA, 999))))

df <- subset(df, select = c(RCRDNUM, GDMT_BBcontra, GDMT_RAASIcontra, GDMT_MRAcontra))

load('gdmt_11.6_final.RData')
df.run = merge(df.run, df, by.x="RCRDNUM", by.y="RCRDNUM", all.x=TRUE)

#save the new data frame 
#save(df.run, file = '/mnt/workspace/rstudio_projects/gdmt_11.7_final.RData')
