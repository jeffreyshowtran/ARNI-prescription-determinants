getwd() 
setwd("/mnt/workspace/rstudio_projects/data/")

library(dplyr)

load('gdmt_bb.RData')
view(df_gdmt_bb)
names(df_gdmt_bb)


                                          
#labels----------------------------------------------------
df_gdmt_bb$ACEIdisc <- factor(df_gdmt_bb$ACEIdisc, 
                            levels = c(1,2,3), 
                            labels = c('Yes- prescribed', 'No- not prescribed', 'No- contraindicated'))

df_gdmt_bb$AHA_ARB <- factor(df_gdmt_bb$AHA_ARB, 
                           levels = c(2,3,4,5,6,7,8,9), 
                           labels = c('candesartan', 'irbesartan', 'losartan', 'valsartan', 'telmisartan', 'eprosartan', 
                                       'tasosartan', 'other'))

df_gdmt_bb$ARBdisc <- factor(df_gdmt_bb$ARBdisc, 
                           levels = c(1,2,3), 
                           labels = c('Yes- prescribed', 'No- not prescribed', 'No- contraindicated'))

df_gdmt_bb$JC_ACESPE <- factor(df_gdmt_bb$JC_ACESPE, 
                             levels = c(2,4,5,9,26,29,32,35,47,50,51), 
                             labels = c('quinapril', 'perindopril', 'ramipril', 'captopril', 'benazepril', 'trandolapril', 
                                        'fosinopril', 'lisinopril', 'enalopril', 'lisinopril', 'other'))

df_gdmt_bb$aha_arni <- factor(df_gdmt_bb$aha_arni, 
                            levels = c(1,2,3), 
                            labels = c('sacubitril/valsartan', 'other', 'unknown'))

#subset out vars of interest----------------------------------------------------
df_raasi <- subset(df_gdmt_bb, select = c(PATIENT_ID, 
                                          ACEIdisc, JC_ACEDISC, JC_ACESPE, 
                                          ACEDISCCONTRASPEC_Missing, AHA_ACEDISCCONTRASPEC_1:AHA_ACEDISCCONTRASPEC_5, JC_ACEDISCCONTRA, 
                                          AHA_ARB, ARBdisc, JC_ARBDISC,
                                          AHA_ARBDISCCONTRASPEC_1:AHA_ARBDISCCONTRASPEC_5, ARBDISCCONTRASPEC_Missing, JC_ARBDISCCONTRA, 
                                          aha_arni, jc_arnidisc, 
                                          aha_arnidisccontraspec_1:aha_arnidisccontraspec_8, jc_arnidisccontra,
                                          aha_arniaceiarb, 
                                          HF_ACEIARBforLVSD))

#create variables summarizing contraindications----------------------------------------------------

#ACEI contraindicated? binary var
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ACEIcontra = ifelse(AHA_ACEDISCCONTRASPEC_1 == 1 | AHA_ACEDISCCONTRASPEC_2 == 1 | AHA_ACEDISCCONTRASPEC_3 == 1| 
                                  AHA_ACEDISCCONTRASPEC_4 == 1 | AHA_ACEDISCCONTRASPEC_5 == 1, 1, 0), .after=AHA_ACEDISCCONTRASPEC_5)
df_raasi$GDMT_ACEIcontra[is.na(df_raasi$GDMT_ACEIcontra)] <- 0
addmargins(table(df_raasi$GDMT_ACEIcontra, exclude = NULL))

#ACEI contraindicated? -> expanded to include those patients with contra as designated by ACEIdisc
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ACEIcontra_exp = ifelse(GDMT_ACEIcontra == 0 & ACEIdisc == 'No- contraindicated', 1, GDMT_ACEIcontra), .after=GDMT_ACEIcontra)
df_raasi$GDMT_ACEIcontra_exp[is.na(df_raasi$GDMT_ACEIcontra_exp)] <- 0
addmargins(table(df_raasi$GDMT_ACEIcontra, df_raasi$GDMT_ACEIcontra_exp, exclude = NULL))

#ARB contraindicated? binary var 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARBcontra = ifelse(AHA_ARBDISCCONTRASPEC_1 == 1 | AHA_ARBDISCCONTRASPEC_2 == 1 | AHA_ARBDISCCONTRASPEC_3 == 1 |
                                 AHA_ARBDISCCONTRASPEC_4 == 1 | AHA_ARBDISCCONTRASPEC_5 == 1, 1, 0), .after=AHA_ARBDISCCONTRASPEC_5)
df_raasi$GDMT_ARBcontra[is.na(df_raasi$GDMT_ARBcontra)] <- 0
addmargins(table(df_raasi$GDMT_ARBcontra, exclude = NULL))

#ACEI contraindicated? -> expanded to include those patients with contra as designated by ACEIdisc
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARBcontra_exp = ifelse(GDMT_ARBcontra == 0 & ARBdisc == 'No- contraindicated', 1, GDMT_ARBcontra), .after=GDMT_ARBcontra)
#df_raasi$GDMT_ARBcontra_exp[is.na(df_raasi$GDMT_ARBcontra_exp)] <- 0
addmargins(table(df_raasi$GDMT_ARBcontra, df_raasi$GDMT_ARBcontra_exp, exclude = NULL))
addmargins(table(df_raasi$ARBdisc, df_raasi$GDMT_ARBcontra_exp, exclude = NULL))

#ARNI contraindicated? binary var 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARNIcontra = ifelse(aha_arnidisccontraspec_1 == 1 | aha_arnidisccontraspec_2 == 1 | aha_arnidisccontraspec_3 == 1 | 
                                  aha_arnidisccontraspec_4 == 1 | aha_arnidisccontraspec_5 == 1 | aha_arnidisccontraspec_6 == 1 | 
                                  aha_arnidisccontraspec_7 == 1 | aha_arnidisccontraspec_8 == 1, 1, 0), .after=aha_arnidisccontraspec_8)
df_raasi$GDMT_ARNIcontra[is.na(df_raasi$GDMT_ARNIcontra)] <- 0
addmargins(table(df_raasi$GDMT_ARNIcontra, exclude = NULL))


#tables----------------------------------------------------
names(df_raasi)
lapply(subset(df_raasi, select = -c(PATIENT_ID, AHA_ACEDISCCONTRASPEC_1:AHA_ACEDISCCONTRASPEC_5, ACEDISCCONTRASPEC_Missing, 
                                    AHA_ARBDISCCONTRASPEC_1:AHA_ARBDISCCONTRASPEC_5, ARBDISCCONTRASPEC_Missing, 
                                    aha_arnidisccontraspec_1:aha_arnidisccontraspec_8)), 
                                 function(x) addmargins(table(x, exclude = NULL)))

#ACEI##################################################### 
#part I: comparing the contra-indication variables 
addmargins(table(df_raasi$ACEIdisc, df_raasi$GDMT_ACEIcontra, exclude = NULL)) 
  #2 elements here: 1) 538pts for whom ACEI is contra got it anyways 
  #                 2) disturbingly there are 7036 patients for whom ACEI based on ACEdisc was contra-indicated who did not 
  #                    register in the ACEIcontra variable > may need to expand it 
addmargins(table(df_raasi$ACEIdisc, df_raasi$GDMT_ACEIcontra_exp, exclude = NULL)) 
  #here's the follow up comparison of the expansion var - the expanded version is valid 
addmargins(table(df_raasi$ACEDISCCONTRASPEC_Missing, df_raasi$GDMT_ACEIcontra, exclude = NULL)) 
  #valid - all patients with a missing contraindication do not have a subcategory contra listed 

#part II: who actually got an ACEI on dc? 
addmargins(table(df_raasi$ACEIdisc, df_raasi$JC_ACEDISC, exclude = NULL)) 
  #ACEIdisc and JC_ACEDISC are consistent but ACEIdisc is better, has less missing and more info - differentiates no/contra from no
addmargins(table(df_raasi$JC_ACESPE, df_raasi$ACEIdisc, exclude = NULL))


#ARB#####################################################
#part III: comparing the contra-indication variables 
addmargins(table(df_raasi$ARBdisc, df_raasi$GDMT_ARBcontra, exclude = NULL))
  #512 patients had a contraindicaiton to ARB and still got one 
  #7230 patients without a condraindication designated by AHA_ARBDISCCONTRASPEC_ vars actually had a contraindication per ARBdisc; build an exp form?
addmargins(table(df_raasi$ARBdisc, df_raasi$GDMT_ARBcontra_exp, exclude = NULL))
  #follow up has folded the 7230 pts noted above into the contraindicated pts for the GDMT_ARBcontra var
addmargins(table(df_raasi$ARBDISCCONTRASPEC_Missing, df_raasi$GDMT_ARBcontra, exclude = NULL)) 
  #it seems as aif the _Missing variable is derived from the HA_ARBDISCCONTRASPEC_ var series 
addmargins(table(df_raasi$ARBdisc, df_raasi$JC_ARBDISCCONTRA, exclude = NULL))

#part IV: who's actually getting an ARB on dc? 
addmargins(table(df_raasi$ARBdisc, df_raasi$AHA_ARB, exclude = NULL))
  #there are 2 patients who received losartan who are not counted as having received an ARB, otherwise ARBdisc is a very good variable
addmargins(table(df_raasi$ARBdisc, df_raasi$JC_ARBDISC, exclude = NULL))
  #JC_ARBDISC is a good variable but there are some some values designated by NA that ARBdisc has knowledge of 

#ARNI#####################################################
#part V: comparing the contra-indications for ARNI
addmargins(table(df_raasi$jc_arnidisc, df_raasi$GDMT_ARNIcontra, exclude = NULL))
addmargins(table(df_raasi$aha_arni, df_raasi$GDMT_ARNIcontra, exclude = NULL))
  #85+8 patients with contra to ARNI got one anyways 

addmargins(table(df_raasi$GDMT_ARNIcontra, df_raasi$jc_arnidisccontra, exclude = NULL)) 
addmargins(table(df_raasi$jc_arnidisc, df_raasi$jc_arnidisccontra, exclude = NULL))

#part VI: who actually got an ARNI? 
addmargins(table(df_raasi$jc_arnidisc, exclude = NULL))
addmargins(table(df_raasi$aha_arni, exclude = NULL))
addmargins(table(df_raasi$jc_arnidisc, df_raasi$aha_arni, exclude = NULL))
  #8679 pts got an ARNI that the aha_arni classification didn't know about?? 
  #1+3 pts got an ARNI that jc_arni classificaiotn didn't know about - we will give the coders the benefit of the doubt 





#other checks
#who's getting both ACEI and ARB?? 
addmargins(table(df_raasi$ARBdisc, df_raasi$ACEIdisc, exclude = NULL)) 
  #there are 772 patient's prescribed both?? 
addmargins(table(df_raasi$AHA_ARB, df_raasi$JC_ACESPE, exclude = NULL))
  #there are 1+5 + 2 +1 +16 73 19 + 1 + 2 + 9+ 2 + 18  patient's getting an ACEI/ARB combo that can be identified?? 
#who's getting ARNI/ACEI or ARNI/ARB?
addmargins(table(df_raasi$ARBdisc, df_raasi$aha_arni, exclude = NULL))
  #there are 526 patients prescribed ARNI and ARB? 
addmargins(table(df_raasi$ACEIdisc, df_raasi$aha_arni, exclude = NULL))
  #there are 183 patients prescribed ACEI and ARNI on dc? 

#create essential variables summarizing prescribed and prescribed/contra----------------------------------------------------

#did the patient get an ACEI on dc? 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ACEI = ifelse(df_raasi$JC_ACESPE %in% 'lisinopril' | df_raasi$ACEIdisc == 'Yes- prescribed', 1, 
                     ifelse(df_raasi$ACEIdisc == 'No- not prescribed' | df_raasi$ACEIdisc == 'No- contraindicated', 0, 99)))
addmargins(table(df_raasi$GDMT_ACEI, df_raasi$ACEIdisc, exclude = NULL))

#this test case of %in% vs == is very interesting... I'm still not sure why R is doing what it's doing 
#df_raasi <- 
#  df_raasi %>% 
#  mutate(GDMT_ACEI = ifelse(df_raasi$JC_ACESPE %in% 'lisinopril' | df_raasi$ACEIdisc %in% 'Yes- prescribed', 1, 
#                            ifelse(df_raasi$ACEIdisc %in% 'No- not prescribed' | df_raasi$ACEIdisc %in% 'No- contraindicated', 0, 99)))

#did the patient get an ACEI on dc or was it contraindicated? 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ACEIorContra = ifelse(df_raasi$GDMT_ACEI == 1 | df_raasi$GDMT_ACEIcontra_exp == 1, 1, 0))
addmargins(table(df_raasi$GDMT_ACEIorContra, df_raasi$ACEIdisc, exclude = NULL))

#did the patient get an ARB on dc? # 34822 + 2
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARB = ifelse(df_raasi$ARBdisc == 'Yes- prescribed' | df_raasi$AHA_ARB %in% 'losartan', 1, 
                    ifelse(df_raasi$ARBdisc == 'No- not prescribed' | df_raasi$ARBdisc == 'No- contraindicated', 0, 99)))
addmargins(table(df_raasi$GDMT_ARB, df_raasi$ARBdisc, exclude = NULL))
                            
#did the patient get an ARB on dc or was it contraindicated? 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARBorContra = ifelse(df_raasi$GDMT_ARB == 1 | df_raasi$GDMT_ARBcontra_exp == 1, 1, 0))
addmargins(table(df_raasi$GDMT_ARBorContra, df_raasi$ARBdisc, exclude = NULL))

#did the patient get an ARNI on dc? #22657
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARNI = ifelse(df_raasi$jc_arnidisc == 1 | df_raasi$aha_arni %in% c('sacubitril/valsartan', 'other'), 1, 0))
addmargins(table(df_raasi$GDMT_ARNI, exclude = NULL))
                          
#did the patient get an ARNI on dc or was it contraindicated? 
df_raasi <- 
  df_raasi %>% 
  mutate(GDMT_ARNIorContra = ifelse(df_raasi$GDMT_ARNI == 1 | df_raasi$GDMT_ARNIcontra == 1, 1, 0))
addmargins(table(df_raasi$GDMT_ARNIorContra, exclude = NULL))

#implement the key var into the gdmt dataframe----------------------------------------------------

#ACEI contraindicated? binary var
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ACEIcontra = ifelse(AHA_ACEDISCCONTRASPEC_1 == 1 | AHA_ACEDISCCONTRASPEC_2 == 1 | AHA_ACEDISCCONTRASPEC_3 == 1| 
                                  AHA_ACEDISCCONTRASPEC_4 == 1 | AHA_ACEDISCCONTRASPEC_5 == 1, 1, 0))
df_gdmt_bb$GDMT_ACEIcontra[is.na(df_gdmt_bb$GDMT_ACEIcontra)] <- 0
addmargins(table(df_gdmt_bb$GDMT_ACEIcontra, exclude = NULL))

#ACEI contraindicated? -> expanded to include those patients with contra as designated by ACEIdisc
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ACEIcontra_exp = ifelse(GDMT_ACEIcontra == 0 & ACEIdisc == 'No- contraindicated', 1, GDMT_ACEIcontra))
df_gdmt_bb$GDMT_ACEIcontra_exp[is.na(df_gdmt_bb$GDMT_ACEIcontra_exp)] <- 0
addmargins(table(df_gdmt_bb$GDMT_ACEIcontra, df_gdmt_bb$GDMT_ACEIcontra_exp, exclude = NULL))

#ARB contraindicated? binary var 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARBcontra = ifelse(AHA_ARBDISCCONTRASPEC_1 == 1 | AHA_ARBDISCCONTRASPEC_2 == 1 | AHA_ARBDISCCONTRASPEC_3 == 1 |
                                 AHA_ARBDISCCONTRASPEC_4 == 1 | AHA_ARBDISCCONTRASPEC_5 == 1, 1, 0))
df_gdmt_bb$GDMT_ARBcontra[is.na(df_gdmt_bb$GDMT_ARBcontra)] <- 0
addmargins(table(df_gdmt_bb$GDMT_ARBcontra, exclude = NULL))

#ACEI contraindicated? -> expanded to include those patients with contra as designated by ACEIdisc
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARBcontra_exp = ifelse(GDMT_ARBcontra == 0 & ARBdisc == 'No- contraindicated', 1, GDMT_ARBcontra))
#df_raasi$GDMT_ARBcontra_exp[is.na(df_raasi$GDMT_ARBcontra_exp)] <- 0
addmargins(table(df_gdmt_bb$GDMT_ARBcontra, df_gdmt_bb$GDMT_ARBcontra_exp, exclude = NULL))
addmargins(table(df_gdmt_bb$ARBdisc, df_gdmt_bb$GDMT_ARBcontra_exp, exclude = NULL))

#ARNI contraindicated? binary var 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARNIcontra = ifelse(aha_arnidisccontraspec_1 == 1 | aha_arnidisccontraspec_2 == 1 | aha_arnidisccontraspec_3 == 1 | 
                                  aha_arnidisccontraspec_4 == 1 | aha_arnidisccontraspec_5 == 1 | aha_arnidisccontraspec_6 == 1 | 
                                  aha_arnidisccontraspec_7 == 1 | aha_arnidisccontraspec_8 == 1, 1, 0))
df_gdmt_bb$GDMT_ARNIcontra[is.na(df_gdmt_bb$GDMT_ARNIcontra)] <- 0
addmargins(table(df_gdmt_bb$GDMT_ARNIcontra, exclude = NULL))


#did the patient get an ACEI on dc? 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ACEI = ifelse(JC_ACESPE %in% 'lisinopril' | ACEIdisc == 'Yes- prescribed', 1, 
                     ifelse(ACEIdisc == 'No- not prescribed' | ACEIdisc == 'No- contraindicated', 0, 99)))
addmargins(table(df_gdmt_bb$GDMT_ACEI, df_gdmt_bb$ACEIdisc, exclude = NULL))

#did the patient get an ACEI on dc or was it contraindicated? 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ACEIorContra = ifelse(GDMT_ACEI == 1 | GDMT_ACEIcontra_exp == 1, 1, 0))
addmargins(table(df_gdmt_bb$GDMT_ACEIorContra, df_gdmt_bb$ACEIdisc, exclude = NULL))

#did the patient get an ARB on dc? # 34822 + 2
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARB = ifelse(ARBdisc == 'Yes- prescribed' | AHA_ARB %in% 'losartan', 1, 
                    ifelse(ARBdisc == 'No- not prescribed' | ARBdisc == 'No- contraindicated', 0, 99)))
addmargins(table(df_gdmt_bb$GDMT_ARB, df_gdmt_bb$ARBdisc, exclude = NULL))

#did the patient get an ARB on dc or was it contraindicated? 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARBorContra = ifelse(GDMT_ARB == 1 | GDMT_ARBcontra_exp == 1, 1, 0))
addmargins(table(df_gdmt_bb$GDMT_ARBorContra, df_gdmt_bb$ARBdisc, exclude = NULL))

#did the patient get an ARNI on dc? #22657
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARNI = ifelse(jc_arnidisc == 1 | aha_arni %in% c('sacubitril/valsartan', 'other'), 1, 0))
addmargins(table(df_gdmt_bb$GDMT_ARNI, exclude = NULL))

#did the patient get an ARNI on dc or was it contraindicated? 
df_gdmt_bb <- 
  df_gdmt_bb %>% 
  mutate(GDMT_ARNIorContra = ifelse(GDMT_ARNI == 1 | GDMT_ARNIcontra == 1, 1, 0))
addmargins(table(df_gdmt_bb$GDMT_ARNIorContra, exclude = NULL))

#clean up excess dc Rx BB-related var----------------------------------------------------
names(df_gdmt_bb)
df_gdmt_bb_raasi <- subset(df_gdmt_bb, select = -c(ACEIdisc, JC_ACEDISC, JC_ACEDISCCONTRA, ACEDISCCONTRASPEC_Missing, 
                                                   JC_ARBDISC, ARBDISCCONTRASPEC_Missing, JC_ARBDISCCONTRA, 
                                                   aha_arni, jc_arnidisc, jc_arnidisccontra, 
                                                   aha_arniaceiarb, 
                                                   HF_ACEIARBforLVSD))
names(df_gdmt_bb_raasi)

#save the new data frame---------------------------------------------------- 
#save(df_gdmt_bb_raasi, file = '/mnt/workspace/rstudio_projects/gdmt_bb_raasi.RData')
