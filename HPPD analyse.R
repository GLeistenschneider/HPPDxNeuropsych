####The never-ending trip: cognitive functioning and hallucinogen perception disorder####
###Author: Georg Leistenschneider
##Date: 21/04/2023

library(readxl)
library(foreign)
library(MatchIt)
library(optmatch)
library(dplyr)
library(survey)
library(ggplot2)
library(RItools)
library(optmatch)
library(tidyverse)
library(skimr)
library(pillar)
library(extrafont)

#reading in data
dataRAW <- read.csv("/Users/georgleistenschneider/Documents/Uni/Masterarbeit/5. Auswertung/HPPD_Raw_Demo_CSV.csv")

#all participants with Symptoms of HPPD in the control group are deleted
dataRAW <- dataRAW %>%
  filter(case != 0 | (a.5.09_flashback != 1 & a.5.10.HPPD != 1))

####Matching####
#selecting only covariates that matter for matching
data <- dataRAW[, c("ID","case","a.1.01_age","a.1.02_gend","a.1.10_education", "a.2.03_lifetime_psy", "a.3.03_lifetime_can", "a.4.01.life_alc")]

#deleting all the empty columns
data <- na.omit(data)

# No matching; constructing a pre-match matchit object
m.out0 <- matchit(case ~ a.1.01_age + a.1.02_gend + a.1.10_education + a.2.03_lifetime_psy + 
                    a.3.03_lifetime_can + a.4.01.life_alc, data = data,
                  method = NULL, distance = "glm")

#checking balance prior to matching
summary(m.out0)

###Actual Matching###
# 1:1 NN PS matching w/o replacement

m.out1 <- matchit(case ~ a.1.01_age + a.1.02_gend + a.1.10_education + a.2.03_lifetime_psy + 
                    a.3.03_lifetime_can + a.4.01.life_alc, data = data,
                  method = "nearest", distance = "glm")

#checking balance 
summary(m.out1, un=FALSE)

# Full matching on a probit PS
m.out2 <- matchit(case ~ a.1.01_age + a.1.02_gend + a.1.10_education + a.2.03_lifetime_psy + 
                    a.3.03_lifetime_can + a.4.01.life_alc, data = data,
                  method = "full", distance = "glm", link = "probit")
summary(m.out2, un=FALSE)

# Optimal matching
m.out3 <- matchit(case ~ a.1.01_age + a.1.02_gend + a.1.10_education + a.2.03_lifetime_psy + 
                    a.3.03_lifetime_can + a.4.01.life_alc, data = data,
                  method = "optimal", distance = "glm")
m.out3
summary(m.out3, un=FALSE)

matched <- match.data(m.out3)   #creates dataframe with the matched participants

demoID <- dataRAW %>%     #creates dataframe of all data with just the matched participants
  filter(ID %in% matched$ID)









####Demographics####
demo <- demoID %>% select(-ID, -Spalte2)  #remove participant codes to ensure anonymity 

##age
#Extract ages of group 1 and group 0
age_case <- demo[demo$case == 1, "a.1.01_age"]
age_control <- demo[demo$case == 0, "a.1.01_age"]

age_test <- t.test(age_case, age_control, alternative = "two.sided", var.equal = FALSE)
age_test
skim(age_case)
skim(age_control)

##summary profession
#degree
degree <- data.frame(degree_case = demo[demo$case == 1, "a.1.09_degree"],
                  degree_control = demo[demo$case == 0, "a.1.09_degree"],
                 stringsAsFactors = FALSE)


result <- list()
for (col in colnames(degree)) {
  result[[col]] <- table(degree[[col]])
}
print(result)

# Print frequency tables
for (col in colnames(degree)) {
  cat("Frequency table for", col, ":\n")
  print(result[[col]])
  cat("\n")
}


#educaysh years
edu_case <- demo[demo$case == 1, "a.1.10_education"]
edu_control <- demo[demo$case == 0, "a.1.10_education"]

edu_test <- t.test(edu_case, edu_control, alternative = "two.sided")

edu_test
skim(edu_case)
skim(edu_control)

#age at first use of psychedelics
age_psy_case <- demo[demo$case == 1, "a.2.01_age_psy"]
age_psy_control <- demo[demo$case == 0, "a.2.01_age_psy"]

age_psy_test <- t.test(age_psy_case, age_psy_control, alternative = "two.sided")

age_psy_test 
skim(age_psy_case)
skim(age_psy_control)

##lifetime use of psychedelics
psyuselife_case <- demo[demo$case == 1, "a.2.03_lifetime_psy"]
psyuselife_control <- demo[demo$case == 0, "a.2.03_lifetime_psy"]

psyuselife_test <- t.test(psyuselife_case, psyuselife_control, alternative = "two.sided")

psyuselife_test
skim(psyuselife_case)
skim(psyuselife_control)

##psychedelics last three months
psyusethree_case <- demo[demo$case == 1, "a.2.04.avg_3mon_psy"]
psyusethree_control <- demo[demo$case == 0, "a.2.04.avg_3mon_psy"]

psyusethree_case[is.na(psyusethree_case)] <- 0
psyusethree_control[is.na(psyusethree_control)] <- 0

psyusethree_test <- t.test(psyusethree_case, psyusethree_control, alternative = "two.sided")

psyusethree_test
skim(psyusethree_case)
skim(psyusethree_control)

##favourite psychedelics

df_favs <- demo %>%
  group_by(case) %>%
  summarise(across(starts_with("a.2.15"), ~sum(. == 1, na.rm = TRUE)/ n() * 100))
print(df_favs)

##intensity of psychedelics

intpsy_case <- demo[demo$case == 1, "a.2.07_avg_intens_psy"]
intpsy_control <- demo[demo$case == 0, "a.2.07_avg_intens_psy"]

intpsy_test <- t.test(intpsy_case, intpsy_control, alternative = "two.sided")

intpsy_test
summary(intpsy_case)
summary(intpsy_control)

##dose of psychedelic

dospsy_case <- demo[demo$case == 1, "a.2.08_avg_dose_psy"]
dospsy_control <- demo[demo$case == 0, "a.2.08_avg_dose_psy"]

dospsy_test <- t.test(dospsy_case, dospsy_control, alternative = "two.sided")

psyuselife_test
summary(dospsy_case)
summary(dospsy_control)

##polydrug

polydrug <- demo %>%
  group_by(case) %>%
  summarise(polydrugyes = sum(a.2.09.true_poly_psy == 1, na.rm = TRUE)/ n() * 100,
            polydrugno = sum(a.2.09.true_poly_psy == 0, na.rm = TRUE)/ n() * 100)
print(polydrug)

##Setting
#alone vs together
alotog <- demo %>%
  group_by(case) %>%
  summarise(people = sum(a.2.10_peop_psy == "peop", na.rm = TRUE) / n() * 100,
            alone = sum(a.2.10_peop_psy == "alon", na.rm = TRUE) / n() * 100)
print(alotog)

#setting
df_setting <- demo %>%
  group_by(case) %>%
  summarise(across(starts_with("a.2.11_set"), ~ sum(. == 1, na.rm = TRUE)/ n() * 100))
df_setting

#Preperation
df_prep <- demo %>%
  group_by(case) %>%
  summarise(across(starts_with("a.2.12."), ~ sum(. == 1, na.rm = TRUE)/ n() * 100))
df_prep

##integration into daily life

integr_test <- t.test(demo[demo$case == 1, "a.2.13_integr_psy"], demo[demo$case == 0, "a.2.13_integr_psy"], alternative = "two.sided")
integr_test

##subj. assumptions
df_ass <- demo %>%
  group_by(case) %>%
  summarise(across(starts_with("a.2.14."), ~ sum(. == 1, na.rm = TRUE)))
df_ass

##age of first cannabis use
age_can_case <- demo[demo$case == 1, "a.3.01_age_first_can"]
age_can_control <- demo[demo$case == 0, "a.3.01_age_first_can"]

age_can_test <- t.test(age_can_case, age_can_control, alternative = "two.sided")

age_can_test
skim(age_psy_case)
skim(age_psy_control)

##lifetime use of cannabis
canuselife_case <- demo[demo$case == 1, "a.3.03_lifetime_can"]
canuselife_control <- demo[demo$case == 0, "a.3.03_lifetime_can"]

canuselife_test <- t.test(canuselife_case, canuselife_control, alternative = "two.sided")

canuselife_test
skim(canuselife_case)
skim(canuselife_control)

##cannabis last three months
canusethree_case <- demo[demo$case == 1, "a.3.04.avg_3_mon_can"]
canusethree_control <- demo[demo$case == 0, "a.3.04.avg_3_mon_can"]

canusethree_test <- t.test(canusethree_case, canusethree_control, alternative = "two.sided")

canusethree_test
skim(canusethree_case)
skim(canusethree_control)

##lifetime use of alcohol
alcuselife_case <- demo[demo$case == 1, "a.4.01.life_alc"]
alcuselife_control <- demo[demo$case == 0, "a.4.01.life_alc"]

alcuselife_test <- t.test(alcuselife_case, alcuselife_control, alternative = "two.sided")

alcuselife_test
skim(alcuselife_case)
skim(alcuselife_control)

##alcohol last three months
alcusethree_case <- demo[demo$case == 1, "a.4.01.3m_alc"]
alcusethree_control <- demo[demo$case == 0, "a.4.01.3m_alc"]

alcusethree_test <- t.test(alcusethree_case, alcusethree_control, alternative = "two.sided")

alcusethree_test
skim(alcusethree_case)
skim(alcusethree_control)

##other ilicit substances life
otherlife_case <- demo[demo$case == 1, "a.4.12.LIFE.COMBINED2"]
otherlife_control <- demo[demo$case == 0, "a.4.12.LIFE.COMBINED2"]

otherlife_test <- t.test(otherlife_case, otherlife_control, alternative = "two.sided")

otherlife_test
skim(otherlife_case)
skim(otherlife_control)

##other ilicit substances 3 month
otherthree_case <- demo[demo$case == 1, "a.4.12.3M.COMBINED2"]
otherthree_control <- demo[demo$case == 0, "a.4.12.3M.COMBINED2"]

otherthree_test <- t.test(otherthree_case, otherthree_control, alternative = "two.sided")

otherthree_test
skim(otherthree_case)
skim(otherthree_control)

##psychiatric history

psychhist_case <- demo[demo$case == 1, "a.7.01.true_life_psych_treat"]
psychhist_control <- demo[demo$case == 0, "a.7.01.true_life_psych_treat"]

psychhist_test <- t.test(psychhist_case, psychhist_control, alternative = "two.sided")

psychhist_test
skim(psychhist_case)
skim(psychhist_control)

##family history
famhist_case <- demo[demo$case == 1, "a.7.04.SUM_fam"]
famhist_control <- demo[demo$case == 0, "a.7.04.SUM_fam"]

famhist_test <- t.test(famhist_case, famhist_control, alternative = "two.sided")

famhist_test
skim(famhist_case)
skim(famhist_control)

##BDI################################################################################################################################################################################################################################
BDI_case <- npdata[npdata$case == 1, "BDI"]
BDI_control <- npdata[npdata$case == 0, "BDI"]

BDI_test <- t.test(BDI_case, BDI_control, alternative = "two.sided")
BDI_test
skim(BDI_case)
skim(BDI_control)

##ravens matrices########################################################################################################################################################################################################################
ra_case <- npdata[npdata$case == 1, "raven.total"]
ra_control <- npdata[npdata$case == 0, "raven.total"]

ra_test <- t.test(ra_case, ra_case, alternative = "two.sided")
ra_test
skim(ra_case)
skim(ra_case)

##Wortschatztest###################################################################################################################################################
wst_case <- npdata[npdata$case == 1, "wst.corr"]
wst_control <- npdata[npdata$case == 0, "wst.corr"]

wst_test <- wilcox.test(wst_case, wst_control, alternative = "two.sided")
wst_test
skim(wst_case)
skim(wst_control)


###FDR correction Demographics###

# Create a list to store the test results
test_results <- list(age_can_test = age_can_test, 
                     age_psy_test = age_psy_test, 
                     age_test = age_test,
                     alcuselife_test = alcuselife_test,
                     alcusethree_test = alcusethree_test,
                     canuselife_test = canuselife_test,
                     canusethree_test = canusethree_test,
                     dospsy_test = dospsy_test,
                     edu_test = edu_test,
                     integr_test = integr_test,
                     intpsy_test = intpsy_test,
                     otherlife_test = otherlife_test,
                     otherthree_test = otherthree_test,
                     psyuselife_test = psyuselife_test,
                     psyusethree_test = psyusethree_test,
                     BDI_test = BDI_test,
                     wst_test = wst_test,
                     ra_test = ra_test)

# Perform FDR adjustment on p-values from the list of test results
adjusted_p_values <- p.adjust(sapply(test_results, function(result) result$p.value), method = "BH")

# Add adjusted p-values to the original test results list
for (i in 1:length(test_results)) {
  test_results[[i]]$adjusted_p_value <- adjusted_p_values[i]
}

# Access adjusted p-values from the list
adjusted_p_value_age_can_test <- adjusted_p_values[["age_can_test"]]
adjusted_p_value_age_psy_test <- adjusted_p_values[["age_psy_test"]]
adjusted_p_value_age_test <- adjusted_p_values[["age_test"]]
adjusted_p_value_alcuselife_test <- adjusted_p_values[["alcuselife_test"]]
adjusted_p_value_alcusethree_test <- adjusted_p_values[["alcusethree_test"]]
adjusted_p_value_canuselife_test <- adjusted_p_values[["canuselife_test"]]
adjusted_p_value_canusethree_test <- adjusted_p_values[["canusethree_test"]]
adjusted_p_value_dospsy_test <- adjusted_p_values[["dospsy_test"]]
adjusted_p_value_edu_test <- adjusted_p_values[["edu_test"]]
adjusted_p_value_integr_test <- adjusted_p_values[["integr_test"]]
adjusted_p_value_intpsy_test <- adjusted_p_values[["intpsy_test"]]
adjusted_p_value_otherlife_test <- adjusted_p_values[["otherlife_test"]]
adjusted_p_value_otherthree_test <- adjusted_p_values[["otherthree_test"]]
adjusted_p_value_psyuselife_test <- adjusted_p_values[["psyuselife_test"]]
adjusted_p_value_psyusethree_test <- adjusted_p_values[["psyusethree_test"]]










####Neuropsychology####

# Reading in neuropsychological assessment (npa) data
npdataRAW <- read.csv("/Users/georgleistenschneider/Documents/Uni/Masterarbeit/5. Auswertung/HPPD_Raw_NPA_CSV.csv")

# creating npdataset with used the matched participants 
npdataID <- npdataRAW %>%     #creates dataframe of all data with just the matched participants
  filter(ID %in% matched$ID)

npdata <- npdataID %>% select(-ID)  #remove participant codes to ensure anonymity 

###Happy Testing
##Digit Symbol Test
digi_case <- npdata[npdata$case == 1, "digit.sym.corr"]
digi_control <- npdata[npdata$case == 0, "digit.sym.corr"]

digi_test <- wilcox.test(digi_case, digi_control, alternative = "two.sided")
digi_test
skim(digi_case)
skim(digi_control)

##Symbol Search
sym_case <- npdata[npdata$case == 1, "sym.search.corr"]
sym_control <- npdata[npdata$case == 0, "sym.search.corr"]

sym_test <- wilcox.test(sym_case, sym_control, alternative = "two.sided")
sym_test
skim(sym_case)
skim(sym_control)

##stroop color
stcol_case <- npdata[npdata$case == 1, "stroop.color"]
stcol_control <- npdata[npdata$case == 0, "stroop.color"]

stcol_test <- wilcox.test(stcol_case, stcol_control, alternative = "two.sided")
stcol_test
skim(stcol_case)
skim(stcol_control)

##stroop word
stwo_case <- npdata[npdata$case == 1, "stroop.word"]
stwo_control <- npdata[npdata$case == 0, "stroop.word"]

stwo_test <- wilcox.test(stwo_case, stwo_control, alternative = "two.sided")
stwo_test
skim(stwo_case)
skim(stwo_control)

##stroop inteference
stin_case <- npdata[npdata$case == 1, "stroop.int"]
stin_control <- npdata[npdata$case == 0, "stroop.int"]

stin_test <- wilcox.test(stin_case, stin_control, alternative = "two.sided")
stin_test
skim(stin_case)
skim(stin_control)

##stroop inteference uncorrected error
stinue_case <- npdata[npdata$case == 1, "stroop.int.unc.err"]
stinue_control <- npdata[npdata$case == 0, "stroop.int.unc.err"]

stinue_test <- wilcox.test(stinue_case, stinue_control, alternative = "two.sided")
stinue_test
skim(stinue_case)
skim(stinue_control)

##stroop inteference corrected error
stince_case <- npdata[npdata$case == 1, "stroop.int.cor.err"]
stince_control <- npdata[npdata$case == 0, "stroop.int.cor.err"]

stince_test <- wilcox.test(stince_case, stince_control, alternative = "two.sided")
stince_test
skim(stince_case)
skim(stince_control)

##reading the mind in the eye
rm_case <- npdata[npdata$case == 1, "read.mind.total"]
rm_control <- npdata[npdata$case == 0, "read.mind.total"]

rm_test <- wilcox.test(rm_case, rm_control, alternative = "two.sided")
rm_test
skim(rm_case)
skim(rm_control)

##tmt-a
tmta_case <- npdata[npdata$case == 1, "tmt.a.time"]
tmta_control <- npdata[npdata$case == 0, "tmt.a.time"]

tmta_test <- wilcox.test(tmta_case, tmta_control, alternative = "two.sided")
tmta_test
skim(tmta_case)
skim(tmta_control)

##tmt-b
tmtb_case <- npdata[npdata$case == 1, "tmt.b.time"]
tmtb_control <- npdata[npdata$case == 0, "tmt.b.time"]

tmtb_test <- wilcox.test(tmtb_case, tmtb_control, alternative = "two.sided")
tmtb_test
skim(tmtb_case)
skim(tmtb_control)

##Regensburger Wortflüssigkeit Category
wfs_case <- npdata[npdata$case == 1, "rbwf.sem"]
wfs_control <- npdata[npdata$case == 0, "rbwf.sem"]

wfs_test <- wilcox.test(wfs_case, wfs_control, alternative = "two.sided")
wfs_test
skim(wfs_case)
skim(wfs_control)

##Regensburger Wortflüssigkeit Category Change
wfc_case <- npdata[npdata$case == 1, "rbwf.cat"]
wfc_control <- npdata[npdata$case == 0, "rbwf.cat"]

wfc_test <- wilcox.test(wfc_case, wfc_control, alternative = "two.sided")
wfc_test
skim(wfc_case)
skim(wfc_control)

##Tower of London
tol_case <- npdata[npdata$case == 1, "tol.corr"]
tol_control <- npdata[npdata$case == 0, "tol.corr"]

tol_test <- wilcox.test(tol_case, tol_control, alternative = "two.sided")
tol_test
skim(tol_case)
skim(tol_control)

##Digitspan Forward
digf_case <- npdata[npdata$case == 1, "wms.dig.forw"]
digf_control <- npdata[npdata$case == 0, "wms.dig.forw"]

digf_test <- wilcox.test(digf_case, digf_control, alternative = "two.sided")
digf_test
skim(digf_case)
skim(digf_control)

##Digitspan Backwards
digb_case <- npdata[npdata$case == 1, "wms.dig.back"]
digb_control <- npdata[npdata$case == 0, "wms.dig.back"]

digb_test <- wilcox.test(digb_case, digb_control, alternative = "two.sided")
digf_test
skim(digb_case)
skim(digb_control)

##Logical memory immediate recall
logi_case <- npdata[npdata$case == 1, "wms.log.imm"]
logi_control <- npdata[npdata$case == 0, "wms.log.imm"]

logi_test <- wilcox.test(logi_case, logi_control, alternative = "two.sided")
logi_test
skim(logi_case)
skim(logi_control)

##Logical memory delayed recall
logd_case <- npdata[npdata$case == 1, "wms.log.del"]
logd_control <- npdata[npdata$case == 0, "wms.log.del"]

logd_test <- wilcox.test(logd_case, logd_control, alternative = "two.sided")
logd_test
skim(logd_case)
skim(logd_control)

##WCST correct
wcstcor_case <- npdata[npdata$case == 1, "wcst.corr"]
wcstcor_control <- npdata[npdata$case == 0, "wcst.corr"]

wcstcor_test <- wilcox.test(wcstcor_case, wcstcor_control, alternative = "two.sided")
wcstcor_test
skim(wcstcor_case)
skim(wcstcor_control)

##WCST errors
wcsterr_case <- npdata[npdata$case == 1, "wcst.err"]
wcsterr_control <- npdata[npdata$case == 0, "wcst.err"]

wcsterr_test <- wilcox.test(wcsterr_case, wcsterr_control, alternative = "two.sided")
wcsterr_test
skim(wcsterr_case)
skim(wcsterr_control)

##WCST perseverative responses
wcstprep_case <- npdata[npdata$case == 1, "wcst.pers.resp"]
wcstprep_control <- npdata[npdata$case == 0, "wcst.pers.resp"]

wcstprep_test <- wilcox.test(wcstprep_case, wcstprep_control, alternative = "two.sided")
wcstprep_test
skim(wcstprep_case)
skim(wcstprep_control)

##WCST perseverative errors
wcstper_case <- npdata[npdata$case == 1, "wcst.pers.err"]
wcstper_control <- npdata[npdata$case == 0, "wcst.pers.err"]

wcstper_test <- wilcox.test(wcstper_case, wcstper_control, alternative = "two.sided")
wcstper_test
skim(wcstper_case)
skim(wcstper_control)

##WCST non-perseverative errors
wcstnper_case <- npdata[npdata$case == 1, "wcst.nonpers.err"]
wcstnper_control <- npdata[npdata$case == 0, "wcst.nonpers.err"]

wcstnper_test <- wilcox.test(wcstnper_case, wcstnper_control, alternative = "two.sided")
wcstnper_test
skim(wcstnper_case)
skim(wcstnper_control)

##WCST conceptual level responses
wcstcrep_case <- npdata[npdata$case == 1, "wcst.conc.resp"]
wcstcrep_control <- npdata[npdata$case == 0, "wcst.conc.resp"]

wcstcrep_test <- wilcox.test(wcstcrep_case, wcstcrep_control, alternative = "two.sided")
wcstcrep_test
skim(wcstcrep_case)
skim(wcstcrep_control)

##WCST categories completed
wcstcc_case <- npdata[npdata$case == 1, "wcst.cat.comp"]
wcstcc_control <- npdata[npdata$case == 0, "wcst.cat.comp"]

wcstcc_test <- wilcox.test(wcstcc_case, wcstcc_control, alternative = "two.sided")
wcstcc_test
skim(wcstcc_case)
skim(wcstcc_control)

##WCST trails to complete first cat.
wcstfc_case <- npdata[npdata$case == 1, "wcst.trial.first.cat"]
wcstfc_control <- npdata[npdata$case == 0, "wcst.trial.first.cat"]

wcstfc_test <- wilcox.test(wcstfc_case, wcstfc_control, alternative = "two.sided")
wcstfc_test
skim(wcstfc_case)
skim(wcstfc_control)

##WCST failure to maintain set
wcstfms_case <- npdata[npdata$case == 1, "wcst.fail.set"]
wcstfms_control <- npdata[npdata$case == 0, "wcst.fail.set"]

wcstfms_test <- wilcox.test(wcstfms_case, wcstfms_control, alternative = "two.sided")
wcstfms_test
skim(wcstfms_case)
skim(wcstfms_control)

##WCST failure to maintain set
wcstll_case <- npdata[npdata$case == 1, "wcst.learn.total"]
wcstll_control <- npdata[npdata$case == 0, "wcst.learn.total"]

wcstll_test <- wilcox.test(wcstll_case, wcstll_control, alternative = "two.sided")
wcstll_test
skim(wcstll_case)
skim(wcstll_control)

##TAP tonische altertness
tapt_case <- npdata[npdata$case == 1, "tap.alert.rea.ow"]
tapt_control <- npdata[npdata$case == 0, "tap.alert.rea.ow"]

tapt_test <- wilcox.test(tapt_case, tapt_control, alternative = "two.sided")
tapt_test
skim(tapt_case)
skim(tapt_control)

##TAP phasische alertness
tapp_case <- npdata[npdata$case == 1, "tap.alert.rea.mw"]
tapp_control <- npdata[npdata$case == 0, "tap.alert.rea.mw"]

tapp_test <- wilcox.test(tapp_case, tapp_control, alternative = "two.sided")
tapp_test
skim(tapp_case)
skim(tapp_control)

##TAP divided attention auditory
tapa_case <- npdata[npdata$case == 1, "tap.divi.aud.mean"]
tapa_control <- npdata[npdata$case == 0, "tap.divi.aud.mean"]

tapa_test <- wilcox.test(tapa_case, tapa_control, alternative = "two.sided")
tapa_test
skim(tapa_case)
skim(tapa_control)

##TAP divided attention visual
tapv_case <- npdata[npdata$case == 1, "tap.divi.vis.mean"]
tapv_control <- npdata[npdata$case == 0, "tap.divi.vis.mean"]

tapv_test <- wilcox.test(tapv_case, tapv_control, alternative = "two.sided")
tapv_test
skim(tapv_case)
skim(tapv_control)

##ROCFT Copy
reyc_case <- npdata[npdata$case == 1, "rey.copy"]
reyc_control <- npdata[npdata$case == 0, "rey.copy"]

reyc_test <- wilcox.test(reyc_case, reyc_control, alternative = "two.sided")
reyc_test
skim(reyc_case)
skim(reyc_control)

##ROCFT immediate recall
reyir_case <- npdata[npdata$case == 1, "rey.imm.rec"]
reyir_control <- npdata[npdata$case == 0, "rey.imm.rec"]

reyir_test <- wilcox.test(reyir_case, reyir_control, alternative = "two.sided")
reyir_test
skim(reyir_case)
skim(reyir_control)

##ROCFT delayed recall
reydr_case <- npdata[npdata$case == 1, "rey.del.rec"]
reydr_control <- npdata[npdata$case == 0, "rey.del.rec"]

reydr_test <- wilcox.test(reydr_case, reydr_control, alternative = "two.sided")
reydr_test
skim(reydr_case)
skim(reydr_control)

###FDR correction NPA###

# Create a list to store the test results
test_results_npa <- list(
  digb_test = digb_test,
  digf_test = digf_test,
  digi_test = digi_test,
  logd_test = logd_test,
  logi_test = logi_test,
  reyc_test = reyc_test,
  reydr_test = reydr_test,
  reyir_test = reyir_test,
  rm_test = rm_test,
  stcol_test = stcol_test,
  stin_test = stin_test,
  stwo_test = stwo_test,
  sym_test = sym_test,
  tapa_test = tapa_test,
  tapp_test = tapp_test,
  tapt_test = tapt_test,
  tapv_test = tapv_test,
  tmta_test = tmta_test,
  tmtb_test = tmtb_test,
  tol_test = tol_test,
  wcstcc_test = wcstcc_test,
  wcstcor_test = wcstcor_test,
  wcstcrep_test = wcstcrep_test,
  wcstfc_test = wcstfc_test,
  wcstfms_test = wcstfms_test,
  wcstll_test = wcstll_test,
  wcstnper_test = wcstnper_test,
  wcstper_test = wcstper_test,
  wcstprep_test = wcstprep_test,
  wfc_test = wfc_test,
  wfs_test = wfs_test
)


# Perform FDR adjustment on p-values from the list of test results
adjusted_p_values_npa <- p.adjust(sapply(test_results_npa, function(result) result$p.value), method = "BH")

# Add adjusted p-values to the original test results list
for (i in 1:length(test_results_npa)) {
  test_results_npa[[i]]$adjusted_p_value <- adjusted_p_values_npa[i]
}


# Access adjusted p-values from the list
adjusted_p_value_digb_test <- adjusted_p_values_npa[["digb_test"]]
adjusted_p_value_digf_test <- adjusted_p_values_npa[["digf_test"]]
adjusted_p_value_digi_test <- adjusted_p_values_npa[["digi_test"]]
adjusted_p_value_logd_test <- adjusted_p_values_npa[["logd_test"]]
adjusted_p_value_logi_test <- adjusted_p_values_npa[["logi_test"]]
adjusted_p_value_reyc_test <- adjusted_p_values_npa[["reyc_test"]]
adjusted_p_value_reydr_test <- adjusted_p_values_npa[["reydr_test"]]
adjusted_p_value_reyir_test <- adjusted_p_values_npa[["reyir_test"]]
adjusted_p_value_rm_test <- adjusted_p_values_npa[["rm_test"]]
adjusted_p_value_stcol_test <- adjusted_p_values_npa[["stcol_test"]]
adjusted_p_value_stin_test <- adjusted_p_values_npa[["stin_test"]]
adjusted_p_value_stwo_test <- adjusted_p_values_npa[["stwo_test"]]
adjusted_p_value_sym_test <- adjusted_p_values_npa[["sym_test"]]
adjusted_p_value_tapa_test <- adjusted_p_values_npa[["tapa_test"]]
adjusted_p_value_tapp_test <- adjusted_p_values_npa[["tapp_test"]]
adjusted_p_value_tapt_test <- adjusted_p_values_npa[["tapt_test"]]
adjusted_p_value_tapv_test <- adjusted_p_values_npa[["tapv_test"]]
adjusted_p_value_tmta_test <- adjusted_p_values_npa[["tmta_test"]]
adjusted_p_value_tmtb_test <- adjusted_p_values_npa[["tmtb_test"]]
adjusted_p_value_tol_test <- adjusted_p_values_npa[["tol_test"]]
adjusted_p_value_wcstcc_test <- adjusted_p_values_npa[["wcstcc_test"]]
adjusted_p_value_wcstcor_test <- adjusted_p_values_npa[["wcstcor_test"]]
adjusted_p_value_wcstcrep_test <- adjusted_p_values_npa[["wcstcrep_test"]]
adjusted_p_value_wcstfc_test <- adjusted_p_values_npa[["wcstfc_test"]]
adjusted_p_value_wcstfms_test <- adjusted_p_values_npa[["wcstfms_test"]]
adjusted_p_value_wcstll_test <- adjusted_p_values_npa[["wcstll_test"]]
adjusted_p_value_wcstnper_test <- adjusted_p_values_npa[["wcstnper_test"]]
adjusted_p_value_wcstper_test <- adjusted_p_values_npa[["wcstper_test"]]
adjusted_p_value_wcstprep_test <- adjusted_p_values_npa[["wcstprep_test"]]
adjusted_p_value_wfc_test <- adjusted_p_values_npa[["wfc_test"]]
adjusted_p_value_wfs_test <- adjusted_p_values_npa[["wfs_test"]]


###Plotting###

# Register Helvetica Neue font
font_add("Helvetica Neue", regular = "path/to/helvetica_neue_regular.ttf", bold = "path/to/helvetica_neue_bold.ttf")

# Set the default font to "Helvetica Neue"
theme_set(theme_minimal(base_family = "Helvetica Neue"))

# Read the data from CSV file
hppq <- read.csv("/Users/georgleistenschneider/Documents/Uni/Masterarbeit/5. Auswertung/HPPD_Raw_SymptQuesh.csv")

# Extract the mean amount and mean duration values from row 9
mean_amount <- as.numeric(hppq[9, seq(1, ncol(hppq), by = 2)])
mean_duration <- as.numeric(hppq[9, seq(2, ncol(hppq), by = 2)])
amount <- as.numeric(hppq[10, seq(1,ncol(hppq), by = 2)])

# Create a new dataframe for plotting
df_plot <- data.frame(Symptom = c("Halos", "Movement", "Stills", "Trails", "Colours", "Patterns", "Things", "Oscillations", "Grids", "Geometric Hallucinations", "Movement in Periphery", "Flashes", "Micropsia", "Macropsia", "Visual Snow", "Afterimages", "Floaters", "Visual Acuity", "Stardust", "Visual Hypersensitivity"),
                      Mean_Amount = mean_amount,
                      Mean_Duration = mean_duration,
                      Amount = amount)

# Melt the dataframe to long format
df_plot_long <- tidyr::pivot_longer(df_plot, cols = c(Mean_Amount, Mean_Duration),
                                    names_to = "Measure", values_to = "Value")

#Reorder to original
df_plot_long <- df_plot_long %>%
  arrange(desc(Amount))

# Convert Symptom column to factor with original levels
df_plot_long$Symptom <- factor(df_plot_long$Symptom, levels = unique(df_plot_long$Symptom))

color_values <- c("#BB2649", "#26BB98")

plot <- ggplot(df_plot_long, aes(x = Symptom, y = Value, fill = Measure, alpha = Amount)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = color_values) +  # Set the color values
  labs(x = "Symptom", y = "Mean", fill = "Measure") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 315, hjust = 0)) +
  guides(fill = guide_legend(title = "Measure"),  # Add a legend for fill
         alpha = guide_legend(title = "Amount"))  # Add a legend for alpha
plot


##plot the p-values

# Create a data frame to store the p-values and corresponding object names
df <- data.frame(Object = c("Digit-Symbol", "Symbol Search", "Digit Span, forwards", "Digit Span, backwards", "Logical Memory - immediate recall", 
                            "Logical Memory - delayed recall", "ROCFT copy","ROCFT immediate recall",  "ROCFT delayed recall", "Stroop Test Word", 
                            "Stroop Test Colour", "Stroop Test Interference", "TMT-A", "TMT-B", "WCST correct responses", "WCST perseverative response", 
                            "WCST non-perseverative error", "WCST perseverative error", "WCST conceptual level responses", "WCST trails to complete first cat.", 
                            "WCST categories completed", "WCST failure to maintain set", "WCST learning to lean", "TAP Alertness tonic", "TAP Alertness phasic",
                            "TAP Divided Attention auditory", "TAP Devided Attention visual", "ToL", "RWT category", "RWT category switch","RMET"),
                 
                 P_Value = c(adjusted_p_value_digi_test, adjusted_p_value_sym_test, adjusted_p_value_digf_test, adjusted_p_value_digb_test,  
                              adjusted_p_value_logi_test, adjusted_p_value_logd_test, adjusted_p_value_reyc_test,
                              adjusted_p_value_reyir_test, adjusted_p_value_reydr_test, adjusted_p_value_stcol_test, 
                             adjusted_p_value_stwo_test, adjusted_p_value_stin_test, adjusted_p_value_tmta_test,
                             adjusted_p_value_tmtb_test, adjusted_p_value_wcstcor_test, adjusted_p_value_wcstprep_test,
                             adjusted_p_value_wcstnper_test, adjusted_p_value_wcstper_test, adjusted_p_value_wcstcrep_test,
                             adjusted_p_value_wcstfc_test, adjusted_p_value_wcstcc_test, adjusted_p_value_wcstfms_test,
                             adjusted_p_value_wcstll_test, adjusted_p_value_tapt_test, adjusted_p_value_tapp_test,
                             adjusted_p_value_tapa_test, adjusted_p_value_tapv_test, adjusted_p_value_tol_test,
                             adjusted_p_value_wfs_test, adjusted_p_value_wfc_test, adjusted_p_value_rm_test))

# Define the specific order of the items in the Object column
specific_order <- c("Digit-Symbol", "Symbol Search", "Digit Span, forwards", "Digit Span, backwards", 
                    "Logical Memory - immediate recall", "Logical Memory - delayed recall", "ROCFT copy",
                    "ROCFT immediate recall", "ROCFT delayed recall", "Stroop Test Word", "Stroop Test Colour",
                    "Stroop Test Interference", "TMT-A", "TMT-B", "WCST correct responses", "WCST perseverative response",
                    "WCST non-perseverative error", "WCST perseverative error", "WCST conceptual level responses",
                    "WCST trails to complete first cat.", "WCST categories completed", "WCST failure to maintain set",
                    "WCST learning to lean", "TAP Alertness tonic", "TAP Alertness phasic", "TAP Divided Attention auditory",
                    "TAP Devided Attention visual", "ToL", "RWT category", "RWT category switch",
                    "RMET")

# Convert the Object column to a factor with the specific order
df$Object <- factor(df$Object, levels = specific_order)

# Create a bar plot of the p-values
 p <- ggplot(df, aes(x = Object, y = P_Value)) +
       geom_bar(stat = "identity", fill = "#26BB98") +
       labs(title = "Adjusted p-values",
                      x = "Test",
                      y = expression(italic("p")~"-value")) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 315, hjust = 0, vjust = 1),
             plot.margin = margin(t = 50, r = 100, b = 50, l = 100)) +
       geom_hline(yintercept = 0.05, linetype = "dashed", color = "#BB2649")
 
p


