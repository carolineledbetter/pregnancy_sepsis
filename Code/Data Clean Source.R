###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 7/28/17
###########################################

### This file imports the NIS data sets and cleans them for analysis. ###



################################################################################
# linking variable for inpatient core file to hospital weights file = hosp.nis
# linking variable for inpatient core file to severity measures = key.nis
# key.nis also links Diagnosis and procedure groups file.

# cluster (id) variable is hosp.nis
# strata variable is nis.stra
# weight variable is discwt

# create character vectors of diagnosis and procedure variables
attach(NISCore)
diag_vars <- ls(pattern = '^dx\\d{1,2}$', NISCore)
proc_vars <- ls(pattern = '^pr\\d{1,2}$', NISCore)
detach(NISCore)

################################################################################
# Pregnancy related hospitalization 
################################################################################

################################################################################
# Abortion
NISCore$Abortion <- apply(NISCore[, c(proc_vars)], 1, 
                          function(x) any(x %in% c('6901','6951','750'))) | 
  apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^V27[1,4,7]|^63[2,4-9]|^6564', x)))


table(NISCore$Abortion, useNA = 'ifany')

################################################################################
# Delivery
NISCore$Delivery <- !NISCore$Abortion & 
  (apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^6[4-7]\\d{2}[1-2]', x))) | 
  apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^6[4-7]\\d{2}0', x))) & 
  (apply(NISCore[, c(proc_vars)], 1, 
         function(x) any(grepl('^07[2-4]', x))) | 
     apply(NISCore[, c(diag_vars)], 1, 
           function(x) any(grepl('^V27[2,3,5,6]', x)))) | 
  apply(NISCore[, diag_vars], 1, function(x) any(x == '650'))) 

table(NISCore$Delivery, useNA = 'always')

################################################################################
# Postpartum
NISCore$Postpartum <- !NISCore$Delivery & !NISCore$Abortion & 
  apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^6[4-7]\\d{2}4', x))) 


table(NISCore$Postpartum, useNA = 'always')

################################################################################
# Antepartum
NISCore$Antepartum <- !NISCore$Delivery & !NISCore$Abortion & 
  apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^6[4-7]\\d{2}3', x)))

table(NISCore$Antepartum, useNA = 'always')

################################################################################
# Unknown
NISCore$Unknown <-  !NISCore$Delivery & !NISCore$Abortion & 
  !NISCore$Antepartum & !NISCore$Postpartum & 
  (apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(grepl('^6[4-7]\\d{2}0', x))) & ! 
  (apply(NISCore[, c(proc_vars)], 1, 
         function(x) any(grepl('^07[2-4]', x))) | 
     apply(NISCore[, c(diag_vars)], 1, 
           function(x) any(grepl('^V27[2,3,5,6]', x)))) |
  apply(NISCore[, c(diag_vars)], 1, 
        function(x) any(x == 'V279')))

table(NISCore$Unknown, useNA = 'always')
################################################################################
# Pregnancy
Preg_Vars <- c('Abortion', 'Antepartum', 'Delivery', 'Postpartum', 'Unknown')

NISCore$PregDiag <- apply(NISCore[, Preg_Vars], 1, sum)

table(NISCore$PregDiag, useNA = 'always')

NISCore$Pregnant <- factor(NA, levels = c('No', 'Yes'))

NISCore$Pregnant[NISCore$PregDiag == 0] <- 'No'
NISCore$Pregnant[NISCore$PregDiag >= 1] <- 'Yes'
table(NISCore$Pregnant)

################################################################################
# Subset out not pregnant observations
PregSubset <- NISCore[NISCore$Pregnant == 'Yes',]
PregSubset <- droplevels(PregSubset)
dx_codes <- unlist(lapply(PregSubset[, diag_vars], levels))
names(dx_codes) <- NULL
dx_codes <- sort(dx_codes[!duplicated(dx_codes)])


################################################################################
# Infection
################################################################################

################################################################################
# Puerperal Infection
PregSubset$Puerperal <- apply(PregSubset[, c(diag_vars)], 1, 
                           function(x) any(grepl('^670', x)))

################################################################################
# Chorioamnionitis
PregSubset$Chorioamnionitis <- apply(PregSubset[, c(diag_vars)], 1, 
                                  function(x) any(grepl('^6584|^7627', x)))

################################################################################
# Endometritis
PregSubset$Endometritis <- apply(PregSubset[, c(diag_vars)], 1, 
                              function(x) any(grepl('^615|^09816', x)))

################################################################################
# Septic Abortion
PregSubset$SepAbortion <- apply(PregSubset[, c(diag_vars)], 1, 
                             function(x) any(grepl('^63[4-9]0', x)))

################################################################################
# Non-Obstetric 

################################################################################
# Infectious and Parasitic Diseases
infect_dx <- grep(paste0('^0[0-3][1-5]|^00[7,9]|^008[0-5]|^01[0,6-8]', 
                         '|^02[0,6,7]|^03[0,6,7,9]|^04[0,1]|^05[3,4]|^07[4,9]', 
                         '|^09[1,3,4,5,7,8]|^`10[0,1]|^11[2,4-8]|^488'), 
                  dx_codes, value = T)

PregSubset$InfParDis <- apply(PregSubset[, c(diag_vars)], 1, 
                           function(x) any(x %in% infect_dx))
NumInfParDis <- sapply(infect_dx, function(x){
  sum(apply(PregSubset[, c(diag_vars)], 1, function(y) {
    any(y == x)
    }), na.rm = T)
})
names(NumInfParDis)
################################################################################
# Respiratory
resp_dx <- grep(paste0('^46[1-5]|^475|^4782[1-2, 4, 9]|^47871|^48[0-2, 4-6]', 
                       '|^483[0-8]|^487[0-1]|^5070|^51[0, 3]|^51901', 
                       '|^9973[1, 9]|^V1261'), 
                dx_codes, value = T)
PregSubset$Respiratory <- apply(PregSubset[, c(diag_vars)], 1, 
                             function(x) any(x %in% resp_dx))

################################################################################
# Genitourinary
PregSubset$Genitourinary <- apply(PregSubset[, c(diag_vars)], 1, 
                               function(x) any(grepl(paste0('^59[0, 7]', 
                                                            '|^5990', 
                                                            '|^61[4, 6]', 
                                                            '|^6466', 
                                                            '|^V1302'), 
                                                     x)))

################################################################################
# Digestive
digest_dx <- grep(paste0('^522[5, 7]|^5264|^52[7-8]3|^54[0-2]', 
                         '|^562[0-1][1, 3]|^56[6-7]|^5695|^56961|^56983', 
                         '|^572[0-1]|^574[0, 3, 6, 8]|^5750|^5751[0, 2]', 
                         '|^5754|^5761'), dx_codes, value = T)
PregSubset$Digestive <- apply(PregSubset[, c(diag_vars)], 1, 
                           function(x) any( x %in% digest_dx))

################################################################################
# Wound, Skin, or Soft Tissue Infection
PregSubset$Wound <- apply(PregSubset[, diag_vars], 1, 
                       function(x) any(grepl('^68[0-6]|^72886|^7854|^9583', 
                                             x)))
################################################################################
# Bacteremia
PregSubset$Bacteremia <- apply(PregSubset[, diag_vars], 1, 
                            function(x) any(x == '7907'))

################################################################################
# Circulatory
PregSubset$Circulatory <- apply(PregSubset[, diag_vars], 1, 
                             function(x) any(grepl('^42[0-2]', x)))

################################################################################
# Nervous
PregSubset$Nervous <- apply(PregSubset[, diag_vars], 1, 
                         function(x) any(grepl(paste0('^04[7, 9]|^32[0-5]', 
                                                      '|^3600|^3760|^38014', 
                                                      '|^383|^V1242'), 
                                               x)))

################################################################################
# Central-Line Associated Bloodstream Infection (CLABSI)
PregSubset$CLABSI <- apply(PregSubset[, diag_vars], 1, 
                        function(x) any(x == '99931'))

################################################################################
# Mastitis
PregSubset$Mastitis <- apply(PregSubset[, diag_vars], 1, 
                          function(x) any(grepl('^6110|^675[1-2, 9]', 
                                                x)))
################################################################################
# Musculoskeletal
PregSubset$Musculoskeletal <- apply(PregSubset[, diag_vars], 1, 
                                 function(x) any(grepl('^711[0, 9]|^730', 
                                                       x)))

################################################################################
# Non - Other sources of infection 
Identified_Sources <- c("Puerperal", "Chorioamnionitis", "Endometritis", 
                        "SepAbortion", "InfParDis", "Respiratory", 
                        "Genitourinary", 'Digestive', "Wound", "Bacteremia", 
                        "Circulatory", "Nervous", "CLABSI", "Mastitis", 
                        "Musculoskeletal")
PregSubset$NumSrcInf <- as.factor(apply(PregSubset[, Identified_Sources], 
                                     1, sum))

################################################################################
# Other
PregSubset$Other <- PregSubset$NumSrcInf == 0 & 
  apply(PregSubset[, diag_vars], 1, 
        function(x) any(grepl(paste0('^038|^6479|^6593|^78552|^9959[1, 2]', 
                                     '|^9966', 
                                     '|^9985|^99939'), 
                              x)))

levels(PregSubset$NumSrcInf)[levels(PregSubset$NumSrcInf) == '0'] <- 'Unknown'
################################################################################
# Infection
Inf_Vars <- c("Puerperal", "Chorioamnionitis", "Endometritis", "SepAbortion", 
              "InfParDis", "Respiratory", "Genitourinary", 'Digestive', 
              "Wound", "Bacteremia", "Circulatory", "Nervous", 
              "CLABSI", "Mastitis", "Musculoskeletal", 'Other')

PregSubset$InfDiag <- apply(PregSubset[, Inf_Vars], 1, sum)

PregSubset$Infection <- NA
PregSubset$Infection[PregSubset$InfDiag == 0] <- F
PregSubset$Infection[PregSubset$InfDiag >= 1] <- T

table(PregSubset$Infection, PregSubset$Pregnant, useNA = 'always')

StudySubset <- PregSubset[PregSubset$Infection == T,]

################################################################################
# Organ Failure 
################################################################################

################################################################################
# Respiratory
StudySubset$RespFail <- apply(StudySubset[, diag_vars], 1, 
                          function(x) any(grepl(paste0('^518[5, 8]', 
                                                       '|^7860[3, 9]', 
                                                       '|^7991'), 
                                                x)))

################################################################################
# Cardiovascular
CardioFail_dx <- grep(paste0('^410|^4150|^4151[1, 9]|^425|^425|^4275', 
                             '|^428[2-4][1, 3]|^458[0, 8, 9]|^6691[0-4]', 
                             '|^6694|^673[2-3][0-4]|^6745[0-4]|^7855|^9971', 
                             '|^9980'), dx_codes, value = T)
StudySubset$CardioFail <- apply(StudySubset[, diag_vars], 1, 
                            function(x) any(x %in% CardioFail_dx))

################################################################################
# Renal
StudySubset$RenalFail <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl(paste0('^584[5-9]|^586', 
                                                        '|^6393', 
                                                        '|^6693[0, 2, 4]'), 
                                                 x)))

################################################################################
# Hepatic
StudySubset$HepFail <- apply(StudySubset[, diag_vars], 1, 
                         function(x) any(grepl(paste0('^570|^572[2, 4]', 
                                                      '|^5734', 
                                                      '|^6467[0, 1, 3]', 
                                                      '|^6748'), 
                                               x)))
################################################################################
# Hematological/Coagulation
StudySubset$BloodFail <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl(paste0('^286[6-7, 9]', 
                                                        '|^2874', 
                                                        '|^6663[0, 2, 4]'), 
                                                 x)))

################################################################################
# Neurological
NeuroFail_dx <- grep(paste0('^2930|^325|^3453|^3466|^3481|^3483[0-1, 9]|^3485', 
                            '|^43[0-4, 6]|^437[1, 6]|^7800[1, 3]|^9970[1-2]'), 
                     dx_codes, value = T)
StudySubset$NeuroFail <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(x %in% NeuroFail_dx))

################################################################################
# Organ Failure
################################################################################
Organ_Vars <- c('RespFail', 'CardioFail', 'RenalFail', 'HepFail', 'BloodFail', 
                'NeuroFail')

StudySubset$OrgDiag <- apply(StudySubset[, Organ_Vars], 1, sum)

StudySubset$OrganFailure <- NA
StudySubset$OrganFailure[StudySubset$OrgDiag == 0] <- F
StudySubset$OrganFailure[StudySubset$OrgDiag >= 1] <- T

table(StudySubset$OrganFailure, useNA = 'always')


################################################################################
# Sepsis
################################################################################

StudySubset$Sepsis <- apply(StudySubset[, diag_vars], 1, 
                        function(x) any(grepl('^038|^6593|^99591', 
                                              x)))

StudySubset$SevSepsis <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(x == '99592')) |
  StudySubset$OrganFailure & (StudySubset$Sepsis | 
                            StudySubset$Infection)

StudySubset$SepShock <- apply(StudySubset[, diag_vars], 1, 
                          function(x) any(x == '78552')) |
  apply(StudySubset[, diag_vars], 1, 
        function(x) any(grepl('^63[4-9]5|^6691|^78550|^9980', x))) &
  (StudySubset$Infection | StudySubset$Sepsis | StudySubset$SevSepsis)


################################################################################
# Set categories to mutually exclusive (an obs should only be yes in the highest
# level)
################################################################################
StudySubset$Outcome <- factor(NA, levels = c('Pregnancy + Infection', 
                                          'Pregnancy + Sepsis', 
                                          'Pregnancy + Severe Sepsis', 
                                          'Pregnancy + Septic Shock'))
StudySubset$Outcome[StudySubset$Infection] <- 'Pregnancy + Infection'
StudySubset$Outcome[StudySubset$Sepsis] <-  'Pregnancy + Sepsis'
StudySubset$Outcome[StudySubset$SevSepsis] <- 'Pregnancy + Severe Sepsis'
StudySubset$Outcome[StudySubset$SepShock] <- 'Pregnancy + Septic Shock'
StudySubset$Outcome[StudySubset$Pregnant != 'Yes'] <- NA

table(StudySubset$Outcome, StudySubset$Pregnant, useNA = 'always')

StudySubset$Subset <- F
StudySubset$Subset[!is.na(StudySubset$Outcome)] <- T

table(StudySubset$Outcome, StudySubset$Subset, useNA = 'always')


################################################################################
# Demographics
################################################################################

StudySubset$race <- factor(StudySubset$race, levels = 1:6, 
                       labels = c('White', 'Black', 'Hispanic', 
                                  'Asian or Pacific Islander', 
                                  'Native American', 
                                  'Other'))

StudySubset$AgeCat <- cut(StudySubset$age, c(0, 25, 35, Inf), 
                      labels = c('LE 25', '25-34', 
                                 '\\(geq\\)35'))
StudySubset$Insurance <- factor(StudySubset$pay1, levels = 1:6,
                            labels = c('Medicare', 'Medicade', 'Private', 
                                       'Self-Pay', 'No Charge', 'Other'))
StudySubset$MedianInc <- factor(StudySubset$zipinc, levels = 1:4, 
                            labels = c('<$25,000', '$25,000 - $34,999',
                                       '$35,000 - $44,999', 
                                       '\\(geq\\) $45,000'))

################################################################################
# Outcomes
################################################################################

StudySubset$discharge <- factor(StudySubset$disp.pt, 
                            levels = c(1:2, 5:7, 20, 99), 
                            labels = c('Routine', 'Txfr to Short Term Hosp', 
                                       "Txfr Other", 'Home Health Care', 
                                       'Left against medical advice', 
                                       'Died', 'Alive, destination unknown')
)
StudySubset$LOS_Cat <- factor(NA, levels = c('<3 days', '3-7 days', '>7 days'))
StudySubset$LOS_Cat[StudySubset$los < 3] <- '<3 days'
StudySubset$LOS_Cat[StudySubset$los >= 3 & StudySubset$los <=7] <- '3-7 days'
StudySubset$LOS_Cat[StudySubset$los > 7] <- '>7 days'
table(StudySubset$LOS_Cat)

StudySubset$died <- as.logical(StudySubset$died)

################################################################################
# Mode of Delivery
################################################################################

################################################################################
# Vaginal
StudySubset$Vaginal <- StudySubset$Delivery & 
  (apply(StudySubset[, diag_vars], 1, 
        function(x) any(grepl('^650', x))) | 
     apply(StudySubset[, proc_vars], 1, 
           function(x) any(grepl('^72|^73[2, 5-6, 8-9]', x))))

################################################################################
# Assisted Vaginal
StudySubset$AssistVag <- StudySubset$Delivery & 
  apply(StudySubset[, proc_vars], 1, 
        function(x) any(grepl('^72|^733', x))) 
################################################################################
# Cesarean
StudySubset$Cesarean <- StudySubset$Delivery & 
  (apply(StudySubset[, proc_vars], 1, 
         function(x) any(grepl('^74', x))) |
     apply(StudySubset[, diag_vars], 1, 
           function(x) any(grepl('^6697|^7634', x)))) 

################################################################################
# Unknown Mode of Delivery

StudySubset$UnkMode <- StudySubset$Delivery & !StudySubset$Vaginal & 
  !StudySubset$AssistVag & !StudySubset$Cesarean


Deliv_Vars <- c('Vaginal', 'AssistVag', 'Cesarean', 'UnkMode')
################################################################################
# Obstetric Risk Factors
################################################################################

################################################################################
# Antepartum
################################################################################

################################################################################
# Cerclage
StudySubset$Cerclage <- apply(StudySubset[, proc_vars], 1, 
                          function(x) any(grepl('^675[1, 9]|^6996', x)))

################################################################################
# Multiple Gestation
StudySubset$MultiGest <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl('^651|^6526|^V27[2-7]', 
                                                 x)))

################################################################################
# Gestational Diabetes
StudySubset$GestDiab <- apply(StudySubset[, diag_vars], 1, 
                          function(x) any(grepl('^6488', x)))

################################################################################
# Gestational Hypertension
StudySubset$GestHyper <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl('^6423', x)))

################################################################################
# Pre-eclampsia
StudySubset$Preeclampsia <- apply(StudySubset[, diag_vars], 1, 
                              function(x) any(grepl('^642[4-5, 7]', x)))

################################################################################
# Eclampsia
StudySubset$Eclampsia <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl('^6426', x)))

################################################################################
# Placental abruption
StudySubset$Abruption <- apply(StudySubset[, diag_vars], 1, 
                           function(x) any(grepl('^6412|^7621', x)))

################################################################################
# Antepartum Hemorrhage
StudySubset$AnteHemorrhage <- apply(StudySubset[, diag_vars], 1, 
                                function(x) any(grepl('^640|^641[1, 3, 8-9]', 
                                                      x)))

################################################################################
# Intrapartum
################################################################################

################################################################################
# Premature Rupture of Membranes
StudySubset$PreRuptMem <- apply(StudySubset[, diag_vars], 1, 
                            function(x) any(grepl('^6581|^7611', 
                                                  x)))

################################################################################
# Preterm Delivery
StudySubset$PretermDel <- apply(StudySubset[, diag_vars], 1, 
                            function(x) any(grepl('^6442', x))) 

################################################################################
# Preterm labor excluding Preterm Delivery
StudySubset$PretermLab <- !StudySubset$PretermDel & 
  apply(StudySubset[, diag_vars], 1, function(x) any(grepl('^644', x))) 

################################################################################
# Induction of labor
StudySubset$Induced <- apply(StudySubset[, diag_vars], 1, 
                         function(x) any(grepl('^659[0-1]', x))) |
  apply(StudySubset[, proc_vars], 1, 
        function(x) any(grepl('^7301|73[1, 4]', x)))

################################################################################
# Prolonged rupture of membranes/prolonged labor
StudySubset$Prolonged <-  apply(StudySubset[, diag_vars], 1, 
                            function(x) any(grepl('^658[2-3]|^6621', x))) 

################################################################################
# Artifical rupture of membranes
StudySubset$Artificial <-  apply(StudySubset[, diag_vars], 1, 
                             function(x) any(grepl('^6583', x))) |
  apply(StudySubset[, proc_vars], 1, 
        function(x) any(grepl('^730', x)))

################################################################################
# Stillbirth
StudySubset$Stillbirth <-  apply(StudySubset[, diag_vars], 1, 
                             function(x) 
                               any(grepl('^768[0-1]|^V27[1, 3-4, 6-7]', x)))

################################################################################
# Postpartum
################################################################################

################################################################################
# Postpartum hemorrhage
StudySubset$PostHemorrage <- apply(StudySubset[, diag_vars], 1, 
                               function(x) any(grepl('^666', x)))

################################################################################
# Retained Products of Conception
StudySubset$Retained <- apply(StudySubset[, diag_vars], 1, 
                          function(x) 
                            any(grepl('^632|^637[0-9]1|^666[0, 2]|^667', 
                                      x)))
Complic_Vars <- c("Cerclage", "MultiGest", "GestDiab", "GestHyper", 
                  "Preeclampsia", "Eclampsia", "Abruption", "AnteHemorrhage", 
                  "PreRuptMem", "PretermLab", "PretermDel", "Induced", 
                  "Prolonged", "Artificial", "Stillbirth", "PostHemorrage", 
                  "Retained")
################################################################################
# Intervention
################################################################################

################################################################################
# Hemodialysis
StudySubset$Hemodialysis <- apply(StudySubset[, proc_vars], 1, 
                              function(x) any(grepl('^3995|^9978', x)))

################################################################################
# Positive pressure ventilation
StudySubset$PosPresVent <- apply(StudySubset[, proc_vars], 1, 
                             function(x) any(grepl('^9390|^9604|9605|967', 
                                                   x)))

################################################################################
# Central line placement
StudySubset$CentLine <- apply(StudySubset[, proc_vars], 1, 
                          function(x) any(grepl('^389[3, 5]', x)))

################################################################################
# Invasive hemodynamic monitoring
StudySubset$Hemodynamic <- apply(StudySubset[, proc_vars], 1, 
                             function(x) any(grepl('^896[0-4, 7, 8]', x)))

################################################################################
# Blood transfusion
StudySubset$Transfusion <- apply(StudySubset[, proc_vars], 1, 
                             function(x) any(grepl('^990', x)))

################################################################################
# Hysterectomy
StudySubset$Hysterectomy <- apply(StudySubset[, proc_vars], 1, 
                              function(x) any(grepl('^68[3-9]', x)))

################################################################################
# Dilation and curettage
StudySubset$dilation <- apply(StudySubset[, proc_vars], 1, 
                                  function(x) any(grepl('^690[1-2, 9]', x)))


################################################################################
# Surgical Intervention (excluding obstetrical procedures, eg Cesarean Section)
StudySubset$Surgical <- apply(StudySubset[, proc_vars], 1, 
                          function(x) 
                            any(grepl(paste0('^00[3-4, 6-8]|^0[1-9]', 
                                             '|^[1-5][0-9]|^6[5-9]',
                                             '|^7[0-1, 6-9]|^8[0-6]'), 
                                      x)))

Intervention_Vars <- c("Hemodialysis", "PosPresVent", "CentLine", "Hemodynamic", 
                       "Transfusion", "Hysterectomy", "Surgical")


################################################################################
# Set all added vars to NA in NISCore Data set and put Data back together 
# because of complex survey designs

miss_preg <- setdiff(names(StudySubset),names(PregSubset))
PregSubset <- PregSubset[PregSubset$Infection != T,]
PregSubset[, miss_preg] <- NA

PregSubset <- rbind(StudySubset, PregSubset)
table(PregSubset$Infection, PregSubset$Pregnant)

table(PregSubset$Subset)

miss_nis <- setdiff(names(PregSubset), names(NISCore))
NISCore <- NISCore[NISCore$Pregnant == 'No',]

NISCore[, miss_nis] <- NA
NISCore <- rbind(PregSubset, NISCore)

#check
setequal(NISCore$key.nis, NISSeverity$key.nis)
table(NISCore$Subset)
# checks


################################################################################
# Collapse outcome (combine infection/sepsis + severe sepsis septic shock)
NISCore$Outcome3 <- NISCore$Outcome2 <- NISCore$Outcome
levels(NISCore$Outcome2) <- c(rep('Infection/ Sepsis', 2), 
                              'Severe Sepsis', 'Septic Shock')
levels(NISCore$Outcome3) <- c(rep('Infection/ Sepsis', 2), 
                              rep('Severe Sepsis/ Septic Shock'), 2)



################################################################################
# Hospital Characterstics
################################################################################

DeliveryVolume <- aggregate(Delivery ~ hosp.nis, data = NISCore, sum)
names(DeliveryVolume)[2] <- 'NumDeliveries'
NISHosp <- merge(NISHosp, DeliveryVolume, by = 'hosp.nis', all = T)

NISHosp$h.contrl <- factor(NISHosp$h.contrl, 
                           levels = 1:3,
                           labels = c('Government', 'Private, non-profit',
                                      'Private, for-profit'))

NISHosp$region <- factor(NISHosp$region, 
                         levels = 1:4, 
                         labels = c('Northeast', 'Midwest', 'South', 
                                    'West'))

NISHosp$bedsize <- factor(NISHosp$bedsize, 
                          levels = 1:3, 
                          labels = c('Small', 'Medium', 'Large'))

NISHosp$locteach <- factor(NISHosp$locteach, 
                           levels = 1:3, 
                           labels = c('Rural', 'Urban, non-teaching', 
                                      'Urban, teaching'))


################################################################################
# Comorbidity
################################################################################

Analysis <- merge(NISCore, NISSeverity[-1], by = 'key.nis')
Analysis <- merge(Analysis, NISHosp[c(1, 3, 5:7, 9:13, 15)], by = 'hosp.nis', 
                  all.x = T)

Analysis$Subset[is.na(Analysis$Subset)] <- F
Analysis_Subset <- Analysis[Analysis$Subset == T, ]

################################################################################
# Hypertension
Analysis_Subset$Hypertension <- Analysis_Subset$cm.htn.c | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^642[0-2, 7, 9]', x)))

################################################################################
# Congestive Heart Failure
Analysis_Subset$CHF <- as.logical(Analysis_Subset$cm.chf)

################################################################################
# Other Cardiovascular Disease
Analysis_Subset$OtherCardio <- Analysis_Subset$cm.peri | Analysis_Subset$cm.pulm | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6486', x)))  

################################################################################
# Chronic pulmonary disease
Analysis_Subset$ChrnLung <- as.logical(Analysis_Subset$cm.clung)

################################################################################
# Renal Disease
Analysis_Subset$ChrRenal <- Analysis_Subset$cm.renl | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6462', x)))

################################################################################
# Liver Disease
Analysis_Subset$ChrLvr <- Analysis_Subset$cm.liver | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6467', x)))

################################################################################
# Diabetes mellitus
Analysis_Subset$Diabetes <- Analysis_Subset$cm.dm | Analysis_Subset$cm.dmcx |
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6480', x)))

################################################################################
# Obesity
Analysis_Subset$Obesity <- Analysis_Subset$cm.obese | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6491', x)))

################################################################################
# HIV/AIDS
Analysis_Subset$HIV_AIDS <- Analysis_Subset$cm.aids | 
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^042|^07953|^V08', x)))

################################################################################
# Rheumatoid arthritis or collagen vascular disease
Analysis_Subset$RheumArth <- as.logical(Analysis_Subset$cm.arth)

################################################################################
# Systemic lupus erythematous
Analysis_Subset$Lupus <- apply(Analysis_Subset[, diag_vars], 1, 
                        function(x) any(grepl('^6954|^7100', x)))

################################################################################
# Anemia
Analysis_Subset$Anemia <- Analysis_Subset$cm.anem | Analysis_Subset$cm.bld |
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6482', x)))

################################################################################
# Cancer
Analysis_Subset$Cancer <- Analysis_Subset$cm.tumor | Analysis_Subset$cm.mets

################################################################################
# Alcohol or substance abuse
Analysis_Subset$Alcuse <- Analysis_Subset$cm.drug | Analysis_Subset$cm.alc |
  apply(Analysis_Subset[, diag_vars], 1, function(x) any(grepl('^6483', x)))

################################################################################
# Tobacco Use
Analysis_Subset$Tobacco <- apply(Analysis_Subset[, diag_vars], 1, 
                          function(x) any(grepl('^3051|^6490|^98984|^V1582', 
                                                x)))

Comorbid_Vars <- c("Hypertension", "CHF", "OtherCardio", "ChrnLung", "ChrRenal",
                   "ChrLvr", "Diabetes", "Obesity", "HIV_AIDS","RheumArth", 
                   "Lupus", "Anemia", "Cancer", "Alcuse", "Tobacco")


################################################################################
# Put back together
miss_anal <-setdiff(names(Analysis_Subset), names(Analysis))
Analysis <- Analysis[Analysis$Subset == F, ]
Analysis[, miss_anal] <- NA

Analysis <- rbind(Analysis_Subset, Analysis)
#check
setequal(Analysis$key.nis, NISCore$key.nis)
#checks

# take out some excess variables
Analysis <- Analysis[, -grep('^dx|^ecode|^e.c|^pr', names(Analysis))]

