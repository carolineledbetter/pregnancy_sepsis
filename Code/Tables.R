###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 11/9/17
###########################################

# This file generates the unweighted and weighted tables 1-6.

n_cat <- length(levels(Anal_clean[, outcome]))

spacerUW <- rep(' ', n_cat)
spacerW <- rep(' ', n_cat+1)


################################################################################
###################### Patient and Hospital Demographics #######################
################################################################################
source(file = 'Code/Table1.R')

UWTable1 <- Table1(c('age', 'AgeCat', 'race', 'Insurance', 'MedianInc', 
                     'h.contrl', 'region', 'bedsize', 'locteach'), 
                   outcome, Anal_clean, 
                   row_var_names = c('Age', 'Age (Categorical)', 
                                     'Race/Ethncity', 
                                     'Insurance/Expected Payer', 
                                     'Median Income by Patient Zip Code', 
                                     'Hospital Ownership', 
                                     'Geographic Region', 
                                     'Hospital Size (#of beds)', 
                                     'Hospital Location/Teaching Status'), 
                   incl_missing = T, incl_pvalues = F)

WTable1 <- Table1(c('age', 'AgeCat', 'race', 'Insurance', 'MedianInc', 
                    'h.contrl', 'region', 'bedsize', 'locteach'), 
                  outcome, designAll, 
                  row_var_names = c('Age', 'Age (Categorical)', 
                                    'Race/Ethncity', 
                                    'Insurance/Expected Payer', 
                                    'Median Income by Patient Zip Code', 
                                    'Hospital Ownership', 
                                    'Geographic Region', 
                                    'Hospital Size (#of beds)', 
                                    'Hospital Location/Teaching Status'), 
                  incl_missing = F, incl_pvalues = T)


################################################################################
########################## Obstetrical Demographics ############################
################################################################################

source('Code/TableTorF.R')
Table2 <- do.call(rbind, lapply(c(Preg_Vars, Deliv_Vars), TablesTorF, 
                                colvariable = outcome, design = designAll))
UWTable2 <- do.call(rbind, lapply(c(Preg_Vars, Deliv_Vars), TablesTorFUW, 
                                  colvariable = outcome, Anal_clean))

N_deliv <- c(paste0('N = ', gsub('\\(.*$', '', Table2[3, -n_cat])), '')
Table2 <- rbind(spacerW, Table2[1:5, ], N_deliv, Table2[6:9, ])

N_deliv <- c(paste0('N = ', gsub('\\(.*$', '', UWTable2[3,])))
UWTable2 <- rbind(spacerUW, UWTable2[1:5, ], N_deliv, UWTable2[6:9, ])



################################################################################
############################# Source of Infection ##############################
################################################################################

Table3 <- TablesTorF(Inf_Vars, colvariable = outcome, 
                     design = designAll)

UWTable3 <- TablesTorFUW(Inf_Vars, colvariable = outcome, Anal_clean)
Num_SrcInf <- Table1('NumSrcInf', outcome, Anal_clean, incl_missing = T, 
                     incl_pvalues = F)


UWTable3 <- rbind(UWTable3, Num_SrcInf[-1, ])


################################################################################
######################## Complications & Comorbidities #########################
################################################################################

Table4 <- do.call(rbind, lapply(c(Complic_Vars, Comorbid_Vars), TablesTorF, 
                                colvariable = outcome, design = designAll))
UWTable4 <- do.call(rbind, lapply(c(Complic_Vars, Comorbid_Vars), TablesTorFUW, 
                                  colvariable = outcome, Anal_clean))


################################################################################
######################## Interventions & Organ Failures ########################
################################################################################

Table5 <- do.call(rbind, lapply(c(Intervention_Vars, Organ_Vars), 
                                TablesTorF,  
                                colvariable = outcome, design = designAll))
UWTable5 <- do.call(rbind, lapply(c(Intervention_Vars, Organ_Vars), 
                                  TablesTorFUW,  
                                  colvariable = outcome, Anal_clean))

################################################################################
############################### Patient Outcomes ###############################
################################################################################

Tbl6Cat <- Table1(c('LOS_Cat', 'discharge'), outcome, designAll, 
                  incl_pvalues = F)
LOS_QTR <- svyby(~los, by = as.formula(paste0('~', outcome)), designAll, 
                 svyquantile, 
                 quantiles = c(0.25, 0.5, 0.75), na.rm = T, keep.var = F)
LOS_mean <- svyby(~los, by = as.formula(paste0('~', outcome)), designAll, 
                  svymean, na.rm = T)
tot_chgQTR <- svyby(~totchg, by = as.formula(paste0('~', outcome)), designAll, 
                    svyquantile, 
                    quantiles = c(0.25, 0.5, 0.75), na.rm = T, keep.var = F)
Table6 <- data.frame(died = TablesTorF('died', outcome, designAll)[1:n_cat+1], 
                     LOSmedian = paste0(LOS_QTR$statistic2, '(', 
                                        LOS_QTR$statistic1, '-', 
                                        LOS_QTR$statistic3, ")"), 
                     LOSmean = paste0(round(LOS_mean$los, 2), "(", 
                                      round(LOS_mean$se, 2), ')'), 
                     HospCharges = paste0('$', 
                                          format(round(tot_chgQTR$statistic2, 
                                                       0), 
                                                 big.mark = ",", trim = T), 
                                          '($', 
                                          format(round(tot_chgQTR$statistic1, 
                                                       0), 
                                                 big.mark = ",", trim = T), 
                                          '-$', 
                                          format(round(tot_chgQTR$statistic3, 
                                                       0), 
                                                 big.mark = ",", trim = T), 
                                          ")"))
Table6 <- t(Table6)
Table6 <- rbind(Tbl6Cat[1, ], Table6[1:3, ], Tbl6Cat[2:5, ], Table6[4, ], 
                Tbl6Cat[6:13, ])


################################################################################
############################### Table Build ####################################
################################################################################

Table3 <- rbind(spacerW, spacerW, Table3[1:4, ], spacerW, Table3[5:16, ])




UWTable3 <- rbind(spacerUW, spacerUW, UWTable3[1:4, ], spacerUW, 
                  UWTable3[5:16, ],  
                  UWTable3[17:25, ])

Table4 <- rbind(spacerW, spacerW, Table4[1:8, ], spacerW, Table4[9:15, ], 
                spacerW, Table4[16:17, ], spacerW, Table4[18:32, ])
UWTable4 <- rbind(spacerUW, spacerUW, UWTable4[1:8, ], spacerUW, UWTable4[9:15, ], 
                  spacerUW, UWTable4[16:17, ], spacerUW, UWTable4[18:32, ])

Table5 <- rbind(spacerW, Table5[1:8, ], spacerW, Table5[9:14, ])
UWTable5 <- rbind(spacerUW, UWTable5[1:8, ], spacerUW, UWTable5[9:14, ])

rownames(UWTable2) <- 
  rownames(Table2) <- c('**Type of Pregnancy Related Hospitalization**', 
                        '\\ Abortion or Fetal loss', 
                        '\\ Antepartum', 
                        '\\ Delivery', 
                        '\\ Postpartum', 
                        '\\ Unknown', 
                        '**Mode of Delivery**', 
                        '\\ Vaginal Delivery', 
                        '\\ Assisted Vaginal Delivery', 
                        '\\ Cesarean Section', 
                        '\\ Unknown')


rownames(UWTable3)[1:19] <- 
  rownames(Table3) <- c('**Source of Infection**', 
                        '***Obstetric***', 
                        '\\ Puerperal Infection', 
                        '\\ Chorioamnionitis', 
                        '\\ Endometritis', 
                        '\\ Septic Abortion', 
                        '***Non-Obstetric***', 
                        '\\ Infectious and Parasitic Diseases', 
                        '\\ Respiratory', 
                        '\\ Genitourinary', 
                        '\\ Digestive', 
                        '\\ Wound, Skin, or Soft Tissue Infection', 
                        '\\ Bacteremia', 
                        '\\ Circulatory', 
                        '\\ Nervous', 
                        '\\ CLABSI', 
                        '\\ Mastitis', 
                        '\\ Musculoskeletal', 
                        '\\ Other') 


rownames(Table4) <- 
  rownames(UWTable4) <- c('**Obstetric Risk Factors/Complications**', 
                          '*Antepartum*', 
                          '\\ Cerclage', 
                          '\\ Multiple Gestation', 
                          '\\ Gestational Diabetes', 
                          '\\ Gestational Hypertension', 
                          '\\ Pre-eclampsia', 
                          '\\ Eclampsia', 
                          '\\ Placental Abruption', 
                          '\\ Antepartum Hemorrhage', 
                          '*Intrapatrum*', 
                          '\\ Premature Ruprture of Membranes', 
                          '\\ Preterm Labor (excl Preterm Delivery)', 
                          '\\ Preterm Delivery',
                          '\\ Induction of Labour',
                          '\\ Prolonged Rupture of Membranes/ Prolonged Labour',
                          '\\ Artificial Ruprture of Membranes', 
                          '\\ Stillbirth', 
                          '*Postpartum*', 
                          '\\ Postpartum Hemorrhage', 
                          '\\ Retained Products of Conception', 
                          '**Chronic Comorbidities**', 
                          '\\ Hypertension', 
                          '\\ Congestive Heart Failure', 
                          '\\ Other Cardiovascular Disease', 
                          '\\ Chronic Pulmonary Disease', 
                          '\\ Renal Disease', 
                          '\\ Liver Disease', 
                          '\\ Diabetes mellitus',
                          '\\ Obesity', 
                          '\\ HIV/AIDS', 
                          '\\ Rheumatoid Arthritis or Collagen Vascular Disease', 
                          '\\ Systemic lupus erythematosus', 
                          '\\ Anemia', 
                          '\\ Cancer', 
                          '\\ Alcohol or Substance Abuse', 
                          '\\ Tobacco Use')

rownames(Table5) <- 
  rownames(UWTable5) <- c('**Interventions and Management Indicators**', 
                          '\\ Hemodialysis', 
                          '\\ Positive Pressure Ventilation', 
                          '\\ Central Line Placement', 
                          '\\ Invasive Hemodynamic Monitoring', 
                          '\\ Blood Transfusion', 
                          '\\ Hysterectomy', 
                          '\\ Dilation and curettage',
                          '\\ Surgical Intervention (excluding Obstetrical Procedures)', 
                          '**Organ Failures**', 
                          '\\ Respiratory', 
                          '\\ Cardiovascular', 
                          '\\ Renal', 
                          '\\ Hepatic', 
                          '\\ Hematological/Coagulation', 
                          '\\ Neurological')


rownames(Table6)[1:10] <- c(' ',
                            '**In-Hospital Mortality**', 
                            '**Length of Stay (days): Median (IQR)**', 
                            '**Length of Stay (days): Mean (SD)**', 
                            '**Length of Stay (days):**',
                            '\\ <3 days', 
                            '\\ 3-7 days', 
                            '\\ >7 days', 
                            '**Hospital Charges: Median (IQR)**', 
                            '**Disposition**')

