###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 7/28/17
###########################################

# This file generates the descriptive statistics used for analysis.

# The final workspace is saved as descriptives.rdata


# cluster (id) variable is hosp.nis
# strata variable is nis.stra
# weight variable is discwt


library(survey)

load(file = 'DataProcessed/Analysis13.rda')
load(file = 'DataProcessed/Analysis14.rda')
load(file = 'DataProcessed/VariableGrps.Rdata')


###############################################################################
# merge 2013 and 2014 data sets
# remove srvcline variable from 2014 cause it's not in 2013. 
Analysis14 <- Analysis14[, -grep('srvcline', names(Analysis14))]

Analysis <- rbind(Analysis13, Analysis14)
save(Analysis, file = 'DataProcessed/AnalysisAll.rda')



designAll <- svydesign(id=~hosp.nis, strata=~nis.stra, weights=~discwt, 
                       nest = TRUE, data = Analysis)
designAll <- subset(designAll, Subset == T)

save(designAll, file = 'DataProcessed/designAll.rda')

# drop unsused obs and levels from analysis data set 
Anal_clean <- droplevels(Analysis[!is.na(Analysis$Outcome), ])



outcome <- 'Outcome2'
source('Code/Tables.R')


PregTable <- table(Analysis$Pregnant, Analysis$Outcome, useNA = 'ifany')[2, ]

save(list = ls(pattern = 'Table'), file = 'DataProcessed/Descriptives.Rdata')


