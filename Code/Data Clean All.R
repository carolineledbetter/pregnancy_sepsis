###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 7/28/17
###########################################

### This file imports the NIS data sets and cleans them for analysis. ###

### Data sets are saved in the DataProcessed folder as:
# 2013Core_Clean.Rda
# 2014Core_Clean.Rda
# 2013Hosp_Clean.Rda
# 2014Hosp_Clean.Rda
# Analysis13.rda
# Analysis14.rda

# Variable Groupings are saved as VariableGrps.Rdata'

# Set location for NIS data
# This data is currently located on the VIPER drive but this may change
# the path is also platform specific
Location <- ifelse(.Platform$OS.type == 'unix', 
                   '/Volumes/Lhotse_Shared/HCUP data/NIS Data/R/', 
                   'Z:/Lhotse_Shared/HCUP data/NIS Data/R/')

# fuunction to remove the year from the data frames before passing to the 
# data clean source
rmyear <- function(obj, year){
  assign(gsub(paste0('^NIS', year), 'NIS', obj), get(obj), .GlobalEnv)
}

#load 2013 files
load(file = paste0(Location,'NIS2013.rdata'))
data2013 <- ls(pattern = '^NIS2013')

# format names for data clean source
for(x in data2013){
  rmyear(x, '2013')
}; rm(x)
remove(list = data2013)

# clean the data
source('Code/Data Clean Source.R')

# save cleaned data
NIS2013Core <- NISCore
save(NIS2013Core, file =  'DataProcessed/2013Core_Clean.rda')

NIS2013Hosp <- NISHosp
save(NIS2013Hosp, file = 'DataProcessed/2013Hosp_Clean.rda')

Analysis13 <- Analysis
save(Analysis13, file = 'DataProcessed/Analysis13.rda')

NumInfParDis <- data.frame(Code = names(NumInfParDis), 
                           Count = NumInfParDis)
NumInfParDis2013 <- NumInfParDis
save(NumInfParDis2013, file = 'DataProcessed/InfParDisCount2013.rda')

remove(list = ls(pattern = '13'))

# repeat for 2014

load(file = paste0(Location,'NIS2014.rdata'))
data2014 <- ls(pattern = '^NIS2014')

for(x in data2014){
  rmyear(x, '2014')
}; rm(x)
remove(list = data2014)

source('Code/Data Clean Source.R')

NIS2014Core <- NISCore
save(NIS2014Core, file = 'DataProcessed/2014Core_Clean.rda')

NIS2014Hosp <- NISHosp
save(NIS2014Hosp, file = 'DataProcessed/2014Hosp_Clean.rda')

Analysis14 <- Analysis
save(Analysis14, file = 'DataProcessed/Analysis14.rda')

NumInfParDis <- data.frame(Code = names(NumInfParDis), 
                           Count = NumInfParDis)
NumInfParDis2014 <- NumInfParDis
save(NumInfParDis2014, file = 'DataProcessed/InfParDisCount2014.rda')

remove(list = ls(pattern = '14'))

# save variable groups for descriptives and analysis
save(list = ls(pattern = '_Vars$'), file = 'DataProcessed/VariableGrps.Rdata')
