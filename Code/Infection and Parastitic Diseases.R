###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 8/22/2017
###########################################

# This file looks at all of the diagnostic codes in the infectious and parasitic 
# diseases category and produces a table of their frequencies. 

# The final workspace is saved as infectious.rdata


################################################################################
# Infectious disease diagnostic codes were obtained and saved in the data clean
# file
load(file = 'DataProcessed/2014Core_Clean.Rda')
load(file = 'DataProcessed/2013Core_Clean.Rda')
load(file = 'DataProcessed/infect_dx2013.rda')
infect_dx2 <- infect_dx
load(file = 'DataProcessed/infect_dx2014.rda')

# combine 2013 and 2014 codes and remove duplicates
infect_codes <- c(infect_dx, infect_dx2)

infect_codes <- sort(infect_codes[!duplicated(infect_codes)])
rm(infect_dx, infect_dx2)


# subset out those with infectious disease as the source of infection
# and those who were not included in out analysis (not pregnant)
# and look at frequency of 
# infection  codes in the diagnostic codes field

# 2013 and 2014 have a different number of dx fields - so they are done 
# seperately

attach(NIS2013Core)
diag_vars <- ls(pattern = '^dx\\d{1,2}$', NIS2013Core)
detach(NIS2013Core)
infect_sub13 <- NIS2013Core[NIS2013Core$InfParDis == T & 
                              NIS2013Core$Subset == T, diag_vars]

attach(NIS2014Core)
diag_vars <- ls(pattern = '^dx\\d{1,2}$', NIS2014Core)
detach(NIS2014Core)
infect_sub14 <- NIS2014Core[NIS2014Core$InfParDis == T & 
                              NIS2014Core$Subset == T, diag_vars]


counts13 <- sapply(infect_codes, function(y) {
  sum(apply(infect_sub13, 1, function(x) any(x %in% y)))
})

counts14 <- sapply(infect_codes, function(y) {
  sum(apply(infect_sub14, 1, function(x) any(x %in% y)))
})

# combine 2013 and 2014 and sum counts
counts <- cbind(counts13, counts14)
counts <- apply(counts, 1, sum)


# merge with codes for table
infect_codes <- data.frame('Diagnostic Code' = 
                                        gsub('(^.{3})', '\\1.', 
                                             infect_codes), 
                                      'Count' = counts)

# remove counts of zero
infect_codes <- infect_codes[infect_codes$Count > 0, ]
save(list = ls(pattern = '^infect'), 
     file = 'DataProcessed/infectious.rdata')

################################################################################