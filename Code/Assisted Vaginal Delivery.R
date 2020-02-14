###########################################
# Project: Pregnancy Sepsis
# Author: Caroline Ledbetter
# Date: 8/22/2017
###########################################

# This file looks at all of the procedure codes in the assisted delivery 
# category and produces a table of there frequencies. 

# The final workspace is saved as assist_delivery.rdata


################################################################################
# Get a list of all procedure codes present in the data
load(file = 'DataProcessed/2014Core_Clean.Rda')
load(file = 'DataProcessed/2013Core_Clean.Rda')

attach(NIS2014Core)
proc_vars <- ls(pattern = '^pr\\d{1,2}$', NIS2014Core)
detach(NIS2014Core)
pr_codes <- unlist(lapply(NIS2014Core[, proc_vars], levels))
names(pr_codes) <- NULL

pr_codes2 <- unlist(lapply(NIS2013Core[, proc_vars], levels))
names(pr_codes2) <- NULL

pr_codes <- c(pr_codes, pr_codes2)

pr_codes <- sort(pr_codes[!duplicated(pr_codes)])
rm(pr_codes2)


# look at those involved in assisted vaginal delivery
grep('^72|^73[2-3, 5-6, 8-9]', pr_codes, value = T)

# make a list with all codes involved in vaginal delivery correctly formatted
assisted_delivery_codes <- grep('^72|^73[2-3, 5-6, 8-9]', pr_codes, value = T)

# subset out those with assisted vaginal delivery and look at frequency of 
# these codes in the procedure codes field
assist_deliv_sub <- rbind(NIS2013Core[NIS2013Core$AssistVag == T, proc_vars], 
                          NIS2014Core[NIS2014Core$AssistVag == T, proc_vars])
counts <- sapply(assisted_delivery_codes, function(y) {
  sum(apply(assist_deliv_sub, 1, function(x) any(x %in% y)))
})


assisted_delivery_codes <- data.frame('Procedure Code' = 
                                        gsub('(^.{2})', '\\1.', 
                                             assisted_delivery_codes), 
                                      'Count' = counts)
save(list = ls(pattern = '^assist'), 
     file = 'DataProcessed/assist_delivery.rdata')

################################################################################