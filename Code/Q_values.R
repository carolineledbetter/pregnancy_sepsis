
load(file = 'DataProcessed/designAll.rda')
load(file = 'DataProcessed/VariableGrps.Rdata')

returnRow <- function(var, design, colvariable){
  n <- survey::svytable(as.formula(paste0("~", var, ' + ', 
                                          colvariable)), design, round = T)[2, ]
  pct <- survey::svyby(formula = as.formula(paste0("~", var)),
                       by = as.formula(paste0("~", colvariable)), 
                       FUN = survey::svymean, design = design, na.rm = T)[ , 3]
  totpct <- survey::svymean(as.formula(paste0('~', var)), design = design)[-1]
  names(totpct) <- 'Total %'
  p <- survey::svychisq(as.formula(paste0("~", var, ' + ', 
                                          colvariable)), design, 
                        statistic = 'F')$p.value
  n_pct <- c(n, pct)
  names(n_pct) <- c(paste0(names(n), 'N'), paste0(names(n), '%'))
  ord <- c(seq(1, length(n_pct), 2), seq(2, length(n_pct), 2))
  row <- c(n_pct[ord], totpct, p)
  return(row)
}

sapply(Comorbid_Vars, returnRow, design = designAll, colvariable = 'Outcome2')
library(qvalue)

