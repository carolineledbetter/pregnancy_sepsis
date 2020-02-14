TablesTorF <- function(rowvars, colvariable, design, row_var_names = NULL, 
                       incl_pvalues = T) {
  if(is.null(row_var_names)) row_var_names <- rowvars
  data <- design$variables[0,]
  returnRow <- function(var){
    n <- survey::svytable(as.formula(paste0("~", var, ' + ', 
                                          colvariable)), design, round = T)[2, ]
    n <- format(n, big.mark = ',', trim = T)
    pct <- survey::svyby(formula = as.formula(paste0("~", var)),
                         by = as.formula(paste0("~", colvariable)), 
                         FUN = survey::svymean, design = design, na.rm = T)[ , 3]
    if(incl_pvalues == T)
    p <- survey::svychisq(as.formula(paste0("~", var, ' + ', 
                                            colvariable)), design, 
                          statistic = 'F')$p.value
    p <- ifelse(p < 0.0001, '<0.0001', sprintf('%.4f',p))
    row <- c(paste0(n, '(', round(pct*100, 0), ')'), p)
    return(row)
  }
  returntabl <- do.call(rbind, lapply(rowvars, returnRow))
  rownames(returntabl) <- row_var_names
  colnames(returntabl) <- c(levels(data[,colvariable]), 'p_value')
  return(returntabl)
}    

TablesTorFUW <- function(rowvars, colvariable, data, row_var_names = NULL) {
  if(is.null(row_var_names)) row_var_names <- rowvars
  returnRow <- function(var){
    n <- table(data[, var], data[, colvariable])
    pct <- n[2, ]/apply(n, 2, sum)
    n <- format(n[2, ], big.mark = ',', trim = T)
    row <- c(paste0(n, '(', round(pct*100, 0), ')'))
    return(row)
  }
  returntabl <- do.call(rbind, lapply(rowvars, returnRow))
  rownames(returntabl) <- row_var_names
  colnames(returntabl) <- levels(data[,colvariable])
  return(returntabl)
}  
  
