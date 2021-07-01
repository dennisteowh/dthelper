#### FUNCTIONS FOR REPORTING ####
#' Reporting p values
#'
#' @param p Input p value
#' @param dp Number of decimal places
#' @param start Character string appended to start of output
#' @param end Character string appended to end of output
#' @return A character string with '= ' (or '< .001' if p falls below threshold) before the input value
#' @examples
#' report.p(.02);
#' report.p(.00003, start = "p ");
#' @export
report.p <- function(p, dp = 3, start = NULL, end = NULL){
  if (!is.numeric(p))  {
    stop("p must be a number")
  } else if (p >= .01 & p <= 1 & p > 0){
    return(paste0(start,"= ", round(p, dp), end))
  } else if (p < .01 & p >= .001 & p <= 1 & p > 0) {
    return(paste0(start, "= ", round(p, dp), end))
  } else if (p < .001 & p <= 1 & p > 0){
    return(paste0(start, "< .001",end))
  } else if (p > 1 & p > 0) {
    stop("p cannot be greater than 1")
  } else if (p < 0) {
    stop("p cannot be less than 0")
  } else if (p == 0) {
    stop("p cannot be equals to 0")
  }
}

#' @export
report.ci <- function(est, se, cutoff = 1.96, dp=2, start = NULL, end = NULL){
  
  if(!is.numeric(est)|!is.numeric(se)){
    stop("both estimates and standard errors have to be numbers")
  } else if(se <= 0){
    stop("standard error has to be greater than 0")
  } else {
    return(paste0(start, "[", round(est-cutoff*se,dp), ", ", round(est+cutoff*se,2),"]",end) )
  }
  
}


# gets summary of lm object and returns specified statistics from coefficient table
#' @export
extract.lm <- function(lm.model, var = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), include.intercept = TRUE){
  lm.sum <- summary(lm.model)
  if(include.intercept == TRUE){
    coef.table <- lm.sum$coefficients
  }else{
    coef.table <- lm.sum$coefficients[-1, ] # remove row with intercept
  }
  if(ncol(coef.table) != 4){
    warning("input is not an lm object")
  }
  labels <- row.names(coef.table)
  output.vector <- coef.table[, var]
  names(output.vector) <- labels
  return(output.vector)
}


