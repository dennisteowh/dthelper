
#' Describe Missing Data
#'
#' @param x Vector or data.frame object
#' @param dp Number of decimal places for percentage
#' @param return.num If set to TRUE, returns number of missing cases
#' @param return.perc If set to TRUE, returns percentage of missing cases
#' @param print.text If set to FALSE, missing cases are not printed
#' @return Default none. When return.num or return.perc equals TRUE, numeric object is returned
#' @export
describe.na <- function (x, dp = 2, return.num = FALSE, return.perc = FALSE, print.text = TRUE) {
  
  if(return.num == TRUE & return.perc == TRUE){
    stop("you can only set EITHER return.num OR return.perc to TRUE")
  }
  
  if(is.data.frame(x)){ # if x is a data.frame
    
    na.count <- apply(x, 2, function(x){sum(is.na(x))})
    na.perc <- apply(x, 2, function(x){sum(is.na(x))/length(x)*100})
    labels <- colnames(x)
    
    if(print.text == TRUE){
      output <- paste0(na.count, " (", round(na.perc,dp) , "%) missing observations")
      names(output) <- labels
      print(output)
    }
    if(return.num == TRUE){
      names(na.count) <- labels
      return(na.count)
    }
    if(return.perc == TRUE){
      names(na.perc) <- labels
      return(na.perc)
    }
    
  }else if(is.vector(x)){ # if x is a vector
    na.count <- sum(is.na(x))
    na.perc <- sum(is.na(x))/length(x)*100
    
    if(print.text == TRUE){
      output <- paste0(na.count, " (", round(na.perc,dp), "%) missing observations")
      print(output)
    }
    if(return.num == TRUE){
      return(na.count)
    }
    if(return.perc == TRUE){
      return(na.perc)
    }
    
  }
}

#' Reduce Listwise Deletion
#' 
#' By specifying a drop value, X, this function returns the number of 
#' missing data that is reduced by dropping X number of variables. 
#' The function returns summary statistics of the missing data (partially)
#' caused by each variable. This allows the user to quickly assess which variables
#' lead to the highest number of listwise deletions
#'
#' @param df data.frame object
#' @param var Input column names (defaults to using all columns in df)
#' @param drop Number of variables to drop. It is recommended to test a few different values starting from 1
#' @return Single row data.frame object that reports number of missing data (partially) caused by each variable
#' @export
reduce.listwise.deletion <- function(df, var = colnames(df), drop){
  
  check.df <- is.na(df[, var]) 
  
  for (i in 1:nrow(check.df)){
    if (sum(check.df[i, ]) <=  drop){ 
      next
    } else if (sum(check.df[i, ]) >  drop) {
      check.df[i, ] <- FALSE
    } 
  }
  
  sum.df <- apply(check.df, 2, sum) # sum each column
  return(sum.df)
}

#' Estimate Listwise Deletion
#' 
#' Estimate number of missingness caused by listwise deletion
#'
#' @param df data.frame object
#' @param var Input column names (defaults to using all columns in df)
#' @param dp Number of decimal places for percentage
#' @param return.num If set to TRUE, returns number of missing cases
#' @param return.perc If set to TRUE, returns percentage of missing cases
#' @param print.text If set to FALSE, missing cases are not printed
#' @return Default none. When return.num or return.perc equals TRUE, numeric object is returned
#' @export
estimate.listwise.deletion <- function(df, var = colnames(df), dp = 2, return.num = FALSE, return.perc = FALSE, print.text = TRUE){
  
  if(return.num == TRUE & return.perc == TRUE){
    stop("you can only set EITHER return.num OR return.perc to TRUE")
  }
  
  df.deleted <- na.omit(df[,var])
  na.count <- nrow(df) - nrow(df.deleted)
  na.perc <- na.count*100/nrow(df)
  
  if(print.text == TRUE){
    output <- paste0(na.count, " (", round(na.perc,dp), "%) missing observations")
    print(output)
  }
  if(return.num == TRUE){
    return(na.count)
  }
  if(return.perc == TRUE){
    return(na.perc)
  }
  
}
