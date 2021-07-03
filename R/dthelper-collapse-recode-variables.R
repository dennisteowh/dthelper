#' Recode Multiple Variables
#' 
#' Recodes variables that share the same scale
#' 
#' @param df data.frame object
#' @param var Variable to recode
#' @param from Original scale (order of input needs to match desired scale)
#' @param to Desired scale (order of input needs to match original scale). Defaults to integer scale starting from 1
#' @param fun Function determining object type of output variables
#' @return data.frame object
#' 
#' @examples 
#' df <- data.frame(
#'       v1 = c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree"),
#'       v2 = c("Strongly Disagree","Disagree","Strongly Disagree","Disagree","Strongly Disagree"),
#'       v3 = c( "Neutral"," Agree", "Strongly Agree", "Neutral"," Agree")
#'       )
#' 
#' recode.multiple(df, var = c("v1","v2","v3"), from = c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree"))
#' recode.multiple(df, var = c("v1","v2","v3"), from = c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree"), to = c(10,20,30,40,50))
#' recode.multiple(df, var = c("v1","v2","v3"), from = c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree"), 
#'                 to = c("ten", "twenty", "thirty", "forty", "fifty"),
#'                 fun = function(x){factor(x, levels = c("ten", "twenty", "thirty", "forty", "fifty"))})
#'
#'@export
recode.multiple <- function(df, var, from, to = 1:length(from), fun = as.numeric){ # creates a function with specified arguments
  
  for(i in 1:length(from)){ # for each response value
    df[, var][df[, var] == from[i]] <- to[i] # change values in "from" vector into "to" vector within the target variables 
  }
  
  if (is.null(fun)){ # this does the coercion when the fun argument is specified
    next             # note that the default argument is as.numeric
  } else{
    df[, var] <- lapply(df[, var], fun)
  }
  
  return(df)
} 


#' Collapse Multiple Variables into a single Column
#' 
#' @param df data.frame object
#' @param var Variable to collapse
#' @param return.conflicts If set to TRUE and there are conflicts within the input variables, the conflicting rows will be returned
#' @return vector object. If return.conflicts set to TRUE, a list object is returned
#' 
#' @examples 
#' df <- data.frame(
#' v1.t1 = c(1:10),
#' v1.t2 = c(1:5, rep(NA, 5)),
#' v2.t1 = c(12,13,14,rep(NA,7)),
#' v2.t2 = c(11:20)
#' )
#' 
#' df$v1 <- collapse.var(df, var = c("v1.t1","v1.t2"))
#' df$v2 <- collapse.var(df, var = c("v2.t1","v2.t2"))
#' conflicts <- collapse.var(df, var = c("v2.t1","v2.t2"), return.conflicts = T)
#'  
#' also see <https://msrcodelibrary.netlify.app/2020/06/10/multiple-recoding-and-collapsing-columns/> for 
#' step-by-step tutorial.
#'
#' @export
collapse.var <- function(df, var = colnames(df), return.conflicts = FALSE) {
  
  ## identifying conflicting columns (NAs non-inclusive)
  df_temp <- df[, var]
  df_conflicts <- c() ## initialise vector to store conflicts
  
  for(i in 1:nrow(df_temp)){
    
    df_non_duplicates <- NA # initialise on each row
    
    ## remove duplicates from these rows
    df_non_duplicates <- df_temp[i , ][as.vector(!duplicated(t(df_temp[i , ])))]
    ## remove missing data
    df_non_duplicates <- df_non_duplicates[!is.na(df_non_duplicates)]
    
    ## are there conflicting values???
    if (length(df_non_duplicates) > 1){ ## assuming there are no conflicts, this length should be 1
      
      df_conflicts <- c(df_conflicts, i)
      
    } 
    
  }
  
  ## reports if there are conflicts
  if (length(df_conflicts) > 0){
    warning("there are non-NA conflicts in the input variables")
    
  } else {
    message("there are no conflicts in the input variables")
  }
  
  df_extract <- df[,var]
  result <- rep(NA, nrow(df_extract))
  #For each specified category column
  for (i in 1:nrow(df_extract)){
    for (j in 1:ncol(df_extract)){
      #if the row of the specified column is not missing
      if(!is.na(df_extract[i,j])){
        #then slot that value into the result
        result[i] <- df[i,j]
      } 
    }
  }
  
  if (length(df_conflicts) > 0 & return.conflicts==TRUE){
    
    message("returning conflicts as list object. Output CANNOT be new column of df")
    conflict.df = df[df_conflicts, ]
    conflict.df$conflict.rows <- df_conflicts
    
    return(list(result = result, conflict.df = conflict.df))
    
  } else {
    return(result)
  }
  
}


