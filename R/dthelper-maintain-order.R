#' String to Vector
#' 
#' Splits a string into its constituent parts based on a given separator. 
#' For example, "A|B" will output "A", "B"
#' 
#' @param string Input character string
#' @param sep Separator value. Defaults to "|"
#' @return character vector
#' 
#' @examples 
#' string <- "A|B"
#' string.to.vect(string)
#' 
#' string2 <- "A.B"
#' string.to.vect(string2, sep = ".")
#' 
#' @export
string.to.vect <- function(string, sep = "|"){
  require(stringr)
  # Remove everything that is not a number or letter (may want to keep more 
  # stuff in your actual analyses). 
  temp <- string
  
  if(sep == "."){
    sep = "\\."
  } else if(sep == "|"){
    sep = "[|]"
  }
  temp <- gsub(sep, " ", temp)
  # Shrink down to just one white space
  temp <- str_replace_all(temp,"[\\s]+", " ")
  # Split it
  temp <- str_split(temp, " ")[[1]]
  # Get rid of trailing "" if necessary
  indexes <- which(temp == "")
  if(length(indexes) > 0){
    temp <- temp[-indexes]
  } 
  return(temp)
}

#' Maintain Order
#' 
#' Given an order and variables, function creates new variables by
#' rearranging input variables based on given order.
#' 
#' @param df data.frame object
#' @param order.by List of variables to order by. Should be a list object
#' @param vars.to.order Input variable names that needs reordering
#' @param new.category.name Name of created variable. Will be subscripted with .1, .2, ,3, etc.
#' @param sep separator used for subscripting new variable names
#' @param exact.match If set to FALSE, vars.to.order and order.by do not have to match exactly
#' @return data.frame object
#' 
#' @examples 
#' 
#' df <- data.frame(px = c(1,2,3),
#'       A = c(2, 4, 6),
#'       B = c(4, 6, 2),
#'       C = c(6, 4, 2),
#'       D = c(5, 6, 7 )
#'       )
#' df$order <- c("A|B|C|D", "D|A|C|B", "D|B|C|A")
#' df$order <- lapply(df$order, function(x){string.to.vect(x,sep="|")})
#' 
#' maintain.order(df = df, order.by = "order", vars.to.order = c("A", "B", "C","D"))
#' maintain.order(df = df, order.by = "order", vars.to.order = c("A", "B", "C"), new.category.name = "response")
#' 
#' df2 <- data.frame(df,
#'                   A.time = c(20.2, 30.2, 40.2),
#'                   B.time = c(10.1, 15.1, 20.1),
#'                   C.time = c(52.6, 58.6, 63.6),
#'                   D.time = c(1,2,3))
#' maintain.order(df2, order.by = "order", vars.to.order = c("A.time", "B.time", "C.time"), exact.match = FALSE)
#' 
#' also see <https://msrcodelibrary.netlify.app/2020/06/10/maintain-order/>
#' for step-by-step tutorial.
#' 
#' @export
maintain.order <- function(df, order.by, vars.to.order, new.category.name = "time",sep=".", exact.match = TRUE, starts.with = TRUE, check = TRUE) {
  
  ## dealing with different list structures
  if(!is.list(df[ ,order.by][[1]])) {
    
    order.col <- df[ ,order.by]
    
  } else if(!is.list(df[ ,order.by][[1]][[1]])) {
    
    order.col <- df[ ,order.by][[1]]
    
  } else if(!is.list(df[ ,order.by][[1]][[1]][[1]])) {
    
    order.col <- df[ ,order.by][[1]][[1]]
    
  } else {
    
    stop("the list structure for order.by is not appropriate")
    
  } ## order.col fixes the structure of the list
  
  if (exact.match == TRUE) { ## if vars.to.order names matches order.by exactly
    
    ## indexing variables to order by list to order by
    list.index <- list()
    for (i in 1:nrow(df)){
      
      temp.index <- which(order.col[[i]] %in% vars.to.order)
      
      list.index <- c(list.index, list(order.col[[i]][temp.index]))
      
    } ## this produces a list of the correct order for each participant
    
    order.length <- c()
    for (i in 1:nrow(df)) {
      
      temp <- length(list.index[[i]])
      order.length <- c(order.length, temp)
      
    }

    for (i in 1:nrow(df)) {
      
      for (j in 1:order.length[i]) {
        df[i, paste(new.category.name, j, sep = sep)] <- df[i , list.index[[i]][j]]
      }
    }
    
    if (check == TRUE){
      print(list.index) ## returns order list for each participants for checking
    } 
    return(df)
    
  } else if (exact.match == FALSE & starts.with == TRUE) { #if order.by variable pattern in vars.to.order
    
    ## getting variable names of vars.to.order via list.index
    
    list.index <- list()
    
    for (i in 1:nrow(df)) {
      
      temp.index <- c()
      
      for (j in 1:length(order.col[[i]])) {
        
        index <- grep(paste0("^", order.col[[i]][j]), vars.to.order)   
        
        if (length(index) == 0){
          
          NULL
          
        } else {
          temp.index <- c(temp.index, vars.to.order[index]) ## getting ordered variables for each participant
        }
      }
      
      list.index <- c(list.index, list(temp.index)) ## getting list of order variables
      
    }
    if (check == TRUE){
      print(list.index)
    } 
    
    order.length <- c()
    for (i in 1:nrow(df)) {
      
      temp <- length(list.index[[i]])
      order.length <- c(order.length, temp)
    }
    for (i in 1:nrow(df)) {
      
      for (j in 1:order.length[i]) {
        df[i, paste(new.category.name, j, sep = sep)] <- df[i , list.index[[i]][j]]
      }
    }
    
    return(df)
    
  } else if (exact.match == FALSE) { #if order.by variable pattern in vars.to.order
  
    ## getting variable names of vars.to.order via list.index
    
    list.index <- list()
    
    for (i in 1:nrow(df)) {
      
      temp.index <- c()
      
      for (j in 1:length(order.col[[i]])) {
        
        index <- grep(order.col[[i]][j], vars.to.order)   
        
        if (length(index) == 0){
          
          next
          
        } else {
          
          temp.index <- c(temp.index, vars.to.order[index]) ## getting ordered variables for each participant
          
        }
        
      }
      
      list.index <- c(list.index, list(temp.index)) ## getting list of order variables
      
    }
    if (check == TRUE){
      print(list.index)
    } 
    
    order.length <- c()
    for (i in 1:nrow(df)) {
      
      temp <- length(list.index[[i]])
      order.length <- c(order.length, temp)
    }
    
    for (i in 1:nrow(df)) {
      for (j in 1:order.length[i]) {
        df[i, paste(new.category.name, j, sep = sep)] <- df[i , list.index[[i]][j]]
      }
    }
    
    return(df)
  } 
}


