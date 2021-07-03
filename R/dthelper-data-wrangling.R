
#' Spread Multiple Variables
#' 
#' Wrangles data from long to wide form
#'
#' @param df data.frame object
#' @param id Identifier variable. Input as string
#' @param key Key variable to spread on. Input as string
#' @param var Variables being spread. Defaults to all other variables (apart from id and key)
#' @param sep Separator that appends to variable names after being spread
#' @param custom.append Customised labels that are appended to variable names. Length should match number of unique keys
#' @return data.frame object
#' @examples 
#' df <- data.frame(
#'       id = c(1,1,1,2,2,2,3,3,3),
#'       time = c(1,2,3,1,2,3,1,2,3),
#'       v1 = 1:9,
#'       v2 = 11:19,
#'       v3 = paste0("a",1:9)
#'       )
#' df.wide <- multi.spread(df, id = "id", key = "time") 
#' df.wide <- multi.spread(df, id = "id", key = "time", custom.append = c("t1","t2","t3")) 
#' df.wide <- multi.spread(df, id = "id", key = "time", var = c("v1","v2")) 
#' @export
multi.spread <- function(df, id, key, var = NULL, sep = "_", custom.append = NULL){
  require(gtools)
  require(tidyverse)
  
  if(is.null(var)){ # default var
    var <- colnames(df)[!colnames(df) %in% id&!colnames(df) %in% key] # takes all other columns apart from id and key
  }
  
  df.list <- lapply(var, 
                    function(x){
                      df.temp <- spread(df[, c(id, key, x)], key, x) # spread 1 variable
                      cname <- mixedsort(unique(as.vector(unlist(df[,key])))) # sort colnames in alphanumeric order
                      df.temp <- df.temp[, c(id, cname)] 
                      if(!is.null(custom.append)){
                        colnames(df.temp)[2:ncol(df.temp)]  <- paste(x, custom.append, sep= sep) 
                      } else{ #default
                        colnames(df.temp)[2:ncol(df.temp)]  <- paste(x, colnames(df.temp)[2:ncol(df.temp)], sep= sep) #add key to column names
                      }
                      return(df.temp) 
                    })
  
  df.output <- df.list[[1]] # initialise
  for(i in 1:(length(var)-1)){ # combine matrices together
    df.output <- full_join(df.output, df.list[[i+1]])
  }
  
  return(as.data.frame(df.output))        
}

#' Add Count
#' 
#' Typically used after group_by(). This function gets the counts for each group and adds
#' it back to the ungrouped dataset.
#'
#' @param df data.frame object
#' @param rename Specify to rename column for counts
#' @return data.frame object
#' @examples 
#' library(tidyverse)
#' mtcars%>%
#'   group_by(am) %>%
#'   add.count()
#' 
#' @export
add.count <- function (df, rename = "N") {
  
  require(tidyverse)
  
  count.data <- df %>%
    count()
  
  if (length(rename) != 0) {
    colnames(count.data)[grep("n", colnames(count.data)) ] <- rename
  }
  
  df.out <- full_join(df, count.data)
  return(df.out)
}

#' Row Bind Matching Column Names
#' 
#' row binds a vector to an existing data.frame object (data.frame can be empty).
#' The vector should be named.
#' This function ensures that the vector values are added to the column
#' that shares their respectives names.
#'
#' @param df.old data.frame object
#' @param x.new vector object. Should be named carefully
#' @param names Desired column names for output df
#' @return data.frame object
#' @examples 
#' df <- data.frame()
#' x <- 1:5
#' names(x) <- c("a","b","c","f","g")
#' rbind.match(df, x, names = c("a","b","c","d","e")) 
#' rbind.match(df, x, names = c("g","f","c","b","a")) 
#' 
#' @export
rbind.match <- function(df.old, x.new, names=colnames(df.old)){ # x is vector
  
  # initialise names 
  if(nrow(df.old)==0){
    temp.matrix <- matrix(nrow=1, ncol = length(names))
    df.old <- as.data.frame(temp.matrix)
    colnames(df.old) <- names
    
    new.names <- names(x.new)
    current.row <- nrow(df.old)
  } else {
    new.names <- names(x.new)
    # initialise new row
    df.old[nrow(df.old)+1, ] <- NA 
    current.row <- nrow(df.old)
  }
  
  for(i in 1:length(new.names)){
    if(new.names[i] %in% names){
      df.old[current.row, new.names[i]] <- x.new[i]
    }
  }
  
  return(as.data.frame(df.old))
  
}

#' Row Bind Matching Column Names
#' 
#' row binds a vector to an existing data.frame object (data.frame can be empty).
#' The vector should be named.
#' This function ensures that the vector values are added to the column
#' that shares their respectives names.
#'
#' @param df data.frame object
#' @param keep Variables that shouldn't be gathered
#' @param var.to.gather Variable that should be gathered. Defaults to all other variables apart from keep
#' @param key Name of key variable after gathering
#' @param value Name of value variable after gathering
#' @return data.frame object
#' @examples 
#' set.seed(100)
#' df <- data.frame(
#'       id = 1:10,
#'       v1_t1 = rnorm(10),
#'       v1_t2 = rnorm(10),
#'       v1_t3 = rnorm(10)
#'       )
#' gather.keep(df, keep = "id")
#' 
#' @export
gather.keep <- function(df, keep = NULL, var.to.gather=NULL, key = "key", value= "value") {
  
  require(tidyverse)
  
  if(is.null(var.to.gather)){ # defaults to all variables other than keep
    var.to.gather <- colnames(df)[!colnames(df) %in% keep]
  }
  
  if (is.null(keep)) {
    
    result <- gather(df[, var.to.gather], key = key, value = value)
    return(result)
    
  } else {
    
    result <- gather(df[, var.to.gather], key = key, value = value) 
    
    no.var <- length(var.to.gather) 
    
    grow <- df[, c(var.to.gather, keep)]
    temp <- grow
    for (i in 1:(no.var - 1)){
      temp <- rbind(temp, grow) ##multiplying temp to match rows for result
      
    }
    
    result <- cbind(result, temp[, keep])
    
    colnames(result) <- c(key, value, keep)
    
    return(result)
  }
}



