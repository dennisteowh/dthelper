#' Compute Root Mean Square Error
#'
#' @param x Input vector object
#' @param na.rm If TRUE, NAs are ignored
#' @return A vector object
#' 
#' @export
rmse <- function(x, na.rm=T){
  return(sqrt(mean(x^2, na.rm = na.rm)))
}

#' Correlation with Grouping Variable
#' 
#' Perform correlations with the main variables of interest.
#' Group can be specified to perform correlations within different subsets
#' of the data.
#'
#' @param df data.frame object
#' @param id Identifier variable. Input as string
#' @param var Variables of main interest
#' @param with Variable to correlate with. Defaults to all other in df
#' @param group Grouping variable. If specified, correlation will be performed within each group separately
#' @param dp Percentage of missing data will be reported. dp allows re-specification of number of decimal places
#' @return data.frame object
#' @examples 
#' library(car)
#' cor.with(mtcars, var = c("vs"), group = "am")
#' cor.with(mtcars, var = c("vs", "cyl"))
#' 
#' @export
cor.with <- function(df, var, with = colnames(df),  group = NULL, dp = 2){
  
  require(tidyverse)

  if (length(with) != length(colnames(df))) {
    with <- c(var, with) ## adds variable for users
  } 
  
  if (length(group) == 0 ) {
    
    result <- cor(df[, with], use = "complete.obs") [ , var]
    if(length(var) == 1){
      result[length(result) + 1] <- nrow(na.omit(df[, with]))
      names(result)[length(result)] <- "N"
    } else if (length(var)>1){
      result <- data.frame(result)
      result[,"N"] <- nrow(na.omit(df[, with]))
    }
    
    na.count <- nrow(df) - nrow(na.omit(df[, with]))
    na.perc <- na.count/nrow(df)
    
    message("complete observations were used to compute correlations")
    message(paste0(na.count, " (", round(na.perc,dp), "%) observations were removed after listwise deletion"))
    
    return(result)
    
  } else {
    
    df.grp <- as.vector(unlist(unique(df[, group])))
    cor.matrix <- c()
    
    message("complete observations were used to compute correlations within each group")
    
    while (length(df.grp) > 0) { # for each factor in group
      
      with <- with[with!=group] # removes group from with variables
      subset.rows <- which(df[, group] == df.grp[1])
      ## subset out group
      temp.df <- df[subset.rows, ]
      
      final.count <- nrow(na.omit(temp.df[, with]))
      na.count <- nrow(temp.df) - final.count
      na.perc <- na.count/nrow(temp.df)
      
      temp.row <- cor(temp.df[, with], use = "complete.obs") [ , var]
      temp.row[length(temp.row) + 1] <- final.count
      names(temp.row)[length(temp.row)] <- "N"
      
      message(paste0(na.count, " (", round(na.perc,dp), "%) observations were removed from group ", df.grp[1]))
      
      ## add temp.row by row
      cor.matrix <- rbind(cor.matrix, temp.row)
      
      ## closing while loop
      df.grp <- df.grp[-1]
    }
    
    ## give row names
    rownames(cor.matrix) <- as.vector(unlist(unique(df[, group])))
    ## give col names
    colnames(cor.matrix) <- c(with, "N")
    message("complete observations are used to compute correlations with each group")
    
    return(cor.matrix)
  }
}

#' Regression with Grouping Variable
#' 
#' Perform separate regressions for each unique group
#'
#' @param form Input formulae as string
#' @param df data.frame object
#' @param group Grouping variable 
#' @param output Desired statistical output. Set to "Estimate", "Std. Error", "t value", or "Pr(>|t|)"
#' @param group Grouping variable. If specified, correlation will be performed within each group separately
#' @param dp Percentage of missing data will be reported. dp allows re-specification of number of decimal places
#' @return data.frame object
#' @examples 
#' library(car)
#' regress.with("mpg ~ wt + cyl",group="am", mtcars, output = "Estimate")
#' regress.with("mpg ~ wt + cyl",group="am", mtcars, output = 2) # standard error
#' 
#' @export
regress.with <- function(form, df, group, output = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), dp = 2){
  
  require(tidyverse)
    
  form <- as.formula(form)
    
  df.grp <- as.vector(unlist(unique(df[, group])))
    
  cor.matrix <- c()
    
  while (length(df.grp) > 0) { # for each factor in group
      
      subset.rows <- which(df[, group] == df.grp[1])
      ## subset out group
      temp.df <- df[subset.rows, ]
      
      temp.row <- c() ## initializing
      
        model <- lm(form, data = temp.df)
        
        model.sum <- summary(model)
        
        count <- nrow(temp.df) - length(model$na.action)
        
        value <- model.sum$coefficients[, output[1]]
        value[length(value)+1] <- count
        names(value)[length(value)] <- "N"
        
        temp.row <- rbind(temp.row, value)
        
        if(length(model$na.action)>0){
          message(paste("removed", length(model$na.action), "(", round(100*length(model$na.action)/nrow(temp.df),2), "%)", "observations due to missingness in", df.grp[1]))
        }
      
      ## add temp.row by row
      cor.matrix <- rbind(cor.matrix, temp.row)
      
      ## closing while loop
      df.grp <- df.grp[-1]
      
    }
    
    # give row names
    rownames(cor.matrix) <- as.vector(unlist(unique(df[, group])))
    
    return(as.data.frame(cor.matrix))
}


locate.element <- function(form){  # gets position of variables
  
  between <- str_locate_all(form, "[[:space:]]|[[+]]|[[-]]|[[*]]|[[~]]")[[1]][,1] 
  betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]] # wont work if within {}
  
  if (length(betw.brack > 0)){
    ignore <- c()
    for(i in 1:nrow(betw.brack)) {
      ignore <- c(ignore, betw.brack[i, 1]:betw.brack[i, 2])
    }
    
    between <- between[!between %in% ignore]
    
  } else {NULL}
  
  length <- 1:nchar(form)
  text <- length[!length %in% between]
  
  return(unname(text))
}

locate.operator <- function(form){# gets position of operators
  
  between <- str_locate_all(form, "[[+]]|[[-]]|[[*]]|[[~]]")[[1]][,1]
  betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]] # wont work if within {}
  
  if (length(betw.brack > 0)){
    
    ignore <- c()
    
    for(i in 1:nrow(betw.brack)) {
      ignore <- c(ignore, betw.brack[i, 1]:betw.brack[i, 2])
    }
    
    between <- between[!between %in% ignore]
    
  } else {NULL}
  
  return(unname(between))
}

break.formulae <- function(form){ # extracts main variables
  
  require(stringr)
  
  ## remove white spaces
  form <- gsub(" ", "", form)
  
  text <- locate.element(form)
  
  group.text <- list()
  
  for (i in 1:length(text)){
    
    if( i == 1 ) { # store first char
      
      group.text[[1]] <- text[1]
      count <- 1
      
    } else if ( text[i] == text[i-1] + 1 ) {
      
      group.text[[count]] <- c(group.text[[count]], text[i])
      
    } else if ( text[i] != text[i-1] + 1 ) {
      
      group.text[[count + 1]] <- text[i]
      count <- count + 1
      
    }
  }
  elements <- c()
  
  for (i in 1:length(group.text)){
    
    elements <- c(elements, substr(form, min(group.text[[i]]), max(group.text[[i]])))
    
  }
  
  return (elements )
}

extract.variable <- function(form, df){ # extract variables from data according to formulae
  
  template <- break.formulae(form)
  
  # change {} to .
  for (i in 1:length(template)){
    
    betw.brack <- str_locate_all(template[i], "(?<=\\{).+?(?=\\})")[[1]]
    
    # replacing {} with .
    if (length(betw.brack) > 0){ 
      
      expr <- paste0("{", substr(template[i], betw.brack[1,1], betw.brack[1, 2]), "}")
      
      template[i] <- gsub(
        pattern = expr,
        replacement = ".", template[[i]], fixed = T)
      
      
    } 
  }
  
  # replacing X with .
  for (i in 1:length(template)){
    
    template[[i]] <- gsub("X", ".", template[[i]] )
    
  }
  
  # getting rid of redundant templates
  template <- as.vector(unlist(unique(template)))
  
  # initialising list
  variable.list <- vector(mode = "list", length = length(template))
  
  names(variable.list) <- template
  
  require(gtools)
  
  for (i in 1:length(template)){
    
    variable.list[[i]] <- mixedsort(colnames(df)[grep(names(variable.list)[i], colnames(df))])
  }
  
  return(as.vector(unlist(variable.list)) )
  
}

simple.operation <- function(expr) { # performs simple operations given a math statement
  
  expr <- gsub(" ", "", expr)
  
  if(length(locate.operator(expr))==0){
    return(expr)
  }
  
  operator <- substr(expr, locate.operator(expr), locate.operator(expr)) 
  
  first <- break.formulae(expr)[1]
  second <- break.formulae(expr)[2]
  
  if(operator == "+"){
    
    as.numeric(first) + as.numeric(second)
    
  } else if (operator == "-"){
    
    as.numeric(first) - as.numeric(second)
    
  } else if (operator == "*"){
    
    as.numeric(first) * as.numeric(second)
    
  } 
}
  
construct.formulae <- function(form, variables, df){
    
    formulae.list <- list()
    
    for ( i in 1:length(variables)){
      
      betw.brack <- str_locate_all(form, "(?<=\\{).+?(?=\\})")[[1]] # position between {}
      
      # dealing with {}
      if (length(betw.brack) > 0){
        
        expr <- c() # initialise
        
        for (j in 1:nrow(betw.brack)){ # for each {}
          expr <- c(expr, substr(form, betw.brack[j,1], betw.brack[j, 2]))
        }
        
        expr <- gsub ("X", i, expr)
        
        for (j in  1:length(expr)){ # for each expression
          expr[j] <- simple.operation(expr[j] )
        } 
        
        
        ### sub expr back to {}
        temp.form <- form
        for (j in 1:nrow(betw.brack)){ # for each {}
          
          temp.expr <- paste0("{", substr(form, betw.brack[j,1], betw.brack[j, 2]), "}")
          temp.form <- gsub(temp.expr, expr[j], temp.form, fixed = T)
          
        }
        
        ### sub X to i
        temp.form <- gsub("X", i, temp.form)
        
        temp.form.elements <- as.vector(unlist(break.formulae(temp.form)))
        
        check <- sum(temp.form.elements %in% colnames(df))
        
        if ( check == length(temp.form.elements)){ # if variables don't match data, then skip
          
          formulae.list <- c(formulae.list, as.formula(temp.form))
          
        }
        
        
      } else { # if no {}
        
        ### sub X to i
        temp.form <- form
        
        temp.form <- gsub("X", i, temp.form)
        
        temp.form.elements <- as.vector(unlist(break.formulae(temp.form)))
        
        check <- sum(temp.form.elements %in% colnames(df))
        
        if ( check == length(temp.form.elements)){
          
          formulae.list <- c(formulae.list, as.formula(temp.form))
          
        }
        
      }
      
    }
    
    return(formulae.list)
    
  }



#' Abstract Regression
#' 
#' Perform linear regressions given an abstract formulae. 
#' This is especially useful for regressing variables over time.
#' Say we have a variable called e in our data that is indexed based on time, 
#' e1, e2, e3, etc (this assumes our data is in wide format). 
#' We can use the abstract formulae "e\{X\} ~ e\{X-1\}" to use e at previous timepoints
#' to predict e at the next time step. 
#' Typically, we get a single estimate of this relationship (or autocorrelation). 
#' However, abstract regression runs a regression separately at each timestep, for
#' e1 ~ e2, then e2 ~ e3, etc. 
#' To use this function, simply replace the index of your variable of interest with 
#' the symbol "X". The  are necessary if any computations such \{X+1\} or \{X-2\} are used
#'
#' @param form Input formulae string.
#' @param df data.frame object
#' @return data.frame object
#' @examples 
#' df <- data.frame(
#'       x = rnorm(10),
#'       e1 = rnorm(10),
#'       e2 = rnorm(10),
#'       e3 = rnorm(10),
#'       e4 = rnorm(10)
#'       )
#' abstract.regression("e{X} ~ e{X-1}", df)
#' abstract.regression("e{X} ~ e{X-1} + x", df)
#' abstract.regression("e{X} ~ e{X-1} * x", df)
#' 
#' @export
abstract.regression <- function(form, df){
  elements <- break.formulae(form)
  
  variables <- extract.variable(form, df) # variables from data
  
  if(length(grep("X", form)) == 0){
    forms <- form # only one formulae needed
  } else {
    forms <- construct.formulae(form, variables, df) # possible formulaes for modelling
  }
  
  ######
  
  model.list <- list()
  
  for (i in 1:length(forms)){ # for each model
    model.list[[i]] <- summary(lm(forms[[i]], data = df))
  }
  
  for (i in 1:length(forms)){ # for each model
    if (i == 1) {
      coefs <-  model.list[[i]]$coefficients[2:nrow(model.list[[i]]$coefficients), c("Estimate", "Std. Error", "Pr(>|t|)")]
    } else {
      coefs <- rbind (coefs, 
                      model.list[[i]]$coefficients[2:nrow(model.list[[i]]$coefficients),c("Estimate", "Std. Error", "Pr(>|t|)")])
    }
    
  }
  
  if (nrow(coefs)==length(forms)){
    for (i in 1:length(forms)){
      rownames(coefs)[i] = format(forms[[i]])
    }
  }
  
  print(forms) # print formulaes for checking
  return(as.data.frame(coefs))
}

add.meta.weight <- function(df, estimate = "estimate", se = "se", group = NULL, random = T, na.rm = T) {
  
  df[, "within.study.variance"] <- NA
  df[, "within.study.variance"] <- df[, se]^2
  df[, "w"] <- NA
  df[, "w"] <- 1/df[, "within.study.variance"]
  
  # calculating weights
  if (random == F){
    
    df[, "weight"] <- NA
    df[,"weight"] <-  df[, "w"]
    
  } else if (random == T & is.null(group)){
    
    q <- c()
    q <- sum( df[, "w"] * (df[, estimate] - mean(df[, estimate], na.rm = na.rm))^2, na.rm = na.rm)
    dfree <- c()
    dfree <- sum(!is.na(df[, se])) - 1
    c <- c()
    c <- sum( df[, "w"], na.rm = na.rm) - sum(( df[, "w"])^2, na.rm = na.rm)/sum( df[, "w"], na.rm = na.rm)
    
    df[,"between.study.variance"] <- NA
    df[,"between.study.variance"] <- (q - dfree)/c
    
    if (TRUE %in% (df[,"between.study.variance"] < 0)) {
      
      warning("there are negative between study variances. Defaulting to fixed effects meta-analysis")
      df[, "weight"] <- NA
      df[,"weight"] <-  df[, "w"]
      
    } else {
      df[,"weight"] <- NA
      df[,"weight"] <- 1/(df[ ,"within.study.variance"] + df[,"between.study.variance"])
      
    }
    
  } else if (random == T & !is.null(group)){
    
    grp <- unique(df[, group])
    
    for (i in 1:length(grp)) {
      
      grp.row <- which(df[, group] == grp[i])
      
      q <- c()
      q <- sum(df[grp.row, "w"] * (df[grp.row, estimate] - mean(df[grp.row, estimate], na.rm = na.rm))^2, na.rm = na.rm)
      dfree <- c()
      dfree <- sum(!is.na(df[grp.row, se])) - 1
      c <- c()
      c <- sum(df[grp.row,"w"], na.rm = na.rm) - sum((df[grp.row,"w"])^2, na.rm = na.rm)/sum(df[grp.row,"w"], na.rm = na.rm)
      
      df[grp.row,"between.study.variance"] <- NA
      df[grp.row,"between.study.variance"] <- (q - dfree)/c
      
      df[grp.row,"weight"] <- NA
      df[grp.row,"weight"] <- 1/(df[grp.row, "within.study.variance"] + df[grp.row, "between.study.variance"])
      
    }
    
    if (TRUE %in% (df[,"between.study.variance"] < 0)) {
      
      warning("there are negative between study variances. Defaulting to fixed effects meta-analysis")
      df[, "weight"] <- NA
      df[,"weight"] <-  df[, "w"]
      
    } 
    
  } 
  
  df[,"weighted.est"] <- NA
  df[,"weighted.est"] <- df[,"weight"]*df[,estimate]
  
  return(df)
  
}

meta.sum <- function(meta.df, group = NULL, na.rm = T) {
  
  if (is.null(group)){
    
    result <- data.frame(
      
      estimate = sum(meta.df$weighted.est,na.rm = na.rm)/sum(meta.df$weight, na.rm = na.rm),
      
      se = sqrt(1/sum(meta.df$weight, na.rm = na.rm))
      
      
    )
    
    result$t <- result$estimate/result$se
    result$p = 2*(1-pnorm(result$t))
    
    
    
    return(result)
    
  } else {
    
    grp <- unique(meta.df[, group])
    
    estimate <- c()
    se <- c()
    
    for (i in 1:length(grp)) {
      
      grp.row <- which(meta.df[, group] == grp[i])
      
      estimate_temp <- sum(meta.df[grp.row, "weighted.est"], na.rm=na.rm)/sum(meta.df[grp.row,"weight"], na.rm=na.rm)
      
      estimate <- c(estimate, estimate_temp)
      
      se_temp <-  sqrt(1/sum(meta.df[grp.row, "weight"], na.rm = na.rm))
      
      se <- c(se, se_temp)
      
      
    }
    
    result <- data.frame(
      
      estimate = estimate,
      
      se = se,
      
      group = grp
      
    )
    
    result$t <- result$estimate/result$se
    result$p = 2*(1-pnorm(result$t))
    
    return(result)
    
    
  }
}

#' Meta-analysis with Grouping Variable
#'
#' @param df data.frame object
#' @param estimate Regression estimates
#' @param se Regression standard errors
#' @param group Grouping variable
#' @param random If set to TRUE, random-effects meta-analysis will be run. If set to FALSE, fixed-effects meta-analysis will be run
#' @param na.rm If set to TRUE, NAs will be ignored in computation of weights
#' @return data.frame object
#' @examples 
#' df <- data.frame(
#'       estimate = c(1,2,3,2,1,4),
#'       se = c(.1,.5,.3,.8,1,.7),
#'       group = c("a","a","a","b","b","b")
#'       )
#' meta.with(df, group = "group")
#' 
#' @export
meta.with <- function(df, estimate = "estimate", se = "se", group = NULL, random = T, na.rm = T){
  meta.df <- add.meta.weight(df, estimate = estimate, se = se, group = group, random = random, na.rm = na.rm)
  return(meta.sum(meta.df, group = group, na.rm = na.rm))
}


#' rowSD similar to rowMeans and rowSums
#'
#' @param df data.frame object
#' @param na.rm If set to TRUE, NAs are ignore in computation
#' @return data.frame object
#' @examples
#' set.seed(100) 
#' df <- data.frame(
#'       a = rnorm(10),
#'       b = rnorm(10),
#'       c = rnorm(10)
#'       )
#' rowSD(df)
#' 
#' @export
rowSD <- function(df,na.rm = T) {
  return( apply(df, 1, function(x){sd(x, na.rm = na.rm)}))
}


#' Prepare ROC
#' 
#' Computes number of true positives, false positive, true negatives,
#' and false negatives for each participant
#'
#' @param df data.frame object
#' @param positive Variables which has the "correct" answer of 1
#' @param negative Variables which has the "correct" answer of 0
#' @return data.frame object
#' @examples
#' set.seed(100)
#' df <- data.frame(
#'      p1 = sample(0:1, size = 10, replace = T),
#'      p2 = sample(0:1, size = 10, replace = T),
#'      n1 = sample(0:1, size = 10, replace = T),
#'      n2 = sample(0:1, size = 10, replace = T)
#'      )
#' prepare.roc(df, positive = c("p1","p2"), negative = c("n1","n2"))
#' 
#' @export
prepare.roc <- function(df, positive, negative){
  
  true.positive <- rep(0, nrow(df))
  false.positive <- rep(0, nrow(df))
  false.negative <- rep(0, nrow(df))
  true.negative <- rep(0, nrow(df))
  
  for(i in 1:nrow(df)){
    
    for(j in 1:length(positive)){
      if (is.na(df[i, positive[j]])){
        next
        
      } else if (df[i , positive[j]] == 1) { # if participant respond positive for positive questions
        
        true.positive[i] <-  true.positive[i] + 1 # add count for true positive
        
      } else if (df[i , positive[j]] == 0) { # if participant respond negative for positive questions
        
        false.positive[i] <- false.positive[i] + 1
        
      }
    }
    
    for(j in 1:length(negative)){
      
      if(is.na(df[i , negative[j]])){
        
        NULL
        
      } else if (df[i , negative[j]] == 1) { # if participant respond positive for negative questions
        
        false.negative[i] <-  false.negative[i] + 1 # add count for true positive
        
      } else if (df[i , negative[j]] == 0) { # if participant respond negative for negative questions
        
        true.negative[i] <- true.negative[i] + 1
        
      }
    }
  }
  
  result <- data.frame(true.positive,
                       false.positive,
                       false.negative,
                       true.negative)
  
  return(result)
}

#' Remove Outliers
#' 
#' Changes outliers to NAs. Outliers are determined based on how
#' far they are away from the mean. If they cross the specified threshold
#' (default as 2 SDs away from mean), then its is treated as an outlier
#'
#' @param x vector object
#' @param sd.away Number of SDs away from mean. Used to compute threshold for outliers.
#' @param na.rm If set to TRUE, NAs are ignored in computation
#' @param print.outlier If set to TRUE, number of outliers will be printed
#' @return vector object
#' @examples
#' set.seed(100)
#' x <- rnorm(100)
#' remove.outlier(x)
#' 
#' @export
remove.outlier <- function(x, sd.away = 2, na.rm = T, print.outlier = TRUE){
  
  lower <- mean(x, na.rm = na.rm) - sd.away * sd(x, na.rm = na.rm)
  upper <- mean(x, na.rm = na.rm) + sd.away * sd(x, na.rm = na.rm)
  
  if(print.outlier == T){
    print(paste(length(x[x <= lower | x >= upper]), "outlier(s) removed"))
  } 
  
  x[x <= lower | x >= upper] <- NA
  return(x)
}

