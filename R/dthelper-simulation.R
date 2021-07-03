#' Resample
#' 
#' Resamples from a given vector. Useful for bootstrapping
#'
#' @param x vector object
#' @param n Number of samples to draw
#' @return vector object
#' @examples
#' set.seed(100)
#' x <- rnorm(50)
#' resample(x, 50)
#' @export
resample <- function(x,n){ 
  output <- sample(x, size=n, replace=T)
  return(output)
}


#' Plot Convergence for Simulations
#' 
#' Over each iteration, the function will compute the mean estimate
#' of the simulated statistic for all past iterations.
#' Curve should stabilise if convergence is reached.
#'
#' @param x vector object. Index should match number of iterations
#' @return ggplot object. Plots mean of estimate against number of iterations
#' @examples
#' set.seed(100)
#' flip.coin <- function(n){
#'   return(sample(0:1, size=n, replace = T))
#' }
#' flips <- flip.coin(100)
#' plot.convergence(flips)
#' 
#' @export
plot.convergence <- function(x){
  require(ggplot2)
  df <- data.frame(
    estimate = x,
    iteration = 1:length(x)
  )
  
  df$mean_estimate <- NA
  for(i in 1:nrow(df)){
    df[i, "mean_estimate"] <- mean(x[1:i])
  }
  
  return(ggplot(df,aes(x = iteration, y = mean_estimate))+
           geom_point()
  )
}

#' Plot Density Plot given a samples from a sampling function
#'
#' @param sampler vector object
#' @return density plot
#' @examples
#' set.seed(100)
#' plot.sampler(rnorm(10000))
#' plot.sampler(runif(1000, min = 0, max = 1))
#' 
#' @export
plot.sampler <- function(sampler){
  plot(density(sampler))
}

#' Track Time 
#' 
#' Typically used in a loop. Reports progress at every 10% interval.
#' 
#' @param i Iteration number
#' @param niter Total number of iterations in loop
#' @param start Start time of loop. Use Sys.time() to get start time
#' @param track If set to TRUE, reports progress at every 10% interval
#' @return None. Prints out time differences
#' 
#' @examples 
#' start = Sys.time()
#' for(i in 1:100){
#'   track.time(100, i, start)
#' }
#' 
#' start = Sys.time()
#' for(i in 1:100){
#'   track.time(100, i, start, track =F)
#' }
#' 
#' @export
track.time <- function(i, niter, start, track = TRUE){
  if(track == TRUE){
    interval <- niter/10
    if(i %% interval == 0){ # every 10% progress
      progress <- i/interval
      diff <- Sys.time() - start
      print(paste("Progress:", progress*10, "% completed"))
      print(diff)
    }
  } else{
    if(i == niter){
      diff <- Sys.time() - start
      print(diff)
    }
  }
}

