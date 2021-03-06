% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dthelper-computation-functions.R
\name{remove.outlier}
\alias{remove.outlier}
\title{Remove Outliers}
\usage{
remove.outlier(x, sd.away = 2, na.rm = T, print.outlier = TRUE)
}
\arguments{
\item{x}{vector object}

\item{sd.away}{Number of SDs away from mean. Used to compute threshold for outliers.}

\item{na.rm}{If set to TRUE, NAs are ignored in computation}

\item{print.outlier}{If set to TRUE, number of outliers will be printed}
}
\value{
vector object
}
\description{
Changes outliers to NAs. Outliers are determined based on how
far they are away from the mean. If they cross the specified threshold
(default as 2 SDs away from mean), then its is treated as an outlier
}
\examples{
set.seed(100)
x <- rnorm(100)
remove.outlier(x)

}
