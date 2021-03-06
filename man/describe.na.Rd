% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dthelper-handling-missing-data.R
\name{describe.na}
\alias{describe.na}
\title{Describe Missing Data}
\usage{
describe.na(
  x,
  dp = 2,
  return.num = FALSE,
  return.perc = FALSE,
  print.text = TRUE
)
}
\arguments{
\item{x}{Vector or data.frame object}

\item{dp}{Number of decimal places for percentage}

\item{return.num}{If set to TRUE, returns number of missing cases}

\item{return.perc}{If set to TRUE, returns percentage of missing cases}

\item{print.text}{If set to FALSE, missing cases are not printed}
}
\value{
Default none. When return.num or return.perc equals TRUE, numeric object is returned
}
\description{
Describe Missing Data
}
