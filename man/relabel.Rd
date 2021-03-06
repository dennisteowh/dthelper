% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dthelper-collapse-recode-variables.R
\name{relabel}
\alias{relabel}
\title{Relabel Single Variable}
\usage{
relabel(x, from, to = 1:length(from))
}
\arguments{
\item{x}{vector object}

\item{from}{Original scale (order of input needs to match desired scale)}

\item{to}{Desired scale (order of input needs to match original scale). Defaults to integer scale starting from 1}
}
\value{
vector object
}
\description{
Relabel Single Variable
}
\examples{
x <- c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree")

relabel(x, from = c("Strongly Disagree","Disagree", "Neutral"," Agree", "Strongly Agree"),
to = c("SD","D","N","A","SA"))

}
