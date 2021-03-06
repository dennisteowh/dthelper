% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dthelper-handling-missing-data.R
\name{reduce.listwise.deletion}
\alias{reduce.listwise.deletion}
\title{Reduce Listwise Deletion}
\usage{
reduce.listwise.deletion(df, var = colnames(df), drop)
}
\arguments{
\item{df}{data.frame object}

\item{var}{Input column names (defaults to using all columns in df)}

\item{drop}{Number of variables to drop. It is recommended to test a few different values starting from 1}
}
\value{
Single row data.frame object that reports number of missing data (partially) caused by each variable
}
\description{
By specifying a drop value, X, this function returns the number of 
missing data that is reduced by dropping X number of variables. 
The function returns summary statistics of the missing data (partially)
caused by each variable. This allows the user to quickly assess which variables
lead to the highest number of listwise deletions
}
