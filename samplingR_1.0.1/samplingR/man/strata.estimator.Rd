% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strata.estimator.R
\name{strata.estimator}
\alias{strata.estimator}
\title{Parameter estimation of stratified data}
\usage{
strata.estimator(
  N,
  Nh,
  data,
  estimator = c("total", "mean", "proportion", "class total"),
  replace = FALSE,
  alpha
)
}
\arguments{
\item{N}{Population size.}

\item{Nh}{Size of each population strata.}

\item{data}{Stratified sample.}

\item{estimator}{Estimator to compute. Can be one of "total", "mean", "proportion", "class total". Default is "total".}

\item{replace}{Whether the sample to be taken can have repeated instances or not.}

\item{alpha}{Optional value to calculate estimation error and build 1-alpha}
}
\value{
A list containing different interest values:
\itemize{
\item estimator
\item variance
\item sampling.error
\item estimation.error
\item confint
}
}
\description{
Function to make estimations of diferent parameters based on
a stratified sample.
}
\details{
Nh length must be equal to number of strata in data.\cr
data is meant to be a returned object of \code{\link{strata.sample}} function.
}
