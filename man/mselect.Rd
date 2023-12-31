% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mselect.R
\name{mselect}
\alias{mselect}
\title{Best subsets regression}
\usage{
mselect(y, X, method = "backward", criterion = "aic", nbest = 3)
}
\arguments{
\item{y}{Numeric response vector.}

\item{X}{Model matrix (with a vector of 1's in the first column for the intercept term)}

\item{method}{Search method. Can be one of \code{c("backward", "forward", "exhaustive", "seqrep")}.}

\item{criterion}{Criterion to use to select final model. Can be one of
\code{c("adjr2", "aic", "bic", "cp")}}

\item{nbest}{Number of models to consider of each model size.}
}
\value{
A vector of 0's and 1's indicating which variables were selected.
}
\description{
This is a wrapper to compute a 0/1 vector for inclusion of each candidate variable.
The underlying methodology is from the \code{leaps} package.
}
