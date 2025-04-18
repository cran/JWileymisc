% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{internalcompareIV}
\alias{internalcompareIV}
\title{Compares the effects of various independent variables}
\usage{
internalcompareIV(
  dv,
  type = c("normal", "binary", "count"),
  iv,
  covariates = character(),
  data,
  multivariate = FALSE,
  ...
)
}
\arguments{
\item{dv}{A character string of the depentent variable}

\item{type}{A character string indicating the type of dependent variable}

\item{iv}{A character string or vector giving the IV(s)}

\item{covariates}{A character string or vector giving the covariate(s)}

\item{data}{The data to be used for analysis}

\item{multivariate}{A logical value whether to have models with
all IVs simultaneously.}

\item{\ldots}{Additional arguments passed on to the internal 
function, \code{.runIt}.}
}
\value{
A list with all the model results.
}
\description{
This is an internal function designed to run many models to compare the
unique predictive effect of different IVs with and without covariates on an
outcome.
}
\examples{
test1 <- JWileymisc:::internalcompareIV(
  dv = "mpg", type = "normal",
  iv = "hp",
  covariates = "am",
  data = mtcars, multivariate = FALSE)
test1$Summary
rm(test1)
}
\keyword{internal}
