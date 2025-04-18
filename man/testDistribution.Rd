% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostics.R
\name{testDistribution}
\alias{testDistribution}
\alias{as.testDistribution}
\alias{is.testDistribution}
\alias{testDistribution.default}
\title{Test the distribution of a variable against a specific distribution}
\usage{
testDistribution(x, ...)

as.testDistribution(x)

is.testDistribution(x)

\method{testDistribution}{default}(
  x,
  distr = c("normal", "beta", "chisq", "f", "gamma", "geometric", "nbinom", "poisson",
    "uniform", "mvnormal"),
  na.rm = TRUE,
  starts,
  extremevalues = c("no", "theoretical", "empirical"),
  ev.perc = 0.001,
  use = c("complete.obs", "pairwise.complete.obs", "fiml"),
  robust = FALSE,
  ...
)
}
\arguments{
\item{x}{The data as a single variable or vector to check the distribution unless
the distribution is \dQuote{mvnormal} in which case it should be a data frame or
data table.}

\item{...}{Additional arguments. If these include mu and sigma and the distribution
is multivariate normal, then it will use the passed values instead of calculating
the mean and covariances of the data.}

\item{distr}{A character string indicating the distribution to be tested.
Currently one of: \dQuote{normal}, \dQuote{beta}, \dQuote{chisq} (chi-squared),
\dQuote{f}, \dQuote{gamma}, \dQuote{geometric}, \dQuote{nbinom} (negative binomial),
\dQuote{poisson}, \dQuote{uniform}, or \dQuote{mvnormal} for multivariate normal where Mahalanobis
distances are calculated and compared against a Chi-squared distribution with
degrees of freedom equal to the number of variables.}

\item{na.rm}{A logical value whether to omit missing values. Defaults to \code{TRUE}.}

\item{starts}{A named list of the starting values. Not required for all distributions.
Passed on to \code{fitdistr} which fits the maximum likelihood estimates of the
distribution parameters.}

\item{extremevalues}{A character vector whether to indicate extreme values.
Should be \dQuote{no} to do nothing, \dQuote{empirical} to show extreme
values based on the observed data percentiles, or \dQuote{theoretical}
to show extreme values based on percentiles of the theoretical distribution.}

\item{ev.perc}{Percentile to use for extreme values.  For example if .01,
then the lowest 1 percent and highest 1 percent will be labelled
extreme values.  Defaults to the lowest and highest 0.1 percent.}

\item{use}{A character vector indicating how the moments
(means and covariance matrix) should be estimated in the presence of
missing data when \code{distr = mvnormal}.
The default is to use complete observations, but
full information maximum likelihood based on functions in
\pkg{lavaan} is also available.  See details.}

\item{robust}{A logical whether to use robust estimation or not.
Currently only applies to normally distributed data
(univariate or multivariate).  Also, when \code{robust = TRUE},
only complete observations are used (i.e., \code{use = "complete.obs"}).
See details.}
}
\value{
A logical whether or not an object is of class
  \code{testDistribution} or an object of the same class.

A list with information about the distribution (parameter estimates,
  name, log likelihood (useful for comparing the fit of different distributions
  to the data), and a dataset with the sorted data and theoretical quantiles.
}
\description{
Function designed to help examine distributions.
It also includes an option for assessing multivariate normality using the
(squared) Mahalanobis distance. A generic function, some methods, and
constructor (\code{as.testDistribution}) and function to check class
(\code{is.testDistribution}) also are provided.

Note that for the \code{use} argument, several options are possible.
By default it is \dQuote{complete.obs}, which uses only cases with complete
data on all variables.
Another option is \dQuote{pairwise.complete.obs}, which uses
all available data for each variable indivdiually to estimate the means and
variances, and all pairwise complete observation pairs for each covariance. Because
the same cases are not used for all estimates, it is possible to obtain a covariance
matrix that is not positive definite (e.g., correlations > +1 or < -1).

Finally, the last option is \dQuote{fiml}, which uses full information maximum likelihood
estimates of the means and covariance matrix.  Depending on the number of cases,
missing data patterns, and variables, this may be quite slow and computationally
demanding.

The \code{robust} argument determines whether to use robust estimates or not
when calculating densities, etc.  By default it is \code{FALSE}, but if
\code{TRUE} and a univariate or multivariate normal distribution is tested,
then robust estimates of the means and covariance matrix (a variance if univariate)
will be used based on \code{covMcd} from the \pkg{robustbase} package.
}
\examples{

## example data
set.seed(1234)
d <- data.table::data.table(
  Ynorm = rnorm(200),
  Ybeta = rbeta(200, 1, 4),
  Ychisq = rchisq(200, 8),
  Yf = rf(200, 5, 10),
  Ygamma = rgamma(200, 2, 2),
  Ynbinom = rnbinom(200, mu = 4, size = 9),
  Ypois = rpois(200, 4))

## testing and graphing
testDistribution(d$Ybeta, "beta", starts = list(shape1 = 1, shape2 = 4))
testDistribution(d$Ychisq, "chisq", starts = list(df = 8))

## for chi-square distribution, extreme values only on
## the right tail
testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
  extremevalues = "empirical", ev.perc = .1)
testDistribution(d$Ychisq, "chisq", starts = list(df = 8),
  extremevalues = "theoretical", ev.perc = .1)

\dontrun{

testDistribution(d$Yf, "uniform")
testDistribution(d$Ypois, "geometric")

testDistribution(d$Yf, "f", starts = list(df1 = 5, df2 = 10))
testDistribution(d$Ygamma, "gamma")
testDistribution(d$Ynbinom, "poisson")
testDistribution(d$Ynbinom, "nbinom")
testDistribution(d$Ypois, "poisson")

## compare log likelihood of two different distributions
testDistribution(d$Ygamma, "normal")$Distribution$LL
testDistribution(d$Ygamma, "gamma")$Distribution$LL

testDistribution(d$Ynorm, "normal")
testDistribution(c(d$Ynorm, 10, 1000), "normal",
  extremevalues = "theoretical")
testDistribution(c(d$Ynorm, 10, 1000), "normal",
  extremevalues = "theoretical", robust = TRUE)

testDistribution(mtcars, "mvnormal")

## for multivariate normal mahalanobis distance
## which follows a chi-square distribution, extreme values only on
## the right tail
testDistribution(mtcars, "mvnormal", extremevalues = "empirical",
  ev.perc = .1)
testDistribution(mtcars, "mvnormal", extremevalues = "theoretical",
  ev.perc = .1)

rm(d) ## cleanup
}
}
\seealso{
\code{\link{SEMSummary}}
}
\keyword{multivariate}
