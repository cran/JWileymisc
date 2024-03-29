% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/styler.R
\name{APAStyler.modelTest.lm}
\alias{APAStyler.modelTest.lm}
\title{APAStyler method for model tests from a linear model}
\usage{
\method{APAStyler}{modelTest.lm}(
  object,
  format = list(FixedEffects = c("\%s\%s [\%s, \%s]"), EffectSizes = c("f2 = \%s, \%s")),
  digits = 2,
  pcontrol = list(digits = 3, stars = TRUE, includeP = FALSE, includeSign = FALSE,
    dropLeadingZero = TRUE),
  ...
)
}
\arguments{
\item{object}{A \code{modelTest.lm} class object,
results from running \code{modelTest()} function on a
class \code{lm} object.}

\item{format}{A list giving the formatting style to be used for
the fixed effecvts and effect sizes.}

\item{digits}{A numeric value indicating the number of digits to print.
This is still in early implementation stages and currently does not
change all parts of the output (which default to 2 decimals per
APA style).}

\item{pcontrol}{A list controlling how p values are formatted.}

\item{...}{Additional arguments.}
}
\value{
Styled results.
}
\description{
APAStyler method for model tests from a linear model
}
\examples{
m1 <- lm(mpg ~ qsec * hp, data = mtcars)
APAStyler(modelTest(m1))

APAStyler(modelTest(m1),
format = list(
  FixedEffects = "\%s, \%s\n(\%s, \%s)",
  EffectSizes = "Cohen's f2 = \%s (\%s)"),
pcontrol = list(digits = 4,
  stars = FALSE, includeP = TRUE,
  includeSign = TRUE,
  dropLeadingZero = TRUE))

## clean up
rm(m1)
}
