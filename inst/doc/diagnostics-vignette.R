## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(JWileymisc)

## ------------------------------------------------------------------------

test <- testDistribution(mtcars$mpg, "normal")
head(test$Data)
table(test$Data$isEV)

plot(test)


## ------------------------------------------------------------------------
test <- testDistribution(mtcars$mpg, "normal",
                         extremevalues = "theoretical",
                         ev.perc = .10)
## view the data with extreme values
head(test$Data)

## count how many extreme values there are
table(test$Data$isEV)

## plot the distribution
plot(test)


## show which values are extreme
test$Data[isEV == "Yes"]

## view extreme values on mpg in the original dataset
## by use the original order, the original rows to select
## the correct rows from the original dataset
mtcars[test$Data[isEV == "Yes", OriginalOrder], ]



## ------------------------------------------------------------------------
test <- testDistribution(mtcars$mpg, "normal",
                         extremevalues = "empirical",
                         ev.perc = .10)
head(test$Data)
table(test$Data$isEV)

plot(test)


## ------------------------------------------------------------------------
testN <- testDistribution(mtcars$mpg, "normal",
                          extremevalues = "theoretical",
                          ev.perc = .05)
testG <- testDistribution(mtcars$mpg, "gamma",
                          extremevalues = "theoretical",
                          ev.perc = .05)

## compare the log likelihood assuming a normal or gamma distribution
testN$Distribution$LL
testG$Distribution$LL

plot(testN)
plot(testG)


## ---- fig.height = 10----------------------------------------------------

m <- lm(mpg ~ hp * factor(cyl), data = mtcars)

md <- modelDiagnostics(m, ev.perc = .05)

plot(md, ncol = 1)


## ------------------------------------------------------------------------
## show extreme values
md$extremeValues

## show extreme values in overall dataset
mtcars[md$extremeValues$Index, 1:4]


## ---- results = "hide"---------------------------------------------------

## exclude extreme values
m2 <- lm(mpg ~ hp * factor(cyl), data = mtcars[-md$extremeValues$Index, ])

## show a summary of coefficients from both models
## and the percent change
round(data.frame(
  M1 = coef(m),
  M2 = coef(m2),
  PercentChange = coef(m2) / coef(m) * 100 - 100), 2)


## ---- echo = FALSE, results = "asis"-------------------------------------
if (requireNamespace("pander", quietly = TRUE)) {
pander::pandoc.table(round(data.frame(
  M1 = coef(m),
  M2 = coef(m2),
  PercentChange = coef(m2) / coef(m) * 100 - 100), 2),
  justify = "left")
} else {
  ""
}

## ---- fig.height = 10----------------------------------------------------

## diagnostics after removing outliers from first model
md2 <- modelDiagnostics(m2, ev.perc = .05)

plot(md2, ask = FALSE, ncol = 1)

## show (new) extreme values
md2$extremeValues


