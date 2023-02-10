## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(JWileymisc)
library(ggplot2)
library(data.table)

## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  
#  egltable(c("mpg", "hp", "qsec", "wt", "vs"),
#           data = mtcars)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
          egltable(c("mpg", "hp", "qsec", "wt", "vs"),
                   data = mtcars),
          caption = "Example descriptive statistics table.",
          justify = "left")


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  
#  egltable(c("mpg", "hp", "qsec", "wt", "vs"),
#           data = mtcars, strict=FALSE)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
          egltable(c("mpg", "hp", "qsec", "wt", "vs"),
                   data = mtcars, strict=FALSE),
          caption = "Example descriptive statistics table with automatic categorical variables.",
          justify = "left")


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  
#  egltable(c("mpg", "hp", "qsec", "wt", "vs"),
#    g = "am", data = mtcars, strict = FALSE)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
          egltable(c("mpg", "hp", "qsec", "wt", "vs"), 
                   g = "am", data = mtcars, strict = FALSE),
          caption = "Example descriptive statistics table by group.",
          justify = "left")


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  
#  egltable(c("mpg", "hp", "qsec", "wt", "vs"),
#           g = "am", data = mtcars, strict = FALSE,
#           parametric = FALSE)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
          egltable(c("mpg", "hp", "qsec", "wt", "vs"), 
                   g = "am", data = mtcars, strict = FALSE,
                   parametric = FALSE),
          caption = "Example descriptive statistics table by group.",
          justify = "left")


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  ## example with paired data
#  egltable(
#    vars = "extra",
#    g = "group",
#    data = sleep,
#    idvar = "ID",
#    paired = TRUE)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
egltable(
  vars = "extra",
  g = "group",
  data = sleep,
  idvar = "ID",
  paired = TRUE),
caption = "Example parametric descriptive statistics for paired data.",
justify = "left")


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  egltable(
#    vars = "extra",
#    g = "group",
#    data = sleep,
#    idvar = "ID",
#    paired = TRUE,
#    parametric = FALSE)
#  

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
egltable(
  vars = "extra",
  g = "group",
  data = sleep,
  idvar = "ID",
  paired = TRUE,
  parametric = FALSE),
caption = "Example non parametric descriptive statistics for paired data.",
justify = "left")


## -----------------------------------------------------------------------------

## paired categorical data example
## using data on chick weights to create categorical data
tmp <- subset(ChickWeight, Time %in% c(0, 20))
tmp$WeightTertile <- cut(tmp$weight,
  breaks = quantile(tmp$weight, c(0, 1/3, 2/3, 1), na.rm = TRUE),
  include.lowest = TRUE)


## ---- eval = FALSE, echo = TRUE, results = "hide"-----------------------------
#  egltable(c("weight", "WeightTertile"), g = "Time",
#    data = tmp,
#    idvar = "Chick", paired = TRUE)

## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
egltable(c("weight", "WeightTertile"), g = "Time",
  data = tmp,
  idvar = "Chick", paired = TRUE),
caption = "Continuous and categorical paired data.",
justify = "left")


## -----------------------------------------------------------------------------

m <- SEMSummary(~ mpg + hp + qsec + wt, data = mtcars)

corTab <- APAStyler(m, type = "cor", stars = TRUE)


## ---- echo = FALSE, results = "asis"------------------------------------------

pander::pandoc.table(
          corTab$table,
          caption = "Example correlation table.",
          justify = "left")


## -----------------------------------------------------------------------------

plot(m) +
  ggtitle("Order by hierarchical clustering")

plot(m, order = "asis") +
  ggtitle("Order as written")


## -----------------------------------------------------------------------------

plot(m, type = "p") +
  ggtitle("Numbers are p-values")


## -----------------------------------------------------------------------------

mg <- SEMSummary(~ Sepal.Length + Petal.Length +
                  Sepal.Width + Petal.Width | Species,
                 data = iris)

plot(mg)


## -----------------------------------------------------------------------------

## simulate some likert style data
set.seed(1234)
d <- data.table(
  Happy = sample(1:5, 200, TRUE, c(.1, .2, .4, .2, .1)),
  Cheerful = sample(1:5, 200, TRUE, c(.1, .2, .2, .4, .1)),
  Peaceful = sample(1:5, 200, TRUE, c(.1, .1, .2, .4, .2)),
  Sad = sample(1:5, 200, TRUE, c(.1, .3, .3, .2, .1)),
  Hopeless = sample(1:5, 200, TRUE, c(.3, .3, .2, .2, 0)),  
  Angry = sample(1:5, 200, TRUE, c(.4, .3, .2, .08, .02)))

dmeans <- melt(d, measure.vars = names(d))[,
  .(Mean = mean(value, na.rm = TRUE)), by = variable]

dmeans[, Low := paste0(variable, "\nNot at all")]
dmeans[, High := paste0(variable, "\nExtremely")]
dmeans[, variable := as.integer(factor(variable))]

## view the summarised data
print(dmeans)

gglikert("Mean", "variable", "Low", "High", data = dmeans,
         xlim = c(1, 5),
         title = "Average Affect Ratings")


## -----------------------------------------------------------------------------

## create a grouping variable
dg <- cbind(d, Group = ifelse(
                 d$Happy > mean(d$Happy, na.rm = TRUE),
                 "General Population", "Depressed"))

dgmeans <- melt(dg, measure.vars = names(d), id.vars = "Group")[,
  .(Mean = mean(value, na.rm = TRUE)), by = .(variable, Group)]

dgmeans[, Low := paste0(variable, "\nNot at all")]
dgmeans[, High := paste0(variable, "\nExtremely")]
dgmeans[, variable := as.integer(factor(variable))]

## view the summarised data
print(dgmeans)

gglikert("Mean", "variable", "Low", "High",
         colour = "Group",
         data = dgmeans,
         xlim = c(1, 5),
         title = "Average Affect Ratings") +
  scale_colour_manual(
    values = c("Depressed" = "black",
               "General Population" = "grey70"))


