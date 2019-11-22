context("CheckVals")

test_that("CheckVals works", {
  expect_true(CheckVals(mtcars$cyl, c(4, 6, 8)))
  expect_true(CheckVals(mtcars[, c("cyl", "am")], c(0, 1, 4, 6, 8)))
  expect_true(CheckVals(as.matrix(mtcars[, c("cyl", "am")]), c(0, 1, 4, 6, 8)))
  expect_error(CheckVals(mtcars[, c("cyl", "am")], c(0, 4, 6, 8)))

  expect_error(
    CheckVals(as.POSIXct(1:10, origin = "1970-01-01 00:00:00"),
              okay = 1:10),
    "not a valid class")
})


context("score")

test_that("score warnings and errors", {
  expect_warning(
    score(mtcars[, 1:3], mean = FALSE, na.rm = TRUE),
    "Summing is not meaningful for missing values.")

  expect_error(
    score(mtcars[, 1:3],
          limits = c(-3, 30),
          rev = 2),
    "Cannot reverse score scale that can take on negative values.")

  expect_is(
    score(mtcars[, 1:3], mean = FALSE, reliability = FALSE,
          na.rm = FALSE),
    "list")

})

context(".scoreCESD and scaleScore")

test_that(".scoreCESD works", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 20 * 5, TRUE), ncol = 20)
  expect_warning(xc <- .scoreCESD(x))
  expect_is(xc, "list")

  expect_is(.scoreCESD(x, reliability = FALSE), "list")

  expect_is(scaleScore(x, type = "CESD", reliability = FALSE), "list")

  expect_is(scaleScore(data.table::as.data.table(x),
                       type = "CESD", reliability = FALSE), "list")

  expect_match(names(xc), "score|reliability")
})

context(".scoreLOTR and scaleScore")

test_that(".scoreLOTR and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:5, size = 6 * 5, TRUE), ncol = 6)
  expect_warning(xc <- .scoreLOTR(x))
  expect_is(xc, "list")

  expect_is(.scoreLOTR(x, reliability = FALSE), "list")

  expect_is(scaleScore(x, type = "LOTR", reliability = FALSE), "list")

  expect_is(scaleScore(data.table::as.data.table(x),
                       type = "LOTR", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

context(".scoreMastery and scaleScore")

test_that(".scoreMastery and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:4, size = 7 * 5, TRUE), ncol = 7)
  expect_warning(xc <- .scoreMastery(x))
  expect_is(xc, "list")

  expect_is(.scoreMastery(x, reliability = FALSE), "list")

  expect_is(scaleScore(x, type = "Mastery", reliability = FALSE), "list")

  expect_is(scaleScore(data.table::as.data.table(x),
                       type = "Mastery", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

context(".scoreMOSSSS and scaleScore")

test_that(".scoreMOSSSS and scaleScore work", {
  set.seed(5234)
  x <- MASS::mvrnorm(n = 200, mu = rep(0, 20),
                     Sigma = matrix(.5, 20, 20) + diag(20),
                     empirical = TRUE)
  x <- apply(x, 2, function(y) {
    as.integer(cut(y, c(-Inf, -1, -.3, .3, 1, Inf),
                   labels = 1:5))
  })

  expect_warning(xc <- .scoreMOSSSS(x))
  expect_is(xc, "list")

  expect_warning(scaleScore(x, type = "MOSSSS"))

  expect_match(names(xc), "score|reliability")
})

context(".scorePANAS and scaleScore")

test_that(".scorePANAS and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(1:5, size = 20 * 5, TRUE), ncol = 20)
  expect_warning(xc <- .scorePANAS(x))
  expect_is(xc, "list")

  expect_is(.scorePANAS(x, reliability = FALSE), "list")

  expect_is(scaleScore(x, type = "PANAS", reliability = FALSE), "list")

  expect_is(scaleScore(data.table::as.data.table(x),
                       type = "PANAS", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

context(".scoreRSES and scaleScore")

test_that(".scoreRSES and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 10 * 5, TRUE), ncol = 10)
  expect_warning(xc <- .scoreRSES(x))
  expect_is(xc, "list")

  expect_is(.scoreRSES(x, reliability = FALSE), "list")

  expect_is(scaleScore(x, type = "RSES", reliability = FALSE), "list")

  expect_is(scaleScore(data.table::as.data.table(x),
                       type = "RSES", reliability = FALSE), "list")
  expect_match(names(xc), "score|reliability")
})

context(".scoreMOOD and scaleScore")

## weakest tests as not sure what MOOD scores should be??
test_that(".scoreMOOD and scaleScore work", {
  set.seed(1234)
  x <- matrix(sample(0:3, size = 21 * 5, TRUE), ncol = 21)
  expect_warning(xc <- .scoreMOOD(x))
  expect_is(xc, "list")
})
