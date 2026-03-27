library(testthat)
library(EnvCpt)

fake_envcpt <- list(
  summary = matrix(
    c(10, 20,
      1,  2),
    nrow = 2,
    byrow = TRUE
  )
)
class(fake_envcpt) <- "envcpt"

test_that("AIC.envcpt respects custom penalty k", {
  out <- AIC(fake_envcpt, k = 3)
  expect_equal(out, c(13, 26))
})

test_that("AICweights.default returns fallback message", {
  out <- AICweights(1:5)
  expect_equal(out, "No default method created for S3 class AICweights.")
})

test_that("AICweights.envcpt returns normalized weights", {
  w <- AICweights(fake_envcpt)
  aic <- AIC(fake_envcpt)
  expected <- exp(-0.5 * (aic - min(aic)))
  expected <- expected / sum(expected)
  expect_equal(w, expected)
})