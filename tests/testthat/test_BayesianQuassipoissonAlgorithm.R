context("QuasipoissonAlgorithm")

test_that("Sandefjord significantByThreshold vs significantByConfidenceIntervals", {
  library(data.table)
  if(interactive()){
    BASE <- "/packages/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"data","formatted_oslo.RDS"))
  setnames(d,"influensa","n")
  res <- QuasipoissonAlgorithm(dataset=d,isDaily=F)

  significantByThreshold <- res[n>threshold2]
  significantByConfidenceIntervals <- res[cumL1>0]
  expect_equal(significantByThreshold,significantByConfidenceIntervals)
})

test_that("Sandefjord significantByThreshold vs significantByZScore", {
  library(data.table)
  if(interactive()){
    BASE <- "/packages/dashboards_sykdomspuls/tests/testthat"
  } else {
    BASE <- getwd()
  }
  d <- readRDS(file.path(BASE,"results","formatted_sandefjord.RDS"))
  setnames(d,"influensa","n")
  setnames(d,"consultWithInfluensa","consult")
  res <- QuasipoissonAlgorithm(dataset=d,isDaily=F)

  significantByThreshold <- res[n>threshold2]
  significantByZScore <- res[zscore>2]
  expect_equal(significantByThreshold,significantByZScore)
})

