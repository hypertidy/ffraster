library(testthat)
context("ffraster")

library(ffraster)
dm <- c(25, 43, 88)
a <- array(1:(prod(dm)), dm)

va <- ffrarr(dm, "double", sprintf("%s.grd", tempfile()), readonly = FALSE)
for (i in seq(dim(a)[3])) va[,,i] <- a[,,i]
test_that("ffrarr works", {
  expect_that(va, is_a("ff"))
  expect_that(dim(va), equals(dm))
  })

# library(raster)
# b <- brick(a)
# fn <- rasterTmpFile()
# dt <- "FLT8S"
# ffraster:::.writeGRD(b, dataType = dt, filename = fn)
# ar <- ffrarr(dim(b), mode = dt, filename = fn)
