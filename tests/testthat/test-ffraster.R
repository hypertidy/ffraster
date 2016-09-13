library(testthat)
context("ffraster")

library(ffraster)
dm <- c(25, 43, 88)
a <- array(1:(prod(dm)), dm)

va <- ffrarr(dm, "FLT8S", sprintf("%s.grd", tempfile()))
for (i in seq(dim(a)[3])) va[,,i] <- a[,,i]
test_that("ffrarr works", {
  expect_that(va, is_a("ff"))
  expect_that(dim(va), equals(dm))
  expect_equal(a[,3,], ff::as.ram(va[,3,]))
})

# library(raster)
# b <- brick(a)
# fn <- rasterTmpFile()
# dt <- "FLT8S"
# ffraster:::.writeGRD(b, dataType = dt, filename = fn)
# ar <- ffrarr(dim(b), mode = dt, filename = fn)
