context("dim order")

library(ffraster)
library(raster)
r <- raster(matrix(1:12, 3))

bsq <- writeRaster(brick(r, r * 5), rasterTmpFile(), bandorder = "BSQ")
bil <- writeRaster(brick(r, r * 5), rasterTmpFile(), bandorder = "BIL")
bip <- writeRaster(brick(r, r * 5), rasterTmpFile(), bandorder = "BIP")

nn <- ncell(bsq) * nlayers(bsq)
bsq_raw <-  readBin(ffraster:::gri_filename(bsq), "numeric", n = nn)
bsq_match <- c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12, 5, 20, 35, 50, 10, 25, 
               40, 55, 15, 30, 45, 60)

bil_raw <-  readBin(ffraster:::gri_filename(bil), "numeric", n = nn)
bil_match <- c(1, 4, 7, 10, 5, 20, 35, 50, 2, 5, 8, 11, 10, 25, 40, 55, 3, 
6, 9, 12, 15, 30, 45, 60)
bip_raw <- readBin(ffraster:::gri_filename(bip), "numeric", n = nn)
bip_match <- c(1, 5, 4, 20, 7, 35, 10, 50, 2, 10, 5, 25, 8, 40, 11, 55, 3, 
              15, 6, 30, 9, 45, 12, 60)

ff_bsq <- ff_object(bsq)
ff_bil <- ff_object(bil)
ff_bip <- ff_object(bip)


test_that("dimension order works", {
  expect_equal(values(bsq), values(bil))
  expect_equal(values(bil), values(bip))
  
  expect_equal(bsq_raw, bsq_match)
  expect_equal(bil_raw, bil_match)
  expect_equal(bip_raw, bip_match)

  ## the virtual layout should all be the same?
  #expect_equal(bsq_raw, c(ff::as.ram(ff_bsq)))
  #expect_equal(bsq_raw, c(ff::as.ram(ff_bil)))
  #expect_equal(bsq_raw, c(ff::as.ram(ff_bip)))
  
})
