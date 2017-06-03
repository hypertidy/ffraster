library(ff)
library(raadtools)
sst <- readsst(Sys.Date() - 12:1, xylim = extent(143, 145, -47, -43), lon180 = FALSE,
               filename = "inst/extdata/raster/sst.grd")

sst <- brick("inst/extdata/raster/sst.grd")
