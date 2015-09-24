library(raster)
d <- brick("\\\\aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data/topex.ucsd.edu/pub/global_topo_1min/.vrt/topo_18.1.vrt")


f <- "\\\\aad.gov.au/files/AADC/Scientific_Data/Data/gridded_new/data/topex.ucsd.edu/pub/global_topo_1min/topo_18.1.img"
rawf <- file(f, open = "rb")


library(ff)


raw_smith_sandwell <- ff(dim = prod(dim(d)), vmode = "boolean", filename = "raw_smith_sandwell.bin")

cnt <- 1 
chunk <- ncol(d)
for (i in seq(nrow(d))) {
  sq <- seq(cnt, (cnt - 1) + chunk)
  rv <- readBin(rawf, "integer", size = 2, signed = TRUE, n =  chunk, endian = "big")

  raw_smith_sandwell[sq] <- rv < 0
  if (i %% 1000 == 0) print(i)
  cnt <- cnt + chunk
}


close(rawf)

file.info("raw_smith_sandwell.bin")$size/1e6

## 45Mb

x <- readBin("raw_smith_sandwell.bin", "raw", n = prod(dim(d))/8)
save(x, file = "raw.RData")

file.info("raw.RData")$size / 1e6
## 1Mb


## round trip

load("raw.RData")
writeBin(x, "ss1.bin", size = 1)

fbin <- ff(filename  = "ss1.bin", vmode = "boolean", dim = prod(dim(d)))



## ask raster for which cells

#ex <- extent(100, 150, -50, -30)
#projectExtent(raster(ex, crs = "+proj=longlat"), projection(d))

ex <- extent(11119487, 16679231, -6439060, -3499628)

ind <- cellsFromExtent(d, ex)

mask <- setValues(raster(ex, res = res(d)), as.logical(fbin[ind]))

