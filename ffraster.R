mat <- volcano

## load required packages
library(raster)
library(ff)
##r <- brick(raster(mat), raster(mat), raster(mat))
##r <- brick(arr)
##r <- raster(mat)

library(raadtools)

newfile <- "ice_series_daily_1991"

dts <- icefiles()$date
dts <- dts[dts > as.POSIXct("1991-01-01 00:00:00")]
nl <- length(dts)
r <- readice(dts[1])
dts <- dts[seq_len(nl)]
## set up file names to use for rasterfile
datafile <- sprintf("%s.gri", tempfile())
hdrfile <- gsub("gri$", "grd", datafile)

## create rasterfile, we only want the hdrfile part
rmat <- writeRaster(r, filename = hdrfile, overwrite = TRUE)

## map types between ff and raster
vm <- c(FLT4S = "single", FLT8S = "double")[rmat@file@datanotation]

## set up data dimensions and order
vdim <- dim(r)
vdim[3] <- nl
if(vdim[length(vdim)] < 2) vdim <- vdim[-(length(vdim))]
dimo <- seq_along(vdim)[c(seq_along(vdim)[-1], 1)]

## create ff object
ffmat <- ff(dim = vdim, vmode = vm, dimorder = dimo, filename = sprintf("%s.gri", newfile), readonly = FALSE, overwrite = TRUE)
for (i in seq_along(dts)) {
  r <- readice(dts[i])
  if (nl == 1) {
    ffmat[,] <- as.matrix(r) 
  } else {
    ffmat[,,i] <- as.matrix(r)
  }
  if (i %% 500 == 0) print(i)
}
##ffarr <- ff(array(c(mat, mat, mat), vdim), dim = vdim, vmode = vm, dimorder = dimo, filename = sprintf("%s.gri", newfile))
txt <- readLines(hdrfile)
ln <- grep("nbands", txt)
txt[ln] <- gsub("1", as.character(nl), txt[ln])
writeLines(txt, sprintf("%s.grd", newfile))


#file.copy(hdrfile, sprintf("%s.grd", newfile))




