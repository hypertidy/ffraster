mat <- volcano

library(raster)
library(ff)
r <- brick(raster(mat), raster(mat), raster(mat))
##r <- brick(arr)
##r <- raster(mat)

datafile <- sprintf("%s.gri", tempfile())
hdrfile <- gsub("gri$", "grd", datafile)

rmat <- writeRaster(r, filename = hdrfile, overwrite = TRUE)

vm <- c(FLT4S = "single", FLT8S = "double")[rmat@file@datanotation]

vdim <- dim(r)
if(vdim[length(vdim)] < 2) vdim <- vdim[length(vdim)-1]
dimo <- seq_along(vdim)[c(seq_along(vdim)[-1], 1)]

ffmat <- ff(dim = vdim, vmode = vm, dimorder = dimo, filename = datafile, readonly = TRUE)


ffarr <- ff(array(c(mat, mat, mat), vdim), dim = vdim, vmode = vm, dimorder = dimo, filename = "afile.gri")
file.copy(hdrfile, "afile.grd")




