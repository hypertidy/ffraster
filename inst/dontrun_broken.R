
#' \dontrun{
#' ## load required packages
#' library(raster)
#' library(ff)
#' r <- brick(raster(mat), raster(mat), raster(mat))
#' ##r <- brick(arr)
#' ##r <- raster(mat)
#' 
#' ## set up file names to use for rasterfile
#' datafile <- sprintf("%s.gri", tempfile())
#' hdrfile <- gsub("gri$", "grd", datafile)
#' 
#' ## create rasterfile, we only want the hdrfile part
#' #rmat <- writeRaster(r, filename = hdrfile, overwrite = TRUE)
#' 
#' ## map types between ff and raster
#' vm <- c(INT1U = "ubyte", FLT4S = "single", FLT8S = "double")[rmat@file@datanotation]
#' 
#' ## set up data dimensions and order
#' vdim <- dim(r)
#' if(vdim[length(vdim)] < 2) vdim <- vdim[length(vdim)-1]
#' dimo <- seq_along(vdim)[c(seq_along(vdim)[-1], 1)]
#' 
#' ## create ff object
#' #ffmat <- ff(dim = vdim, vmode = vm, dimorder = dimo, filename = datafile, readonly = TRUE)
#' 
#' 
#' #ffarr <- ff(array(c(mat, mat, mat), vdim), dim = vdim, vmode = vm, dimorder = dimo, filename = "afile.gri")
#' #file.copy(hdrfile, "afile.grd")
#' }