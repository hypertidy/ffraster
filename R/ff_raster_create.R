#' Create an ffraster
#' 
#' Use a raster template to set up an ff array for filling. 
#' 
#' Note that no data is transferred, this is a set up function to give an ff array to be populated. 
#' 
#' There is currently no  control over the bandorder, set to band interleaved (BIL) currently. 
#' 
#' @param x a raster object, used as the template for the data to come
#' @param nlayers the number of layers to instantiate
#' @param filename the .grd filename
#' @param ... arguments passed to .writeGRD
#' @param setZ optional z values, must be of length 'nlayers'
#'
#' @examples
#' \dontrun{
#' n <- 5
#' files <- raadtools::sstfiles()[1:n, ]
#' 
#' library(raster)
#' fun <- function(date) raadtools::readsst(date, xylim = extent(120, 160, -50, -30), inputfiles = files)
#' arr <- ff_raster(fun(files$date[1]), nlayers = n, filename = "afile.grd", overwrite = TRUE)
#' for (i in seq_along(files$date)) {
#' arr[,,i] <-   raster::values(t(fun(files$date[i])))
#' }
#' # plot(brick("afile.grd"))
#' }
ff_raster <- function(x, nlayers = NULL, filename = NULL, ..., setZ = NULL) {
  if (is.null(nlayers)) nlayers = raster::nlayers(x)
  stopifnot(!is.null(filename))
  stopifnot(grepl("grd$", filename))
  if (!is.null(setZ)) {
    if (length(setZ) != nlayers) {
      stop("length of 'setZ' and value of 'nlayers' must match")
    }
  }
  ffraster:::.writeGRD(x, 
                       dataType = "FLT4S", 
                       filename = filename, 
                       nbands = nlayers, 
                       ## if dates are known, input them here for each file
                       dates = setZ, ...)
  ffrarr(c(nrow(x), ncol(x), 
           nlayers), 
         mode = "single", 
         filename = filename, 
         readonly = FALSE)

}
