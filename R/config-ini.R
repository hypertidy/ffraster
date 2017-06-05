#' dimension order
#'
#' The dimension order as used by ff and raster native grid, see `raster::writeRaster`. 
#' The raster package uses the named conventions "BSQ", "BIL" and
#' "BIP" which are domain-specialized ways to record the dimension 
#' order of (virtual) 3D arrays. They stand for "band sequential"
#' "band interleaved" and ... correspond to c(1, 2, 3) etc. etc. 
#' 
#' This function always returns an integer vector indicating the order
#' of the dims of a `length(dim_order(x))` array oriented relative
#' to the R array convention. 
#' @param x filename or raster object or ff object
#' @param ...  ignored
#'
#' @return integer vector of dimension positions (1-based)
#' @export
#'
#' @examples
#' r <- raster::raster(volcano)
#' dim_order(r)
#' w <- raster::writeRaster(r, raster::rasterTmpFile(), bandorder = "BIP")
#' dim_order(w)
#' dim_order(raster::filename(w))
dim_order <- function(x, ...) {
  UseMethod("dim_order")
}
#' @name dim_order
#' @export
dim_order.BasicRaster <- function(x, ...) {
  ## Q: what if this is not in memory?
  ## A: in memory is always BSQ
  band_arrangement <- raster_bandorder(x)
  dim_index <- 
    switch(band_arrangement, 
         BIL = c(2, 3, 1),
         BIP = c(3, 1, 2), 
         BSQ = c(1, 2, 3))
  if (dim(x)[1] < 2) {
    dim_index <- 
      switch(band_arrangement, 
             BIL = c(2, 1),
             BIP = c(2, 1), 
             BSQ = c(1, 2))
    
  }
  dim_index

}
#' @name dim_order
#' @export
dim_order.character <- function(x, ...) {
  dim_order(raster::brick(x))  
}
raster_bandorder <- function(x) {
  x@file@bandorder
}

gri_filename <- function(x) {
  gsub("grd$", "gri", raster::filename(x))
}
#' ff type from raster type
#'
#' @param x filename, raster object, or ini object
#' @param ... ignored
#'
#' @return type of ff, see `ff::vmode`
#' @export
ff_type <- function(x, ...) {
  UseMethod("ff_type")
}
#' @name ff_type
#' @export
ff_type.character <- function(x, ...) {
  c(LOG1S = "boolean", INT1S = "byte", INT1U = "ubyte", 
    INT2S = "short", INT2U = "ushort", 
    INT4S = "integer", INT4U = NA_character_, FLT4S = "single", FLT8S = "double")[x]
}
#' @name ff_type
#' @export
ff_type.BasicRaster <- function(x, ...) {
  ff_type(raster::dataType(x))
}
#' @name ff_type
#' @export
ff_type.raster_ini <- function(x, ...) {
 ff_type(x$data$datatype) 
}



#' raster ini file
#'
#' Read the configuration file in raw list form, you can 
#' use either the filename (.grd) or a raster object. 
#' 
#' @param x file name or raster
#'
#' @return ini object from raster native binary
#' @export
#'
#' @examples
#' f <- system.file("extdata", "raster", "sst.grd", package  = "ffraster")
#' ini_file(f)
ini_file <- function(x) {
  x <- UseMethod("ini_file")
  structure(x, class = "raster_ini")
}
#' @name ini_file
#' @export
ini_file.character <- function(x) {
  configr::read.config(x)
}
#' @name ini_file
#' @export
#' @importFrom raster filename
ini_file.BasicRaster <- function(x) {
  f <- raster::filename(x)
  if (is.null(f)) stop("no filename behind this raster")
  if (!grepl("\\.grd$", f)) stop("this is definitely not a native raster (.grd)")
  prs <- try(configr::read.config(f, file.type = "ini"), silent = TRUE)
  if (inherits(prs, "try-error")) stop("cannot read config file", f)
  prs 
}