ff_type <- function(x, ...) {
  UseMethod("ff_type")
}
ff_type.character <- function(x, ...) {
  c(LOG1S = "boolean", INT1S = "byte", INT1U = "ubyte", 
    INT2S = "short", INT2U = "ushort", 
    INT4S = "integer", INT4U = NA_character_, FLT4S = "single", FLT8S = "double")[x]
}
ff_type.BasicRaster <- function(x, ...) {
  ff_type(raster::dataType(x))
}
ff_type.raster_ini <- function(x, ...) {
 ff_type(x$data$datatype) 
}
ini_file <- function(x) {
  x <- UseMethod("ini_file")
  structure(x, class = "raster_ini")
}
ini_file.character <- function(x) {
  configr::read.config(x)
}
#' @importFrom raster filename
ini_file.BasicRaster <- function(x) {
  f <- raster::filename(x)
  if (is.null(f)) stop("no filename behind this raster")
  if (!grepl("\\.grd$", f)) stop("this is definitely not a native raster (.grd)")
  prs <- try(configr::read.config(f, file.type = "ini"), silent = TRUE)
  if (inherits(prs, "try-error")) stop("cannot read config file", f)
  prs 
}