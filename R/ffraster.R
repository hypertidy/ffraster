
#' ffraster
#' 
#' Build a ff array to be used as a raster. 
#'
#' For mapping between raster and ff types, see \code{\link[ff]{vmode}} and \code{\link[raster]{dataType}}
#' @param dim dimensions in Raster order (nrow, ncol, nlayer)
#' @param mode ff data mode see details
#' @param dataType raster dataType, see Details
#' @param filename file name as per \code{\link{writeRaster}}
#' @importFrom ff ff
#' @importFrom raster raster
#' @return ff
#' @export
#' @examples
#' 
#' mat <- volcano
#' library(raster)
#' b <- brick(raster(mat), raster(mat), raster(mat))
#' fn <- rasterTmpFile()
#' dt <- "INT1U"
#' ffraster:::.writeGRD(b, dataType = dt, filename = fn)
#' a <- ffrarr(dim(b), mode = dt, filename = fn)
#' 
ffrarr <- function(dim, mode, filename) {
  ## set up data dimensions and order
  vdim <- dim
  vm <- c(INT1U = "ubyte", FLT4S = "single", FLT8S = "double")[mode]
  ## bit of overkill to work with n-D
  if(vdim[length(vdim)] < 2) vdim <- vdim[length(vdim)-1]
  dimo <- seq_along(vdim)[c(seq_along(vdim)[-1], 1)]
  ff(dim = vdim, vmode = vm, dimorder = dimo, filename = gsub("grd$", "gri", filename))
}

#' @importFrom raster xmin ymin xmax ymax projection nlayers trim minValue maxValue getZ
.writeGRD <- function (x, type = "raster", filename = NULL, dataType = NULL, byteorder = "little", bandorder = "BIL", 
                       nbands = NULL, dates = NULL) 
{
  rastergrd <- raster:::.setFileExtensionHeader(filename, type)
  thefile <- file(rastergrd, "w")
  cat("[general]", "\n", file = thefile, sep = "")
  cat("creator=R package 'raster'", "\n", file = thefile, sep = "")
  cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
      "\n", file = thefile, sep = "")
  cat("[georeference]", "\n", file = thefile, sep = "")
  cat("nrows=", nrow(x), "\n", file = thefile, sep = "")
  cat("ncols=", ncol(x), "\n", file = thefile, sep = "")
  cat("xmin=", as.character(xmin(x)), "\n", file = thefile, 
      sep = "")
  cat("ymin=", as.character(ymin(x)), "\n", file = thefile, 
      sep = "")
  cat("xmax=", as.character(xmax(x)), "\n", file = thefile, 
      sep = "")
  cat("ymax=", as.character(ymax(x)), "\n", file = thefile, 
      sep = "")
  cat("projection=", projection(x), "\n", file = thefile, sep = "")
  cat("[data]", "\n", file = thefile, sep = "")
  cat("datatype=", dataType, "\n", file = thefile, 
      sep = "")
  cat("byteorder=", byteorder, "\n", file = thefile, 
      sep = "")
  if (is.null(nbands)) {
    nl <- nlayers(x)
    miv <- paste(minValue(x, -1, warn = FALSE), collapse = ":")
    mav <- paste(maxValue(x, -1, warn = FALSE), collapse = ":")
    ln <- gsub(":", ".", names(x))
    ln <-  paste(ln, collapse = ":")
    
    
  } else {
    nl <- nbands
    miv <- paste(rep(minValue(x, -1, warn = FALSE), nbands), collapse = ":")
    mav <- paste(rep(maxValue(x, -1, warn = FALSE), nbands), collapse = ":")
    ln <- gsub(":", ".", names(x))
    ln <-  paste(paste(ln, seq(nbands), sep = "."), collapse = ":")
    
   }
  cat("nbands=", nl, "\n", file = thefile, sep = "")
  cat("bandorder=", bandorder, "\n", file = thefile, 
      sep = "")
  if (nl == 1) {
    fact <- is.factor(x)[1]
    cat("categorical=", paste(fact, collapse = ":"), "\n", 
        file = thefile, sep = "")
    if (any(fact)) {
      r <- x@data@attributes[[1]]
      cat("ratnames=", paste(colnames(r), collapse = ":"), 
          "\n", file = thefile, sep = "")
      cat("rattypes=", paste(sapply(r, class), collapse = ":"), 
          "\n", file = thefile, sep = "")
      cat("ratvalues=", paste(trim(as.character(as.matrix(r))), 
                              collapse = ":"), "\n", file = thefile, sep = "")
    }
    if (length(x@legend@colortable) > 1) {
      cat("colortable=", paste(x@legend@colortable, collapse = ":"), 
          "\n", file = thefile, sep = "")
    }
  }
  cat("minvalue=", miv, 
      "\n", file = thefile, sep = "")
  cat("maxvalue=", mav, 
      "\n", file = thefile, sep = "")
  cat("nodatavalue=", raster:::.nodatavalue(x), "\n", file = thefile, 
      sep = "")
  cat("[legend]", "\n", file = thefile, sep = "")
  # cat("legendtype=", x@legend@type, "\n", file = thefile, sep = "")
  #  cat("values=", paste(x@legend@values, collapse = ":"), "\n", 
  #     file = thefile, sep = "")
  #  cat("color=", paste(x@legend@color, collapse = ":"), "\n", 
  #      file = thefile, sep = "")
  cat("[description]", "\n", file = thefile, sep = "")
  
  cat("layername=", ln, "\n", file = thefile, 
      sep = "")
  z <- getZ(x)
  if (!is.null(nbands)) {
    if (is.null(dates)) stop("nbands also needs dates input")
    z <- dates
  }
  if (!is.null(z)) {
    zname <- names(x@z)[1]
    if (is.null(zname)) {
      zname <- "z-value"
    }
    zclass <- class(z)
    if (inherits(z, "POSIXct")) {
      z <- format(z, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }
    else {
      z <- as.character(z)
    }
    cat("zvalues=", paste(c(zname, z), collapse = ":"), "\n", 
        file = thefile, sep = "")
    cat("zclass=", zclass, "\n", file = thefile, sep = "")
  }
  a <- NULL
  try(a <- unlist(x@history), silent = TRUE)
  if (!is.null(a)) {
    cat("history=", a, "\n", file = thefile, sep = "")
  }
  a <- NULL
  try(a <- rapply(x@history, function(x) paste(as.character(x), 
                                               collapse = "#,#")), silent = TRUE)
  if (!is.null(a)) {
    a <- gsub("\n", "#NL#", a)
    type <- rapply(x@history, class)
    type_value <- apply(cbind(type, a), 1, function(x) paste(x, 
                                                             collapse = ":"))
    name_type_value <- apply(cbind(names(a), type_value), 
                             1, function(x) paste(x, collapse = "="))
    name_type_value <- paste(name_type_value, "\n", sep = "")
    cat("[metadata]", "\n", file = thefile, sep = "")
    cat(name_type_value, file = thefile, sep = "")
  }
  close(thefile)
  return(TRUE)
}


