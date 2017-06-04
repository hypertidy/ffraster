## ------------------------------------------------------------------------
d24 <- 1:24

## ------------------------------------------------------------------------
cols <- viridis::viridis(length(d24))

## worker function to drive image as I wish it were
mimage <- function(x, label  = TRUE, ...) {
  UseMethod("mimage")
}
mimage.matrix <- function(x, label = TRUE, cols, ...) {
  mat <- x
  x <- seq(0, nrow(mat))
  y <- seq(0, ncol(mat))
  image(x, y, mat, col = cols)
}
mimage.array <- function(x, label = TRUE, ...) {
  dn <- length(dim(x))
  if (dn < 2 | dn > 3) {
    warning("we expect a matrix or 3D array but dim(x):")
    print(dim(x))
  }
  on.exit(par(p), add = TRUE)
  p <- par(mfrow = grDevices::n2mfrow(dim(x)[3]))
  xx <- seq(0, nrow(x))
  yy <- seq(0, ncol(x))
  xy <- expand.grid(x = head(xx, -1) + 0.5, 
                    y = head(yy, -1) + 0.5)
  
  for (i in seq_len(dim(x)[3])) {
    mimage(x[,,i, drop = TRUE], cols = cols[(1:12) + 12 * (i-1)])
  if (label) text(xy$x, xy$y, label = seq_len(nrow(xy)) + nrow(xy) * (i - 1))

}
}

## ------------------------------------------------------------------------
matrix(d24)

## ------------------------------------------------------------------------
a <- array(d24, c(3, 4, 2))
a

## ------------------------------------------------------------------------
mimage(a)

## ------------------------------------------------------------------------
plot(d24, col = cols, pch = 19, cex = 3)

## ------------------------------------------------------------------------
a[, 1:2, 1]
a[, 1:2, 2]

## ------------------------------------------------------------------------
library(raster)
b <- setExtent(brick(a, transpose = TRUE), c(0, nrow(a), 0, ncol(a)))
p <- par(mfrow = grDevices::n2mfrow(nlayers(b)))
for (i in seq_len(nlayers(b))) {
  plot(b[[i]], col = cols, breaks = d24, zlim = cellStats(b[[i]], range), asp = "")
  text(coordinates(b), lab = d24[(1:12) + 12 * (i-1)])
}

## ------------------------------------------------------------------------
values(b)

## ------------------------------------------------------------------------
file0 <- rasterTmpFile()
file1 <- rasterTmpFile()
b0 <- writeRaster(b, file0, datatype = "FLT4S")

b1 <- writeRaster(b, file1, bandorder = "BIL")

rb <- function(x) {
  readBin(x,"raw", n= file.info(x)$size)
}
identical(digest::digest(rb(file0)), 
          digest::digest(rb(file1)))

## ----eval=FALSE----------------------------------------------------------
#  ## something is wrong...
#  library(ff)
#  b0_ff <- ff(filename = file0, vmode = "single", dim = dim(b0), readonly = TRUE  )

## ------------------------------------------------------------------------

library(ffraster)
ff_b <- ff_object(brick(file0))
ff::as.ram(ff_b)


