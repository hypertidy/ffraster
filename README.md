
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/mdsumner/ffraster.svg?branch=master)](https://travis-ci.org/mdsumner/ffraster) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mdsumner/ffraster?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/ffraster) [![Coverage Status](https://img.shields.io/codecov/c/github/mdsumner/ffraster/master.svg)](https://codecov.io/github/mdsumner/ffraster?branch=master)

ffraster allows loading a file-backed raster as an `ff` object.

``` r
library(raster)

b <_ brick("/some/huge/brick.grd")

library(ffraster)
ff_object(b)
```

Please see related work in library mmap, in GDAL virtualmem, VRT-linked binary, GDAL driver for R-raster, R packages ff, spatial.tools, mmap, bigmemory, rasterfaster, Manifold's Raw Binary surface, and Radian.

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
