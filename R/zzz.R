sktime <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("sktime")
  sktime <<- reticulate::import("sktime", delay_load = TRUE)
}
