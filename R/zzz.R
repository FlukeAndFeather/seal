pd <- NULL
sktime <- NULL

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("sktime")
  pd <<- reticulate::import("pandas", delay_load = TRUE)
  sktime <<- reticulate::import("sktime", delay_load = TRUE)
}
