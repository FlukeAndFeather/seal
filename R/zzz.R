pd <- NULL
sktime <- NULL
py_slice <- reticulate::import_builtins()$slice

.onLoad <- function(libname, pkgname) {
  reticulate::py_require("sktime")
  pd <<- reticulate::import("pandas", delay_load = TRUE, convert = FALSE)
  sktime <<- reticulate::import("sktime", delay_load = TRUE)
}
