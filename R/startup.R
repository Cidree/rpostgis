.onAttach <- function(libname, pkgname) {
  msg <- paste(
    paste(" Welcome to rpostgis -", sprintf("Version: %s (%s)", utils::packageVersion("rpostgis"), utils::packageDate("rpostgis"))),
    "- Support for `sp` and `raster` objects is deprecated.",
    "- These will be removed in a future release.",
    "- Please use `sf` and `terra` objects with rpostgis.",
    sep = "\n"
  )

  packageStartupMessage(msg)
}

