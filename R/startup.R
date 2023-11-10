# Startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "This is 'rpostgis' version ", utils::packageVersion("rpostgis"), " (",
    utils::packageDate("rpostgis"), ") \n",
    "------------------ \n",
    "`rpostgis` is under new maintenance, and development have been moved to https://github.com/Cidree/rpostgis/ \n",
    "------------------ \n",
    "Support for `sp` and `raster` objects will be deprecated in {rpostgis} and will be removed ",
    "in a future release of the package. Please use `sf` and `terra` objects with {rpostgis}"
  )
}
