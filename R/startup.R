# Startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "This is 'rpostgis' version ", utils::packageVersion("rpostgis"), " (",
    utils::packageDate("rpostgis"),
    ").\n\n",
    "Due to retirement of the package 'rgeos', 'rpostgis' will retire in September 2023.\n",
    "  * For vector operations, please check package 'sf', which provides a mechanism to connect to PostGIS databases.\n",
    "  * For raster operations, no alternative solution is identified yet.\n",
    "  * For general database operations, use 'RPostgreSQL' directly.\n\n",
    "If you are interested in the development and maintenance of 'rpostgis', please check:\n",
    "https://github.com/mablab/rpostgis/issues/28"
  )
}