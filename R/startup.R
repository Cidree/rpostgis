# Startup message
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "This is 'rpostgis' version ", utils::packageVersion("rpostgis"), " (",
    utils::packageDate("rpostgis"),
    ")"
  )
}