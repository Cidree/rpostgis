.onAttach <- function(libname, pkgname) {
  cli::cli_h1("Welcome to rpostgis!")

  cli::cli_alert_info("Version: {utils::packageVersion('rpostgis')} ({utils::packageDate('rpostgis')})")

  cli::cli_rule("Notice")

  cli::cli_alert_warning("Support for `sp` and `raster` objects is deprecated.")
  cli::cli_alert_danger("These will be removed in a future release.")
  cli::cli_alert_info("Please use `sf` and `terra` objects with {{rpostgis}}.")
}
