## pgPostGIS

##' Check and create PostGIS extension.
##'
##' The function checks for the availability of the PostGIS extension,
##' and if it is available, but not installed, install
##' it. Additionally, can also install Topology, Tiger Geocoder,
##' SFCGAL and Raster extensions.
##'
##' @param conn A connection object (required, even if \code{exec =
##'     FALSE}).
##' @param topology Logical. Whether to check/install the Topology
##'     extension.
##' @param tiger Logical. Whether to check/install the Tiger Geocoder
##'     extension. Will also install extensions "fuzzystrmatch",
##'     "address_standardizer", and "address_standardizer_data_us" if
##'     all are available.
##' @param sfcgal Logical. Whether to check/install the SFCGAL
##'     extension.
##' @param raster Logical. Whether to check/install the Raster extension
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return If \code{exec = TRUE}, returns (invisibly) \code{TRUE} if PostGIS is installed.
##' @author Mathieu Basille \email{mathieu@@basille.org} and
##'         Adrián Cidre González \email{adrian.cidre@@gmail.com}
##' @export
##' @examples
##' ## 'exec = FALSE' does not install any extension, but nevertheless
##' ## check for available and installed extensions:
##' \dontrun{
##'     pgPostGIS(con, topology = TRUE, tiger = TRUE, sfcgal = TRUE,
##'         exec = FALSE)
##' }

pgPostGIS <- function(conn, topology = FALSE, tiger = FALSE,
                      sfcgal = FALSE, raster = FALSE, display = TRUE, exec = TRUE) {
    dbConnCheck(conn)
    ## Get the list of extensions from PostgreSQL
    ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
    ## Check if PostGIS is available:
    if (!("postgis" %in% ext$name))
        cli::cli_abort("PostGIS extension not available.")
    ## Extract it and check if installed; if not, install it:
    post <- subset(ext, ext$name == "postgis")
    if (is.na(post$installed_version)) {
        # check if installed but not using CREATE EXTENSION
        pgis_check2 <- dbGetQuery(conn, "SELECT table_name FROM information_schema.tables WHERE
                 table_name IN ('spatial_ref_sys','geometry_columns');")$table_name

        if (length(pgis_check2) >= 2) {
            pgis_ver <- NULL
            try(pgis_ver <- dbGetQuery(conn, "SELECT postgis_full_version();")$postgis_full_version,
                silent = TRUE)
            if (!is.null(pgis_ver)) {
                full_ver <- unlist(strsplit(pgis_ver, "\"", fixed = TRUE))[2]
                full_ver <- unlist(strsplit(full_ver, " ", fixed = TRUE))[1]
                maj_ver <- unlist(strsplit(full_ver, ".", fixed = TRUE))[1]
                if (maj_ver < 2) {
                    cli::cli_abort("Unsupported version of PostGIS already installed (supported versions: 2.0 and above).")
                } else {
                    cli::cli_alert_info("PostGIS is installed, but not registered in \"pg_available_extensions\". rpostgis should work, \nbut use the recommended \"CREATE EXTENSION postgis;\" (or this function) to enable PostGIS in the future.")
                    if (any(tiger, sfcgal, topology)) {
                        cli::cli_alert_info("Due to non-standard install of PostGIS, Other extensions cannot be checked/installed using this function.")
                    }
                    return(TRUE)
                }
            }
        } else {
            ## Print message:
            cli::cli_h1("Installing PostGIS extension version {post$default_version}:")
            ## Build the query
            query <- "CREATE EXTENSION postgis;"
            ## Display the query
            if (display) {
                cli::cli_alert_info(query)
            }
            ## Execute the query
            if (exec)
                dbSendQuery(conn, query)

            ## Should now be installed; stop if not:
            ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
            post <- subset(ext, ext$name == "postgis")
            full_ver <- post$installed_version
        }

        if (is.na(full_ver))
            cli::cli_abort("PostGIS extension not installed.") else cli::cli_alert_success("PostGIS extension version {full_ver} installed.")
    } else {
        cli::cli_alert_success("PostGIS extension version {post$installed_version} installed.")
    }

    ## Topology extension
    if (topology) {
        ## Check if Topology is available:
        if (!("postgis_topology" %in% ext$name))
            cli::cli_alert_danger("PostGIS Topology extension not available.") else {
                ## Extract it and check if installed; if not, install it:
                topo <- subset(ext, ext$name == "postgis_topology")
                if (is.na(topo$installed_version)) {
                    ## Print message:
                    cli::cli_h1("Installing PostGIS Topology extension version {topo$default_version}:")
                    ## Build the query
                    query <- "CREATE EXTENSION postgis_topology;"
                    ## Display the query
                    if (display) {
                        cli::cli_alert_info(query)
                    }
                    ## Execute the query
                    if (exec)
                        dbSendQuery(conn, query)
                }
                ## Should now be installed; print a message if not:
                ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
                topo <- subset(ext, ext$name == "postgis_topology")
                if (is.na(topo$installed_version)) {
                    cli::cli_alert_danger("PostGIS Topology extension not installed.")
                } else {
                    cli::cli_alert_success("PostGIS Topology extension version {topo$installed_version} installed.")
                }
            }
    }
    ## Tiger Geocoder extension
    if (tiger) {
        ## Check if Tiger Geocoder is available:
        tiger_exts <- c("fuzzystrmatch", "postgis_tiger_geocoder",
                        "address_standardizer", "address_standardizer_data_us")
        if (!all(tiger_exts %in% ext$name))
            cli::cli_alert_danger("PostGIS Tiger Geocoder extension not available.") else {
                ## Extract it and check if installed; if not, install it:
                tiger <- subset(ext, ((ext$name %in% tiger_exts) &
                                          is.na(ext$installed_version)))
                if (length(tiger$name) > 0) {
                    ## Build the query
                    for (i in order(tiger$name)) {
                        cli::cli_h1("Installing {tiger$name[i]} extension version {tiger$default_version[i]}:")
                        query <- paste0("CREATE EXTENSION ", tiger$name[i],
                                        ";")
                        ## Display the query
                        if (display) {
                            cli::cli_alert_info(query)
                        }
                        ## Execute the query
                        if (exec)
                            dbSendQuery(conn, query)
                    }
                }
                ## Should now be installed; print a message if not:
                ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
                tiger <- subset(ext, ext$name == "postgis_tiger_geocoder")
                if (is.na(tiger$installed_version)) {
                    cli::cli_alert_danger("PostGIS Tiger Geocoder extension not installed.")
                } else {
                    cli::cli_alert_success("PostGIS Tiger Geocoder extension version {tiger$installed_version} installed.")
                }

            }
    }
    ## SFCGAL extension
    if (sfcgal) {
        ## Check if SFCGAL is available:
        if (!("postgis_sfcgal" %in% ext$name))
            cli::cli_alert_danger("PostGIS SFCGAL extension not available.") else {
                ## Extract it and check if installed; if not, install it:
                sfc <- subset(ext, ext$name == "postgis_sfcgal")
                if (is.na(sfc$installed_version)) {
                    ## Print message:
                    cli::cli_h1("Installing PostGIS SFCGAL extension version {sfc$default_version}:")
                    ## Build the query
                    query <- paste0("CREATE EXTENSION postgis_sfcgal;")
                    ## Display the query
                    if (display) {
                        cli::cli_alert_info(query)
                    }
                    ## Execute the query
                    if (exec)
                        dbSendQuery(conn, query)
                }
                ## Should now be installed; print a message if not:
                ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
                sfc <- subset(ext, ext$name == "postgis_sfcgal")
                if (is.na(sfc$installed_version)) {
                    cli::cli_alert_danger("PostGIS SFCGAL extension not installed.")
                } else {
                    cli::cli_alert_success("PostGIS SFCGAL extension version {sfc$installed_version} installed.")
                }
            }
    }

    ## Raster extension
    if (raster) {
        ## Check if Topology is available:
        if (!("postgis_raster" %in% ext$name))
            cli::cli_alert_danger("PostGIS Topology extension not available.") else {
                ## Extract it and check if installed; if not, install it:
                rast.ext <- subset(ext, ext$name == "postgis_raster")
                if (is.na(rast.ext$installed_version)) {
                    ## Print message:
                    cli::cli_h1("Installing PostGIS Raster extension version {rast.ext$default_version}:")
                    ## Build the query
                    query <- "CREATE EXTENSION postgis_raster;"
                    ## Display the query
                    if (display) {
                        cli::cli_alert_info(query)
                    }
                    ## Execute the query
                    if (exec) dbSendQuery(conn, query)
                }
                ## Should now be installed; print a message if not:
                ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
                rast.ext <- subset(ext, ext$name == "postgis_raster")
                if (is.na(rast.ext$installed_version)) {
                    cli::cli_alert_danger("PostGIS Raster extension not installed.")
                } else {
                    cli::cli_alert_success("PostGIS Raster extension version {rast.ext$installed_version} installed.")
                }
            }
    }

    ## Return TRUE
    cli::cli_alert_success("Query executed")
    if (exec) return(invisible(TRUE))


}
