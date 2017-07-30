## pgPostGIS

##' Check and create PostGIS extension.
##'
##' The function checks for the availability of the PostGIS extension,
##' and if it is available, but not installed, install
##' it. Additionally, can also install Topology, Tiger Geocoder and
##' SFCGAL extensions.
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
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if PostGIS is installed.
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## 'exec = FALSE' does not install any extension, but nevertheless
##' ## check for available and installed extensions:
##' \dontrun{
##'     pgPostGIS(con, topology = TRUE, tiger = TRUE, sfcgal = TRUE,
##'         exec = FALSE)
##' }

pgPostGIS <- function(conn, topology = FALSE, tiger = FALSE, 
    sfcgal = FALSE, display = TRUE, exec = TRUE) {
    dbConnCheck(conn)
    ## Get the list of extensions from PostgreSQL
    ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
    ## Check if PostGIS is available:
    if (!("postgis" %in% ext$name)) 
        stop("PostGIS extension not available.")
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
                  stop("Unsupported version of PostGIS already installed (supported versions: 2.0 and above).")
                } else {
                  message("PostGIS is installed, but not registered in \"pg_available_extensions\". rpostgis should work, \nbut use the recommended \"CREATE EXTENSION postgis;\" (or this function) to enable PostGIS in the future.")
                  if (any(tiger, sfcgal, topology)) {
                    message("Due to non-standard install of PostGIS, Other extensions cannot be checked/installed using this function.")
                  }
                  return(TRUE)
                }
            }
        } else {
            ## Print message:
            message(paste0("Installing PostGIS extension version ", 
                post$default_version), ":")
            ## Build the query
            query <- paste0("CREATE EXTENSION postgis;")
            ## Display the query
            if (display) {
                message(paste0("Query ", ifelse(exec, "", "not "), 
                  "executed:"))
                message(query)
                message("--")
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
            stop("PostGIS extension not installed.") else message(paste0("PostGIS extension version ", full_ver, 
            " installed."))
    } else {
        message(paste0("PostGIS extension version ", post$installed_version, 
            " installed."))
    }
    
    ## Topology extension
    if (topology) {
        ## Check if Topology is available:
        if (!("postgis_topology" %in% ext$name)) 
            message("PostGIS Topology extension not available.") else {
            ## Extract it and check if installed; if not, install it:
            topo <- subset(ext, ext$name == "postgis_topology")
            if (is.na(topo$installed_version)) {
                ## Print message:
                message(paste0("Installing PostGIS Topology extension version ", 
                  topo$default_version), ":")
                ## Build the query
                query <- paste0("CREATE EXTENSION postgis_topology;")
                ## Display the query
                if (display) {
                  message(paste0("Query ", ifelse(exec, "", "not "), 
                    "executed:"))
                  message(query)
                  message("--")
                }
                ## Execute the query
                if (exec) 
                  dbSendQuery(conn, query)
            }
            ## Should now be installed; print a message if not:
            ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
            topo <- subset(ext, ext$name == "postgis_topology")
            if (is.na(topo$installed_version)) 
                message("PostGIS Topology extension not installed.") else message(paste0("PostGIS Topology extension version ", 
                topo$installed_version, " installed."))
        }
    }
    ## Tiger Geocoder extension
    if (tiger) {
        ## Check if Tiger Geocoder is available:
        tiger_exts <- c("fuzzystrmatch", "postgis_tiger_geocoder", 
            "address_standardizer", "address_standardizer_data_us")
        if (!all(tiger_exts %in% ext$name)) 
            message("PostGIS Tiger Geocoder extension not available.") else {
            ## Extract it and check if installed; if not, install it:
            tiger <- subset(ext, ((ext$name %in% tiger_exts) & 
                is.na(ext$installed_version)))
            if (length(tiger$name) > 0) {
                ## Build the query
                for (i in order(tiger[1])) {
                  message(paste0("Installing ", tiger$name[i], 
                    " extension version ", tiger$default_version[i]), 
                    ":")
                  query <- paste0("CREATE EXTENSION ", tiger$name[i], 
                    ";")
                  ## Display the query
                  if (display) {
                    message(paste0("Query ", ifelse(exec, "", 
                      "not "), "executed:"))
                    message(query)
                    message("--")
                  }
                  ## Execute the query
                  if (exec) 
                    dbSendQuery(conn, query)
                }
            }
            ## Should now be installed; print a message if not:
            ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
            tiger <- subset(ext, ext$name == "postgis_tiger_geocoder")
            if (is.na(tiger$installed_version)) 
                message("PostGIS Tiger Geocoder extension not installed.") else message(paste0("PostGIS Tiger Geocoder extension version ", 
                tiger$installed_version, " installed."))
        }
    }
    ## SFCGAL extension
    if (sfcgal) {
        ## Check if SFCGAL is available:
        if (!("postgis_sfcgal" %in% ext$name)) 
            message("PostGIS SFCGAL extension not available.") else {
            ## Extract it and check if installed; if not, install it:
            sfc <- subset(ext, ext$name == "postgis_sfcgal")
            if (is.na(sfc$installed_version)) {
                ## Print message:
                message(paste0("Installing PostGIS SFCGAL extension version ", 
                  sfc$default_version), ":")
                ## Build the query
                query <- paste0("CREATE EXTENSION postgis_sfcgal;")
                ## Display the query
                if (display) {
                  message(paste0("Query ", ifelse(exec, "", "not "), 
                    "executed:"))
                  message(query)
                  message("--")
                }
                ## Execute the query
                if (exec) 
                  dbSendQuery(conn, query)
            }
            ## Should now be installed; print a message if not:
            ext <- dbGetQuery(conn, "SELECT * FROM pg_available_extensions;")
            sfc <- subset(ext, ext$name == "postgis_sfcgal")
            if (is.na(sfc$installed_version)) 
                message("PostGIS SFCGAL extension not installed.") else message(paste0("PostGIS SFCGAL extension version ", 
                sfc$installed_version, " installed."))
        }
    }
    ## Return TRUE
    if (exec) return(TRUE)
}
