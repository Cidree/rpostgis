## pgSRID

##' Find (or create) PostGIS SRID based on CRS object.
##'
##' This function takes [sf::st_crs()]-class object and a
##' PostgreSQL database connection (with PostGIS extension), and
##' returns the matching SRID(s) for that CRS. If a match is not
##' found, a new entry can be created in the PostgreSQL
##' `spatial_ref_sys` table using the parameters specified by the
##' CRS. New entries will be created with `auth_name =
##' 'rpostgis_custom'`, with the default value being the next open value
##' between 880001-889999 (a different SRID value can be entered if desired.)
##'
##' @param conn A connection object to a PostgreSQL database.
##' @param crs crs object, created through a call to
##'     [sf::st_crs()].
##' @param create.srid Logical. If no matching SRID is found, should a new
##'     SRID be created? User must have write access on
##'     `spatial_ref_sys` table.
##' @param new.srid Integer. Optional SRID to give to a newly created
##'     SRID. If left NULL (default), the next open value of
##'     `srid` in `spatial_ref_sys` between 880001 and
##'     889999 will be used.
##' @return SRID code (integer).
##' @author David Bucklin \email{david.bucklin@@gmail.com} and Adrián Cidre
##' González \email{adrian.cidre@@gmail.com}
##' @export
##' @importFrom sf st_crs
##' @examples
##' \dontrun{
##' drv <- dbDriver("PostgreSQL")
##' conn <- dbConnect(drv, dbname = "dbname", host = "host", port = "5432",
##'     user = "user", password = "password")
##' (crs <- sf::st_crs("+proj=longlat"))
##' pgSRID(conn, crs)
##' (crs2 <- sf::st_crs(paste("+proj=stere", "+lat_0=52.15616055555555 +lon_0=5.38763888888889",
##'     "+k=0.999908 +x_0=155000 +y_0=463000", "+ellps=bessel",
##'     "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
##'     "+units=m")))
##' pgSRID(conn, crs2, create.srid = TRUE)
##' }

pgSRID <- function(conn, crs, create.srid = FALSE, new.srid = NULL) {
    ## Check if PostGIS is enabled
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        cli::cli_abort("PostGIS is not enabled on this database.")
    }
    ## check object
    if (!inherits(crs, "crs")) {
        cli::cli_abort("Object is not of class crs.")
    }

    ## check if can extract EPSG directly
    epsg <- crs$epsg
    if (!is.na(epsg)) {
        temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE auth_name = 'EPSG' AND auth_srid = ",
                             epsg, ";")
        srid <- dbGetQuery(conn, temp.query)$srid
        if (length(srid) > 0) return(srid)
    }

    ## check if can extract SRID directly (for ESRI)
    srid.code <- crs$srid
    if (!is.na(srid.code) & grepl("ESRI", srid.code)) {
        srid <- gsub("[^0-9]", "", srid.code)
        temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE auth_name != 'EPSG' AND auth_srid = ",
                             srid, ";")
        srid <- dbGetQuery(conn, temp.query)$srid
        if (length(srid) > 0) return(srid)
    }

    ## extract p4s
    p4s <- crs$proj4string
    ## if crs is undefined (NA), error
    if (is.na(p4s)) cli::cli_abort("CRS undefined (NA).")

    ## check for matching p4s in spatial_ref_sys (with or without
    ## trailing white space)
    temp.query <- paste0("SELECT srid FROM spatial_ref_sys\nWHERE\n(proj4text = '",
                         p4s, "'\n OR\n regexp_replace(proj4text,'[[:space:]]+$','') = '",
                         p4s, "');")
    srid <- dbGetQuery(conn, temp.query)$srid

    if (length(srid) > 0) {
        return(srid)
    }

    ## stop of create new SRID
    if (!create.srid) {
        cli::cli_abort("No SRID matches found. Re-run with 'create.srid = TRUE' to create new SRID entry in spatial_ref_sys.")
    }
    ## if none of the above methods worked, create new SRID
    if (!is.null(new.srid)) {
        ## check if exists
        temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE srid = ",
                             new.srid, ";")
        check.srid <- dbGetQuery(conn, temp.query)
        if (length(check.srid) > 0) {
            cli::cli_abort("SRID {new.srid} already exists in 'spatial_ref_sys'.Select another 'new.srid' or leave it to 'NULL' to select the next open SRID between 880000 and 889999.")
        }
        srid <- new.srid
    } else {
        ## find next SRID for custom set (prefix 88, first value =
        ## 880001)
        temp.query <- "SELECT min(series) AS new FROM generate_series(880001,890000) AS series WHERE series NOT IN\n  (SELECT srid FROM spatial_ref_sys WHERE srid > 880000 AND srid < 890000)"
        new.srid <- dbGetQuery(conn, temp.query)$new
        if (is.na(new.srid)) {
            cli::cli_abort("No available SRIDs between 880001 and 889999. Delete some or manually set 'new.srid'.")
        } else {
            srid <- new.srid
        }
    }
    proj.wkt <- "NA"
    proj.wkt <- sf::st_crs(p4s)$Wkt
    ## insert new SRID
    temp.query <- paste0("INSERT INTO spatial_ref_sys (srid,auth_name,auth_srid,srtext,proj4text) VALUES (",
                         srid, ",'rpostgis_custom',", srid, ",'", proj.wkt, "','",
                         p4s, "');")
    dbSendQuery(conn, temp.query)
    cli::cli_alert_success("No matches were found in spatial_ref_sys. New SRID created ({srid}).")
    return(srid)
}
