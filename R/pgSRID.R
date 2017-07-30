## pgSRID

##' Find (or create) PostGIS SRID based on CRS object.
##'
##' This function takes \code{\link[sp]{CRS}}-class object and a
##' PostgreSQL database connection (with PostGIS extension), and
##' returns the matching SRID(s) for that CRS. If a match is not
##' found, a new entry can be created in the PostgreSQL
##' \code{spatial_ref_sys} table using the parameters specified by the
##' CRS. New entries will be created with \code{auth_name =
##' 'rpostgis_custom'}, with the default value being the next open value
##' between 880001-889999 (a different SRID value can be entered if desired.)
##'
##' @param conn A connection object to a PostgreSQL database.
##' @param crs CRS object, created through a call to
##'     \code{\link[sp]{CRS}}.
##' @param create.srid Logical. If no matching SRID is found, should a new
##'     SRID be created? User must have write access on
##'     \code{spatial_ref_sys} table.
##' @param new.srid Integer. Optional SRID to give to a newly created
##'     SRID. If left NULL (default), the next open value of
##'     \code{srid} in \code{spatial_ref_sys} between 880001 and
##'     889999 will be used.
##' @return SRID code (integer).
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @export
##' @importFrom sp CRS
##' @examples
##' \dontrun{
##' drv <- dbDriver("PostgreSQL")
##' conn <- dbConnect(drv, dbname = "dbname", host = "host", port = "5432",
##'     user = "user", password = "password")
##' (crs <- CRS("+proj=longlat"))
##' pgSRID(conn, crs)
##' (crs2 <- CRS(paste("+proj=stere", "+lat_0=52.15616055555555 +lon_0=5.38763888888889",
##'     "+k=0.999908 +x_0=155000 +y_0=463000", "+ellps=bessel",
##'     "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
##'     "+units=m")))
##' pgSRID(conn, crs2, create.srid = TRUE)
##' }

pgSRID <- function(conn, crs, create.srid = FALSE, new.srid = NULL) {
    ## Check if PostGIS is enabled
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    ## check object
    if (!inherits(crs, "CRS")) {
        stop("Object is not of class CRS.")
    }
    ## extract p4s
    p4s <- crs@projargs
    ## if crs is undefined (NA), return 0
    if (is.na(p4s)) {
        srid <- 0
        message("CRS undefined (NA).")
        return(srid)
    }
    ## check if can extract EPSG directly
    epsg.ext <- regmatches(p4s, regexpr("init=epsg:(\\d*)", p4s))
    if (length(epsg.ext) == 1) {
        epsg <- strsplit(epsg.ext, ":")[[1]][2]
        temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE auth_name = 'EPSG' AND auth_srid = ", 
            epsg, ";")
        srid <- dbGetQuery(conn, temp.query)$srid
        if (length(srid) > 0) {
            return(srid)
        }
    }
    ## check for matching p4s in spatial_ref_sys (with or without
    ## trailing white space)
    temp.query <- paste0("SELECT srid FROM spatial_ref_sys\nWHERE\n(proj4text = '", 
        p4s, "'\n OR\n regexp_replace(proj4text,'[[:space:]]+$','') = '", 
        p4s, "');")
    srid <- dbGetQuery(conn, temp.query)$srid
    
    if (length(srid) > 0) {
        return(srid)
    }
    ## check for matching EPSG with showEPSG (rgdal dependency)
    if (suppressPackageStartupMessages(requireNamespace("rgdal", 
        quietly = TRUE))) {
        message("Using function 'rgdal::showEPSG' to look for a match.")
        epsg <- "OGRERR_UNSUPPORTED_SRS"
        try(epsg <- rgdal::showEPSG(p4s))
        if (epsg != "OGRERR_UNSUPPORTED_SRS") {
            temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE auth_name = 'EPSG' AND auth_srid = ", 
                epsg, ";")
            srid <- dbGetQuery(conn, temp.query)$srid
            if (length(srid) > 0) {
                return(srid)
            }
        }
    }
    if (!create.srid) {
        stop("No SRID matches found. Re-run with 'create.srid = TRUE' to create new SRID entry in spatial_ref_sys.")
    }
    ## if none of the above methods worked, create new SRID
    if (!is.null(new.srid)) {
        ## check if exists
        temp.query <- paste0("SELECT srid FROM spatial_ref_sys WHERE srid = ", 
            new.srid, ";")
        check.srid <- dbGetQuery(conn, temp.query)
        if (length(check.srid) > 0) {
            stop(paste0("SRID ", new.srid, " already exists in 'spatial_ref_sys'.\nSelect another 'new.srid' or leave it to 'NULL' to select the next open SRID between 880000 and 889999."))
        }
        srid <- new.srid
    } else {
        ## find next SRID for custom set (prefix 88, first value =
        ## 880001)
        temp.query <- "SELECT min(series) AS new FROM generate_series(880001,890000) AS series WHERE series NOT IN\n  (SELECT srid FROM spatial_ref_sys WHERE srid > 880000 AND srid < 890000)"
        new.srid <- dbGetQuery(conn, temp.query)$new
        if (is.na(new.srid)) {
            stop("No available SRIDs between 880001 and 889999. Delete some or manually set 'new.srid'.")
        } else {
            srid <- new.srid
        }
    }
    proj.wkt <- "NA"
    if (suppressPackageStartupMessages(requireNamespace("rgdal", 
        quietly = TRUE))) {
        try(proj.wkt <- rgdal::showWKT(p4s))
    } else {
        message("Package 'rgdal' is not installed.\nNew SRID will be created, but 'srtext' column (WKT representation of projection) will be 'NA'.")
    }
    ## insert new SRID
    temp.query <- paste0("INSERT INTO spatial_ref_sys (srid,auth_name,auth_srid,srtext,proj4text) VALUES (", 
        srid, ",'rpostgis_custom',", srid, ",'", proj.wkt, "','", 
        p4s, "');")
    dbSendQuery(conn, temp.query)
    message(paste0("No matches were found in spatial_ref_sys. New SRID created (", 
        srid, ")."))
    return(srid)
}
