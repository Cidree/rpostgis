## pgGetBoundary

##' Retrieve bounding envelope of geometries or rasters.
##'
##' Retrieve bounding envelope (rectangle) of all geometries or
##' rasters in a PostGIS table.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param geom character, Name of the column in \code{name} holding the
##'     geometry/(geography) or raster object (Default = \code{"geom"})
##' @param clauses character, additional SQL to append to modify select
##'     query from table. Must begin with an SQL clause (e.g., "WHERE ...",
##'     "ORDER BY ...", "LIMIT ..."); same usage as in \code{pgGetGeom}.
##' @author David Bucklin \email{david.bucklin@@gmail.com}
##' @importFrom sp CRS
##' @importFrom sp SpatialPolygons
##' @importFrom rgeos readWKT
##' @export
##' @return SpatialPolygon
##' @examples
##' \dontrun{
##' pgGetBoundary(conn, c("schema", "polys"), geom = "polygon")
##' pgGetBoundary(conn, c("schema", "rasters"), geom = "rast")
##' }

pgGetBoundary <- function(conn, name, geom = "geom", clauses = NULL) {
    dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
      stop("PostGIS is not enabled on this database.")
    }
    ## Check and prepare the schema.name
    nameque <- paste(dbTableNameFix(conn,name), collapse = ".")
    namechar <- dbQuoteString(conn, 
                  paste(dbTableNameFix(conn,name, as.identifier = FALSE), collapse = "."))
    ## prepare clauses
    if (!is.null(clauses)) clauses <- sub("^where", "AND", sub(";$","", sub("\\s+$","",clauses)),
                                          ignore.case = TRUE)
    
    ## Check table exists
    tmp.query <- paste0("SELECT geo FROM\n  (SELECT (gc.f_table_schema||'.'||gc.f_table_name) AS tab,
                        gc.f_geography_column AS geo\n  FROM geography_columns AS gc\n   UNION\n
                        SELECT (gc.f_table_schema||'.'||gc.f_table_name) AS tab,
                        gc.f_geometry_column AS geo\n  FROM geometry_columns AS gc\n   UNION\n   
                        SELECT rc.r_table_schema||'.'||rc.r_table_name AS tab, rc.r_raster_column AS geo\n   
                        FROM raster_columns as rc) a\n  WHERE tab  = ",
                        namechar, ";")
    tab.list <- dbGetQuery(conn, tmp.query)$geo
    if (is.null(tab.list)) {
        stop(paste0("Table/view ", namechar, " is not listed in geometry_columns or raster_columns."))
    } else if (!geom %in% tab.list) {
        stop(paste0("Table/view ", namechar, " geometry/raster column not found.\nAvailable geometry/raster columns: ",
            paste(tab.list, collapse = ", ")))
    }
    geomque <- DBI::dbQuoteIdentifier(conn, geom)
    ## Check data type
    tmp.query <- paste0("SELECT DISTINCT pg_typeof(", geomque , ") AS type FROM ",
        nameque, "\n  WHERE ", geomque , " IS NOT NULL ",clauses,";")
    type <- suppressWarnings(dbGetQuery(conn, tmp.query))
    if (length(type$type) == 0) {
        stop("No records found.")
    } else if (type$type == "raster") {
        func <- "ST_Union"
    } else if (type$type == "geometry") {
        func <- "ST_Collect"
    } else if (type$type == "geography") {
        func <- "ST_Collect"
        geomque <- paste0(DBI::dbQuoteIdentifier(conn, geom),"::geometry")
    } else {
        stop(paste0("Column", geom, " does not contain geometry/geography or rasters"))
    }
    ## Retrieve the SRID
    tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geomque, ")) FROM ",
        nameque, " WHERE ", geomque, " IS NOT NULL ",clauses,";")
    srid <- dbGetQuery(conn, tmp.query)
    ## Check if the SRID is unique, otherwise throw an error
    if (nrow(srid) > 1) {
      stop("Multiple SRIDs in geometry/raster")
    } else if (nrow(srid) < 1) {
      stop("Database table is empty.")
    }
    p4s <- sp::CRS(as.character(NA))@projargs
    tmp.query <- paste0("SELECT proj4text AS p4s FROM spatial_ref_sys WHERE srid = ",
                        srid$st_srid, ";")
    db.proj4 <- dbGetQuery(conn, tmp.query)$p4s
    if (!is.null(db.proj4)) {
      try(p4s <- sp::CRS(db.proj4)@projargs, silent = TRUE)
    }
    if (is.na(p4s)) {
      warning("Table SRID not found. Projection will be undefined (NA)")
    }
    ## Retrieve envelope
    tmp.query <- paste0("SELECT ST_Astext(ST_Envelope(", func,
        "(", geomque , "))) FROM ", nameque, " WHERE ", geomque , " IS NOT NULL ",clauses,";")
    wkt <- suppressWarnings(dbGetQuery(conn, tmp.query))
    env <- rgeos::readWKT(wkt$st_astext, p4s = p4s)
    return(env)
}
