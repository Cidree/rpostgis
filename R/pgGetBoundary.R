## pgGetBoundary

##' Retrieve bounding envelope of geometries or rasters.
##'
##' Retrieve bounding envelope (rectangle) of all geometries or
##' rasters in a PostGIS table as a \code{sfc} object.
##'
##' @param conn A connection object to a PostgreSQL database
##' @param name A character string specifying a PostgreSQL schema and
##'     table/view name holding the geometry (e.g., \code{name =
##'     c("schema","table")})
##' @param geom A character string specifying the name of the geometry column
##' in the table \code{name} (Default = \code{"geom"}). Note that for raster objects
##' you will need to change the default value
##' @param clauses character, additional SQL to append to modify select
##'     query from table. Must begin with an SQL clause (e.g., "WHERE ...",
##'     "ORDER BY ...", "LIMIT ..."); same usage as in \code{pgGetGeom}.
##' @param returnclass 'sf' by default; 'terra' for \code{SpatVector};
##'     or 'sp' for \code{sp} objects.
##' @author David Bucklin \email{david.bucklin@@gmail.com} and Adrian Cidre
##' González \email{adrian.cidre@@gmail.com}
##' @export
##' @return object of class sfc (list-column with geometries);
##'     SpatVector or sp object
##' @examples
##' \dontrun{
##' pgGetBoundary(conn, c("schema", "polys"), geom = "geom")
##' pgGetBoundary(conn, c("schema", "rasters"), geom = "rast")
##' }

pgGetBoundary <- function(conn, name, geom = "geom", clauses = NULL,
                          returnclass = "sf") {
  ## Message
  warn_deprecated_rc(
    returnclass,
    "pgGetBoundary(returnclass = 'must be a `sf` or `terra` object')"
  )

  dbConnCheck(conn)
  if (!suppressMessages(pgPostGIS(conn))) {
    cli::cli_abort("PostGIS is not enabled on this database.")
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
    cli::cli_abort("Table/view {namechar} is not listed in geometry_columns or raster_columns.")
  } else if (!geom %in% tab.list) {
    cli::cli_abort("Table/view {namechar} geometry/raster column not found.\nAvailable geometry/raster columns: {paste(tab.list, collapse = ', ')}")
  }
  geomque <- DBI::dbQuoteIdentifier(conn, geom)
  ## Check data type
  tmp.query <- paste0("SELECT DISTINCT pg_typeof(", geomque , ") AS type FROM ",
      nameque, "\n  WHERE ", geomque , " IS NOT NULL ",clauses,";")
  type <- suppressWarnings(dbGetQuery(conn, tmp.query))
  if (length(type$type) == 0) {
    cli::cli_abort("No records found.")
  } else if (type$type == "raster") {
      func <- "ST_Union"
  } else if (type$type == "geometry") {
      func <- "ST_Collect"
  } else if (type$type == "geography") {
      func <- "ST_Collect"
      geomque <- paste0(DBI::dbQuoteIdentifier(conn, geom),"::geometry")
  } else {
    cli::cli_abort("Column {geom} does not contain geometry/geography or rasters")
  }
  ## Retrieve the SRID
  tmp.query <- paste0("SELECT DISTINCT(ST_SRID(", geomque, ")) FROM ",
      nameque, " WHERE ", geomque, " IS NOT NULL ",clauses,";")
  srid <- dbGetQuery(conn, tmp.query)
  ## Check if the SRID is unique, otherwise throw an error
  if (nrow(srid) > 1) {
    cli::cli_abort("Multiple SRIDs in geometry/raster")
  } else if (nrow(srid) < 1) {
    cli::cli_abort("Database table is empty.")
  }
  ## use SRID
  srid_obj <- sf::st_crs()
  srid_obj <- sf::st_crs(as.character(NA))
  tmp.query <- paste0("SELECT auth_srid, auth_name FROM spatial_ref_sys WHERE srid = ", srid$st_srid, ";")
  db.srid <- dbGetQuery(conn, tmp.query)
  if (!is.null(db.srid)) {
    srid_code <- paste0(db.srid$auth_name, ":", db.srid$auth_srid)
    try(srid_obj <- sf::st_crs(srid_code), silent = TRUE)
    ## use only nb if it failed
    if (is.na(srid_obj$input)) try(srid_obj <- sf::st_crs(db.srid$auth_srid), silent = TRUE)
  }


  if (is.na(srid_obj)) {
    cli::cli_warn("Table SRID not found. Projection will be undefined (NA)")
  }
  ## Retrieve envelope
  tmp.query <- paste0("SELECT ST_Astext(ST_Envelope(", func,
      "(", geomque , "))) FROM ", nameque, " WHERE ", geomque , " IS NOT NULL ",clauses,";")
  wkt <- suppressWarnings(dbGetQuery(conn, tmp.query))
  env <- sf::st_as_sfc(wkt$st_astext, crs = srid_obj)

  # Return class
  if (returnclass == "sf") {
    return(env)
  } else if (returnclass == "sp") {
    return(sf::as_Spatial(env))
  } else if (returnclass == "terra") {
    return(terra::vect(env))
  } else {
    cli::cli_abort("`returnclass` must be 'sf' or 'terra'")
  }



}
