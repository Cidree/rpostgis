## pgListGeom

##' List geometries/rasters
##'
##' List all geometry/(geography) or raster columns available in a PostGIS database.
##'
##' @param conn A PostgreSQL database connection.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @param geog Logical. For pgListGeom, whether to include PostGIS geography-type 
##'     columns stored in the database
##' @return If \code{exec = TRUE}, a data frame with schema, table,
##'     geometry/(geography) or raster (for pgListRast) column, and geometry/(geography) type.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @examples
##' \dontrun{
##' pgListGeom(conn)
##' 
##' pgListRast(conn)
##' }

pgListGeom <- function(conn, display = TRUE, exec = TRUE, geog = FALSE) {
    dbConnCheck(conn)
    ## Check if PostGIS is enabled
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    ## SQL Query:
    ## --
    ## SELECT
    ##     f_table_schema AS schema_name,
    ##     f_table_name AS table_name,
    ##     f_geometry_column AS geom_column,
    ##     type AS geometry_type
    ## FROM geometry_columns;
    ## --
    if (!geog) end <- ";" else 
      end <- paste("UNION"," SELECT", "    f_table_schema AS schema_name,",
        "    f_table_name AS table_name,", "    f_geography_column AS geom_column,",
        "    type AS geometry_type,", "    'GEOGRAPHY'::character(9) AS type", "FROM geography_columns",
        "    ORDER BY type desc;",
        sep = "\n")
    tmp.query <- paste("SELECT", "    f_table_schema AS schema_name,",
        "    f_table_name AS table_name,", "    f_geometry_column AS geom_column,",
        "    type AS geometry_type,", "    'GEOMETRY'::character(8) AS type", "FROM geometry_columns ", end , 
        sep = "\n")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query and return it if successful
    if (exec) {
        tab <- dbGetQuery(conn, tmp.query)
        return(tab)
    }
}


# pgListRast

##' @rdname pgListGeom
pgListRast <- function(conn, display = TRUE, exec = TRUE) {
    dbConnCheck(conn)
    ## Check if PostGIS is enabled
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    ## SQL Query:
    ## --
    ## SELECT
    ##     r_table_schema AS schema_name,
    ##     r_table_name AS table_name,
    ##     r_raster_column AS raster_column
    ## FROM raster_columns;
    ## --

    tmp.query <- paste("SELECT", "    r_table_schema AS schema_name,",
        "    r_table_name AS table_name,", "    r_raster_column AS raster_column",
        "FROM raster_columns;",
        sep = "\n")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query and return it if successful
    if (exec) {
        tab <- dbGetQuery(conn, tmp.query)
        return(tab)
    }
}