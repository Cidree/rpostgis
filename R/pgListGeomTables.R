## pgListGeomTables

##' List tables with geometry columns in the database.
##'
##' @param conn A PostgreSQL database connection
##' @return A data frame with schema, table, geometry column, and
##'     geometry type.
##' @export
##' @examples
##' \dontrun{
##' pgListGeomTables(conn)
##' }

pgListGeomTables <- function(conn) {
    if (suppressMessages(pgPostGIS(conn))) {
        tmp.query <- "SELECT f_table_schema AS schema_name, f_table_name AS table_name, f_geometry_column AS geom_column, type AS geometry_type\nFROM public.geometry_columns;"
        tab <- dbGetQuery(conn, tmp.query)
    } else {
        stop("PostGIS not enabled on this database.")
    }
    return(tab)
}
