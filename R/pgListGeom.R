## pgListGeom

##' List geometries.
##'
##' List all geometries in a PostGIS database.
##'
##' @param conn A PostgreSQL database connection.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return If \code{exec = TRUE}, a data frame with schema, table,
##'     geometry column, and geometry type.
##' @author David Bucklin \email{dbucklin@@ufl.edu}
##' @export
##' @examples
##' \dontrun{
##' pgListGeom(conn)
##' }

pgListGeom <- function(conn, display = TRUE, exec = TRUE) {
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
    ## FROM public.geometry_columns;
    ## --
    tmp.query <- paste("SELECT", "    f_table_schema AS schema_name,",
        "    f_table_name AS table_name,", "    f_geometry_column AS geom_column,",
        "    type AS geometry_type", "FROM public.geometry_columns;",
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
