## dbIndex

##' Create an index.
##'
##' Defines a new index on a PostgreSQL table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column to which the key will be associated.
##' @param idxname A character string specifying the name of the index
##'     to be created. By default, this is the name of the table
##'     (without the schema) suffixed by \code{_idx}.
##' @param unique Logical. Causes the system to check for duplicate
##'     values in the table when the index is created (if data already
##'     exist) and each time data is added. Attempts to insert or
##'     update data which would result in duplicate entries will
##'     generate an error.
##' @param method The name of the method to be used for the
##'     index. Choices are \code{"btree"}, \code{"hash"},
##'     \code{"rtree"}, and \code{"gist"}. The default method is
##'     \code{"btree"}, although \code{"gist"} should be the index of
##'     choice for Post GIS spatial types (geometry, geography,
##'     raster).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the index was successfully created.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-createindex.html};
##'     the PostGIS documentation for GiST indexes:
##'     \url{http://postgis.net/docs/using_postgis_dbmanagement.html#id541286}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' dbIndex(name = c("fla", "bli"), colname = "geom", method = "gist",
##'     exec = FALSE)

dbIndex <- function(conn, name, colname, idxname, unique = FALSE,
    method = c("btree", "hash", "rtree", "gist"), display = TRUE,
    exec = TRUE) {
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2) {
        table <- paste(name, collapse = ".")
    } else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## Check the name of the column
    if (missing(colname))
        stop("'colname' not provided.")
    ## Check and prepare the name of the index
    if (missing(idxname))
        idxname <- paste(name[length(name)], colname, "idx",
            sep = "_")
    ## Argument UNIQUE
    unique <- ifelse(unique, "UNIQUE ", "")
    ## Check and prepare the method for the index
    method <- match.arg(method)
    usemeth <- ifelse(method == "btree", "", paste(" USING",
        toupper(method)))
    ## Build the query
    tmp.query <- paste0("CREATE ", unique, "INDEX ", idxname,
        " ON ", table, usemeth, " (", colname, ");")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec)
        dbSendQuery(conn, tmp.query)
    ## Return TRUE
    return(TRUE)
}
