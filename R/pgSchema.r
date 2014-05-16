## pgSchema
##
##' Create a schema.
##'
##' @title Create schema
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL schema
##' name.
##' @param display Logical. Whether to display the query (defaults to
##' \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##' \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##' \url{http://www.postgresql.org/docs/current/static/sql-createschema.html}
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' pgSchema(name = "schema", exec = FALSE)
pgSchema <- function(conn, name, display = TRUE, exec = TRUE)
{
    ## Check the name of the schema
    if (length(name) != 1)
        stop("The schema name should be of length 1.")
    ## Build the query
    str <- paste0("CREATE SCHEMA ", name, ";")
    ## Display the query
    if (display)
        cat(paste0("Query ", ifelse(exec, "", "not "), "executed:\n",
            str, "\n--\n"))
    ## Execute the query
    if (exec)
        dbSendQuery(conn, str)
    ## Return nothing
    return(invisible())
}
