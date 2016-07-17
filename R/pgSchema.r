## pgSchema
##
##' Checks the existence, and if necessary, create a schema.
##'
##' @title Check and create schema.
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL schema
##'     name.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-createschema.html}
##' @return \code{TRUE} if the schema exists (whether it was already
##'     available or was just created).
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' pgSchema(name = "schema", exec = FALSE)
pgSchema <- function(conn, name, display = TRUE, exec = TRUE) {
    ## Check the name of the schema
    if (length(name) != 1)
        stop("The schema name should be of length 1.")
    ## Check existence of the schema
    str <- paste0("SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = '",
        name, "');")
    schema <- dbGetQuery(conn, str)[1, 1]
    ## If exists, return TRUE, otherwise create the schema
    if (isTRUE(schema))
        return(TRUE) else {
        ## Build the query
        str <- paste0("CREATE SCHEMA ", name, ";")
        ## Display the query
        if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "),
                "executed:"))
            message(str)
            message("--")
        }
        ## Execute the query
        if (exec)
            dbSendQuery(conn, str)
        ## Return nothing
        return(TRUE)
    }
}
