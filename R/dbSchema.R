## dbSchema

##' Check and create schema.
##'
##' Checks the existence, and if necessary, creates a schema.
##'
##' @param conn A connection object (required, even if \code{exec =
##'     FALSE}).
##' @param name A character string specifying a PostgreSQL schema
##'     name.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}). Note: if \code{exec = FALSE}, the function still
##'     checks the existence of the schema, but does not create it if
##'     it does not exists.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-createschema.html}
##' @return \code{TRUE} if the schema exists (whether it was already
##'     available or was just created).
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' \dontrun{
##'     dbSchema(name = "schema", exec = FALSE)
##' }

dbSchema <- function(conn, name, display = TRUE, exec = TRUE) {
    ## Check the name of the schema
    if (length(name) != 1)
        stop("The schema name should be of length 1.")
    ## make schema name
    namechar<-gsub("'","''",name)
    nameque<-DBI::dbQuoteIdentifier(conn,name)
    ## Check existence of the schema
    tmp.query <- paste0("SELECT EXISTS(SELECT 1 FROM pg_namespace WHERE nspname = '",
        namechar, "');")
    schema <- dbGetQuery(conn, tmp.query)[1, 1]
    ## If exists, return TRUE, otherwise create the schema
    if (isTRUE(schema))
        return(TRUE) else {
        ## Build the query
        tmp.query <- paste0("CREATE SCHEMA ", nameque[1], ";")
        ## Display the query
        if (display) {
            message(paste0("Query ", ifelse(exec, "", "not "),
                "executed:"))
            message(tmp.query)
            message("--")
        }
        ## Execute the query
        if (exec)
            dbSendQuery(conn, tmp.query)
        ## Return nothing
        return(TRUE)
    }
}
