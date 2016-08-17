## dbVacuum

##' Vacuum.
##'
##' Performs a VACUUM (garbage-collect and optionally analyze) on a
##' table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param full Logical. Whether to perform a "full" vacuum, which can
##'     reclaim more space, but takes much longer and exclusively
##'     locks the table.
##' @param verbose Logical. Whether to print a detailed vacuum
##'     activity report for each table.
##' @param analyze Logical. Whether to update statistics used by the
##'     planner to determine the most efficient way to execute a query
##'     (default to \code{TRUE}).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-vacuum.html}
##' @return TRUE if query is successfully executed.
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbVacuum(conn, name = c("fla", "bli"), full = TRUE, exec = FALSE)

dbVacuum <- function(conn, name, full = FALSE, verbose = FALSE,
    analyze = TRUE, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    ## Full VACUUM?
    full <- ifelse(full, "FULL ", "")
    ## Argument VERBOSE
    verbose <- ifelse(verbose, "VERBOSE ", "")
    ## Argument ANALYZE
    analyze <- ifelse(analyze, "ANALYZE ", "")
    ## Build the query
    tmp.query <- paste0("VACUUM ", full, verbose, analyze, nameque,
        ";")
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
