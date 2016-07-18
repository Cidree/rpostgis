## pgVacuum

##' Performs a VACUUM (garbage-collect and optionally analyze) on a
##' table.
##'
##' @title VACUUM
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
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' pgVacuum(name = c("fla", "bli"), full = TRUE, exec = FALSE)

pgVacuum <- function(conn, name, full = FALSE, verbose = FALSE,
    analyze = TRUE, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2) {
        table <- paste(name, collapse = ".")
    } else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## Full VACUUM?
    full <- ifelse(full, "FULL ", "")
    ## Argument VERBOSE
    verbose <- ifelse(verbose, "VERBOSE ", "")
    ## Argument ANALYZE
    analyze <- ifelse(analyze, "ANALYZE ", "")
    ## Build the query
    query <- paste0("VACUUM ", full, verbose, analyze, table,
        ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(query)
        message("--")
    }
    ## Execute the query
    if (exec)
        dbSendQuery(conn, query)
    ## Return nothing
    return(invisible())
}
