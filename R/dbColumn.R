## dbColumn

##' Add or remove a column.
##'
##' Add or remove a column to/from a table.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column to which the key will be associated.
##' @param action A character string specifying if the column is to be
##'     added (\code{"add"}, default) or removed (\code{"drop"}).
##' @param coltype A character string indicating the type of the
##'     column, if \code{action = "add"}.
##' @param cascade Logical. Whether to drop foreign key constraints of
##'     other tables, if \code{action = "drop"}.
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the column was successfully added or
##'     removed.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' ## Add an integer column
##' dbColumn(conn, name = c("fla", "bli"), colname = "field", exec = FALSE)
##' ## Drop a column (with CASCADE)
##' dbColumn(conn, name = c("fla", "bli"), colname = "field", action = "drop",
##'     cascade = TRUE, exec = FALSE)

dbColumn <- function(conn, name, colname, action = c("add", "drop"),
    coltype = "integer", cascade = FALSE, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    colname<-DBI::dbQuoteIdentifier(conn,colname)
    ## Check and translate to upper case the action
    action <- toupper(match.arg(action))
    ## 'args' for the coltype or cascade
    args <- ifelse(action == "ADD", coltype, ifelse(cascade,
        "CASCADE", ""))
    ## Build the query
    tmp.query <- paste0("ALTER TABLE ", nameque, " ", action, " COLUMN ",
        colname, " ", args, ";")
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
