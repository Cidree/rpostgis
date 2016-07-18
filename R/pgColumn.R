## pgColumn
##
##' Add or remove a column to/from a table.
##'
##' @title Add or remove a column
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##' column to which the key will be associated.
##' @param action A character string specifying if the column is to be
##' added (\code{"add"}, default) or removed (\code{"drop"}).
##' @param coltype A character string indicating the type of the
##' column, if \code{action = "add"}.
##' @param cascade Logical. Whether to drop foreign key constraints of
##' other tables, if \code{action = "drop"}.
##' @param display Logical. Whether to display the query (defaults to
##' \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##' \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##' \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Add an integer column
##' pgColumn(name = c("fla", "bli"), colname = "field", exec = FALSE)
##' ## Drop a column (with CASCADE)
##' pgColumn(name = c("fla", "bli"), colname = "field", action = "drop",
##'     cascade = TRUE, exec = FALSE)
pgColumn <- function(conn, name, colname, action = c("add", "drop"),
    coltype = "integer", cascade = FALSE, display = TRUE, exec = TRUE)
{
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2)
        table <- paste(name, collapse = ".")
    else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## Check and translate to upper case the action
    action <- toupper(match.arg(action))
    ## 'args' for the coltype or cascade
    args <- ifelse(action == "ADD", coltype, ifelse(cascade,
        "CASCADE", ""))
    ## Build the query
    str <- paste0("ALTER TABLE ", table, " ", action, " COLUMN ",
        colname, " ", args, ";")
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
