## pgAddKey
##
##' Add a primary or foreign key to a table column.
##'
##' @title Add key
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##' column to which the key will be assign.
##' @param type The type of the key, either \code{primary} or
##' \code{foreign}
##' @param reference A character string specifying a foreign table
##' name to which the foreign key will be associated.
##' @param colref A character string specifying the name of the
##' primary key in the foreign table to which the foreign key will be
##' associated.
##' @param display Logical. Whether to display the query (defaults to
##' \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##' \code{TRUE}).
##' @seealso The PostgreSQL documentation:
##' \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ase-research.org}
##' @export
##' @examples
##' pgAddKey(name = c("fla", "bli"), colname = "id", type = "foreign",
##'     reference = c("flu", "bla"), colref = "id", exec = FALSE)
pgAddKey <- function(conn, name, colname, type = c("primary",
    "foreign"), reference, colref, display = TRUE, exec = TRUE) {
    type <- toupper(match.arg(type))
    ## Check and prepare the schema.name
    if (length(name) %in% 1:2)
        table <- paste(name, collapse = ".")
    else stop("The table name should be \"table\" or c(\"schema\", \"table\").")
    ## If no reference, empty string
    if (missing(reference))
        references <- ""
    ## Else, check and prepare the schema.name of the reference table
    else {
        if (length(name) %in% 1:2)
            reftable <- paste(reference, collapse = ".")
        else stop("The reference table name should be \"table\" or c(\"schema\", \"table\").")
        references <- paste0(" REFERENCES ", reftable, " (",
            colref, ")")
    }
    ## Build the query
    str <- paste0("ALTER TABLE ", table, " ADD ", type, " KEY (",
        colname, ")", references, ";")
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
