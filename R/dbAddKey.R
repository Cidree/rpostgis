## dbAddKey

##' Add key.
##'
##' Add a primary or foreign key to a table column.
##'
##' @param conn A connection object.
##' @param name A character string, or a character vector, specifying
##'     a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column to which the key will be assign; alternatively, a
##'     character vector specifying the name of the columns for keys
##'     spanning more than one column.
##' @param type The type of the key, either \code{"primary"} or
##'     \code{"foreign"}
##' @param reference A character string specifying a foreign table
##'     name to which the foreign key will be associated (ignored if
##'     \code{type == "primary"}).
##' @param colref A character string specifying the name of the
##'     primary key in the foreign table to which the foreign key will
##'     be associated; alternatively, a character vector specifying
##'     the name of the columns for keys spanning more than one column
##'     (ignored if \code{type == "primary"}).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the key was successfully added.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-altertable.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## Examples use a dummy connection from DBI package
##' conn <- DBI::ANSI()
##'
##' ## Primary key
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = "id1", exec = FALSE)
##'
##' ## Primary key using multiple columns
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = c("id1", "id2",
##'     "id3"), exec = FALSE)
##'
##' ## Foreign key
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = "id", type = "foreign",
##'     reference = c("sch2", "tbl2"), colref = "id", exec = FALSE)
##'
##' ## Foreign key using multiple columns
##' dbAddKey(conn, name = c("sch1", "tbl1"), colname = c("id1", "id2"),
##'     type = "foreign", reference = c("sch2", "tbl2"), colref = c("id3",
##'         "id4"), exec = FALSE)
dbAddKey <- function(conn, name, colname, type = c("primary",
    "foreign"), reference, colref, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name and column name
    name <- dbTableNameFix(conn, name)
    nameque <- paste(name, collapse = ".")
    colname <- paste(DBI::dbQuoteIdentifier(conn, colname), collapse = ", ")
    ## Check 'type' and set it to upper case
    type <- toupper(match.arg(type))
    ## If primary key, both 'reference' and 'colref' are ignored
    ## (empty strings)
    if (type == "PRIMARY") {
        colref <- ""
        references <- ""
    ## If foreign key, check identifiers for 'reference' and 'colref'
    } else if (type == "FOREIGN") {
        colref <- paste(DBI::dbQuoteIdentifier(conn, colref),
            collapse = ", ")
        reference <- dbTableNameFix(conn, reference)
        references <- paste0(" REFERENCES ", paste(reference,
            collapse = "."), " (", colref, ")")
    }
    ## Build the query
    tmp.query <- paste0("ALTER TABLE ", nameque, " ADD ", type,
        " KEY (", colname, ")", references, ";")
    ## Display the query
    if (display) {
        message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
        message(tmp.query)
        message("--")
    }
    ## Execute the query
    if (exec) {
        dbConnCheck(conn)
        dbSendQuery(conn, tmp.query)
    }
    ## Return TRUE
    return(TRUE)
}
