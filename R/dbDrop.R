## dbDrop

##' Drop table/view/schema.
##'
##' Drop a table, a view or a schema.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table, view
##'     or schema name.
##' @param type The type of the object to comment, either \code{table}
##'     or \code{view}
##' @param ifexists Do not throw an error if the table does not
##'     exist. A notice is issued in this case.
##' @param cascade Automatically drop objects that depend on the table
##'     (such as views).
##' @param display Logical. Whether to display the query (defaults to
##'     \code{TRUE}).
##' @param exec Logical. Whether to execute the query (defaults to
##'     \code{TRUE}).
##' @return \code{TRUE} if the table/view/schema was successfully
##'     dropped.
##' @seealso The PostgreSQL documentation:
##'     \url{http://www.postgresql.org/docs/current/static/sql-droptable.html},
##'     \url{http://www.postgresql.org/docs/current/static/sql-dropview.html},
##'     \url{http://www.postgresql.org/docs/current/static/sql-dropschema.html}
##' @author Mathieu Basille \email{basille@@ufl.edu}
##' @export
##' @examples
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbDrop(conn, name = c("fla", "bli"), type = "view", exec = FALSE)
##' dbDrop(conn, name = "fla", type = "schema", cascade = "TRUE", exec = FALSE)

dbDrop <- function(conn, name, type = c("table", "view", "schema"),
    ifexists = FALSE, cascade = FALSE, display = TRUE, exec = TRUE) {
    type <- toupper(match.arg(type))
    ## Check and prepare name
    if (type %in% c("TABLE","VIEW")) {
      name <- dbTableNameFix(name)
      nameque <- paste(name, collapse = ".")
    } else {
      if (length(name) > 1) {stop("Schemas should be a character of length = 1.")}
      nameque<-DBI::dbQuoteIdentifier(conn,name)
    }
    ## Argument IF EXISTS
    ifexists <- ifelse(ifexists, " IF EXISTS ", " ")
    ## Argument CASCADE
    cascade <- ifelse(cascade, " CASCADE", "")
    ## Build the query
    tmp.query <- paste0("DROP ", type, ifexists, nameque, cascade,
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
    ## Return nothing
    return(TRUE)
}
