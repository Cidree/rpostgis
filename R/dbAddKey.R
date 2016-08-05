## dbAddKey

##' Add key.
##'
##' Add a primary or foreign key to a table column.
##'
##' @param conn A connection object.
##' @param name A character string specifying a PostgreSQL table name.
##' @param colname A character string specifying the name of the
##'     column to which the key will be assign.
##' @param type The type of the key, either \code{primary} or
##'     \code{foreign}
##' @param reference A character string specifying a foreign table
##'     name to which the foreign key will be associated.
##' @param colref A character string specifying the name of the
##'     primary key in the foreign table to which the foreign key will
##'     be associated.
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
##' ## examples use a dummy connection from DBI package
##' conn<-DBI::ANSI()
##' dbAddKey(conn, name = c("fla", "bli"), colname = "id", type = "foreign",
##'     reference = c("flu", "bla"), colref = "id", exec = FALSE)

dbAddKey <- function(conn, name, colname, type = c("primary",
    "foreign"), reference, colref, display = TRUE, exec = TRUE) {
    ## Check and prepare the schema.name
    name <- dbTableNameFix(name)
    nameque <- paste(name, collapse = ".")
    colname<-DBI::dbQuoteIdentifier(conn,colname)
    if (missing(colref)) {
      # If no reference, empty string
      colref <- "" 
      } else { 
      colref<-DBI::dbQuoteIdentifier(conn,colref)
      }
    ## 'type' in upper case
    type <- toupper(match.arg(type))
    ## If no reference, empty string
    if (missing(reference)) {
        references <- ""
    } else {
        ## Else, check and prepare the schema.name of the reference
        ## table
        reference <- dbTableNameFix(reference)
        reftable <- paste(reference, collapse = ".")
        references <- paste0(" REFERENCES ", reftable, " (",
            colref, ")")
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
    if (exec)
        dbSendQuery(conn, tmp.query)
    ## Return TRUE
    return(TRUE)
}
